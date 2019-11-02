import ts from "typescript";
import {
	appendDeclarationIfMissing,
	compileCallArguments,
	compileCallArgumentsAndJoin,
	compileExpression,
	getReadableExpressionName,
	inheritsFromRoact,
} from ".";
import { CompilerState, DeclarationContext } from "../CompilerState";
import { CompilerError, CompilerErrorType } from "../errors/CompilerError";
import { joinIndentedLines, skipNodesDownwards, skipNodesUpwards } from "../utility/general";
import { suggest } from "../utility/text";
import { getType, inheritsFrom, isTupleType } from "../utility/type";

function compileMapElement(state: CompilerState, element: ts.Expression) {
	if (ts.isArrayLiteralExpression(element)) {
		const [key, value] = compileCallArguments(state, element.elements.map(e => skipNodesDownwards(e)));
		return `[${key}] = ${value};\n`;
	} else if (ts.isCallExpression(element) && isTupleType(element.getReturnType())) {
		const key = state.getNewId();
		const value = state.getNewId();
		state.pushPrecedingStatementToNewId(
			element,
			compileExpression(state, element).slice(2, -2),
			`${key}, ${value}`,
		);
		return `[${key}] = ${value};\n`;
	} else {
		const id = getReadableExpressionName(state, element, compileExpression(state, element));
		return `[${id}[1]] = ${id}[2];\n`;
	}
}

function compileSetElement(state: CompilerState, element: ts.Expression) {
	const [key] = compileCallArguments(state, [element]);
	return `[${key}] = true;\n`;
}

const compileMapSetElement = new Map<
	"set" | "map",
	{ addMethodName: string; compile: (state: CompilerState, element: ts.Expression) => string }
>([
	["map", { compile: compileMapElement, addMethodName: "set" }],
	["set", { compile: compileSetElement, addMethodName: "add" }],
]);

function compileSetMapConstructorHelper(
	state: CompilerState,
	node: ts.NewExpression,
	args: Array<ts.Expression>,
	type: "set" | "map",
	mode: "" | "k" | "v" | "kv" = "",
) {
	const preDeclaration = mode ? "setmetatable(" : "";
	const postDeclaration = mode ? `, { __mode = "${mode}" })` : "";

	const typeArgument = getType(node).getTypeArguments()[0];

	if (typeArgument.isNullable() || typeArgument.isUndefined()) {
		throw new CompilerError(
			`Cannot create a ${type} with a nullable index!`,
			node,
			CompilerErrorType.NullableIndexOnMapOrSet,
		);
	}

	const firstParam = skipNodesDownwards(args[0]) as ts.Expression | undefined;

	let exp: ts.Node = node;
	let parent = skipNodesUpwards(node.parent);
	const { compile: compileElement, addMethodName: addMethodName } = compileMapSetElement.get(type)!;

	while (ts.isPropertyAccessExpression(parent) && addMethodName === getName(parent)) {
		const grandparent = skipNodesUpwards(parent.parent!);
		if (ts.isCallExpression(grandparent)) {
			exp = grandparent;
			parent = skipNodesUpwards(grandparent.parent!);
		} else {
			break;
		}
	}

	const pushCondition = ts.isNewExpression(exp)
		? () => true
		: (declaration: DeclarationContext) => declaration.isIdentifier;

	if (
		firstParam &&
		(!ts.isArrayLiteralExpression(firstParam) ||
			firstParam.getChildrenOfKind(ts.SyntaxKind.SpreadElement).length > 0)
	) {
		state.usesTSLibrary = true;
		const id = state.pushToDeclarationOrNewId(
			exp,
			preDeclaration + `TS.${type}_new(${compileCallArgumentsAndJoin(state, args)})` + postDeclaration,
			pushCondition,
		);
		return id;
	} else {
		let id = "";
		const lines = new Array<string>();
		let hasContext = false;

		if (firstParam) {
			for (let element of firstParam.elements) {
				element = skipNodesDownwards(element);
				if (hasContext) {
					state.pushPrecedingStatements(exp, id + compileElement(state, element));
				} else {
					state.enterPrecedingStatementContext();
					const line = compileElement(state, element);
					const context = state.exitPrecedingStatementContext();

					if (context.length > 0) {
						hasContext = true;
						id = state.pushToDeclarationOrNewId(
							exp,
							preDeclaration + "{}" + postDeclaration,
							declaration => declaration.isIdentifier,
						);
						state.pushPrecedingStatements(
							exp,
							...lines.map(current => state.indent + id + current),
							...context,
							state.indent + id + line,
						);
					} else {
						lines.push(line);
					}
				}
			}
		}

		if (!hasContext) {
			id = state.pushToDeclarationOrNewId(
				exp,
				lines.length === 0
					? preDeclaration + "{}" + postDeclaration
					: preDeclaration +
							lines.reduce(
								(result, line) => result + state.indent + joinIndentedLines([line], 1),
								"{\n",
							) +
							state.indent +
							"}" +
							postDeclaration,

				pushCondition,
			);
		}

		return id;
	}
}

const ARRAY_NIL_LIMIT = 200;

export function compileNewExpression(state: CompilerState, node: ts.NewExpression) {
	const expNode = skipNodesDownwards(node.expression);
	const expressionType = getType(expNode);
	const name = compileExpression(state, expNode);
	const args = node.getFirstChildByKind(ts.SyntaxKind.OpenParenToken)
		? (node.arguments.map(arg => skipNodesDownwards(arg)) as Array<ts.Expression>)
		: [];

	if (inheritsFromRoact(expressionType)) {
		throw new CompilerError(
			`Roact components cannot be created using new\n` +
				suggest(`Proper usage: Roact.createElement(${name}), <${name}></${name}> or <${name}/>`),
			node,
			CompilerErrorType.RoactNoNewComponentAllowed,
		);
	}

	if (inheritsFrom(expressionType, "ArrayConstructor")) {
		if (args.length === 0) {
			return "{}";
		}

		let result = `{`;
		if (args.length === 1) {
			const arg = args[0];
			if (
				ts.isNumericLiteral(arg) &&
				arg.getText().match(/^\d+$/) &&
				arg.getLiteralValue() <= ARRAY_NIL_LIMIT
			) {
				const literalValue = arg.getLiteralValue();
				if (literalValue !== 0) {
					result += ", nil".repeat(literalValue).substring(1) + " ";
				}
			} else {
				throw new CompilerError(
					"Invalid argument #1 passed into ArrayConstructor. Expected a simple integer fewer or equal to " +
						ARRAY_NIL_LIMIT +
						".",
					node,
					CompilerErrorType.BadBuiltinConstructorCall,
				);
			}
		} else if (args.length !== 0) {
			throw new CompilerError(
				"Invalid arguments passed into ArrayConstructor!",
				node,
				CompilerErrorType.BadBuiltinConstructorCall,
			);
		}

		return appendDeclarationIfMissing(state, skipNodesUpwards(node.parent), result + `}`);
	}

	if (inheritsFrom(expressionType, "MapConstructor") || inheritsFrom(expressionType, "ReadonlyMapConstructor")) {
		return appendDeclarationIfMissing(
			state,
			skipNodesUpwards(node.parent),
			compileSetMapConstructorHelper(state, node, args, "map"),
		);
	}

	if (inheritsFrom(expressionType, "SetConstructor") || inheritsFrom(expressionType, "ReadonlySetConstructor")) {
		return appendDeclarationIfMissing(
			state,
			skipNodesUpwards(node.parent),
			compileSetMapConstructorHelper(state, node, args, "set"),
		);
	}

	if (inheritsFrom(expressionType, "WeakMapConstructor")) {
		return appendDeclarationIfMissing(
			state,
			skipNodesUpwards(node.parent),
			compileSetMapConstructorHelper(state, node, args, "map", "k"),
		);
	}

	if (inheritsFrom(expressionType, "WeakSetConstructor")) {
		return appendDeclarationIfMissing(
			state,
			skipNodesUpwards(node.parent),
			compileSetMapConstructorHelper(state, node, args, "set", "k"),
		);
	}

	return `${name}.new(${compileCallArgumentsAndJoin(state, args)})`;
}
