import ts from "typescript";
import { compileExpression, compileMethodDeclaration } from ".";
import { CompilerState } from "../CompilerState";
import { CompilerError, CompilerErrorType } from "../errors/CompilerError";
import { joinIndentedLines, safeLuaIndex, skipNodesDownwards } from "../utility/general";
import { getKindName } from "../utility/ast";

function assignMembers(state: CompilerState, from: string, target: string) {
	state.pushIdStack();
	const i = state.getNewId();
	const v = state.getNewId();
	const str = state.indent + `for ${i}, ${v} in pairs(${from}) do ${target}[${i}] = ${v}; end;\n`;
	state.popIdStack();
	return str;
}

export function compileObjectLiteralExpression(state: CompilerState, node: ts.ObjectLiteralExpression) {
	const properties = node.properties;

	if (properties.length === 0) {
		return "{}";
	}

	const lines = new Array<string>();
	let hasContext = false;
	let id = "";
	let line: string;

	for (const prop of properties) {
		const context = state.enterPrecedingStatementContext();

		if (ts.isPropertyAssignment(prop) || ts.isShorthandPropertyAssignment(prop)) {
			let lhs: ts.Expression;
			let n = 0;
			let child = prop.getChildAtIndex(n);

			while (ts.isJSDoc(child)) {
				child = prop.getChildAtIndex(++n);
			}

			child = skipNodesDownwards(child);

			if (ts.isComputedPropertyName(child)) {
				lhs = skipNodesDownwards(child.expression);
			} else if (
				ts.isIdentifier(child) ||
				ts.isStringLiteral(child) ||
				ts.isNumericLiteral(child)
			) {
				lhs = child;
			} else {
				throw new CompilerError(
					`Unexpected type of object index! (${getKindName(child)})`,
					child,
					CompilerErrorType.UnexpectedObjectIndex,
				);
			}

			let rhs: ts.Expression; // You may want to move this around
			if (ts.isShorthandPropertyAssignment(prop) && ts.isIdentifier(child)) {
				lhs = prop.name;
				rhs = child;
			} else {
				rhs = skipNodesDownwards(prop.getInitializerOrThrow());
			}

			let lhsStr = compileExpression(state, lhs);
			state.enterPrecedingStatementContext();
			const rhsStr = compileExpression(state, rhs);
			const rhsContext = state.exitPrecedingStatementContext();

			if (rhsContext.length > 0) {
				if (!ts.isIdentifier(child) && !context.isPushed) {
					lhsStr = state.pushPrecedingStatementToNewId(lhs, lhsStr);
				}
				context.push(...rhsContext);
				context.isPushed = rhsContext.isPushed;
			}

			line = `${
				ts.isIdentifier(child) ? safeLuaIndex("", lhs.getText()) : `[${lhsStr}]`
				} = ${rhsStr};\n`;
		} else if (ts.isMethodDeclaration(prop)) {
			line = "";
		} else if (ts.isSpreadAssignment(prop)) {
			line = compileExpression(state, skipNodesDownwards(prop.expression));
		} else {
			throw new CompilerError(
				`Unexpected property type in object! Got ${getKindName(prop)}`,
				prop,
				CompilerErrorType.BadObjectPropertyType,
				true,
			);
		}

		state.exitPrecedingStatementContext();

		if (
			hasContext ||
			context.length > 0 ||
			ts.isSpreadAssignment(prop) ||
			ts.isMethodDeclaration(prop)
		) {
			if (!hasContext) {
				id = state.pushToDeclarationOrNewId(node, "{}", declaration => declaration.isIdentifier);
			}

			if (ts.isSpreadAssignment(prop)) {
				line = assignMembers(state, line, id);
			} else if (ts.isMethodDeclaration(prop)) {
				line = state.indent + compileMethodDeclaration(state, prop, id).trimLeft();
			} else {
				line = state.indent + id + (line.startsWith("[") ? "" : ".") + line;
			}

			if (hasContext) {
				state.pushPrecedingStatements(node, ...context, line);
			} else {
				state.pushPrecedingStatements(
					node,
					...lines.map(current => state.indent + id + (current.startsWith("[") ? "" : ".") + current),
					...context,
					line,
				);
				hasContext = true;
			}
		} else {
			lines.push(line);
		}
	}

	if (id) {
		state.getCurrentPrecedingStatementContext(node).isPushed = true;
		return id;
	} else {
		return "{\n" + lines.map(myLine => state.indent + joinIndentedLines([myLine], 1)).join("") + state.indent + "}";
	}
}
