import ts from "typescript";
import {
	checkApiAccess,
	checkNonAny,
	compileCallExpression,
	compileExpression,
	CompilerDirective,
	getPropertyAccessExpressionType,
	PropertyCallExpType,
	sanitizeTemplate,
	shouldWrapExpression,
} from ".";
import { CompilerState } from "../CompilerState";
import { CompilerError, CompilerErrorType } from "../errors/CompilerError";
import { safeLuaIndex, skipNodesDownwards } from "../utility/general";
import {
	getCompilerDirectiveWithLaxConstraint,
	getType,
	isArrayType,
	isArrayTypeLax,
	isConstantExpression,
	isMapType,
	isNumberTypeStrict,
	isNumericLiteralTypeStrict,
	isSetType,
	isStringType,
	isTupleReturnTypeCall,
} from "../utility/type";
import { getName, isConstEnum, getFirstAncestorByKind, isThisExpression, isSuperExpression, isTupleType } from "../utility/ast";

export function isIdentifierDefinedInConst(exp: ts.Identifier) {
	// I have no idea why, but getDefinitionNodes() cannot replace this
	for (const def of exp.getDefinitions()) {
		const node = def.getNode();
		// Namespace name identifiers are not variables which can be changed at run-time
		if (ts.isModuleDeclaration(node.parent!)) {
			return true;
		}

		const definition = getFirstAncestorByKind(node, ts.SyntaxKind.VariableDeclarationList);

		if (definition && definition.getDeclarationKind() === ts.VariableDeclarationKind.Const) {
			return true;
		}
	}
	return false;
}

export function isIdentifierDefinedInExportLet(exp: ts.Identifier) {
	// I have no idea why, but getDefinitionNodes() cannot replace this
	for (const def of exp.getDefinitions()) {
		const definition = getFirstAncestorByKind(def.getNode(), ts.SyntaxKind.VariableStatement);
		if (
			definition &&
			definition.hasExportKeyword() &&
			definition.getDeclarationKind() === ts.VariableDeclarationKind.Let
		) {
			return true;
		}
	}
	return false;
}

/**
 * Gets the writable operand name, meaning the code should be able to do `returnValue = x;`
 * The rule in this case is that if there is a depth of 3 or more, e.g. `Foo.Bar.i`, we push `Foo.Bar`
 */
export function getWritableOperandName(state: CompilerState, operand: ts.Expression, doNotCompileAccess = false) {
	if (ts.isPropertyAccessExpression(operand) || ts.isElementAccessExpression(operand)) {
		const child = skipNodesDownwards(operand.expression);

		if (
			!isThisExpression(child) &&
			!isSuperExpression(child) &&
			(!ts.isIdentifier(child) || isIdentifierDefinedInExportLet(child))
		) {
			const id = state.pushPrecedingStatementToNewId(operand, compileExpression(state, child));

			let propertyStr: string;
			if (doNotCompileAccess) {
				propertyStr = "";
			} else if (ts.isPropertyAccessExpression(operand)) {
				propertyStr = safeLuaIndex(" ", compileExpression(state, operand.name));
			} else {
				const exp = skipNodesDownwards(operand.getArgumentExpressionOrThrow());
				const fromNode = skipNodesDownwards(operand.expression);
				const access = getComputedPropertyAccess(state, exp, fromNode);
				propertyStr = `[${access}]`;
			}

			return { expStr: id + propertyStr, isIdentifier: false };
		} else if (doNotCompileAccess) {
			return { expStr: compileExpression(state, child), isIdentifier: false };
		} else if (ts.isElementAccessExpression(operand)) {
			const id = compileExpression(state, child);
			const exp = skipNodesDownwards(operand.getArgumentExpressionOrThrow());
			const fromNode = skipNodesDownwards(operand.expression);

			if (
				(isArrayType(getType(fromNode)) && !isNumericLiteralTypeStrict(getType(exp))) ||
				!isConstantExpression(exp, 0)
			) {
				const access = getComputedPropertyAccess(state, exp, fromNode);
				return {
					expStr: id + "[" + state.pushPrecedingStatementToNewId(exp, access) + "]",
					isIdentifier: false,
				};
			}
		}
	}

	return {
		expStr: compileExpression(state, operand),
		isIdentifier: ts.isIdentifier(operand) && !isIdentifierDefinedInExportLet(operand),
	};
}

/**
 * Similar to getWritableOperandName, but should push anything with any depth. This includes export let vars.
 */
export function getReadableExpressionName(
	state: CompilerState,
	exp: ts.Expression,
	expStr = compileExpression(state, exp),
) {
	const nonNullExp = skipNodesDownwards(exp);
	if (
		expStr.match(/^\(*_\d+\)*$/) ||
		(ts.isIdentifier(nonNullExp) && !isIdentifierDefinedInExportLet(nonNullExp)) ||
		isThisExpression(nonNullExp) ||
		isSuperExpression(nonNullExp) ||
		// We know that new Sets and Maps are already ALWAYS pushed
		(ts.isNewExpression(nonNullExp) && (isSetType(getType(exp)) || isMapType(getType(exp))))
	) {
		return expStr;
	} else {
		return state.pushPrecedingStatementToNewId(nonNullExp, expStr);
	}
}

export function compilePropertyAccessExpression(state: CompilerState, node: ts.PropertyAccessExpression) {
	const exp = skipNodesDownwards(node.expression);
	const propertyStr = getName(node);
	const expType = getType(exp);
	const propertyAccessExpressionType = getPropertyAccessExpressionType(state, node);

	if (
		getCompilerDirectiveWithLaxConstraint(expType, CompilerDirective.Array, t => isTupleType(t)) &&
		propertyStr === "length"
	) {
		throw new CompilerError(
			`Cannot access the \`length\` property of a tuple! Instead use \`${exp.getText()}.size()\``,
			node,
			CompilerErrorType.TupleLength,
		);
	} else if (propertyAccessExpressionType !== PropertyCallExpType.None) {
		throw new CompilerError(
			`Invalid property access! Cannot index non-member "${propertyStr}" (a roblox-ts macro function)`,
			node,
			CompilerErrorType.InvalidMacroIndex,
		);
	}

	const nameNode = node.name;
	checkApiAccess(state, nameNode);

	checkNonAny(exp);
	checkNonAny(nameNode);

	if (isSuperExpression(exp)) {
		return safeLuaIndex("self", propertyStr);
	}

	const symbol = expType.getSymbol();
	if (symbol) {
		const valDec = symbol.valueDeclaration;
		if (valDec) {
			if (
				ts.isFunctionDeclaration(valDec) ||
				ts.isArrowFunction(valDec) ||
				ts.isFunctionExpression(valDec) ||
				ts.isMethodDeclaration(valDec)
			) {
				throw new CompilerError("Cannot index a function value!", node, CompilerErrorType.NoFunctionIndex);
			} else if (ts.isEnumDeclaration(valDec)) {
				if (isConstEnum(valDec)) {
					const value = valDec.getMemberOrThrow(propertyStr).getValue();
					if (typeof value === "number") {
						return `${value}`;
					} else if (typeof value === "string") {
						return '"' + sanitizeTemplate(value) + '"';
					}
				}
			} else if (ts.isClassDeclaration(valDec)) {
				if (propertyStr === "prototype") {
					throw new CompilerError(
						"Class prototypes are not supported!",
						node,
						CompilerErrorType.NoClassPrototype,
					);
				}
			}
		}
	}

	let expStr = compileExpression(state, exp);

	if (shouldWrapExpression(exp, false)) {
		expStr = `(${expStr})`;
	}

	return expStr === "TS.Symbol" ? `${expStr}_${propertyStr}` : safeLuaIndex(expStr, propertyStr);
}

export function addOneToArrayIndex(valueStr: string) {
	if (!valueStr.includes("e") && !valueStr.includes("E")) {
		const valueNumber = Number(valueStr);
		if (!Number.isNaN(valueNumber)) {
			return (valueNumber + 1).toString();
		}
	}
	return valueStr + " + 1";
}

export function getComputedPropertyAccess(state: CompilerState, exp: ts.Expression, fromNode: ts.Node) {
	const expType = getType(exp);
	let expStr = compileExpression(state, exp);
	const fromType = getType(fromNode);

	if (isArrayType(fromType)) {
		if (isNumberTypeStrict(expType)) {
			expStr = addOneToArrayIndex(expStr);
		} else {
			throw new CompilerError(
				`Invalid indexing of ${fromType.getText()}. Got ${expType.getText()}, expected number`,
				exp,
				CompilerErrorType.InvalidComputedIndex,
			);
		}
	} else if (isSetType(fromType) || isMapType(fromType) || isStringType(fromType) || isArrayTypeLax(fromType)) {
		throw new CompilerError(
			`Invalid index type: ${expType.getText()}.` + ` Type ${fromType.getText()} is not indexable.`,
			exp,
			CompilerErrorType.InvalidComputedIndex,
		);
	}

	return expStr;
}

export function compileElementAccessBracketExpression(state: CompilerState, node: ts.ElementAccessExpression) {
	return getComputedPropertyAccess(
		state,
		skipNodesDownwards(node.getArgumentExpressionOrThrow()),
		skipNodesDownwards(node.expression),
	);
}

export function compileElementAccessDataTypeExpression(
	state: CompilerState,
	node: ts.ElementAccessExpression,
	expStr = "",
) {
	const expNode = skipNodesDownwards(checkNonAny(node.expression));

	if (expStr === "") {
		if (ts.isCallExpression(expNode) && isTupleReturnTypeCall(expNode)) {
			expStr = compileCallExpression(state, expNode, true);
			return (argExpStr: string) => (argExpStr === "1" ? `(${expStr})` : `(select(${argExpStr}, ${expStr}))`);
		} else {
			expStr = compileExpression(state, expNode);
		}
	}

	if (shouldWrapExpression(expNode, false)) {
		return (argExpStr: string) => `(${expStr})[${argExpStr}]`;
	} else {
		return (argExpStr: string) => `${expStr}[${argExpStr}]`;
	}
}

export function compileElementAccessExpression(state: CompilerState, node: ts.ElementAccessExpression) {
	return compileElementAccessDataTypeExpression(state, node)(compileElementAccessBracketExpression(state, node));
}
