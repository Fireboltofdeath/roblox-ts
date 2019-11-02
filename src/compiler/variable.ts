import ts from "typescript";
import { checkReserved, compileCallExpression, compileExpression, concatNamesAndValues } from ".";
import { CompilerState } from "../CompilerState";
import { CompilerError, CompilerErrorType } from "../errors/CompilerError";
import { skipNodesDownwards, skipNodesUpwards } from "../utility/general";
import {
	getType,
	isArrayType,
	isGeneratorType,
	isIterableFunctionType,
	isMapType,
	isObjectType,
	isSetType,
	isTupleType,
	shouldHoist,
} from "../utility/type";
import { compileBindingPatternAndJoin } from "./binding";
import { isValidLuaIdentifier } from "./security";
import { isThisExpression } from "../utility/ast";

export function compileVariableDeclaration(state: CompilerState, node: ts.VariableDeclaration) {
	state.enterPrecedingStatementContext();
	const lhs = node.name;
	const rhs = skipNodesDownwards(node.initializer);

	const parent = skipNodesUpwards(node.parent);
	const grandParent = skipNodesUpwards(parent.parent);
	const isExported = ts.isVariableStatement(grandParent) && grandParent.isExported();

	let decKind = ts.VariableDeclarationKind.Const;
	if (ts.isVariableDeclarationList(parent)) {
		decKind = parent.getDeclarationKind();
	}

	if (ts.isArrayBindingPattern(lhs)) {
		const isFlatBinding = lhs
			.elements
			.filter(v => ts.isBindingElement(v))
			.every(v => ts.isIdentifier(v.getChildAtIndex(0)));

		if (isFlatBinding && rhs && ts.isCallExpression(rhs) && isTupleType(getType(rhs))) {
			const names = new Array<string>();
			const values = new Array<string>();
			for (const element of lhs.elements) {
				if (ts.isBindingElement(element)) {
					const nameNode = element.name;
					if (ts.isIdentifier(nameNode)) {
						checkReserved(nameNode);
						names.push(compileExpression(state, nameNode));
					}
				} else if (ts.isOmittedExpression(element)) {
					names.push("_");
				}
			}
			values.push(compileCallExpression(state, rhs, true));
			if (isExported && decKind === ts.VariableDeclarationKind.Let) {
				let returnValue: string | undefined;
				concatNamesAndValues(state, names, values, false, str => (returnValue = str));
				return state.exitPrecedingStatementContextAndJoin() + returnValue || "";
			} else {
				if (isExported && ts.isVariableStatement(grandParent)) {
					names.forEach(name => state.pushExport(name, grandParent));
				}
				let returnValue: string | undefined;
				concatNamesAndValues(state, names, values, true, str => (returnValue = str));
				return state.exitPrecedingStatementContextAndJoin() + returnValue || "";
			}
		}
	}

	let result = "";
	if (ts.isIdentifier(lhs)) {
		const name = checkReserved(lhs);
		if (rhs) {
			if (isExported && decKind === ts.VariableDeclarationKind.Let) {
				const parentName = state.getExportContextName(grandParent);
				state.declarationContext.set(rhs, {
					isIdentifier: false,
					set: `${parentName}.${name}`,
				});
				const value = compileExpression(state, rhs);
				if (state.declarationContext.delete(rhs)) {
					result += state.indent + `${parentName}.${name} = ${value};\n`;
				}
			} else {
				if (isExported && ts.isVariableStatement(grandParent)) {
					state.pushExport(name, grandParent);
				}
				if (shouldHoist(grandParent, lhs)) {
					state.pushHoistStack(name);
					state.declarationContext.set(rhs, { isIdentifier: true, set: `${name}` });
					const value = compileExpression(state, rhs);
					if (state.declarationContext.delete(rhs)) {
						result += state.indent + `${name} = ${value};\n`;
					}
				} else {
					state.declarationContext.set(rhs, {
						isIdentifier: true,
						needsLocalizing: true,
						set: `${name}`,
					});
					const value = compileExpression(state, rhs);
					if (state.declarationContext.delete(rhs)) {
						result += state.indent + `local ${name} = ${value};\n`;
					}
				}
			}
		} else if (!isExported) {
			if (shouldHoist(grandParent, lhs)) {
				state.pushHoistStack(name);
			} else {
				result += state.indent + `local ${name};\n`;
			}
		}
	} else if ((ts.isArrayBindingPattern(lhs) || ts.isObjectBindingPattern(lhs)) && rhs) {
		// binding patterns MUST have rhs
		let rhsStr = compileExpression(state, rhs);

		if (!isValidLuaIdentifier(rhsStr)) {
			const id = state.getNewId();
			state.pushPrecedingStatements(node, state.indent + `local ${id} = ${rhsStr};\n`);
			rhsStr = id;
		}

		if (ts.isArrayBindingPattern(lhs)) {
			const rhsType = getType(rhs);
			if (
				!isArrayType(rhsType) &&
				!isMapType(rhsType) &&
				!isSetType(rhsType) &&
				!isGeneratorType(rhsType) &&
				!isIterableFunctionType(rhsType) &&
				(isObjectType(rhsType) || isThisExpression(rhs))
			) {
				state.usesTSLibrary = true;
				rhsStr = rhsStr;
				const id = state.getNewId();
				state.pushPrecedingStatements(
					node,
					state.indent + `local ${id} = ${rhsStr}[TS.Symbol_iterator](${rhsStr});\n`,
				);
				rhsStr = id;
			}
		}

		let exportVars: boolean;
		let noLocal: boolean;

		if (isExported) {
			if (decKind === ts.VariableDeclarationKind.Let) {
				exportVars = false;
				noLocal = true;
			} else {
				exportVars = true;
				noLocal = false;
			}
		} else {
			exportVars = false;
			noLocal = false;
		}

		result += compileBindingPatternAndJoin(state, lhs, rhsStr, exportVars, noLocal);
	}

	return state.exitPrecedingStatementContextAndJoin() + result;
}

export function compileVariableDeclarationList(state: CompilerState, node: ts.VariableDeclarationList) {
	const declarationKind = node.getDeclarationKind();
	if (declarationKind === ts.VariableDeclarationKind.Var) {
		throw new CompilerError(
			"'var' keyword is not supported! Use 'let' or 'const' instead.",
			node,
			CompilerErrorType.NoVarKeyword,
		);
	}

	return node
		.getDeclarations()
		.reduce((result, declaration) => result + compileVariableDeclaration(state, declaration), "");
}

export function compileVariableStatement(state: CompilerState, node: ts.VariableStatement) {
	const list = node.getFirstChildByKindOrThrow(ts.SyntaxKind.VariableDeclarationList);
	return compileVariableDeclarationList(state, list);
}
