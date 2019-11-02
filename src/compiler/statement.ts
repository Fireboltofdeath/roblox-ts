import ts from "typescript";
import {
	compileBlock,
	compileBreakStatement,
	compileClassDeclaration,
	compileContinueStatement,
	compileDoStatement,
	compileEnumDeclaration,
	compileExportAssignment,
	compileExportDeclaration,
	compileExpressionStatement,
	compileForOfStatement,
	compileForStatement,
	compileFunctionDeclaration,
	compileIfStatement,
	compileImportDeclaration,
	compileImportEqualsDeclaration,
	compileNamespaceDeclaration,
	compileReturnStatement,
	compileSwitchStatement,
	compileThrowStatement,
	compileTryStatement,
	compileVariableStatement,
	compileWhileStatement,
} from ".";
import { CompilerState } from "../CompilerState";
import { CompilerError, CompilerErrorType } from "../errors/CompilerError";
import { isTypeStatement } from "../utility/type";
import { getKindName } from "../utility/ast";

export function compileStatement(state: CompilerState, node: ts.Statement): string {
	if (isTypeStatement(node) || (ts.isAmbientableNode(node) && node.hasDeclareKeyword())) {
		return "";
	} else if (ts.isBlock(node)) {
		return compileBlock(state, node);
	} else if (ts.isImportDeclaration(node)) {
		return compileImportDeclaration(state, node);
	} else if (ts.isImportEqualsDeclaration(node)) {
		return compileImportEqualsDeclaration(state, node);
	} else if (ts.isExportDeclaration(node)) {
		return compileExportDeclaration(state, node);
	} else if (ts.isFunctionDeclaration(node)) {
		return compileFunctionDeclaration(state, node);
	} else if (ts.isClassDeclaration(node)) {
		return compileClassDeclaration(state, node);
	} else if (ts.isModuleDeclaration(node)) {
		return compileNamespaceDeclaration(state, node);
	} else if (ts.isDoStatement(node)) {
		return compileDoStatement(state, node);
	} else if (ts.isIfStatement(node)) {
		return compileIfStatement(state, node);
	} else if (ts.isBreakStatement(node)) {
		return compileBreakStatement(state, node);
	} else if (ts.isExpressionStatement(node)) {
		return compileExpressionStatement(state, node);
	} else if (ts.isContinueStatement(node)) {
		return compileContinueStatement(state, node);
	} else if (ts.isForInStatement(node)) {
		throw new CompilerError("For..in loops are disallowed!", node, CompilerErrorType.ForInLoop);
	} else if (ts.isForOfStatement(node)) {
		return compileForOfStatement(state, node);
	} else if (ts.isForStatement(node)) {
		return compileForStatement(state, node);
	} else if (ts.isReturnStatement(node)) {
		return compileReturnStatement(state, node);
	} else if (ts.isThrowStatement(node)) {
		return compileThrowStatement(state, node);
	} else if (ts.isVariableStatement(node)) {
		return compileVariableStatement(state, node);
	} else if (ts.isWhileStatement(node)) {
		return compileWhileStatement(state, node);
	} else if (ts.isEnumDeclaration(node)) {
		return compileEnumDeclaration(state, node);
	} else if (ts.isExportAssignment(node)) {
		return compileExportAssignment(state, node);
	} else if (ts.isSwitchStatement(node)) {
		return compileSwitchStatement(state, node);
	} else if (ts.isTryStatement(node)) {
		return compileTryStatement(state, node);
	} else if (ts.isLabeledStatement(node)) {
		throw new CompilerError("Labeled statements are not supported!", node, CompilerErrorType.NoLabeledStatement);
	}

	/* istanbul ignore next */
	if (
		ts.isEmptyStatement(node) ||
		ts.isTypeAliasDeclaration(node) ||
		ts.isInterfaceDeclaration(node)
	) {
		return "";
	}

	/* istanbul ignore next */
	throw new CompilerError(
		`Unexpected statement ( ${getKindName(node)} ) in compiledStatement`,
		node,
		CompilerErrorType.BadStatement,
		true,
	);
}

export function compileStatementedNode(state: CompilerState, node: ts.Node & ts.StatementedNode) {
	state.pushIdStack();
	state.exportStack.push(new Set<string>());
	let result = "";

	const shouldMakeHoistStack = !ts.isCaseClause(node) && !ts.isDefaultClause(node);

	if (shouldMakeHoistStack) {
		state.hoistStack.push(new Set<string>());
	}

	for (const child of node.statements) {
		result += compileStatement(state, child);
		if (ts.isReturnStatement(child) || ts.isBreakStatement(child)) {
			break;
		}
	}

	if (shouldMakeHoistStack) {
		result = state.popHoistStack(result);
	}

	const scopeExports = state.exportStack.pop();
	if (scopeExports && scopeExports.size > 0) {
		scopeExports.forEach(scopeExport => (result += state.indent + scopeExport));
	}
	state.popIdStack();
	return result;
}
