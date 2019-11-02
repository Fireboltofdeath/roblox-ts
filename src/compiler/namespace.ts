import ts from "typescript";
import { checkReserved, compileStatementedNode } from ".";
import { CompilerState } from "../CompilerState";
import { CompilerError, CompilerErrorType } from "../errors/CompilerError";
import { isTypeOnlyNamespace } from "../utility/type";
import { getKindName, getFirstAncestorByKind } from "../utility/ast";

function safeMapGet<T, R>(map: Map<T, R>, key: T, node: ts.Node) {
	const find = map.get(key);
	if (!find) {
		throw new CompilerError(
			`Failed to find context for ${getKindName(node)} ${node.getText()}`,
			node,
			CompilerErrorType.BadContext,
		);
	}
	return find;
}

export function compileNamespaceDeclaration(state: CompilerState, node: ts.ModuleDeclaration) {
	if (isTypeOnlyNamespace(node)) {
		return "";
	}
	state.pushIdStack();
	const name = checkReserved(node.name);
	const parentNamespace = getFirstAncestorByKind(node, ts.SyntaxKind.ModuleDeclaration);
	state.pushExport(name, node);
	state.pushHoistStack(name);
	let result = "";
	const id = state.getNewId();
	const previousName = state.namespaceStack.get(name);
	if (parentNamespace) {
		const parentName = safeMapGet(state.namespaceStack, getName(parentNamespace), node);
		result += state.indent + `${name} = ${parentName}.${name} or {} do\n`;
	} else {
		result += state.indent + `${name} = ${name} or {} do\n`;
	}
	state.namespaceStack.set(name, id);
	state.pushIndent();
	result += state.indent + `local ${id} = ${name};\n`;
	result += compileStatementedNode(state, node);
	if (previousName) {
		state.namespaceStack.set(name, previousName);
	} else {
		state.namespaceStack.delete(name);
	}
	state.popIndent();
	result += state.indent + `end;\n`;
	state.popIdStack();
	return result;
}
