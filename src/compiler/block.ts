import ts from "typescript";
import { compileStatementedNode } from ".";
import { CompilerState } from "../CompilerState";
import { getParentIfKind } from "../utility/ast";

export function compileBlock(state: CompilerState, node: ts.Block) {
	if (node.statements.length === 0) {
		return "";
	}
	let result = "";
	const parent = getParentIfKind(node, ts.SyntaxKind.SourceFile) || getParentIfKind(node, ts.SyntaxKind.Block);
	if (parent) {
		result += state.indent + "do\n";
		state.pushIndent();
	}
	result += compileStatementedNode(state, node);
	if (parent) {
		state.popIndent();
		result += state.indent + "end;\n";
	}
	return result;
}
