import ts from "typescript";
import { CompilerState } from "../CompilerState";

export function compileBreakStatement(state: CompilerState, node: ts.BreakStatement) {
	return state.indent + "break;\n";
}
