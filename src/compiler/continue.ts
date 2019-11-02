import ts from "typescript";
import { CompilerState } from "../CompilerState";

export function compileContinueStatement(state: CompilerState, node: ts.ContinueStatement) {
	return state.indent + `_continue_${state.continueId} = true; break;\n`;
}
