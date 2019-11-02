/* istanbul ignore file */

import ts from "typescript";
import { compileExpression } from ".";
import { CompilerState } from "../CompilerState";
import { skipNodesDownwards } from "../utility/general";

export function compileAwaitExpression(state: CompilerState, node: ts.AwaitExpression) {
	const expStr = compileExpression(state, skipNodesDownwards(node.expression));
	state.usesTSLibrary = true;
	return `TS.await(${expStr})`;
}
