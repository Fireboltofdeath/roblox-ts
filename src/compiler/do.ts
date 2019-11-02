import ts from "typescript";
import { compileLoopBody, compileTruthyCheck } from ".";
import { CompilerState } from "../CompilerState";
import { removeBalancedParenthesisFromStringBorders, skipNodesDownwards } from "../utility/general";

export function compileDoStatement(state: CompilerState, node: ts.DoStatement) {
	state.pushIdStack();
	let result = state.indent + "repeat\n";
	state.pushIndent();
	result += state.indent + "do\n";
	state.pushIndent();
	result += compileLoopBody(state, node.statement);
	state.popIndent();
	result += state.indent + "end;\n";
	state.enterPrecedingStatementContext();
	const condition = removeBalancedParenthesisFromStringBorders(
		compileTruthyCheck(state, skipNodesDownwards(node.expression)),
	);
	result += state.exitPrecedingStatementContextAndJoin();
	state.popIndent();
	result += state.indent + `until not (${condition});\n`;
	state.popIdStack();
	return result;
}
