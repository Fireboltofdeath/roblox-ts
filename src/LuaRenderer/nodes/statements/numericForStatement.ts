import * as lua from "../../../LuaAST";
import { RenderState } from "../../RenderState";
import { render } from "../..";
import { renderStatements } from "../../util/statements";

export function renderNumericForStatement(state: RenderState, node: lua.NumericForStatement) {
	let result = "";

	const idStr = render(state, node.id);
	const minStr = render(state, node.min);
	const maxStr = render(state, node.max);

	let predicateStr: string;
	if (node.step) {
		const stepStr = render(state, node.step);
		predicateStr = `${minStr}, ${maxStr}, ${stepStr}`;
	} else {
		predicateStr = `${minStr}, ${maxStr}`;
	}

	result += state.indent + `for ${idStr} = ${predicateStr} do\n`;
	state.pushIndent();
	result += renderStatements(state, node.statements);
	state.popIndent();
	result += state.indent + `end\n`;

	return result;
}
