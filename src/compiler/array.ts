import ts from "typescript";
import { compileList, compileSpreadableListAndJoin, compileSpreadExpression, shouldCompileAsSpreadableList } from ".";
import { CompilerState } from "../CompilerState";
import { skipNodesDownwards } from "../utility/general";
import { getType, isArrayType } from "../utility/type";

export function compileArrayLiteralExpression(state: CompilerState, node: ts.ArrayLiteralExpression) {
	const elements = node.elements;
	if (elements.length === 0) {
		return "{}";
	}

	// optimizations
	if (elements.length === 1) {
		const element = elements[0];
		if (ts.isSpreadElement(element)) {
			const exp = skipNodesDownwards(element.expression);

			if (!isArrayType(getType(exp))) {
				return compileSpreadExpression(state, exp);
			}
		}
	}

	if (shouldCompileAsSpreadableList(elements)) {
		return compileSpreadableListAndJoin(state, elements);
	} else {
		return `{ ${compileList(state, elements).join(", ")} }`;
	}
}
