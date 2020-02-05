import * as lua from "LuaAST";
import { TransformState } from "TSTransformer";
import { transformExpression } from "TSTransformer/nodes/expressions/transformExpression";
import { transformLogical } from "TSTransformer/util/transformLogical";
import ts from "typescript";

const SIMPLE_OPERATOR_MAP = new Map([
	[ts.SyntaxKind.PlusToken, lua.BinaryOperator.Plus],
	[ts.SyntaxKind.MinusToken, lua.BinaryOperator.Minus],
	[ts.SyntaxKind.AsteriskToken, lua.BinaryOperator.Asterisk],
	[ts.SyntaxKind.SlashToken, lua.BinaryOperator.Slash],
	[ts.SyntaxKind.AsteriskAsteriskToken, lua.BinaryOperator.Caret],
	[ts.SyntaxKind.PercentToken, lua.BinaryOperator.Percent],
	[ts.SyntaxKind.EqualsEqualsEqualsToken, lua.BinaryOperator.EqualEqual],
	[ts.SyntaxKind.ExclamationEqualsEqualsToken, lua.BinaryOperator.TildeEqual],
]);

export function transformBinaryExpression(state: TransformState, node: ts.BinaryExpression) {
	const operatorKind = node.operatorToken.kind;

	if (
		operatorKind === ts.SyntaxKind.AmpersandAmpersandToken ||
		operatorKind === ts.SyntaxKind.BarBarToken ||
		operatorKind === ts.SyntaxKind.QuestionQuestionToken
	) {
		return transformLogical(state, node);
	}

	const operator = SIMPLE_OPERATOR_MAP.get(operatorKind);
	if (operator === undefined) {
		throw new Error(`Unrecognized operatorToken: ${ts.SyntaxKind[operatorKind]}`);
	}

	return lua.create(lua.SyntaxKind.BinaryExpression, {
		left: transformExpression(state, node.left),
		operator,
		right: transformExpression(state, node.right),
	});
}
