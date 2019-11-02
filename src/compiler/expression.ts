import ts from "typescript";
import {
	compileArrayLiteralExpression,
	compileAwaitExpression,
	compileBinaryExpression,
	compileBooleanLiteral,
	compileCallExpression,
	compileClassExpression,
	compileConditionalExpression,
	compileElementAccessExpression,
	compileFunctionExpression,
	compileIdentifier,
	compileJsxElement,
	compileJsxSelfClosingElement,
	compileNewExpression,
	compileNumericLiteral,
	compileObjectLiteralExpression,
	compileParenthesizedExpression,
	compilePostfixUnaryExpression,
	compilePrefixUnaryExpression,
	compilePropertyAccessExpression,
	compileSpreadElement,
	compileStringLiteral,
	compileSuperExpression,
	compileTaggedTemplateExpression,
	compileTemplateExpression,
	compileYieldExpression,
	isSetToken,
} from ".";
import { CompilerState } from "../CompilerState";
import { CompilerError, CompilerErrorType } from "../errors/CompilerError";
import { isIdentifierWhoseDefinitionMatchesNode, skipNodesDownwards, skipNodesUpwards } from "../utility/general";
import { isMethodDeclaration } from "./function";
import { getKindName, isThisExpression, isSuperExpression } from "../utility/ast";

export function compileExpression(state: CompilerState, node: ts.Expression): string {
	if (ts.isStringLiteral(node) || ts.isNoSubstitutionTemplateLiteral(node)) {
		return compileStringLiteral(state, node);
	} else if (ts.isNumericLiteral(node)) {
		return compileNumericLiteral(state, node);
	} else if (ts.isBooleanLiteral(node)) {
		return compileBooleanLiteral(state, node);
	} else if (ts.isArrayLiteralExpression(node)) {
		return compileArrayLiteralExpression(state, node);
	} else if (ts.isObjectLiteralExpression(node)) {
		return compileObjectLiteralExpression(state, node);
	} else if (ts.isFunctionExpression(node) || ts.isArrowFunction(node)) {
		return compileFunctionExpression(state, node);
	} else if (ts.isCallExpression(node)) {
		return compileCallExpression(state, node);
	} else if (ts.isIdentifier(node)) {
		return compileIdentifier(state, node);
	} else if (ts.isBinaryExpression(node)) {
		return compileBinaryExpression(state, node);
	} else if (ts.isPrefixUnaryExpression(node)) {
		return compilePrefixUnaryExpression(state, node);
	} else if (ts.isPostfixUnaryExpression(node)) {
		return compilePostfixUnaryExpression(state, node);
	} else if (ts.isPropertyAccessExpression(node)) {
		return compilePropertyAccessExpression(state, node);
	} else if (ts.isNewExpression(node)) {
		return compileNewExpression(state, node);
	} else if (ts.isParenthesizedExpression(node)) {
		return compileParenthesizedExpression(state, node);
	} else if (ts.isTemplateExpression(node)) {
		return compileTemplateExpression(state, node);
	} else if (ts.isTaggedTemplateExpression(node)) {
		return compileTaggedTemplateExpression(state, node);
	} else if (ts.isElementAccessExpression(node)) {
		return compileElementAccessExpression(state, node);
	} else if (ts.isAwaitExpression(node)) {
		return compileAwaitExpression(state, node);
	} else if (ts.isConditionalExpression(node)) {
		return compileConditionalExpression(state, node);
	} else if (ts.isJsxExpression(node)) {
		return compileExpression(state, node.getExpressionOrThrow());
	} else if (ts.isJsxSelfClosingElement(node)) {
		return compileJsxSelfClosingElement(state, node);
	} else if (ts.isJsxElement(node)) {
		return compileJsxElement(state, node);
	} else if (ts.isSpreadElement(node)) {
		return compileSpreadElement(state, node);
	} else if (ts.isClassExpression(node)) {
		return compileClassExpression(state, node);
	} else if (ts.isYieldExpression(node)) {
		return compileYieldExpression(state, node);
	} else if (ts.isOmittedExpression(node)) {
		return "nil";
	} else if (isThisExpression(node)) {
		if (
			!node.getFirstAncestor(
				ancestor =>
					isMethodDeclaration(ancestor) ||
					ts.isClassDeclaration(ancestor) ||
					ts.isObjectLiteralExpression(ancestor) ||
					ts.isClassExpression(ancestor),
			)
		) {
			throw new CompilerError(
				"'this' may only be used inside a class definition, object literal, or method function",
				node,
				CompilerErrorType.NoThisOutsideClass,
			);
		}
		return "self";
	} else if (isSuperExpression(node)) {
		return compileSuperExpression(state, node);
	} else if (
		ts.isAsExpression(node) ||
		ts.isTypeAssertion(node) ||
		ts.isNonNullExpression(node)
	) {
		return compileExpression(state, skipNodesDownwards(node.expression));
	} else if (ts.isNullLiteral(node)) {
		throw new CompilerError("'null' is not supported! Use 'undefined' instead.", node, CompilerErrorType.NoNull);
	} else if (ts.isTypeOfExpression(node)) {
		throw new CompilerError(
			"'typeof' operator is not supported! Use `typeIs(value, type)` or `typeOf(value)` instead.",
			node,
			CompilerErrorType.NoTypeOf,
		);
	} else {
		throw new CompilerError(
			`Unexpected expression ( ${getKindName(node)} ) in compileExpression`,
			node,
			CompilerErrorType.BadExpression,
			true,
		);
	}
}

export function compileExpressionStatement(state: CompilerState, node: ts.ExpressionStatement) {
	state.enterPrecedingStatementContext();

	let expStr: string;
	const expression = skipNodesDownwards(node.expression);

	if (ts.isCallExpression(expression)) {
		expStr = compileCallExpression(state, expression, true);
	} else {
		expStr = compileExpression(state, expression);

		// big set of rules for expression statements
		if (
			!ts.isNewExpression(expression) &&
			!ts.isAwaitExpression(expression) &&
			!ts.isPostfixUnaryExpression(expression) &&
			!(
				ts.isPrefixUnaryExpression(expression) &&
				(expression.operatorToken === ts.SyntaxKind.PlusPlusToken ||
					expression.operatorToken === ts.SyntaxKind.MinusMinusToken)
			) &&
			!(ts.isBinaryExpression(expression) && isSetToken(expression.operatorToken.kind)) &&
			!ts.isYieldExpression(expression) &&
			!ts.isJsxElement(expression)
		) {
			expStr = `local _ = ${expStr}`;
		}
	}

	const result = state.exitPrecedingStatementContextAndJoin();

	// this is a hack for the time being, to prevent double indenting
	// situations like these: ({ length } = "Hello, world!")
	const indent = expStr.match(/^\s+/) ? "" : state.indent;
	return expStr ? result + indent + expStr + ";\n" : result;
}

export function expressionModifiesVariable(
	node: ts.Node,
	lhs?: ts.Identifier,
): node is ts.BinaryExpression | ts.PrefixUnaryExpression | ts.PostfixUnaryExpression {
	if (
		ts.isPostfixUnaryExpression(node) ||
		(ts.isPrefixUnaryExpression(node) &&
			(node.operator === ts.SyntaxKind.PlusPlusToken ||
				node.operator === ts.SyntaxKind.MinusMinusToken))
	) {
		if (lhs) {
			return isIdentifierWhoseDefinitionMatchesNode(node.operand, lhs);
		} else {
			return true;
		}
	} else if (ts.isBinaryExpression(node) && isSetToken(node.operatorToken.kind)) {
		if (lhs) {
			return isIdentifierWhoseDefinitionMatchesNode(node.left, lhs);
		} else {
			return true;
		}
	}
	return false;
}

export function appendDeclarationIfMissing(
	state: CompilerState,
	possibleExpressionStatement: ts.Node,
	compiledNode: string,
) {
	if (
		compiledNode.match(/^_d+$/) ||
		ts.isExpressionStatement(skipNodesUpwards(possibleExpressionStatement))
	) {
		return "local _ = " + compiledNode;
	} else {
		return compiledNode;
	}
}

export function placeIncrementorInStatementIfExpression(
	state: CompilerState,
	incrementor: ts.Expression<ts.Expression>,
	incrementorStr: string,
) {
	if (ts.isExpression(incrementor)) {
		if (
			!ts.isCallExpression(incrementor) &&
			!expressionModifiesVariable(incrementor) &&
			!ts.isVariableDeclarationList(incrementor)
		) {
			incrementorStr = `local _ = ` + incrementorStr;
		}
	}
	return incrementorStr;
}
