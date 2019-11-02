import ts from "typescript";
import {
	compileExpression,
	compileLoopBody,
	compileVariableDeclarationList,
	expressionModifiesVariable,
	nodeHasParameters,
	placeIncrementorInStatementIfExpression,
} from ".";
import { CompilerState, PrecedingStatementContext } from "../CompilerState";
import {
	isIdentifierWhoseDefinitionMatchesNode,
	joinIndentedLines,
	removeBalancedParenthesisFromStringBorders,
	skipNodesDownwards,
} from "../utility/general";
import { getType, isNumberType } from "../utility/type";
import { compileTruthyCheck } from "./truthiness";

function isConstantNumberVariableOrLiteral(condValue: ts.Node) {
	return (
		ts.isNumericLiteral(condValue) ||
		(isNumberType(getType(condValue)) &&
			ts.isIdentifier(condValue) &&
			condValue.getDefinitions().every(a => {
				const declNode = a.getDeclarationNode();
				if (declNode && ts.isVariableDeclaration(declNode)) {
					const declParent = declNode.parent;
					if (ts.isVariableDeclarationList(declParent)) {
						return declParent.getDeclarationKind() === ts.VariableDeclarationKind.Const;
					}
				}
				return false;
			}))
	);
}

function isExpressionConstantNumbers(node: ts.Node): boolean {
	const children = node.getChildren().map(child => skipNodesDownwards(child));
	return (
		children.length > 0 &&
		children.every(
			child =>
				isConstantNumberVariableOrLiteral(child) ||
				child.kind === ts.SyntaxKind.PlusToken ||
				child.kind === ts.SyntaxKind.MinusToken ||
				child.kind === ts.SyntaxKind.AsteriskToken ||
				child.kind === ts.SyntaxKind.SlashToken ||
				child.kind === ts.SyntaxKind.AsteriskAsteriskToken ||
				child.kind === ts.SyntaxKind.PercentToken ||
				(ts.isPrefixUnaryExpression(child) &&
					child.operatorToken === ts.SyntaxKind.MinusToken) ||
				(ts.isBinaryExpression(child) && isExpressionConstantNumbers(child)),
		)
	);
}

function getSignAndIncrementorForStatement(
	state: CompilerState,
	incrementor: ts.BinaryExpression | ts.PrefixUnaryExpression | ts.PostfixUnaryExpression,
	id: ts.Identifier,
) {
	let forIntervalStr = "";
	let sign: "" | "+" | "-" = "";

	if (ts.isBinaryExpression(incrementor)) {
		// const lhs = skipNodesDownwards(incrementor.left);
		let rhs = skipNodesDownwards(incrementor.right);
		const operator = skipNodesDownwards(incrementor.operatorToken);

		if (
			isNumberType(getType(rhs)) &&
			operator.kind === ts.SyntaxKind.EqualsToken &&
			ts.isBinaryExpression(rhs)
		) {
			const rhsLhs = skipNodesDownwards(rhs.left);
			const rhsOp = skipNodesDownwards(rhs.operatorToken);

			if (ts.isIdentifier(rhsLhs) && isIdentifierWhoseDefinitionMatchesNode(rhsLhs, id)) {
				switch (rhsOp.kind) {
					case ts.SyntaxKind.MinusToken:
						sign = "-";
						break;
					case ts.SyntaxKind.PlusToken:
						sign = "+";
						break;
				}
			}
		} else {
			switch (operator.kind) {
				case ts.SyntaxKind.PlusEqualsToken:
					sign = "+";
					break;
				case ts.SyntaxKind.MinusEqualsToken:
					sign = "-";
					break;
			}
		}

		if (rhs && ts.isPrefixUnaryExpression(rhs)) {
			if (rhs.operatorToken === ts.SyntaxKind.MinusToken) {
				switch (sign) {
					case "+":
						sign = "-";
						break;
					case "-":
						sign = "+";
						break;
					case "":
						return ["", ""];
				}
				rhs = rhs.operand;
			}
		}

		if (ts.isNumericLiteral(rhs)) {
			forIntervalStr = compileExpression(state, rhs);
		}
	} else if (incrementor.operator === ts.SyntaxKind.MinusMinusToken) {
		forIntervalStr = "1";
		sign = "-";
	} else {
		forIntervalStr = "1";
		sign = "+";
	}
	return [sign, forIntervalStr];
}

function getLimitInForStatement(
	state: CompilerState,
	condition: ts.Expression,
	id: ts.Identifier,
): [string, ts.Node | undefined] {
	if (ts.isBinaryExpression(condition)) {
		const lhs = skipNodesDownwards(condition.left);
		const operator = condition.operatorToken;
		const rhs = skipNodesDownwards(condition.right);

		if (isIdentifierWhoseDefinitionMatchesNode(lhs, id)) {
			switch (operator.kind) {
				case ts.SyntaxKind.GreaterThanEqualsToken: // >=
					return [">=", rhs];
				case ts.SyntaxKind.LessThanEqualsToken: // <=
					return ["<=", rhs];
			}
		} else if (isIdentifierWhoseDefinitionMatchesNode(rhs, id)) {
			switch (operator.kind) {
				case ts.SyntaxKind.GreaterThanEqualsToken: // >=
					return ["<=", lhs];
				case ts.SyntaxKind.LessThanEqualsToken: // <=
					return [">=", lhs];
			}
		}
	}
	return ["", undefined];
}

function safelyHandleExpressionsInForStatement(
	state: CompilerState,
	incrementor: ts.Expression<ts.Expression>,
	incrementorStr: string,
	context: PrecedingStatementContext,
	indentation: number,
) {
	return (
		joinIndentedLines(context, indentation) +
		state.indent +
		(context.isPushed
			? incrementorStr
			: placeIncrementorInStatementIfExpression(state, incrementor, incrementorStr))
	);
}

function getSimpleForLoopString(
	state: CompilerState,
	initializer: ts.VariableDeclarationList,
	forLoopVars: string,
	statement: ts.Statement<ts.Statement>,
) {
	let result = "";
	state.popIndent();
	const first = compileVariableDeclarationList(state, initializer)
		.trim()
		.replace(/^local /, "")
		.replace(/;$/, "");

	result = state.indent + `for ${first}, ${forLoopVars} do\n`;
	state.pushIndent();
	result += compileLoopBody(state, statement);
	state.popIndent();
	result += state.indent + `end;\n`;
	return result;
}

export function compileForStatement(state: CompilerState, node: ts.ForStatement) {
	state.pushIdStack();
	const statement = node.statement;
	const condition = skipNodesDownwards(node.condition);
	const incrementor = skipNodesDownwards(node.getIncrementor());

	const localizations = new Array<string>();
	const cleanups = new Array<() => void>();
	let result = state.indent + "do\n";
	state.pushIndent();
	const initializer = skipNodesDownwards(node.initializer);
	let conditionStr: string | undefined;
	let incrementorStr: string | undefined;

	let conditionContext: PrecedingStatementContext | undefined;
	let incrementorContext: PrecedingStatementContext | undefined;
	let initializerContext: PrecedingStatementContext | undefined;

	if (initializer) {
		if (ts.isVariableDeclarationList(initializer)) {
			const declarations = initializer.getDeclarations();
			const statementDescendants = statement.getDescendants();
			if (initializer.getDeclarationKind() === ts.VariableDeclarationKind.Let) {
				if (declarations.length === 1) {
					const [declaration] = declarations;
					const lhs = skipNodesDownwards(declaration.name);
					const rhs = skipNodesDownwards(declaration.initializer);

					if (
						ts.isIdentifier(lhs) &&
						!statementDescendants.some(statementDescendant =>
							expressionModifiesVariable(statementDescendant, lhs),
						) &&
						incrementor &&
						condition &&
						rhs
					) {
						// check if we can convert to a simple for loop
						// IF there aren't any in-loop modifications to the let variable
						// AND the let variable is a single numeric variable
						// AND the incrementor is a simple +/- expression of the let var
						// AND the conditional expression is a binary expression
						// with one of these operators: <= >= < >
						// AND the conditional expression compares the let var to a numeric literal
						// OR the conditional expression compares the let var to an unchanging number

						const rhsType = getType(rhs);
						if (isNumberType(rhsType)) {
							if (expressionModifiesVariable(incrementor, lhs)) {
								const [incrSign, incrValue] = getSignAndIncrementorForStatement(
									state,
									incrementor,
									lhs,
								);
								if (incrSign && incrValue) {
									const [condSign, condValue] = getLimitInForStatement(state, condition, lhs);
									// numeric literals, or constant number identifiers are safe
									if (
										condValue &&
										ts.isExpression(condValue) &&
										(isConstantNumberVariableOrLiteral(condValue) ||
											isExpressionConstantNumbers(condValue))
									) {
										if (
											(incrSign === "+" && condSign === "<=") ||
											(incrSign === "-" && condSign === ">=")
										) {
											const incrStr = incrSign === "-" ? incrSign + incrValue : incrValue;

											return getSimpleForLoopString(
												state,
												initializer,
												compileExpression(state, condValue) +
												(incrStr === "1" ? "" : ", " + incrStr),
												statement,
											);
										}
									}
								}
							}
						}
					}
				}
			}

			state.enterPrecedingStatementContext();
			const expStr = compileVariableDeclarationList(state, initializer);
			result += state.exitPrecedingStatementContextAndJoin() + expStr; // + ";\n";

			// if we can't convert to a simple for loop:
			// if it has any internal function declarations, make sure to locally scope variables
			if (statementDescendants.some(nodeHasParameters)) {
				state.enterPrecedingStatementContext();
				conditionStr = condition
					? removeBalancedParenthesisFromStringBorders(compileTruthyCheck(state, condition))
					: "true";
				conditionContext = state.exitPrecedingStatementContext();
				state.enterPrecedingStatementContext();
				incrementorStr = incrementor ? compileExpression(state, incrementor) + ";\n" : undefined;
				incrementorContext = state.exitPrecedingStatementContext();

				declarations.forEach(declaration => {
					const lhs = declaration.getChildAtIndex(0);

					if (ts.isIdentifier(lhs)) {
						const name = lhs.getText();
						const isLoopVarModified = statementDescendants.some(statementDescendant =>
							expressionModifiesVariable(statementDescendant, lhs),
						);
						const alias = state.getNewId();
						state.pushIndent();
						localizations.push(`local ${alias} = ${name};\n`);
						state.popIndent();

						// don't leak
						const previous = state.variableAliases.get(name);

						cleanups.push(() => {
							if (previous) {
								state.variableAliases.set(name, previous);
							} else {
								state.variableAliases.delete(name);
							}

							if (isLoopVarModified) {
								result += state.indent + `${name} = ${alias};\n`;
							}
						});

						state.variableAliases.set(name, alias);
					}
				});
			}
		} else if (ts.isExpression(initializer)) {
			state.enterPrecedingStatementContext();
			const expStr = compileExpression(state, initializer);
			initializerContext = state.exitPrecedingStatementContext();
			result += safelyHandleExpressionsInForStatement(state, initializer, expStr, initializerContext, 0) + ";\n";
		}
	}
	// order matters
	if (conditionStr === undefined) {
		state.enterPrecedingStatementContext();

		conditionStr = condition
			? removeBalancedParenthesisFromStringBorders(compileTruthyCheck(state, condition))
			: "true";
		conditionContext = state.exitPrecedingStatementContext();
		state.enterPrecedingStatementContext();
		incrementorStr = incrementor ? compileExpression(state, incrementor) + ";\n" : undefined;
		incrementorContext = state.exitPrecedingStatementContext();
	}

	const conditionContextHasStatements = conditionContext && conditionContext.length > 0;
	result += state.indent + `while ${conditionContextHasStatements ? "true" : conditionStr} do\n`;
	state.pushIndent();

	if (conditionContextHasStatements) {
		result += joinIndentedLines(conditionContext!, 1);
		result += state.indent + `if not (${conditionStr}) then break; end;\n`;
	}

	for (const localization of localizations) {
		result += state.indent + localization;
	}

	result += compileLoopBody(state, statement);
	cleanups.forEach(cleanup => cleanup());

	if (incrementor && incrementorStr) {
		result += safelyHandleExpressionsInForStatement(state, incrementor, incrementorStr, incrementorContext!, 1);
	}

	state.popIndent();
	result += state.indent + "end;\n";
	state.popIndent();
	result += state.indent + `end;\n`;
	state.popIdStack();
	return result;
}
