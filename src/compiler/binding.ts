import ts from "typescript";
import {
	checkPropertyCollision,
	compileExpression,
	CompilerDirective,
	getComputedPropertyAccess,
	HasParameters,
	isIdentifierDefinedInExportLet,
} from ".";
import { CompilerState } from "../CompilerState";
import { CompilerError, CompilerErrorType } from "../errors/CompilerError";
import { joinIndentedLines, safeLuaIndex, skipNodesDownwards } from "../utility/general";
import {
	getCompilerDirectiveWithLaxConstraint,
	getType,
	isArrayMethodType,
	isArrayType,
	isGeneratorType,
	isIterableFunctionType,
	isMapMethodType,
	isMapType,
	isObjectType,
	isSetMethodType,
	isSetType,
	isStringMethodType,
	isStringType,
	isTupleType,
} from "../utility/type";
import { compileIdentifier } from "./identifier";
import { checkReserved } from "./security";
import { getKindName, getName, isThisExpression, isSuperExpression, getParentOrThrow, getFirstAncestorOrThrow, getFirstChild, getLastChildOrThrow, isTupleType } from "../utility/ast";

type BindingPattern = ts.ArrayBindingPattern | ts.ObjectBindingPattern;
type BindingLiteral = ts.ArrayLiteralExpression | ts.ObjectLiteralExpression;

function compileParamDefault(state: CompilerState, exp: ts.Expression, name: string) {
	const initializer = skipNodesDownwards(exp);
	state.enterPrecedingStatementContext();

	state.declarationContext.set(initializer, {
		isIdentifier: ts.isIdentifier(initializer) && !isIdentifierDefinedInExportLet(initializer),
		set: name,
	});
	const expStr = compileExpression(state, initializer);
	const context = state.exitPrecedingStatementContext();

	state.pushIndent();

	const declaration = state.declarationContext.delete(initializer) ? `${name} = ${expStr};` : "";
	let newline: string;
	let indentation: string;
	let tab: string;
	let contextLines: string;

	if (context.length > (declaration ? 0 : 1)) {
		newline = "\n";
		indentation = state.indent;
		tab = "\t";
		contextLines = joinIndentedLines(context, 2);
	} else {
		newline = " ";
		indentation = "";
		tab = "";
		contextLines = joinIndentedLines(context, 0).replace(/\r?\n/g, " ");
	}

	state.popIndent();

	return [
		`if ${name} == nil then`,
		newline,
		contextLines,
		indentation,
		declaration ? tab + `${declaration}` + newline + indentation : "",
		"end;",
	].join("");
}

export function getParameterData(
	state: CompilerState,
	paramNames: Array<string>,
	initializers: Array<string>,
	node: HasParameters,
	defaults?: Array<string>,
) {
	for (const param of node.parameters) {
		const child = getFirstChild(param, exp =>
			ts.isIdentifier(exp) ||
			ts.isArrayBindingPattern(exp) ||
			ts.isObjectBindingPattern(exp),
		);

		/* istanbul ignore next */
		if (child === undefined) {
			throw new CompilerError(
				"Child missing from parameter!",
				param,
				CompilerErrorType.ParameterChildMissing,
				true,
			);
		}

		let name: string;
		if (ts.isIdentifier(child)) {
			if (getName(param) === "this") {
				continue;
			}
			name = compileExpression(state, child);
			checkReserved(child);
		} else {
			name = state.getNewId();
		}

		if (param.isRestParameter()) {
			paramNames.push("...");
			initializers.push(`local ${name} = { ... };`);
		} else {
			paramNames.push(name);
		}

		if (param.initializer !== undefined) {
			(defaults ? defaults : initializers).push(compileParamDefault(state, param.initializer, name));
		}

		const parent = node.parent;
		if (
			(ts.isClassDeclaration(parent) || ts.isClassExpression(parent)) &&
			(param.hasScopeKeyword() || param.isReadonly())
		) {
			checkPropertyCollision(parent, param);
			initializers.push(`${safeLuaIndex("self", name)} = ${name};`);
		}

		if (ts.isArrayBindingPattern(child) || ts.isObjectBindingPattern(child)) {
			initializers.push(...compileBindingPattern(state, child, name));
		}
	}
}

function arrayAccessor(state: CompilerState, node: ts.Node, t: string, key: number) {
	return `${t}[${key}]`;
}

function objectAccessor(
	state: CompilerState,
	t: string,
	node: ts.Node,
	nameNode: ts.Node = node,
	aliasNode: ts.Node = node,
): string {
	let name: string;

	const rhs = getLastChildOrThrow(getParentOrThrow(getFirstAncestorOrThrow(node, ancestor => ts.isObjectLiteralExpression(ancestor) || ts.isObjectBindingPattern(ancestor))), () => true);

	if (ts.isIdentifier(nameNode)) {
		name = compileExpression(state, nameNode);
	} else if (ts.isComputedPropertyName(nameNode)) {
		const exp = skipNodesDownwards(nameNode.expression);
		name = getComputedPropertyAccess(state, exp, rhs);
		return `${t}[${name}]`;
	} else if (ts.isNumericLiteral(nameNode) || ts.isStringLiteral(nameNode)) {
		name = compileExpression(state, nameNode);
		return `${t}[${name}]`;
	} else {
		throw new CompilerError(
			`Cannot index an object with type ${getKindName(nameNode)}.`,
			nameNode,
			CompilerErrorType.BadExpression,
			true,
		);
	}

	const type = getType(aliasNode);
	if (isArrayMethodType(type) || isMapMethodType(type) || isSetMethodType(type) || isStringMethodType(type)) {
		throw new CompilerError(
			`Cannot index method ${name} (a roblox-ts internal)`,
			aliasNode,
			CompilerErrorType.BadDestructuringType,
		);
	}

	// We need this because length is built-in to the TS compiler, even if we removed it from our types
	if (
		getCompilerDirectiveWithLaxConstraint(getType(rhs), CompilerDirective.Array, r => isTupleType(r)) &&
		name === "length"
	) {
		throw new CompilerError(
			`Cannot access the \`length\` property of a tuple! Instead use \`${rhs.getText()}.size()\``,
			node,
			CompilerErrorType.TupleLength,
		);
	}

	return safeLuaIndex(t, name);
}

function stringAccessor(state: CompilerState, node: ts.Node, t: string, key: number) {
	return `string.sub(${t}, ${key}, ${key})`;
}

function setAccessor(state: CompilerState, node: ts.Node, t: string, key: number, idStack: Array<string>) {
	const lastId = idStack[idStack.length - 1] as string | undefined;
	const id = state.pushPrecedingStatementToNewId(node, `next(${t}${lastId ? `, ${lastId}` : ""})`);
	idStack.push(id);
	return id;
}

function mapAccessor(
	state: CompilerState,
	node: ts.Node,
	t: string,
	key: number,
	idStack: Array<string>,
	isHole = false,
) {
	const keyId = state.getNewId();
	const lastId = idStack[idStack.length - 1] as string | undefined;

	let valueId: string;
	let valueIdStr = "";
	if (!isHole) {
		valueId = state.getNewId();
		valueIdStr = `, ${valueId}`;
	}

	if (lastId !== undefined) {
		state.pushPrecedingStatements(node, state.indent + `local ${keyId}${valueIdStr} = next(${t}, ${lastId});\n`);
	} else {
		state.pushPrecedingStatements(node, state.indent + `local ${keyId}${valueIdStr} = next(${t});\n`);
	}
	idStack.push(keyId);
	return `{ ${keyId}${valueIdStr} }`;
}

function iterAccessor(
	state: CompilerState,
	node: ts.Node,
	t: string,
	key: number,
	idStack: Array<string>,
	isHole = false,
) {
	if (isHole) {
		state.pushPrecedingStatements(node, state.indent + `${t}.next();\n`);
		return "";
	} else {
		const id = state.getNewId();
		state.pushPrecedingStatements(node, state.indent + `local ${id} = ${t}.next();\n`);
		return `${id}.value`;
	}
}

function iterableFunctionAccessor(
	state: CompilerState,
	node: ts.Node,
	t: string,
	key: number,
	idStack: Array<string>,
	isHole = false,
) {
	if (isHole) {
		state.pushPrecedingStatements(node, state.indent + `${t}();\n`);
		return "";
	} else {
		return `${t}()`;
	}
}

function getAccessorForBindingNode(bindingPattern: ts.Node) {
	return getAccessorForBindingType(bindingPattern, getType(bindingPattern), bindingPattern);
}

function getAccessorForBindingType(binding: ts.Node, type: ts.Type | Array<ts.Type>, node?: ts.Node) {
	if (!(type instanceof ts.Type) || isArrayType(type)) {
		return arrayAccessor;
	} else if (isStringType(type)) {
		return stringAccessor;
	} else if (isSetType(type)) {
		return setAccessor;
	} else if (isMapType(type)) {
		return mapAccessor;
	} else if (isIterableFunctionType(type)) {
		return iterableFunctionAccessor;
	} else if (
		isGeneratorType(type) ||
		isObjectType(type) ||
		(node && (isThisExpression(node) || isSuperExpression(node)))
	) {
		return iterAccessor;
	}

	/* istanbul ignore next */
	throw new CompilerError(
		`Cannot destructure an object of type ${type.getText()}`,
		binding,
		CompilerErrorType.BadDestructuringType,
		true,
	);
}

export function concatNamesAndValues(
	state: CompilerState,
	names: Array<string>,
	values: Array<string>,
	isLocal: boolean,
	func: (str: string) => void,
	includeSpacing = true,
	includeSemicolon = true,
) {
	if (values.length > 0) {
		names[0] = names[0] || "_";
		func(
			`${includeSpacing ? state.indent : ""}${isLocal ? "local " : ""}${names.join(", ")} = ${values.join(", ")}${
			includeSemicolon ? ";" : ""
			}${includeSpacing ? "\n" : ""}`,
		);
	}
}

function compileArrayBindingPattern(
	state: CompilerState,
	bindingPattern: ts.ArrayBindingPattern,
	parentId: string,
	exportVars: boolean,
	noLocal: boolean,
) {
	let childIndex = 1;
	const idStack = new Array<string>();
	const getAccessor = getAccessorForBindingNode(bindingPattern);
	for (const element of bindingPattern.elements) {
		if (ts.isOmittedExpression(element)) {
			getAccessor(state, element, parentId, childIndex, idStack, true);
		} else {
			if (element.getDotDotDotToken()) {
				throw new CompilerError(
					"Operator ... is not supported for destructuring!",
					element,
					CompilerErrorType.SpreadDestructuring,
				);
			}
			const name = element.name;
			const rhs = getAccessor(state, name, parentId, childIndex, idStack);
			if (ts.isIdentifier(name)) {
				checkReserved(name);
				const prefix = noLocal ? "" : "local ";
				const nameStr = compileIdentifier(state, name, true);
				state.pushPrecedingStatements(bindingPattern, state.indent + `${prefix}${nameStr} = ${rhs};\n`);
				if (exportVars) {
					state.pushExport(nameStr, bindingPattern.parent);
				}
				const initializer = element.initializer;
				if (initializer) {
					state.pushPrecedingStatements(
						bindingPattern,
						state.indent + compileParamDefault(state, initializer, nameStr) + "\n",
					);
				}
			} else {
				const id = state.getNewId();
				state.pushPrecedingStatements(bindingPattern, state.indent + `local ${id} = ${rhs};\n`);
				compileBindingPatternInner(state, name, id, exportVars, noLocal);
			}
		}
		childIndex++;
	}
}

function compileObjectBindingPattern(
	state: CompilerState,
	bindingPattern: ts.ObjectBindingPattern,
	parentId: string,
	exportVars: boolean,
	noLocal: boolean,
) {
	for (const element of bindingPattern.elements) {
		if (element.getDotDotDotToken()) {
			throw new CompilerError(
				"Operator ... is not supported for destructuring!",
				element,
				CompilerErrorType.SpreadDestructuring,
			);
		}
		const name = element.name;
		const prop = element.getPropertyNameNode();
		if (ts.isIdentifier(name)) {
			checkReserved(name);
			const prefix = noLocal ? "" : "local ";
			const nameStr = compileIdentifier(state, name, true);
			const rhs = objectAccessor(state, parentId, name, prop, name);
			state.pushPrecedingStatements(bindingPattern, state.indent + `${prefix}${nameStr} = ${rhs};\n`);
			if (exportVars) {
				state.pushExport(nameStr, bindingPattern.parent);
			}
			const initializer = element.initializer;
			if (initializer) {
				state.pushPrecedingStatements(
					bindingPattern,
					state.indent + compileParamDefault(state, initializer, nameStr) + "\n",
				);
			}
		} else {
			const id = state.getNewId();
			const rhs = objectAccessor(state, parentId, name, prop, name);
			state.pushPrecedingStatements(bindingPattern, state.indent + `local ${id} = ${rhs};\n`);
			compileBindingPatternInner(state, name, id, exportVars, noLocal);
		}
	}
}

function compileBindingPatternInner(
	state: CompilerState,
	bindingPattern: BindingPattern,
	parentId: string,
	exportVars: boolean,
	noLocal: boolean,
) {
	if (ts.isArrayBindingPattern(bindingPattern)) {
		compileArrayBindingPattern(state, bindingPattern, parentId, exportVars, noLocal);
	} else if (ts.isObjectBindingPattern(bindingPattern)) {
		compileObjectBindingPattern(state, bindingPattern, parentId, exportVars, noLocal);
	}
}

export function compileBindingPatternAndJoin(
	state: CompilerState,
	bindingPattern: BindingPattern,
	parentId: string,
	exportVars = false,
	noLocal = false,
) {
	state.enterPrecedingStatementContext();
	compileBindingPatternInner(state, bindingPattern, parentId, exportVars, noLocal);
	return state.exitPrecedingStatementContextAndJoin();
}

export function compileBindingPattern(
	state: CompilerState,
	bindingPattern: BindingPattern,
	parentId: string,
	exportVars = false,
	noLocal = false,
) {
	state.enterPrecedingStatementContext();
	compileBindingPatternInner(state, bindingPattern, parentId, exportVars, noLocal);
	// TODO: remove .trim(), fix call sites
	return state.exitPrecedingStatementContext().map(v => v.trim());
}

export function getSubTypeOrThrow(
	node: ts.Node,
	type: ts.Type | Array<ts.Type>,
	index: string | number,
): ts.Type | Array<ts.Type> {
	if (!Array.isArray(type)) {
		if (typeof index === "string") {
			const prop = type.getProperty(index);
			if (prop) {
				const valDec = prop.valueDeclaration;
				if (valDec) {
					return getType(valDec);
				}
			}
		} else if (isTupleType(type)) {
			return getSubTypeOrThrow(node, type.aliasTypeArguments![0], index);
		} else if (isArrayType(type)) {
			if (isTupleType(type)) {
				return type.getTupleElements()[index];
			} else {
				const numIndexType = type.getNumberIndexType();
				if (numIndexType) {
					return numIndexType;
				}
			}
		} else if (isStringType(type)) {
			// T -> T
			return type;
		} else if (isSetType(type)) {
			// Set<T> -> T
			return type.getTypeArguments()[0];
		} else if (isMapType(type)) {
			// Map<K, V> -> [K, V]
			return type.getTypeArguments();
		} else if (isGeneratorType(type)) {
			// IterableIterator<T> -> T
			return type.getTypeArguments()[0];
		}
	} else if (typeof index === "number") {
		return type[index];
	}

	/* istanbul ignore next */
	throw new CompilerError("Could not find subtype!", node, CompilerErrorType.BadDestructSubType, true);
}

function compileArrayBindingLiteral(
	state: CompilerState,
	bindingLiteral: ts.ArrayLiteralExpression,
	parentId: string,
	accessType: ts.Type | Array<ts.Type>,
) {
	let childIndex = 1;
	const idStack = new Array<string>();
	const getAccessor = getAccessorForBindingType(bindingLiteral, accessType);
	for (const element of bindingLiteral.elements) {
		if (ts.isOmittedExpression(element)) {
			getAccessor(state, element, parentId, childIndex, idStack, true);
		} else {
			const rhs = getAccessor(state, element, parentId, childIndex, idStack);
			if (
				ts.isIdentifier(element) ||
				ts.isElementAccessExpression(element) ||
				ts.isPropertyAccessExpression(element)
			) {
				const nameStr = compileExpression(state, element);
				state.pushPrecedingStatements(bindingLiteral, state.indent + `${nameStr} = ${rhs};\n`);
			} else if (ts.isBinaryExpression(element)) {
				const nameStr = compileExpression(state, skipNodesDownwards(element.left));
				state.pushPrecedingStatements(bindingLiteral, state.indent + `${nameStr} = ${rhs};\n`);
				const initializer = skipNodesDownwards(element.right);
				state.pushPrecedingStatements(
					bindingLiteral,
					state.indent + compileParamDefault(state, initializer, nameStr) + "\n",
				);
			} else if (
				ts.isArrayLiteralExpression(element) ||
				ts.isObjectLiteralExpression(element)
			) {
				const id = state.getNewId();
				state.pushPrecedingStatements(bindingLiteral, state.indent + `local ${id} = ${rhs};\n`);
				compileBindingLiteralInner(
					state,
					element,
					id,
					getSubTypeOrThrow(bindingLiteral, accessType, childIndex - 1),
				);
			} else {
				throw new CompilerError(
					`Unexpected ${getKindName(element)} in compileArrayBindingLiteral.`,
					element,
					CompilerErrorType.UnexpectedBindingPattern,
					true,
				);
			}
		}
		childIndex++;
	}
}

function compileObjectBindingLiteral(
	state: CompilerState,
	bindingLiteral: ts.ObjectLiteralExpression,
	parentId: string,
	accessType: ts.Type | Array<ts.Type>,
) {
	for (const property of bindingLiteral.properties) {
		if (ts.isShorthandPropertyAssignment(property)) {
			const name = property.name;
			const nameStr = compileExpression(state, name);
			const rhs = objectAccessor(state, parentId, name, name, name);
			state.pushPrecedingStatements(bindingLiteral, state.indent + `${nameStr} = ${rhs};\n`);
			const initializer = property.objectAssignmentInitializer;
			if (initializer) {
				state.pushPrecedingStatements(
					bindingLiteral,
					state.indent + compileParamDefault(state, initializer, nameStr) + "\n",
				);
			}
		} else if (ts.isPropertyAssignment(property)) {
			const name = property.name;
			const init = property.initializer;
			const rhs = objectAccessor(state, parentId, name, name, name);
			if (
				ts.isIdentifier(init) ||
				ts.isElementAccessExpression(init) ||
				ts.isPropertyAccessExpression(init)
			) {
				const nameStr = compileExpression(state, init);
				state.pushPrecedingStatements(bindingLiteral, state.indent + `${nameStr} = ${rhs};\n`);
			} else if (ts.isBinaryExpression(init)) {
				const nameStr = compileExpression(state, skipNodesDownwards(init.left));
				state.pushPrecedingStatements(bindingLiteral, state.indent + `${nameStr} = ${rhs};\n`);
				const initializer = skipNodesDownwards(init.right);
				state.pushPrecedingStatements(
					bindingLiteral,
					state.indent + compileParamDefault(state, initializer, nameStr) + "\n",
				);
			} else if (ts.isObjectLiteralExpression(init) || ts.isArrayLiteralExpression(init)) {
				const id = state.getNewId();
				state.pushPrecedingStatements(bindingLiteral, state.indent + `local ${id} = ${rhs};\n`);
				compileBindingLiteralInner(
					state,
					init,
					id,
					getSubTypeOrThrow(bindingLiteral, accessType, getName(property)),
				);
			}
		} else {
			throw new CompilerError(
				`Unexpected ${getKindName(property)} in compileArrayBindingLiteral.`,
				property,
				CompilerErrorType.UnexpectedBindingPattern,
				true,
			);
		}
	}
}

function compileBindingLiteralInner(
	state: CompilerState,
	bindingLiteral: BindingLiteral,
	parentId: string,
	accessType: ts.Type | Array<ts.Type>,
) {
	if (ts.isArrayLiteralExpression(bindingLiteral)) {
		compileArrayBindingLiteral(state, bindingLiteral, parentId, accessType);
	} else if (ts.isObjectLiteralExpression(bindingLiteral)) {
		compileObjectBindingLiteral(state, bindingLiteral, parentId, accessType);
	}
}

export function compileBindingLiteral(
	state: CompilerState,
	bindingLiteral: BindingLiteral,
	parentId: string,
	accessType: ts.Type | Array<ts.Type> = getType(bindingLiteral),
) {
	state.enterPrecedingStatementContext();
	compileBindingLiteralInner(state, bindingLiteral, parentId, accessType);
	// TODO: remove .trim(), fix call sites
	return state.exitPrecedingStatementContext().map(v => v.trim());
}
