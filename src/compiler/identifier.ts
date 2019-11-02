import ts from "typescript";
import { CompilerState } from "../CompilerState";
import { luaStringify } from "../utility/general";
import { getFirstAncestorByKind } from "../utility/ast";

const BUILT_INS = new Set(["Promise", "Symbol", "typeIs", "opcall"]);

const replacements = new Map<string, string>([["undefined", "nil"], ["typeOf", "typeof"]]);

const PKG_VERSION_ID = "PKG_VERSION";

export function compileIdentifier(state: CompilerState, node: ts.Identifier, isDefinition: boolean = false) {
	let name = node.getText();

	const replacement = replacements.get(name);
	if (replacement) {
		return replacement;
	}

	if (BUILT_INS.has(name)) {
		state.usesTSLibrary = true;
		name = `TS.${name}`;
	}

	if (name === PKG_VERSION_ID) {
		return luaStringify(state.pkgVersion);
	}

	const definitions = isDefinition ? [node] : node.getDefinitions().map(def => def.getNode());

	for (const definition of definitions) {
		// I have no idea why, but getDefinitionNodes() cannot replace this
		if (definition.getSourceFile() === node.getSourceFile()) {
			let parent = definition;

			do {
				if (ts.isVariableStatement(parent)) {
					if (parent.hasExportKeyword()) {
						const declarationKind = parent.getDeclarationKind();
						if (declarationKind === ts.VariableDeclarationKind.Let) {
							return state.getExportContextName(parent) + "." + name;
						} else if (declarationKind === ts.VariableDeclarationKind.Const) {
							const idContext = getFirstAncestorByKind(node, ts.SyntaxKind.ModuleDeclaration);
							const defContext = getFirstAncestorByKind(parent, ts.SyntaxKind.ModuleDeclaration);

							if (idContext && defContext && idContext !== defContext) {
								state.pushHoistStack(`local ${name} = ${state.getNameForContext(defContext)}.${name}`);
							}
						}
					}
					break;
				} else if (ts.isModuleDeclaration(parent)) {
					// If within a namespace, scope it. If it is a namespace, don't
					if (parent !== definition.parent) {
						const parentName = state.namespaceStack.get(getName(parent));
						if (parentName) {
							return parentName + "." + name;
						}
					}
					break;
				} else if (parent.kind === ts.SyntaxKind.OpenParenToken) {
					parent = parent.parent!;
					if (!ts.isArrowFunction(parent)) {
						break;
					}
				} else if (
					!ts.isVariableDeclarationList(parent) &&
					!ts.isIdentifier(parent) &&
					!ts.isBindingElement(parent) &&
					!ts.isArrayBindingPattern(parent) &&
					!ts.isVariableDeclaration(parent) &&
					!ts.isObjectBindingPattern(parent)
				) {
					break;
				}
				parent = parent.parent;
			} while (parent);
		}
	}

	return state.getAlias(name);
}
