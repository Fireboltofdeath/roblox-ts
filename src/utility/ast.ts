import ts from "typescript";
import { KindToTSNode } from "./KindToTSNode";

export function getKindName(node: ts.Node) {
	return ts.SyntaxKind[node.kind];
}

export function getAncestors(node: ts.Node) {
	const result = new Array<ts.Node>();
	while (node.parent) {
		result.push(node.parent);
		node = node.parent;
	}
	return result;
}

export function is<T extends keyof KindToTSNode>(node: ts.Node, kind: T): node is KindToTSNode[T] {
	return node.kind === kind;
}

export function getParentOrThrow(node: ts.Node) {
	if (node.parent) {
		return node.parent;
	}
	throw new Error("Unable to find parent");
}

export function getParentIfKind<T extends keyof KindToTSNode>(node: ts.Node, kind: T): KindToTSNode[T] | undefined {
	if (is(node.parent, kind)) {
		return node.parent;
	}
}

export function getFirstAncestorByKind<T extends keyof KindToTSNode>(node: ts.Node, kind: T): KindToTSNode[T] | undefined {
	while (node.parent) {
		if (is(node.parent, kind)) {
			return node.parent;
		}
		node = node.parent;
	}
}

export function getFirstAncestorOrThrow(node: ts.Node, condition: (ancestor: ts.Node) => boolean) {
	while (node.parent) {
		if (condition(node.parent)) {
			return node.parent;
		}
		node = node.parent;
	}
	throw new Error("Unable to find ancestor");
}

export function getFirstChild(node: ts.Node, condition: (child: ts.Node) => boolean) {
	for (let i = 0; i < node.getChildCount(); i++) {
		const child = node.getChildAt(i);
		if (condition(child)) {
			return child;
		}
	}
}

export function getLastChild(node: ts.Node, condition: (child: ts.Node) => boolean) {
	for (let i = node.getChildCount() - 1; i >= 0; i--) {
		const child = node.getChildAt(i);
		if (condition(child)) {
			return child;
		}
	}
}

export function getFirstChildOrThrow(node: ts.Node, condition: (child: ts.Node) => boolean) {
	const child = getFirstChild(node, condition);
	if (child) {
		return child;
	}
	throw new Error("Unable to find child");
}

export function getLastChildOrThrow(node: ts.Node, condition: (child: ts.Node) => boolean) {
	const child = getLastChild(node, condition);
	if (child) {
		return child;
	}
	throw new Error("Unable to find child");
}

type HasName = ts.Node & { name: ts.Identifier | ts.PropertyName | ts.BindingName };

export function getName(node: HasName) {
	const nameNode = node.name;
	if (ts.isComputedPropertyName(nameNode)) {
		return nameNode.getText();
	} else if (ts.isObjectBindingPattern(nameNode)) {
		return ""; // TODO
	} else if (ts.isArrayBindingPattern(nameNode)) {
		return ""; // TODO
	} else {
		return nameNode.text;
	}
}

export function isConstEnum(node: ts.EnumDeclaration) {
	return node.modifiers !== undefined && node.modifiers.some(modifier => modifier.kind === ts.SyntaxKind.ConstKeyword)
}

export function isThisExpression(node: ts.Node): node is ts.ThisExpression {
	return node.kind === ts.SyntaxKind.ThisKeyword;
}

export function isSuperExpression(node: ts.Node): node is ts.SuperExpression {
	return node.kind === ts.SyntaxKind.SuperKeyword;
}

export function isArrayType(type: ts.Type): boolean {
	throw new Error("Not implemented");
}

export function isTupleType(type: ts.Type): boolean {
	throw new Error("Not implemented");
}
