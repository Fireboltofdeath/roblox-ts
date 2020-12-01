/* eslint-disable @typescript-eslint/no-var-requires */
/* eslint-disable @typescript-eslint/no-require-imports */
import ts from "byots";
import { TransformerPluginConfig } from "Project/types";
import resolve from "resolve";

export interface TransformerBasePlugin {
	before?: ts.TransformerFactory<ts.SourceFile>;
	after?: ts.TransformerFactory<ts.SourceFile>;
	afterDeclarations?: ts.TransformerFactory<ts.SourceFile | ts.Bundle>;
}
export type TransformerList = Required<ts.CustomTransformers>;

export type TransformerPlugin = TransformerBasePlugin | ts.TransformerFactory<ts.SourceFile>;

export type LSPattern = (ls: ts.LanguageService, config: unknown) => TransformerPlugin;
export type ProgramPattern = (program: ts.Program, config: unknown, helpers?: { ts: typeof ts }) => TransformerPlugin;
export type CompilerOptionsPattern = (compilerOpts: ts.CompilerOptions, config: unknown) => TransformerPlugin;
export type ConfigPattern = (config: unknown) => TransformerPlugin;
export type TypeCheckerPattern = (checker: ts.TypeChecker, config: unknown) => TransformerPlugin;
export type RawPattern = (
	context: ts.TransformationContext,
	program: ts.Program,
	config: unknown,
) => ts.Transformer<ts.SourceFile>;
export type PluginFactory =
	| LSPattern
	| ProgramPattern
	| ConfigPattern
	| CompilerOptionsPattern
	| TypeCheckerPattern
	| RawPattern;

export function getTransformerFromFactory(
	factory: PluginFactory,
	config: TransformerPluginConfig,
	program: ts.Program,
) {
	const { after, afterDeclarations, transform, type, ...manualConfig } = config;
	let transformer: TransformerPlugin;
	switch (type) {
		case undefined:
		case "program":
			transformer = (factory as ProgramPattern)(program, manualConfig, { ts });
			break;
		case "checker":
			transformer = (factory as TypeCheckerPattern)(program.getTypeChecker(), manualConfig);
			break;
		case "compilerOptions":
			transformer = (factory as CompilerOptionsPattern)(program.getCompilerOptions(), manualConfig);
			break;
		case "config":
			transformer = (factory as ConfigPattern)(manualConfig);
			break;
		case "raw":
			transformer = (ctx: ts.TransformationContext) => (factory as RawPattern)(ctx, program, manualConfig);
			break;
		default:
			return undefined;
	}

	if (typeof transformer === "function") {
		if (after) return { after: transformer };
		if (afterDeclarations)
			return { afterDeclarations: transformer as ts.TransformerFactory<ts.SourceFile | ts.Bundle> };
		return { before: transformer };
	}
	return transformer;
}

export function flattenIntoTransformers(
	transformers: ts.CustomTransformers,
): Array<ts.TransformerFactory<ts.SourceFile | ts.Bundle>> {
	const result: Array<ts.TransformerFactory<ts.SourceFile | ts.Bundle>> = [];
	result.push(
		...(transformers.after as Array<ts.TransformerFactory<ts.SourceFile | ts.Bundle>>),
		...(transformers.before as Array<ts.TransformerFactory<ts.SourceFile | ts.Bundle>>),
		...(transformers.afterDeclarations as Array<ts.TransformerFactory<ts.SourceFile | ts.Bundle>>),
	);
	return result;
}

let tsNodeIncluded = false;
export function createTransformerList(
	param: { program: ts.Program } | { ls: ts.LanguageService },
	configs: Array<TransformerPluginConfig>,
	baseDir: string,
): ts.CustomTransformers {
	const transforms: ts.CustomTransformers = {
		before: [],
		after: [],
		afterDeclarations: [],
	};
	let program, ls;
	if ("ls" in param) {
		program = param.ls.getProgram()!;
		ls = param.ls;
	} else {
		program = param.program;
	}
	for (const config of configs) {
		if (!config.transform) continue;
		if (
			!tsNodeIncluded &&
			config.transform.match(/\.tsx?$/) &&
			(module.parent!.parent === null ||
				module.parent!.parent!.parent === null ||
				module.parent!.parent!.parent!.id.split(/[\/\\]/).indexOf("ts-node") === -1)
		) {
			require("ts-node").register({
				transpileOnly: true,
				skipProject: true,
				compilerOptions: {
					target: "ES2018",
					jsx: "react",
					esModuleInterop: true,
					module: "commonjs",
				},
			});
			tsNodeIncluded = true;
		}
		const modulePath = resolve.sync(config.transform, { basedir: baseDir });
		const commonjsModule: PluginFactory | { [key: string]: PluginFactory } = require(modulePath);

		const factoryModule = typeof commonjsModule === "function" ? { default: commonjsModule } : commonjsModule;
		const factory = factoryModule[config.import ?? "default"];

		if (factory && typeof factory === "function") {
			const transformer = getTransformerFromFactory(factory, config, program);
			if (transformer) {
				if (transformer.afterDeclarations) {
					transforms.afterDeclarations?.push(transformer.afterDeclarations);
				}
				if (transformer.after) {
					transforms.after?.push(transformer.after);
				}
				if (transformer.before) {
					transforms.before?.push(transformer.before);
				}
			}
		}
	}

	return transforms;
}
