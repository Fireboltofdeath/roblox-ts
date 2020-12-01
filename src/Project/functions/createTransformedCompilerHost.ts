import ts from "byots";
import { LogService } from "Shared/classes/LogService";

type SourceMapping = Map<string, { original: ts.SourceFile; transformed: ts.SourceFile; cache?: string }>;
const printer = ts.createPrinter();
export function createTransformedCompilerHost(options: ts.CompilerOptions, mapping: SourceMapping): ts.CompilerHost {
	const host = ts.createCompilerHost(options, true);
	host.readFile = file => {
		const map = mapping.get(file);
		if (map) {
			if (map.original !== map.transformed) {
				if (!map.cache) map.cache = printer.printFile(map.transformed);
				return map.cache;
			}
		}
		return ts.sys.readFile(file);
	};

	const getSourceFile = host.getSourceFile.bind(host);
	host.getSourceFile = (fileName, languageVersion, onError, shouldCreate) => {
		const original = getSourceFile(fileName, languageVersion, onError, shouldCreate);
		if (original) {
			original.version = mapping.get(fileName)?.original.version ?? "0.0.0";
		}
		return original;
	};
	return host;
}
