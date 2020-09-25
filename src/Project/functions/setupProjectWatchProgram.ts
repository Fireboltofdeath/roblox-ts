import ts from "byots";
import chokidar from "chokidar";
import fs from "fs-extra";
import { createProjectServices, ProjectData } from "Project";
import { cleanup, tryRemoveOutput } from "Project/functions/cleanup";
import { compileFiles } from "Project/functions/compileFiles";
import { copyFiles, copyItem } from "Project/functions/copyFiles";
import { copyInclude } from "Project/functions/copyInclude";
import { createProgramFactory } from "Project/functions/createProgramFactory";
import { getChangedSourceFiles } from "Project/functions/getChangedSourceFiles";
import { getParsedCommandLine } from "Project/functions/getParsedCommandLine";
import { getRootDirs } from "Project/functions/getRootDirs";
import { ProjectServices } from "Project/types";
import { DiagnosticError } from "Shared/errors/DiagnosticError";
import { assert } from "Shared/util/assert";
import { isCompilableFile } from "Shared/util/isCompilableFile";
import { walkDirectorySync } from "Shared/util/walkDirectorySync";

export function setupProjectWatchProgram(data: ProjectData, usePolling: boolean) {
	const { fileNames, options } = getParsedCommandLine(data);
	const fileNamesSet = new Set(fileNames);

	let initialCompileCompleted = false;
	let collecting = false;
	let filesToAdd = new Set<string>();
	let filesToChange = new Set<string>();
	let filesToDelete = new Set<string>();

	const watchReporter = ts.createWatchStatusReporter(ts.sys, true);
	const diagnosticReporter = ts.createDiagnosticReporter(ts.sys, true);

	function reportText(messageText: string) {
		watchReporter(
			{
				category: ts.DiagnosticCategory.Message,
				messageText,
				code: 0,
				file: undefined,
				length: undefined,
				start: undefined,
			},
			ts.sys.newLine,
			options,
		);
	}

	function reportEmitResult(emitResult: ts.EmitResult) {
		for (const diagnostic of emitResult.diagnostics) {
			diagnosticReporter(diagnostic);
		}
		reportText(
			`Found ${emitResult.diagnostics.length} error${
				emitResult.diagnostics.length === 1 ? "" : "s"
			}. Watching for file changes.`,
		);
	}

	let program: ts.EmitAndSemanticDiagnosticsBuilderProgram | undefined;
	let services: ProjectServices | undefined;
	const createProgram = createProgramFactory(data, options);
	function refreshProgram() {
		try {
			program = createProgram([...fileNamesSet], options);
			services = createProjectServices(program, data);
		} catch (e) {
			if (e instanceof DiagnosticError) {
				for (const diagnostic of e.diagnostics) {
					diagnosticReporter(diagnostic);
				}
			} else {
				throw e;
			}
		}
	}

	function runInitialCompile() {
		refreshProgram();
		assert(program && services);
		cleanup(services.pathTranslator);
		copyInclude(data);
		copyFiles(services, new Set(getRootDirs(options)));
		const sourceFiles = getChangedSourceFiles(program);
		const emitResult = compileFiles(program, data, services, sourceFiles);
		if (emitResult.diagnostics.length === 0) {
			initialCompileCompleted = true;
		}
		return emitResult;
	}

	function runIncrementalCompile(additions: Set<string>, changes: Set<string>, removals: Set<string>): ts.EmitResult {
		const filesToCompile = new Set<string>();
		const filesToCopy = new Set<string>();
		const filesToClean = new Set<string>();

		for (const fsPath of additions) {
			if (isCompilableFile(fsPath)) {
				fileNamesSet.add(fsPath);
				filesToCompile.add(fsPath);
			} else {
				filesToCopy.add(fsPath);
			}
			if (fs.statSync(fsPath).isDirectory()) {
				walkDirectorySync(fsPath, item => {
					if (isCompilableFile(item)) {
						filesToCompile.add(item);
					}
				});
			}
		}

		for (const fsPath of changes) {
			if (isCompilableFile(fsPath)) {
				filesToCompile.add(fsPath);
			} else {
				filesToCopy.add(fsPath);
			}
		}

		for (const fsPath of removals) {
			fileNamesSet.delete(fsPath);
			filesToClean.add(fsPath);
		}

		refreshProgram();
		assert(program && services);
		for (const fsPath of filesToClean) {
			tryRemoveOutput(services.pathTranslator, services.pathTranslator.getOutputPath(fsPath));
		}
		for (const fsPath of filesToCopy) {
			copyItem(services, fsPath);
		}
		const sourceFiles = getChangedSourceFiles(program, options.incremental ? undefined : [...filesToCompile]);
		const emitResult = compileFiles(program, data, services, sourceFiles);
		return emitResult;
	}

	function closeEventCollection() {
		collecting = false;
		const additions = filesToAdd;
		const changes = filesToChange;
		const removals = filesToDelete;
		filesToAdd = new Set();
		filesToChange = new Set();
		filesToDelete = new Set();

		const emitResult = !initialCompileCompleted
			? runInitialCompile()
			: runIncrementalCompile(additions, changes, removals);
		reportEmitResult(emitResult);
	}

	function collectEvent(type: "add" | "change" | "remove", fsPath: string) {
		if (type === "add") {
			filesToAdd.add(fsPath);
		} else if (type === "change") {
			filesToChange.add(fsPath);
		} else {
			filesToDelete.add(fsPath);
		}
		if (!collecting) {
			collecting = true;
			reportText("File change detected. Starting incremental compilation...");
			setTimeout(closeEventCollection, 100);
		}
	}

	chokidar
		.watch(getRootDirs(options), {
			awaitWriteFinish: {
				pollInterval: 10,
				stabilityThreshold: 50,
			},
			ignoreInitial: true,
			disableGlobbing: true,
			usePolling,
		})
		.on("add", fsPath => collectEvent("add", fsPath))
		.on("addDir", fsPath => collectEvent("add", fsPath))
		.on("change", fsPath => collectEvent("change", fsPath))
		.on("unlink", fsPath => collectEvent("remove", fsPath))
		.on("unlinkDir", fsPath => collectEvent("remove", fsPath));

	reportText("Starting compilation in watch mode...");
	reportEmitResult(runInitialCompile());
}