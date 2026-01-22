#!/usr/bin/env node

const { spawn } = require("child_process");
const path = require("path");
const fs = require("fs");

function findProjectByWalkingUp(filePath) {
  const workspaceRoot = process.cwd();
  let currentDir = path.dirname(path.resolve(filePath));

  while (
    currentDir !== workspaceRoot &&
    currentDir !== path.dirname(currentDir)
  ) {
    const projectJsonPath = path.join(currentDir, "project.json");

    if (fs.existsSync(projectJsonPath)) {
      try {
        const projectConfig = JSON.parse(
          fs.readFileSync(projectJsonPath, "utf-8")
        );
        return projectConfig.name;
      } catch (error) {
        // Silently continue searching
      }
    }

    currentDir = path.dirname(currentDir);
  }

  console.error(`No project found for file: ${filePath}`);
  process.exit(1);
}

// Parse arguments
const args = process.argv.slice(2);
const filePath = args[0];
let testPattern = null;

// Look for test pattern in various formats
for (let i = 1; i < args.length; i++) {
  if (args[i] === "-t" && i + 1 < args.length) {
    testPattern = args[i + 1];
    break;
  } else if (args[i].startsWith("--testNamePattern=")) {
    testPattern = args[i].split("=")[1];
    break;
  } else if (args[i] === "--testNamePattern" && i + 1 < args.length) {
    testPattern = args[i + 1];
    break;
  }
}

if (!filePath) {
  process.exit(1);
}

const projectName = findProjectByWalkingUp(filePath);

// Convert to relative path for the test command
const workspaceRoot = process.cwd();
const relativePath = path.isAbsolute(filePath)
  ? path.relative(workspaceRoot, filePath)
  : filePath;

// Build command arguments array - this prevents shell interpretation issues
const commandArgs = [
  "nx",
  "test",
  projectName,
  `--testPathPattern=${relativePath}`,
];

if (
  testPattern &&
  testPattern !== "-c" &&
  !testPattern.includes("jest.config")
) {
  // Pass the test pattern as a separate argument - no shell escaping needed
  commandArgs.push(`--testNamePattern=${testPattern}`);
}

// Use spawn instead of execSync to avoid shell interpretation
const child = spawn("npx", commandArgs, {
  stdio: "inherit",
  cwd: process.cwd(),
});

child.on("close", (code) => {
  process.exit(code);
});

child.on("error", (error) => {
  console.error("Failed to start test:", error.message);
  process.exit(1);
});
