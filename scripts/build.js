const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');
const fsExtra = require('fs-extra');

const codyDir = process.env.CODY_DIR;

// Determine the agent directory based on CODY_DIR or sibling directory
const agentDir = codyDir ? path.resolve(codyDir, 'agent') : path.resolve(__dirname, '../../cody/agent');
const distDir = path.resolve(__dirname, '../dist');

console.log(`Using agent directory: ${agentDir}`);

if (!fs.existsSync(agentDir)) {
    console.error(`Agent directory does not exist: ${agentDir}`);
    process.exit(1);
}

const agentDistDir = path.join(agentDir, 'dist');
if (!fs.existsSync(agentDistDir)) {
    console.error(`Agent dist directory does not exist: ${agentDistDir}`);
    process.exit(1);
}

try{
    console.log('Running pnpm install...');
    execSync('pnpm install', { cwd: agentDir, stdio: 'inherit' });

    console.log('Running pnpm build...');
    execSync('pnpm build', { cwd: agentDir, stdio: 'inherit' });
} catch (error) {
    console.error('Failed to run pnpm install or pnpm build:', error.message);
    process.exit(1);
}

try {
    console.log(`Copying from ${agentDistDir} to ${distDir}...`);
    fsExtra.copySync(agentDistDir, distDir, { overwrite: true });
    console.log('Directory copied successfully!');
} catch (error) {
    console.error('Failed to copy directory:', error.message);
}

console.log('Build and copy completed.');
