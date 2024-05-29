const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const codyDir = process.env.CODY_DIR;

// Determine the agent directory based on CODY_DIR or sibling directory
const agentDir = codyDir ? path.resolve(codyDir, 'agent') : path.resolve(__dirname, '../cody/agent');
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

try {
    console.log('Running pnpm install...');
    execSync('pnpm install', { cwd: agentDir, stdio: 'inherit' });

    console.log('Running pnpm build...');
    execSync('pnpm build', { cwd: agentDir, stdio: 'inherit' });
} catch (error) {
    console.error('Failed to run pnpm install or pnpm build:', error.message);
    process.exit(1);
}

if (!fs.existsSync(distDir)) {
    fs.mkdirSync(distDir);
}

// List of files to copy
const filesToCopy = [
    'index.js',
    'index.js.map'
];

filesToCopy.forEach(file => {
    const srcFile = path.join(agentDistDir, file);
    const destFile = path.join(distDir, file);

    try {
        if (fs.existsSync(srcFile)) {
            fs.copyFileSync(srcFile, destFile);
            console.log(`Copied ${file} to ${distDir}`);
        } else {
            console.error(`File does not exist: ${srcFile}`);
        }
    } catch (error) {
        console.error(`Failed to copy ${file}:`, error.message);
    }
});

// Copy .wasm files
const wasmFiles = fs.readdirSync(agentDistDir)
    .filter(file => file.endsWith('.wasm'));

wasmFiles.forEach(file => {
    const srcFile = path.join(agentDistDir, file);
    const destFile = path.join(distDir, file);

    try {
        fs.copyFileSync(srcFile, destFile);
        console.log(`Copied ${file} to ${distDir}`);
    } catch (error) {
        console.error(`Failed to copy ${file}:`, error.message);
    }
});

console.log('Build and copy completed.');
