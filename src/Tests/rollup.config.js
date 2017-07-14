import fable from 'rollup-plugin-fable';
const path = require('path');

function resolve(filePath) {
    return path.resolve(__dirname, filePath)
}

export default {
    entry: resolve('Tests.fsproj'),
    dest: resolve('../../build/tests.bundle.js'),
    plugins: [
        fable()
    ],
    external: ['assert'],
    format: 'cjs'
};
