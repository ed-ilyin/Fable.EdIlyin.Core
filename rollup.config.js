import fable from 'rollup-plugin-fable';
const path = require('path');

function resolve(filePath) {
    return path.resolve(__dirname, filePath)
}

export default {
    input: resolve('src/Tests/Tests.fsproj'),
    output: {
        file: resolve('build/tests.bundle.js'),
        format: 'cjs'
    },
    plugins: [fable({})],
    external: ['assert', 'es6-promise', 'isomorphic-fetch'],
};