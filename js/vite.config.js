const path = require('path')
const { defineConfig } = require('vite')

module.exports = defineConfig({
  build: {
    lib: {
      entry: path.resolve(__dirname, 'lib/main.js'),
      name: 'trails',
      formats: ['es'],
      fileName: (format) => `trails.${format}.js`
    }
  }
});
