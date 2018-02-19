const path = require('path');

module.exports = {
  entry: './src/entry.js',
  output: {
    path: path.join(__dirname, "public"),
    filename: 'bundle.js',
  },
};

