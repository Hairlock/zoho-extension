const path = require('path')
const merge = require('webpack-merge')
const ExtractTextPlugin = require('extract-text-webpack-plugin')

const common = require('../common/webpack.common')

module.exports = merge(common, {
  //entry: ['./popup/src/index.js'],
  entry: [path.join(__dirname, './src/index.js')],
  output: {
    path: path.join(__dirname, '../dist'),
    filename: 'popup.js',
  },
  plugins: [new ExtractTextPlugin('content.css')],
  module: {
    rules: [
      {
        test: /\.(css|scss)$/,
        use: ExtractTextPlugin.extract({
          fallback: 'style-loader',
          use: ['css-loader'],
        }),
      },
    ]
  }
})
