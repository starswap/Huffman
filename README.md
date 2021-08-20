# Huffman
Huffman Coding utility in Haskell

Usage: 
```huffman [encode/decode] inputFile outputFile```

Files to be compressed should be newline terminated in compliance with POSIX.

Currently supports only ASCII text files.

Todo: <ul>
  <li>Use Zippers to improve priority queue efficiency</li>
  <li>Make it work on bytes and not just ASCII files</li>
  <li>Modulise and comment and reduce the number of functions, rewriting in point free style for clarity</li>
</ul>
