Revision history for capricon
===============

### release-capricon-0.13.1 / package-capricon-0.13.1

  - Implement a new kind of "quiet" mustache in CaPriCon, to allow precise formatting commands to be inserted into a document

### release-capricon-0.13 / package-capricon-0.13

  - Upend the CaPriCon rendering pipeline, to allow for multiple output backends (for now, HTML and LaTeX)

### release-capricon-0.12.3 / package-capricon-0.12.3

  - Correct the behavior `type_of` function, causing it to fail on ill-typed terms instead of falsely succeeding

### release-capricon-0.12.2.1 / package-capricon-0.12.2.1

  - Make CaPriCon preserve whitespace at the end of documents

### release-capricon-0.12.2 / package-capricon-0.12.2

  - Update the CaPriCon HTML scaffold to fit the new stylesheet

### release-capricon-0.12.1 / package-capricon-0.12.1

  - Introduce a new 'set-stack' builtin, to go with the new backquote features
  - Annotate matching braces with text spans when generating CaPriCon paragraphs, to allow better syntax highlighting to take place
  - Output quoted characters as-is when generating Markdown from CaPriCon, to preserve the correct newline count
  - Make the order of evaluation left-to-right at all backquote depths in concatenative languages.

### release-capricon-0.12 / package-capricon-0.12

  - Add support for writing custom examples after CaPriCon code blocks, to make the resulting pages more easily explorable
  - Make better backquotes for CaPriCon

### release-capricon-0.11

  - Change WiQEE.hs to be used as a Web Worker instead of running in the application thread
  - Release CaPriCon 0.11, now with a working module system

### package-capricon-0.11

  - Start offering basic SVG generation of formulae with the %g format in CaPriCon
  - Adjust CaPriCon output to allow syntax highlighting to take place in code blocks
  - Notify the user of changes in the console's state when it becomes active (in WiQEE)
  - Define a 'cons' builtin for CaPriCon (and other concatenative languages)
  - Remove the 'module' builtin in favor of the more flexible 'redirect' / 'set-vocabulary' combination
