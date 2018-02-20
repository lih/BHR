% The Curly Document format

Some features of Curly use what is called the *document* format to
store and display loosely structured textual descriptions, for various
purposes from documentation to structured queries.

The document structure and syntax
=================================

This document format is similar in structure to a lightweight DOM :

  - a document is a node
  
  - a node can either be a single span of verbatim text, or a tag,
    written `{TAG_NAME(.CLASS)... SUBNODE...}` where each subnode can
    itself be either text or a tag

    Tags can have additional classes, which are treated as secondary
    tag names for styling purposes (see below for more information on
    styles).

  - the parser treats non-empty spans of whitespace as separators for
    tags and text. Thus, the document `{p An example
    document.}` has the following structure :

        {p}
        - "An"
        - "example"
        - "document."

  - an exception to that rule is that any untagged text between double
    quotes (`"`) is taken verbatim without consideration for
    whitespace. This means that `{p "An example verbatim document"}` will
    be structured like this instead :

        {p}
        - "An example verbatim document"

    Subtags can appear between the double quotes, in which case those
    subtags will be spliced between text spans, and wrapped inside a
    `splice` tag. To illustrate, the document `{p "An example verbatim
    document with {em subtags} !"}` will have the following structure
    :
    
        {p}
        - {splice}
          - "An example verbatim document with "
          - {em}
            - "subtags"
          - " !"

    (Note the preservation of whitespace at the beginning and end of
    text spans)

  - characters with special meaning like `{` or `"` can be inserted
    into regular text by prefixing them with a `\`

Rendering documents
===================

Most output in Curly is rendered from documents, to provide a uniform
look-and-feel to the interface. This rendering can be configured by
the `style <tag-or-class-name> <property> <value>` command within
Curly sessions.

Here is a quick description of the available styling properties :

  - `color` and `bgcolor` to set the foreground/background colors of
    any text within the tag. Colors can be names (`red`, `black`, ...)
    or hexadecimal RGB descriptions of the form `#RRGGBB` or `#RGB`

  - `bold`, `underline`, `italic` are pretty self-explanatory

  - `display` controls whether the text within the tag needs to be
    `inline` (the default), on a different `line` or on a different
    paragraph (with `block`)

  - `indent` is an integer that describes how many columns should be
    added to the current indentation.

  - `prefix` behaves like `indent`, except that the first line is
    indented with a custom prefix

  - `word-wrap` contains the maximum width of text that is allowed
    before wrapping to the next line. If unset, text will never be
    wrapped.

Documents as formatting patterns
================================

In some cases, Curly uses documents as templates for structured
queries. In those cases, the documents are "evaluated" before being
printed out.

Evaluation doesn't affect the structure of the document, except for a
few special tags, that are substituted upon each appearance. Under
certain conditions (described below), evaluation can fail. When that
happens, the whole pattern is considered invalid in its environment,
and will be omitted from the result.

Curly recognizes the following special forms during document
evaluation :

  - `{env VAR_NAME}` evaluates to the value of an environment
    variable. If that variable isn't set, evaluation fails.

  - `{= X Y}`, `{<= X Y}`, `{>= X Y}`, `{< X Y}`, `{> X Y}` are
    comparison operators, acting on version-aware strings. If the
    comparison fails, so does evaluation for this pattern. If it
    succeeds, the value of X is returned.

  - `{matches X Y}` returns X if its value matches the wildcard
    expression Y. This operator allows queries like 

  - `{when X BODY...}` and `{unless X BODY...}` are conditional
    expressions. As their name implies, they succeed in producing
    `BODY` when (or unless) the condition X succeeds.

    In both cases, the value of X is discarded after successful
    evaluation, and will not appear in the output document.

  - `{or X1 ... XN}` returns the value of the first of its parameters
    to succeed. It fails when all its parameters fail.

  - `{$ VAR_NAME}` evaluates the variable name and returns the
    value of the corresponding variable in the current evaluation
    environment. If there is no variable by that name, evaluation
    fails.

    The evaluation environment contains different sorts of variables
    depending on the context :

      - when evaluating for a symbol as part of the `show` command,
        the variables `name`, `type`, `doc`, `impl` and `strictness`
        are positioned to that symbol's corresponding property.

        when evaluating for an expression, with `show (EXPRESSION)
        PATTERN`, the same properties are made available, with the
        exception of `doc`. The `name` property contains the entire
        expression in textual form.

      - when evaluating for a library, as part of the `repository`
        command or the `package PATTERN` mount description, the
        variables are found in that library's metadata. They usually
        include fields like `name`, `version`, `synopsis`, `publisher
        public-key`.

        Additionally, the variables `repository key-name` and
        `repository branch-name` are positioned respectively to the
        names of the key and branch on which that library was found.

      - when evaluating for an option in a configuration file, the
        variables are set from the names given on the corresponding
        flag constraint. Simply put, a constraint of `+flag:param`
        will introduce the `{$ param}` variable to its option

  - during interactive sessions, you can define meta-patterns with the
    `pattern PATTERN_NAME ARG... = PAT` command. Meta-patterns can
    then be called using the `{call PATTERN_NAME PATTERN_ARGS}` form,
    or the simpler but less general `{PATTERN_NAME PATTERN_ARGS}`.

    Meta-patterns are only available during interactive sessions, and
    cannot be used as part of a mount description, on the command line
    or within configuration files (or rather, using them will cause
    evaluation to fail).





