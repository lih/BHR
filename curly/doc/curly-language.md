% The Curly Source Format

Curly Expressions
================

Curly is based on the lambda-calculus of old. It is called Curly
because, rather than use a prefix notation for quantification (like
`λx y z. E`{.curly}), I chose a more balanced alternative in the curly braces,
with `{x y z: E}`{.curly}. The block notation should be familiar to many
developpers already, except that Curly blocks accept parameters as
well. 

The rules of lambda-calculus still apply, though. With few exceptions,
the following will hold true : `{x y ...: E} X`{.curly} is equivalent
to `{y ...: E[x / X]}`{.curly} where `E[x/X]`{.curly} means "the expression E,
with all occurrences of x replaced by X".

Here are a few functions to help you get a feel of the language : `{x:
x}`{.curly}, the identity function; `{x _: x}`{.curly}, the constant function; `{f x
y: f y x}`{.curly}, a function to flip its first arguments' parameters.

Functions and operators
---------------------

Functions (or "blocks", or "lambdas", however one may want to call
them) can be applied to their parameters with the following syntax `f
x y z`{.curly}, where `f`{.curly} is a function and `x`{.curly},
`y`{.curly}, `z`{.curly} its parameters. This is called *prefix
notation* (or Polish notation, in honor of its inventor, Jan
Łukasiewicz).

In addition to the prefix notation, Curly allows the definition of
syntactic operators, that may appear in infix (`x + y`{.curly}),
postfix (`n!`{.curly}) or even multifix (`if X then Y else Z`{.curly})
notation.

Operators are always associated with a symbol, that determines the
operator's scope. The name of that symbol describes the syntax of the
operator, where `_`{.curly} marks the need for a parameter.

For example, you can define the usual arithmetic operators by
associating the symbols `_+_`{.curly}, `_-_`{.curly}, `_*_`{.curly}
and `_/_`{.curly} to their corresponding function. The "square"
function can be abbreviated to `_²`{.curly} in the same manner.

~~~~{.curly}
# Example : arithmetic operators
define _+_ = addInt
define _-_ = subInt
define _*_ = mulInt
define _/_ = divInt
define _² x = x*x
~~~~~~~

In some cases, there can be ambiguity in the syntax. For example, the
expression `x+y*z`{.curly} can be interpreted as either
`(x+y)*z`{.curly} or `x+(y*z)`{.curly}. In those cases, the operators
defined last have priority over the ones defined earlier. Thus, if we
want our arithmetic operators to be parsed correctly, we only have to
define them in the correct order, that is `_+_`{.curly}, then
`_-_`{.curly}, and `_*_`{.curly}, and finally `_/_`{.curly}.

Operators can also be exported to / imported from other modules. In
those cases, since the order of definition is important, the operators
are imported in the same order that they were exported from the
original module (unless specified otherwise by the `import`{.curly}
statement).

### Partial application of operators

Since operators are basically "syntaxes with holes", it seems natural
to sometimes leave some of the holes empty for future use. Curly
allows any combination of operators within parentheses to be left
incomplete by writing `_`{.curly} where a parameter should be left
blank.

To illustrate, you can easily define affine functions with `define
affine f0 df = f0+_*df`{.curly}, or boolean operators with `define
_or_ = if _ then true else _`{.curly}. In each case, the underscores
stand for missing arguments, which are abstracted in the order in
which they appear.

For instance, `x+_*y`{.curly} is equivalent to `{z: x+z*y}`{.curly},
and `if _ then true else _`{.curly} is equivalent to `{x y: if x then
true else y}`{.curly}.

### Local definitions in argument lists

We've now seen that functions can accept abstract arguments in the
form of variables. If we want to give a name to the value `(1+2)` in
the expression `(1+2)*((1+2)+3)` we can do so by writing the
equivalent expression `{x: x*(x+3)} (1+2)`.

This is a functionally correct way to factor out a value, but it's not
a very practical one for two reasons :

  - the variable name can be syntactically far away from its value,
    forcing the reader to jump back and forth in the code to find out its flow
  - when dealing with multiple nested patterns, a lot of brackets become necessary

To avoid both those problems, Curly allows the previous expression to
be rewritten as `{{x = 1+2}: x*(x+3)}`. More generally, the special
form `{var arg... = body}` can appear anywhere in an argument list,
and has the effect of declaring a local variable `var` with a value of
`{arg...: body}`.

A local definition can refer to any parameter that precedes it,
including other local definitions. For example, the expression `{x {x2
= x*x} y {xy2 = x2+y*y}: sqrt xy2}` describes a function of two
arguments (`x` and `y`), that returns `sqrt (x*x+y*y)`.

### Continuations in argument lists

**TODO** : describe how the syntax `{x... (f) y...: z}` is equivalent to
`{x...: f {y...: z}}`, and why it can be useful.

Source Directives
=================

A Curly source file is a series of directives, that act upon a
"library context". Each directive will modify the context, until the
end of the file is reached, at which point the resulting library is
ready to be used by other parts of your build.

### Defining symbols

*Usage:* `define NAME ARG... = EXPRESSION`{.curly}
	 *OR* `define NAME = {ARG...: EXPRESSION}`{.curly}

This directive, as it name implies, defines the symbol NAME to the
given expression. The first form is syntactic sugar for the second
form.

Seeing as you may want to categorize the definitions of different
sorts of objects, the following keywords are also recognized instead
of `define` : `operator`, `function`, and `let`. Thus, `operator _++ x
= add x 1` is a perfectly valid Curly definition.

### Definining types

*Usage:* `type TC : TN VISIBLE_ARG... = TD HIDDEN_ARG... : WITNESS`{.curly}

Defines the polymorphic type `TN a b ...`, along with two symbols `TC`
and `TD`, the type's constructor and destructor.

If `TC` and `TN` are identical, you can simply write `type TN ...`
instead.

The witness is an expression whose type is used as the internal
representation of `TN`. If we call that type `TI`{.curly}, then we
have `TC : TI -> TN VISIBLE_ARG...`{.curly} and `TD : TN
VISIBLE_ARG... -> TI`{.curly}.

#### Examples

Booleans :

~~~~{.curly}
type mkBool : Bool = boolImpl : {x _: x} or {x _: x}
define true = Bool {x _: x}
define false = Bool {_ x: x}
~~~~~~~

Lists :

~~~~{.curly}
type List : [_] a = listImpl : {_ x: x} or {k x: k a(...) x}
define nil = List {_ x: x}
define cons a l = List {k x: k a (listImpl l k x)}
~~~~~~~~

Showable :

~~~~{.curly}
type Showable = showableImpl a : const a(...) Show(a(...))
define defShowable = Showable 1
define useShowable sh = showableImpl sh {x: "Value of x: ${show x}"}
~~~~~~

### Type-indexed families

*Usage:* `family NAME([INDEX])... ARG... : WITNESS`

Defines a symbol describing a family of type-indexed functions. That
symbol can then be `define`d multiple times to provide the instances
for this family.

The ARGS are type constraints, as in `type`, and can be referenced
by the WITNESS to infer the symbol's type.

The INDICES can be any nonempty subset of the ARGS, and are used for
instance resolution. A family constraint can be resolved if any of its
indices lead to an instance. A new instance may not be defined if it
overrides another, even at a single index.

### Importing/exporting symbols

*Usage:* `import TREE` *OR* `export TREE`

Imports a subtree of the local source context. The second form exports
a tree where the leaves are local symbols.

In its simplest form, a tree can be either a symbol, or a module node
of the form `MODULE_NAME{SUBTREE...}`. If the tree is a symbol, it can
optionally be followed by a *local name* in parentheses, whose meaning
will be described below.

When a module node has a single subtree, Curly also recognizes the
syntax `MODULE_NAME.SUBTREE` as a shorthand for
`MODULE_NAME{SUBTREE}`. 

#### Imports

During imports, the TREE acts as a filter for the local module. Each
module node of the tree is matched with an equivalent nonempty node
from the local context, and each leaf symbol is imported.

If a leaf symbol corresponds to a module node in the context, Curly
will import all symbols under that node. Thus, you can avoid manually
listing all the symbols of every library you import.

If a leaf symbol has a local name, then it is imported under that
name, independently of the one it had originally. This can be useful
in order to avoid the imprecision of importing similarly named symbols
from different libraries.

#### Exports

During exports, the tree is simply exported as-is, with each leaf
symbol being made to correspond with a local symbol (either defined
locally, or imported from another module).

If a leaf symbol has a local name, then the local symbol of that name
is exported instead of the leaf's name.

### Definining system-specific values

Sometimes, in the interest of efficiency or portability, it can be
useful to have a symbol represent different implementations of a
function on different systems. To define such symbols, Curly provides
the `multi` pragma, with the following syntax :

*Usage*: `multi SYMBOL = DEFAULT_SYMBOL [, SYSTEM_NAME SYSTEM_SYMBOL]...`

This pragma define the multi-system symbol `SYMBOL`, with a system-specific
implementation for each `SYSTEM_NAME`, and a fallback implementation
defined in `DEFAULT_SYMBOL`.

#### Example: packaging an external C library

_Warning_: this is still a thought experiment. The Curly FFI is not yet
capable of integrating with C, although it will be very soon.

Imagine you have a C library called libX. You have the source for this
library, and maybe a C cross-compiling toolchain for several
systems. Using all this, you manage to compile libX into three dynamic
libraries, that each run on a different ABI and maybe a different
architecture. Let's call these `libX_arm-linux.so`,
`libX_x86-linux.so`, and `libX_x86_64-linux.so`.

You can now use Curly to create a library of bindings to libX, in a
portable way. First, mount each .so to a point in context, using the
"external" input source, along with a `libX.cy` source file :

    #!/usr/bin/env curly
    # A simple context file for libX
    mount C libX arm = external libX_arm-linux.so 
    mount C libX x86 = external libX_x86-linux.so 
    mount C libX x86-64 = external libX_x86_64-linux.so 
    
    mount libX = source libX.cy

That `libX.cy` file can now define a multi-symbol for each function of
the libX library, handling each system accordingly :

~~~~~~{.curly}
module libX: Bindings to a library

# Since each library exports the same symbols, we have to rename them during import
import C.libX{
  arm{f(arm'f) ...}
  x86{f(x86'f) ...}
  x86-64{f(x64'f) ...}
  }    

let defaultImpl = undefined
multi f = defaultImpl, linux-x86 x86'f, linux-arm arm'f, linux-x86-64 x64'f
....

export f ...
~~~~~~~~~~

You can now import the `libX` module anywhere, and use its functions
on any of the three handled systems. The C binaries are no longer
needed once `libX` has been compiled.

