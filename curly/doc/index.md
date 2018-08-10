% Curly, a language for simple-minded programmers

Welcome, fellow programmer, to a world of bliss, purity and curly
brackets.

What is Curly ?
---------------

Curly is a programming language, that is, a way for people to
communicate with other people about what they want computers to do. It
looks like `{f x y: f y x}`{.curly}, but can also look like
`<a:href="path/to/${file}">"${file}"</>`{.curly} or `_² = {x:
x*x}`{.curly}.

It is based on the simply-typed lambda-calculus. In other words, it is
a very, very, VERY minimalistic language. Apart from the various
built-in functions (such as addition, multiplication, opening files,
...), there are only two distinct features of the language, that every
other aspect builds on.

First, you can create functions by using Curly brackets : `{x y z:
x+y+(z-x*x)}`{.curly}. Secondly, you can *apply* a function to another
by adjoining them : `f x y z`{.curly}. You can also use parentheses to
nest function calls, in one of two ways :

  - either wrap the parentheses around the whole expression :
    `f x y (g z)`{.curly}
  - or, if you prefer a more "mathematical" syntax, the following is
    also recognized, and means the same thing : `f(x y g(z))`{.curly}

In Curly, almost everything is represented as a function, from mere
booleans to the most complex of graph structures. Even the class
system uses a functional representation to dispatch method calls.


Why use Curly ?
--------------

There are many programming languages out there that do wonderful
things for their programmers. C++ offers efficiency and power; Python
offers simplicity and portability; Rust is all about memory safety and
can keep you from making fatal mistakes; and Haskell teaches you how
to be lazy with class. In short, most languages bring something
different to the table, something that makes them stand out proudly
amongst other extraordinary languages.

In contrast, Curly is a very ordinary language. It just tries to make
everything easy for the programmers, testers, and end-users who wish
to work with it. If you decide to use Curly, I cannot promise
world-class performance or a programming experience that will rock
your brain. What I can promise is a pleasant programming experience,
without many of the hassles of modern programming environments.

### Simplicity of tooling

One of the aforementioned hassles comes from the multiplicity of tools
needed to keep up with the best and latest developments, especially
the need to configure those tools with varying degrees of
expressivity. For example, I frequently find JS projects that need no
less than five configuration files (package.json, .npmrc,
rollup-config.js, .flowconfig, yarn.lock, ...) to function.

With Curly the only tool you'll need to know is `curly`{.terminal},
the compiler, which doesn't take much effort to learn (if I do say so
myself). It is modeless, accepts fewer than 20 different options, and
has its own configuration format based on those options (basically,
you can write those options to a file and have Curly read that file).

To illustrate, here is the only configuration file that I use to work
with the Curly Standard Library (all the source is placed under the
`src/` tree, and object files are cached in the `cache/` directory) :

~~~~~~~~{.curly}
#!/usr/bin/env curly
- instance stdlib

mount		= source src cache
mount data	= resource data cache/data
mount builtins	= builtins

# If this file is the main context, and no task was given on the command-line, spawn an interactive shell
+command +default - interactive

?commit Commit the latest version of our libraries to the branches 'hello' and 'stdlib'
+command +commit % repository commit stdlib -add base -add core -keep {= {$ commit stdlib} true} -keep maximum {$ version} by {$ name}
+command +commit % repository commit hello -add main -add data -keep {= {$ commit hello} true} -keep maximum {$ version} by {$ name} -drop {unless {$ name} true}
~~~~~~~~~~~~~~~

Using this configuration file, many common programming tasks are
reduced to a single command :

  - generate an executable from a function somewhere in the context : `curly -t bin/main=main.main4`{.terminal}
  - generate an executable for a different system : `curly -t bin/main@linux-x86=main.main4`{.terminal}
  - run a function without generating a binary : `curly %'run main.main4'`{.terminal}
  - edit a function that comes from a source file : `curly %'edit core.Fix.fix'`{.terminal}
  - start an interactive session to explore the context : `curly -i`{.terminal} (or just `curly`{.terminal} in this case since the default is to run an interactive session)
  - show information about imported or compiled functions : `curly %'show stdlib.List.append'`{.terminal}
  - publish the latest versions of our libraries to a repository : `curly +publish`{.terminal}
  - ... and so much more

### Portability

Did I mention that Curly was very minimal ? That minimalism allows it
to be ported almost effortlessly to any platform imaginable. As a
proof-of-concept, the Curly compiler can already generate code for 32-
and 64-bit x86 Linux systems, as well as runnable pseudo-code in
JavaScript and/or other C-like languages, using a single algorithm.

I am currently working on an ARM backend, a JVM class file generator,
and a WebAssembly backend, as well as extending the x86 implementation
to handle more varied system calls, on more than just Linux (I may even
get to implement a Windows backend, if I get my hands on a free
Windows license to test it).

Curly is designed to be a universal language, and as such it should be
able to run on virtually anything. I am even envisioning a Verilog/HSL
backend for fun, to see how flow-driven functional programs behave on
specialized hardware.

Cross-compilation is a just few keystrokes away in the
command-line. If you want you compile your program for your own
system, run `curly <my-context> --translate bin/prog=my.program`{.terminal}. If
you want to compile it for another system instead, run `curly
<my-context> --translate bin/prog@other-system=my.program`{.terminal}.

### Distribution

When a program or library is written and tested, it should be made
available for all its happy users to enjoy. This is as much a part of
the programming process as unit testing. And it is very difficult to
get right, even with experience.

It is so difficult in fact, that many languages choose to delegate
that part of the process to an external tool/infrastructure (NPM,
Hackage, PyPI, C*AN, Cargo, or even private repositories on GitHub, to
name a few). In constrast, Curly integrates package distribution into
its workflow, via an interactive `repository`{.terminal} command that
is able to query from, and publish to, various repository backends,
such as a raw filesystem, a HTTP server or a DHT-based storage system
(integration with Git may be coming too, if it makes sense).

From the information it can gather in its repositories, Curly can act
as a rudimentary library browser (exemplified by the `repository
browse`{.terminal} subcommand), that allows you to navigate the
structure, types and documentation of any package that is made
available by other developpers. A graphical version of this browser is
in the works as well.

Publishing code is similarly easy, using the `repository
commit`{.terminal} subcommand, which will attempt a simultaneous
commit of any specified module hierarchy to all known repositories. To
commit all modules under the current context, simply run `repository
commit some-branch -add .`{.terminal}, and wait for Curly to compile,
cache, sign, and send your libraries where they are needed.

To cut a long explanation short, Curly tries its hardest to abstract
away all issues that are unrelated to application design and
implementation, so that programmers can concentrate on programming.

How to use Curly ?
------------------

If you read this far, let me first thank you very much indeed for your
interest in Curly. I hope it only brings you small and infrequent
amounts of pain.

The [quick start guide](getting-started.html) should give you all the
information you need to start exploring the wondrous land of Curly.

#### Where to complain ?

If Curly isn't to your liking, you can complain about it on the
[dedicated issue tracker][curly-complaints] (login/password:
guest/guest). All complaints thus far have been exceedingly
well-received.
