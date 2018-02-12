% Curly, a language for simple-minded people

Welcome, fellow programmer, to a simpler world.

What is Curly ?
===============

Curly is a programming language. It looks like `{f x y: f y x}`{.curly}, but
can also look like `<a:href="path/to/${file}">"${file}"</>`{.curly} or `_² = {x: x*x}`{.curly}.

It is based on the simply-typed lambda-calculus. In other words, it is
a very, very, VERY minimalistic language. Apart from the various
built-in functions (such as addition, multiplication, opening files,
...), there are only two distinct features of the language, that every
other aspect builds on.

First, you can create functions by using Curly brackets : `{x y z:
x+y+(z-x*x)}`{.curly}. Secondly, you can *apply* a function to another by
adjoining them : `f x y z`{.curly}. It goes without saying that you can use
parentheses to nest function calls, as you would expect.

In Curly, everything is a function. You may even call it a functional
language, if the fancy strikes you. 

Why use Curly ?
===============

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

One of those hassles comes from the multiplicity of tools needed to
keep up with the best and latest developments, especially the need to
configure those tools with varying degrees of expressivity. For
example, I frequently find JS projects that need no less than five
configuration files (package.json, .npmrc, rollup-config.js,
.flowconfig, yarn.lock, ...) to function.

With Curly the only tool you'll need to know is `curly`, the
compiler. It is modeless, accepts fewer than 20 different options, and
has its own configuration format based on those options (basically,
you can write those options to a file and have Curly read that file).

To illustrate, here is the only configuration file that I use to work
with the Curly Standard Library :

~~~~~~~~{.curly}
#!/usr/bin/env curly
- instance stdlib

mount = source src cache
mount data = resource data cache/data
mount builtins = builtins

+default - interactive

?publish Publish the latest version of our libraries to branches 'main', 'data' and 'stdlib'
+publish % repository commit stdlib -add main -add stdlib -add data -keep maximum {$ version} by {$ name}
~~~~~~~~~~~~~~~

Using this configuration file, many common programming tasks are
reduced to a single command :

  - generate an executable from a function somewhere in the context : `curly -t bin/main=main.main4`
  - generate an executable for a different system : `curly -t bin/main@linux-x86=main.main4`
  - run a function without generating a binary : `curly %'run main.main4'`
  - edit a function that comes from a source file : `curly %'edit stdlib.Fix.fix'`
  - start an interactive session to explore the context : `curly -i` (or just `curly` in this case since the default is to run an interactive session)
  - show information about imported or compiled functions : `curly %'show stdlib.List.append'`
  - publish the latest versions of our libraries to a repository : `curly +publish`
  - ... and so much more

How to use Curly ?
==================

Curly should be pretty easy to get acquainted with (either that or
I've failed in my design). The [quick start
guide](getting-started.html) should give you all the information you
need to get productive right away.
