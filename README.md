Marc's Big Haskell Repository
=============================

This BHR serves as the entire collection of all the Haskell packages
I've written over the years. There are some libraries (definitive-*),
some serious executables (curly), and some random unfinished wankery
(grow/woosh).

You're welcome to use any of the libraries therein in your own code,
if you can make sense of them. If you just want to test out one of the
programs, you can also check out the [release
page](https://github.com/lih/stack-libs/releases) to find a compiled
and ready-to-use version.

Here is a short description of the different packages in this BHR.

### The `definitive-*` libraries

Those are basic libraries, designed to minimize external dependencies
while providing most of the functionality found in modern Haskell
programs. Most notably, optics (Lenses and the like) are defined very
early on in the definitive-base library, and serve as the basis for
many standard abstractions.

The `definitive-base` library defines all the standard Monad
transformer combinators, from StateT to LogicT, along with a
generalization of the containers library that simplifies the use of
Sets and Maps, and introduces the Bimap, Relation and Equiv containers
under the same interface.

The `definitive-parser` library, as it name indicates, defines the
usual parser combinators (using the LogicT transformer for
non-determinism), and is used for both binary and textual parsing in
most other packages. It also defines Serializable and Format
typeclasses, as well as their instances for most basic data types and
a mechanism for automatic derivation of those classes for types that
are instances of the Generic class.

The `definitive-graphics` library provides high-level wrappers for the
GTK windowing framework, allowing one to describe GUIs and their
behaviour in the perfect mix of declarative layout and imperative
event handling.

The other two (`definitive-network` and `definitive-filesystem`) are
much less interesting, and should definitely not be included in any
serious project.

### The Curly compiler and libraries

The packages whose name start with `curly` are part of the Curly
compiler infrastructure (on which more information can be found at
[curly-lang.org](https://www.curly-lang.org/)).

The `curly` package contains the library that describes how the
compiler interacts with its environment (reads configuration files,
locates packages, host interactive sessions, ...). Using that library,
it also implements the `curly` executable, aka the compiler
itself. The `curly` library is also used by `curly-gui`, a GUI version
of the compiler, to handle all its context-related operations.

At a lower level, the `curly-system` library contains the
implementation of all of Curly's backends, from x86 assembly to
JavaScript. You could write a basic batch compiler using only this
library.

All of the above packages depend on the `curly-core` library, which
defines all the aspects of the core language, in an
architecture-independent way. This is where you can find a description
of the object format (in the Library type), the type system, and the
syntax and semantic of the Curly language itself.

### Random wankery

This BHR also serves as a testing ground for potentially interesting
ideas to develop, mainly because if I want to import stuff from
another library in here, all I have to do is declare it in the
dependencies of the corresponding .cabal file. I blame this all
squarely on Stack, for being such a great tool.

#### CaPriCon : a concatenative proof assistant

CaPriCon is the answer to the question "what would an assembly
language for mathematical proofs look like ?". It's a low-level,
stack-based language capable of manipulating terms in the Calculus of
Prismatic Constructions. More information is available [here](https://wiqee.curly-lang.org).

#### Logos : a concatenative 3D game engine

Since I already had a monad for concatenative languages, why not use
it for fun, too ?

Logos is a language that makes it easy to define 3D scenes, using the
modern OpenGL rendering pipeline for efficiency. It also provide the
basic event handling capability necessary for implenting a
full-fledged game.

One caveat : you can't yet draw any text. You can load font textures,
and manually map their contents onto a quad, but there's no
abstraction for it yet (and there will be, in the far future).

#### hreadline : a pure Haskell ReadLine library

After spending months wrestling with libncursesw version issues on
different distros, I came to the conclusion that writing my own would
actually save me some time. So I did. And it did.

It's not a clone of the "real" ReadLine, not even close. But it
handles simple text editing, programmable autocompletion, and command
history. Plus, it fits in one ~200LoC file, so it's easy to change
something and understand what you did.
