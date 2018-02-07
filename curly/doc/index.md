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

Getting Started
===============

What better way is there to assess Curly than to just try it ?

Installing Curly
----------------

(Curly currently has only been tested on 64-bit Linux, which is what
the following instructions assume. Other platforms will be coming
soon).

Curly is pretty straightforward to install. You can either [download a
pre-compiled binary [Linux, x86-64]][curly-package] (unpack it with
`tar -xJf curly.tar.xz`) or build it from [the source][curly-source],
which may require you to use the [Stack build tool][stack].

[curly-source]: https://gricad-gitlab.univ-grenoble-alpes.fr/coiffiem/curly
[stack]: https://docs.haskellstack.org/en/stable/README/
[curly-package]: ../pkg/curly.tar.xz

If you're feeling lazy, you can also elect to use this generously
provided [install script](install-curly.sh), which downloads and
unpacks the above archive in the directory of your choice, and creates
a link to `curly` in your PATH.

Curly doesn't depend on much to do its work. Still, if something is
missing, it's either one of the following :

  - The Z library to (de)compress files
  - GMP (GNU Multi-Precision) because it's a Haskell program that makes calculations with large numbers
  - The GNU C Library (glibc), because what doesn't depend on it ? 

First steps with the `curly` compiler
-------------------------------------

I hope the previous steps went well. If they did, you should now be
able to run Curly, and it should be in your PATH as well. You can test
it by running `curly --help`, which should present you with a screen
like the following

    Usage: curly OPTION...
      -h  --help            (nothing)          Display the help menu and some basic information (inhibits other flags)                   
      -v  --version         (nothing)          Show the current Curly version                                                            
          --goody           FILE               Dump the contents of an installed data file. The 'list' file contains all available names
                                               --- Inputs ---                                                                            
      -M  --mount           PATH=MOUNT         Mount an input source to a path in the default context                                    
                                               --- Outputs ---                                                                           
      -t  --translate       FILE[@SYS][=PATH]  Translate a Curly function for a system                                                   
                                               --- Session Context ---                                                                   
      -P  --prelude         COMMAND            Set the prelude for the next targets                                                      
      -p  --prelude+        COMMAND            Append the given command to the prelude                                                   
          --banner          BANNER             Set the banner for the next targets                                                       
          --banner+         BANNER             Add a line to the banner file for the next targets                                        
          --instance        INSTANCE           Set the instance name for the next targets                                                
          --at              [SERVER]/INSTANCE  Select a server for the next targets                                                      
                                               --- Running Sessions ---                                                                  
      -i  --interactive     (nothing)          Launche an interactive session                                                            
      -e  --execute         COMMAND            Execute an interactive command                                                            
      -r  --run             FILE               Run interactive commands from the given file, or stdin if the file is -                   
                                               --- Hosting Sessions ---                                                                  
      -s  --serve-instance  (nothing)          Launche an instance server for the current instance.                                      
      -l  --list-instances  (nothing)          List all available instances on the selected server (the previous --at target)            
    
    Known systems: host, html, javascript, jsasm, linux-arm, linux-x86, linux-x86-64
    Repositories (default value, set CURLY_VCS to override):
      * http://curly-vc.coiffier.net/vcs
    Library cache: /home/USER/.curly/libraries (default value, set CURLY_LIBCACHE to override)
    Server port: 25465 (default value, set CURLY_PORT to override)
    Publisher key: (default value, set CURLY_PUBLISHER to override)
    
    Mounts: none
    Targets: none

_Tip:_ As indicated above, the `--help` option inhibits all
others. When you are unsure of a command-line, you can always append
`--help` to see what Curly understood without actually running
anything.

Curly works by first creating a context by *mounting* input sources at
certain paths (shown in the "Mounts" section above), and running one
or more targets that make use of this context (visible in the
"Targets" section).

Input sources can be many things. As of now (version 0.59.1), they can either be :

  - source files or directory (`source <src>`{.curly} or `source <src>
    <cache>`{.curly} to specify the cache file name)
  - the Builtin library (`builtins`{.curly})
  - resource files or directories (`resource <src>`{.curly})
  - libraries (`library <lib-id>`{.curly}) and packages, which we will cover a bit later

Let's put that knowledge in practice with an exercise. Try running
`curly -M builtins=builtins -i` to walk around the builtin library and
discover the interactive environment. In that environment, you can
define functions and use them, just like you would in source files
(we'll get to the source format in a moment).

You can also, once in that environment, use additional commands to
rummage around within the context. For example, try running `show
builtins`{.curly}, or `meta builtins`{.curly}. Enter `help`{.curly} to
see a list of all interactive commands, `help <cmd>`{.curly} for a
description of their individual usage.

Hello, Curly !
--------------

As is traditional with programming languages, we shall now write a
program that prints out "Hello !" to the world.

There are many ways to do so with Curly.

### The traditional way

The most traditional of those ways would be to define a main function
in a file (let's say "main.cy") and turn it into an executable using
the `-t` option. Here's what it looks like :

~~~~~{.curly}
module Main

import builtins

define main = write stdout "Hello, world !\n"

export main
~~~~~~~~~~~

Running `curly -M builtins=builtins -M main=source:main.cy -t
main=main.main` creates an executable `main` from the function that we
just defined.

### The quick-and-dirty way

For such a simple example, you could even write the program inline, as
an interactive expression that uses the builtins library directly
`curly -M builtins=builtins -p 'import builtins' -e 'run (write stdout
"Hello, world !\n")'`

### The Curly Way

In fact, there is no need to embarass ourselves with writing a
function that's been written a thousand times already. There's
probably already a library for it somewhere. It so happens that the
Curly Standard Repository provides a Main module that defines a
function just like the one we need.

To use it, you'll need to inform Curly of the existence of the
repository, by importing its public key :

    curly -e 'key import curly-std curly-std.coiffier.net'

After that, the package can be found by Curly under the name
"Main". All we have to do is mount it and use the `main`{.curly}
function that it exports, like so :

    curly -M Main=package:Main -e 'run Main.main'
    # to create an executable
    curly -M Main=package:Main -t main=Main.main

Curly would also recognize `package:<name>` instead of the heavier `-M
<name>=package:<name>` for the mount point.

Going Further
=============

As seen above, you can reference packages from a repository and use
them as easily as you would a source file. Still, for complex projects
that pull in many dependencies, it can be a pain to list them all at
each invocation.

Configuration files (aka. "contexts")
-------------------------------------

To alleviate that pain, Curly can read additional options from one or
more configuration files that are passed as arguments. Here is an
example of such a configuration file, that performs the same actions
as our previous commands :

~~~~~{.curly}
#!/usr/bin/env curly
# ^ This first line is optional, but shows that Curly
#   configurations can be made executable, effectively
#   acting as specialized Curly invocations

# we can mount the same things as the command-line
mount Main = package Main

+build - translate main = Main.main
+default,run   % run Main.main
~~~~~~~

If you save this file as 'main.curly', you can achieve the same
results as before by running `curly main.curly +build`, or `curly
main.curly +run`

The configuration format is pretty simple :

  - options are listed one per line. Blank lines are ignored.

  - lines that begin with `#`{.curly} are comments, ignored by the
    parser

  - mount options are introduced by the `mount`{.curly} keyword,
    followed by a mount description

  - targets are introduced by the `target`{.curly} keyword, or a
    simple `-`{.curly}, followed by the option name as documented by
    `--help` (for example, `- translate main = Main.main`{.curly})

    The `execute`{.curly} target is so common that it has its own
    shorthand. Instead of `- execute <command>`{.curly}, you can just
    write `% <command>`{.curly}.

  - you can include other configurations with the `include <path> =
    <file>`{.curly} pragma. This pragma will include the options
    listed in `<file>`{.curly}, and prefix all its mount points by
    `<path>`{.curly}

  - optionally, a line can be prefixed by one or several "flag
    constraints" that dictate whether the option is activated on a
    given invocation. Options whose constraints don't match the flags
    provided on the command-line will be ignored, as though they were
    commented. If several constraints are specified, all must match
    for the option to be activated.

    Constraints fall into two categories :

      - constraints of the form `+<flag-1>,...,<flag-n>`{.curly} test
        whether either of the flags was specified. For example, a
        constraint of `+build,clean`{.curly} will activate its option
        if Curly was invoked with either `+build`, `+clean` or both.

      - contraints of the form `+!<flag-1>,...,<flag-n>`{.curly} test
      	whether either of the flags was absent from the
      	command-line. In other words, it only fails when all the flags
      	are specified.

### Anchored paths in context files

Within a context file, all relative paths are made relative to the
directory containing that file. Thus, if you have a project with the
following structure :

    /path/to/project/
      .curly      # The default configuration file name
      src/
        X.cy

you can mount the 'src' folder by simply adding `mount = source
src`{.curly} to the configuration. If you then load the '.curly' file
from any directory, the path to the 'src' folder will be updated
accordingly. For example, running `curly /path/to/project/.curly
--help` will show `mount = source /path/to/project/src`{.curly} in the
"Mounts" section.

Paths are translated for every option that references the file
system. For the moment, those consist of the `include`{.curly},
`mount`{.curly}, `translate`{.curly}, and `run`{.curly} options.

#### Symbolic links and path resolution

Curly will follow any symbolic links that lead to a context file when
determining its containing directory.

Since Curly contexts are executable scripts, you can add somewhere in
your PATH (under '$HOME/.local/bin/curly.&lt;project&gt;', for instance) a
symbolic link to any project configuration to allow easy access to
that project's context, as a simple shell command.

### Default context files

Curly implicitly loads the file '$HOME/.curly/default.curly' at every
invocation. This file is where you can customize Curly, and provide
default actions for all your projects.

Additionally, if no other configuration file is specified, and there
exists a file called '.curly' in the current directory or its parents,
then Curly loads that file before other options are processed.

In the previous example, `cd`ing into the project directory and
entering `curly -i` would start an interactive session in the context
of the project.




