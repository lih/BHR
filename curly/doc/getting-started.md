% Getting Started with Curly

What better way is there to assess Curly than to just try it ?

Installing Curly
===============

(Curly currently has only been tested on 64-bit Linux, which is what
the following instructions assume. Other platforms will be coming,
perhaps even soon).

Curly is pretty straightforward to install. You can either [download a
pre-compiled binary [Linux, x86-64]][curly-package] (unpack it with
`tar -xJf curly.tar.xz`{.terminal}) or build it from [the source][curly-source],
which may require you to use the [Stack build tool][stack].

[stack]: https://docs.haskellstack.org/en/stable/README/

If you're feeling lazy, you can also elect to use this generously
provided [install script][curly-install-script], which downloads and
unpacks the above archive in the directory of your choice, and creates
a link to Curly in your PATH.

Curly doesn't depend on much to do its work. Still, if something is
missing, it's either one of the following :

  - The Z library to (de)compress files
  - GMP (GNU Multi-Precision) because it's a Haskell program that makes calculations with large numbers
  - The GNU C Library (glibc), because what doesn't depend on it ? 

First steps with the Curly compiler
====================================

I hope the previous steps went well. If they did, you should now be
able to run Curly, and it should be in your PATH as well. You can test
it by running `curly --help`{.terminal}, which should present you with a screen
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

_Tip:_ As indicated above, the "--help" option inhibits all
others. When you are unsure of a command-line, you can always append
"--help" to see what Curly understood without actually running
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
`curly -M builtins=builtins -i`{.terminal} to walk around the builtin
library and discover the interactive environment. In that environment,
you can define functions and use them, just like you would in source
files (we'll get to the source format in a moment).

You can also, once in that environment, use additional commands to
rummage around within the context. For example, try running `show
builtins`{.terminal}, or `meta builtins`{.terminal}. Enter `help`{.terminal} to
see a list of all interactive commands, `help <cmd>`{.terminal} for a
description of their individual usage.

Hello, Curly !
==============

As is traditional with programming languages, we shall now write a
program that prints out "Hello !" to the world.

There are many ways to do so with Curly.

### The traditional way

The most traditional of those ways would be to define a main function
in a file (let's say "main.cy") and turn it into an executable using
the "-t" option. Here's what it looks like :

~~~~~{.curly}
module Main

import builtins

define main = write stdout "Hello, world !\n"

export main
~~~~~~~~~~~

Running `curly -M builtins=builtins -M main=source:main.cy -t
main=main.main`{.terminal} creates an executable called "main" from
the function that we just defined.

### The quick-and-dirty way

For such a simple example, you could even write the program inline, as
an interactive expression that uses the builtins library directly
`curly -M builtins=builtins -p 'import builtins' -e 'run (write stdout
"Hello, world !\n")'`{.terminal}

### The Curly Way

In fact, there is no need to embarass ourselves with writing a
function that's been written a thousand times already. There's
probably already a library for it somewhere. It so happens that the
Curly Standard Repository provides a Main module that defines a
function just like the one we need.

To use it, you'll need to inform Curly of the existence of the
repository, by importing its public key (this only needs to be done
once) :

~~~~{.terminal}
curly -e 'key import curly-std standard.curly-lang.org' -e 'key set curly-std follow-branches = stdlib hello'
~~~~~


After that, the package can be found by Curly under the name
"hello". All we have to do is mount it and use the `main`{.curly}
function that it exports, like so :

~~~{.terminal}
curly package:hello %'run hello.main'
# to create an executable
curly package:hello -t hello.main
~~~~~

In the above commands, "package:hello" is a shorthand for "--mount
hello=package:hello", and "%<cmd>" is another form for "--execute
<cmd>".

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
results as before by running `curly main.curly +build`{.terminal}, or
`curly main.curly +run`{.terminal}

The configuration format is pretty straightforward :

  - options are listed one per line. Blank lines are ignored, as are
    lines that start with a `#`{.curly}

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

      - constraints of the form `+<flag-1>,...,<flag-n>`{.curly} are
        _positive_, and serve to assert whether either of the flags
        was specified. For example, a constraint of
        `+build,clean`{.curly} will activate its option if Curly was
        invoked with either `+build`, `+clean` or both.
        
        Each of the `<flag>`{.curly}s can optionally take additional
        parameters that may be referenced within the flagged option,
        using the [format syntax][format]. Thus, flags can be used to
        group related actions, such as installing a package or running
        a specific function.
        
        The following example shows how to write a simple installation
        context, so that calling `curly +install:<pkg1>
        ... +install:<pkgn>`{.terminal} will install all the requested executables
        from named packages to a common location : 
	
        ~~~~~{.curly}
        +install:pkg  mount {$ pkg} = package {$ pkg}
        +install:pkg  > Installing package {$ pkg} to {env HOME}/.local/bin
        +install:pkg  - translate {env HOME}/.local/bin/{$ pkg} = {$ pkg}.main
        ~~~~~~~
		
      - contraints of the form `+!<flag-1>,...,!<flag-n>`{.curly} test
      	whether either of the flags was absent from the
      	command-line. In other words, it only fails when all the flags
      	are specified.

[format]: curly-document.html

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
--help`{.terminal} will show `mount = source /path/to/project/src`{.curly} in the
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

In the previous example, `cd`{.terminal}ing into the project directory and
entering `curly -i`{.terminal} would start an interactive session in the context
of the project.

### Goodies

You may have noticed the "--goody" option. This option does not affect
the behaviour of Curly, but rather prints out the contents of a
predetermined file that was shipped with Curly, such as completion
functions, scaffolding templates, or desktop files. The "install.sh"
goody offers a script that can create packages for various systems.

In the following section, I will assume the existence of a
`curly-install`{.terminal} shell function defined as follows :

~~~~{.terminal}
curly-install() { curly --goody install.sh | sh -s "$@"; }
~~~~~

#### Emacs modes for editing Curly files

`curly-install emacs`{.terminal} will create a package archive that
can be installed by running `M-x package-install-file` in your
favorite editor. This package provides a major mode for Curly source
files ("curly-mode.el") and another for Curly configuration files
("curly-conf-mode.el").



