# Curly, a language for simple-minded people

Welcome, fellow programmer, to a simpler world.

What is Curly ?
---------------

Curly is a programming language. It looks like `{f x y: f y x}`, but
can also look like `<a:href="path/to/${file}">"${file}"</>` or `_² = {x: x*x}`.

It is based on the simply-typed lambda-calculus. In other words, it is
a very, very, VERY minimalistic language. Apart from the various
built-in functions (such as addition, multiplication, opening files,
...), there are only two distinct features of the language, that every
other aspect builds on.

First, you can create functions by using Curly brackets : `{x y z:
x+y+(z-x*x)}`. Secondly, you can *apply* a function to another by
adjoining them : `f x y z`. It goes without saying that you can use
parentheses to nest function calls, as you would expect.

In Curly, everything is a function. You may even call it a functional
language, if the fancy strikes you. 

Why use Curly ?
---------------

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

    #!/usr/bin/env curly
    - instance stdlib
    
    mount = source src cache
    mount data = resource data cache/data
    mount builtins = builtins
    
    +default - interactive
    
    ?publish Publish the latest version of our libraries to branches 'main', 'data' and 'stdlib'
    +publish % repository commit stdlib -add main -add stdlib -add data -keep maximum {$ version} by {$ name}

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
---------------

What better way is there to assess Curly than to just try it ?

First of all, you'll need a working version of Curly. You can either
[download a pre-compiled binary [Linux, x86-64]](../pkg/curly.tar.xz)
(unpack it with `tar -xJf curly.tar.xz`) or build it from [the
source](https://gricad-gitlab.univ-grenoble-alpes.fr/coiffiem/curly)

If you're feeling lucky, you can also elect to use the following
generously provided install script :

    curl -s https://coiffiem.gricad-pages.univ-grenoble-alpes.fr/curly/doc/install.sh | sh -s -- --prefix "$HOME/.local" 

I assume the previous steps went well. If they did, you should now be
able to run Curly, and it may as well be in your PATH. You can test it
by running `curly --help`, which should present you with a screen like the following

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
others. When you are unsure of a command-line, you can always append a
`--help` to see what Curly understood without actually running
anything.

Curly works by first creating a context by *mounting* input sources at
certain paths (shown in the "Mounts" section above), and running one
or more targets that make use of this context (visible in the
"Targets" section).

Input sources can be many things. As of now (version 0.59.1), they can either be :

  - source files or directory (`source <src>` or `source <src> <cache>` to specify the cache file name)
  - the Builtin library (`builtins`)
  - resource files or directories (`resource <src>`)
  - libraries (`library <lib-id>`) and packages, which we will cover a bit later

Let's put that knowledge in practice with an exercise. Try running
`curly -M builtins=builtins -i` to walk around the builtin library and
discover the interactive environment. In that environment, you can
define functions and use them, just like you would in source files
(we'll get to the source format in a mo').

