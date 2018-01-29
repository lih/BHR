% Curly(1) Compilers | A minimal Curly Calculus compiler
% Marc Coiffier

SYNOPSIS
========

**Usage:** curly ARGUMENT...

    ARGUMENT = INPUT | TARGET | SCRIPT_NAME | "+"FLAG
    INPUT = ("-M"|"--mount") MODPATH"="MODULE
    MODPATH = [NAME MODPATH]
    MODULE = "source:"SOURCEPATH[ CACHEPATH]
           | "library:"LIBID | "library:@"LIBRARY_FILE
           | "builtins"
    TARGET = "-h" | "--help" | "-v" | "--version"
           | "--instance" INSTANCE | ":"INSTANCE
           | "--banner"["+"] BANNER_TEXT
           | "--at" [SERVER]"/"INSTANCE
           | "--prelude"["+"] COMMAND
           | "-i" | "--interactive"
           | ("-e"|"--execute") COMMAND | "%"COMMAND
           | ("-r"|"--run") CMDFILE
           | ("-s"|"--serve") SERVER_TYPE | ("-l"|"--list") SERVER_TYPE
           | ("-d"|"--dump") SRCLIBNAME
           | ("-t"|"--translate") FILEPATH["@"SYSTEM]"="MODPATH
    SERVER_TYPE = "libraries" | "instances"
    SERVER = "_" | IP_ADDRESS | DNS_NAME
    
    LIB = "@"LIBID
        | FILEPATH".cyl"
        | FILEPATH".cy"

    CONDSPEC = "+"ARGFLAGS CONDSPEC
             | ARGCLAUSE
    ARGFLAGS = FLAG[","ARGFLAGS]
    ARGCLAUSE = "mount" MODPATH "=" MODULE
              | ("target"|"-") TGTSPEC
              | "%" COMMAND
              | ">" ECHO_STRING
              | "include" MODPATH "=" SCRIPT_NAME
    TGTSPEC = "help" | "version"
            | "banner"["+"] BANNER_TEXT
            | "instance" INSTANCE
            | "at" [SERVER]"/"INSTANCE
            | "prelude"["+"] COMMAND
            | "interactive" | "execute" COMMAND
            | "run" CMDFILE
            | "serve" SERVER_TYPE | "list" SERVER_TYPE
            | "dump" SRCLIBNAME
            | "translate" FILEPATH[@SYSTEM] "=" MODPATH

DESCRIPTION
===========

Curly is a humble compiler from Curly Calculus (a derivative of
lambda-calculus) to machine code. It can find, fetch, parse, run,
compile and/or distribute Curly code, automatically resolving
dependencies as it does so.

This command works in a simple way. It constructs an evaluation
context from the INPUTs, then produces each TARGET within that
context. INPUTs can come from various places, and TARGETs can produce
various results.

INPUTS
======

When Curly encounters an import statement in a source file or an
interactive session, it zips that import with the context constructed
from the INPUTs that were given as arguments.

An INPUT can either be a verbatim mount specification, as seen above,
or a Curly script. If it is the latter, that script file should
contain a list of conditional argument specifications (CONDSPEC), one
per line. Each argument can be flagged by one or many arbitrary flag
names, any of which can be specified on the command-line (with +FLAG)
to enable that argument, thus enabling some sort of modal
compilation. Relative paths in an argument shall be treated relative
to the directory containing the script instead of the working
directory.

The default context is constructed by starting with an empty module,
and mounting each module at its path for each mount
specification. Modules can be defined from source files and
directories, library files or library IDs, which are simply sha256
checksums of their corresponding library files.

Library IDs are used by the compiler to unambiguously locate libraries
on the local filesystem or on distant repositories.

Whenever Curly needs a library, it caches it in the directory
specified by the environment variable CURLY_LIBCACHE (or
$HOME/.curly/libraries if unset). If a library can't be found in the
cache, the environment variable CURLY_PATH contains a list of
comma-separated Curly repositories that can be queried for that
library. If none of the repositories know the library, only then does
Curly fail to resolve the dependency.

There are two kinds of Curly repositories : native Curly repositories,
of the form @host[:port] (see the --serve option for more information
about curly repositories); and backend repositories, of the form
*\<backend-name\>(:\<arg\>)...*. A backend repository is an executable
in the '/usr/share/curly/backend' directory, which takes a library ID
as its first argument, and outputs the corresponding library's
contents, if it can be found. This behaviour makes it easy to write
your own custom backends to retrieve libraries in non-standard
ways. Three backends are already provided in this bundle : a backend
to retrieve libraries from HTTP repositories, with
'http://server.example/prefix'; a backend for local directories, with
'local:directory'; and a backend for the Curly version control system,
with 'curly-vc://user@host[:port]/branch' (more on Curly version
control can be found at )

HTTP and directory repositories must be structured in a certain way
for Curly to be able to query them (Curly repositories already offer
the expected functionality, so nothing is to be done for them) :

  * Any available library of hash H should be found in the file H.cyl.
  * There must be a file named "libraries" containing a list of
    library identifiers, one per line, that are available in this
    repository

You'll note that repositories may not be specified as flags on the
command-line. The reason for that is simple : if Curly succeeds in
producing an output, that output will be the same regardless of the
available repositories. Since repositories do not affect the result of
compilation, they should be part of the the environment rather than
the command-line.

TARGETS
=======

Once its inputs are properly positioned, Curly can produce a variety
of outputs and effects :

-h,--help,-v,--version

:   The usual help and version flags. The --help flag inhibits all
others, in order to allow you to see what Curly actually understood of
the options you gave it.

-i,--interactive

:   Launches an interactive session. You can import anything mounted
from the INPUTs in this session. If a previous --at option was set to a
nonempty value, launches a distant interactive session on that server
instead.

-e, --execute, %

:   Runs its parameter, which can be any valid interactive
expression. The third form can be used as a shortcut, like *./context
%clean*. Like -i, the expression is sent to the given server set from
a previous --at option.

-d,--dump

:   Shows the contents of the given Curly source or library file. Can
also show the contents of a library from its ID, with the special
syntax **@<library-id>**

-s,--serve libraries

:   Launches a Curly library server over the port specified by the
CURLY_PORT environment variable, or 25465 if the latter is
unset. Curly clients can use this server as a repository by adding
`@host[:port]` to their CURLY_PATH. If the port is omitted, Curly
connects to the same port it would listen on.

    This proxy forwards requests to its own CURLY_PATH, caching them
for future requests to avoid repetitious downloads.

    Many library servers can borrow the same port number. Each
additional library server connects to the process holding the socket
and serves libraries that the latter can forward to its other clients.

-s,--serve instances

:   Launches an interactive server for the current instance through the
port specified by CURLY_PORT (or 25465 by default). Clients can
connect to this server by specifying the --at option before launching
an interactive session or running a batch command or a command file.

    For example, if you want to make a given context accessible by the
name 'myProject', you can run **curly context --instance myProject -si**
for a server, to which clients can connect by running **curly
--at \<server-address\>/myProject -i**. The alternate syntax **curly
@\<server-address\>/myProject -i** is also recognized. If the server
address is omitted, it defaults to 127.0.0.1.

    Like with library servers, several instance servers can use the
same port, with the caveat that the main server only acts as a port
mapper for the instance servers. Clients connect to the main port to
ask for the port number of the instance they need, and servers ask it
to allocate a free port number number for them to listen on.

    The port mapper's allocation strategy is simple : allocate the next
port above CURLY_PORT that isn't already used by another instance.
Thus, if a machine hosting instance servers is behind a firewall, that
firewall should leave several ports numbers above CURLY_PORT open if
clients are to be able to connect to those instances.

-l,--list libraries

:   Lists all available libraries from the current CURLY_PATH.

-l,--list instances

:   Lists all instances that are available on the server previously
specified with the --at option, or localhost if no server was
specified.

-t,--translate

:   Translates a Curly function into an executable file, or some other
kind of representation of that function. In the future, Curly will be
able to generate Javascript, Verilog and other intermediate languages
for which an ABI cannot readily be determined, as well as executables
for alternative systems like Windows or MacOS.

--prelude, --prelude+

:   Sets or adds to the prelude. The prelude is a list of commands that
are run before each local session. It is especially useful when
serving instances, as it allows a default context to be defined for
all clients.

--banner

:   Uses the contents of a file as the initial text that is displayed
before each interactive session is started. When set before serving
instances, this sets the banner for each client.

--at

:   Sets the server to connect to when running interactive sessions,
commands or files. If the server is empty, those sessions are hosted
in the local context instead.

--instance

: Changes the name of the current instance. This name is used in the
prompt of client interactive sessions and as a handle for the instance
server, if one is launched. Curly also accepts the alternate syntax
**:INSTANCE** for this flag.

METADATA
========

Since version 0.50.3, Curly libraries can contain arbitrary metadata,
in the form of a hierarchy of strings (sort of like a simplified
DOM). This metadata can be leveraged by the compiler to search its
repositories for libraries based on their external attributes.

For example, instead of manually locating the library 'stdlib' in your
repositories and mounting its hash, you can now let Curly do the
searching, by writing "curly --mount stdlib=package:'{= {$ name}
stdlib}'". Queries are written in a small language that provides basic
pattern-matching and general numeric comparison, as well as the
ability to test the presence of a metadata field.

Although they can be used for locating packages, queries can also
format the results of a library listing. With every command that
displays a library index, you can customize how the metadata is
displayed by specifying a query to run. For example, here is how you
would list libraries with their names and version numbers :

    curly --list 'libraries {$ name}-{$ version}:\ {$ synopsis}'

Since the format is also a query, you can use it to restrict the
number of libraries that are printed. For example, the next command
only shows different versions of 'stdlib', otherwise using the same
format as before :

    curly --list 'libraries {= {$ name} stdlib}-{$ version}:\ {$ synopsis}'

SECURITY
========

Since Curly can host interactive sessions accessible from other
computers, there is a potential security risk if we let those sessions
run arbitrary code from unauthenticated clients.

That is why, as of version 0.44, Curly can authenticate clients before
allowing them access to an instance. You can still host
unauthenticated instances if you wish, but those only grant readonly
access to their clients.

As with everything else, Curly tries to make security as simple to setup
and use as possible. Case in point, here are the steps required to
host a secure instance that allows all access only to a single client :

  * Generate a private key on the client : **cy %'key gen \<key-name\>'**
  * Export a claim for that key : **cy %'key export \<key-name\>'**
  * Import that claim on the server : **cy %'key import \<key-name\> #\<key-export\>'**
  * Grant access to the target instance for that claim **cy --instance=\<instance\> %'key grant almighty \<key-name\>'**
  * Launch the daemon on the server **cy --instance=\<instance\> -si**

The client can now connect to the instance in the usual ways, as long
as it holds an authorized key. If the client and server agree on
several keys, the one with the highest access level is chosen.

VERSION CONTROL
===============

As of version 0.50.2, Curly comes with a builtin interface to its own
distributed version control system, to help you track and retrieve the
results of your work.

Curly store this information on a DHT for redundancy and cache
behaviour, as well as towards an ideal of global code sharing. If you
use the default Curly server, everything you commit will be made
publicly available, from source to libraries. However, if you prefer
working privately, you can easily host your own local version control
server, by installing the 'curly-dht' package from Hackage and running
'curly-dht \<server-port\>'.

In order to prevent fake source information from being spread through
the DHT, every piece of data is either hash-indexed (using a
cryptographically secure hash function), or signed by a public key
that can be retrieved through the value's index. Thus, every peer can
verify every piece of information it is offered before propagating it.

Under other aspects, version control in Curly works similarly to other
such systems. Every identity can store its own list of branches, which
are each a reference to the tip of a commit chain. Every branch
describes an incrementally-defined library index, which can be used to
locate libraries based on the metadata they export (which can
include a package's name, version and author, among other things).

Since version 0.50.3, Curly also provides a 'checkout' function, that
recreates a full project directory from a library of your choice. This
directory contains the source and libraries for all the dependencies
of the original library, as well as a context file that encapsulates
those dependencies. Together, those files provide a working
environment in which you are free to make changes as you see fit (as
opposed to using raw libraries, which do not allow modifications).
