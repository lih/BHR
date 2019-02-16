% Get your Curly on

Binary packages for the hurried soul
------------------------------------

Installing Curly is either trivial or impossible. You may download a
package for your system by following the correct link
below. Installing the package is then just a matter of decompressing
the archive and running the `curly`{.terminal} executable that is
inside. Here are the available packages for today :

  - [Linux (x86, 64-bit)][curly-linux-x86-64]

If you're feeling lucky, you can try to run the following command in a
terminal, which will perform all the necessary steps for you :

~~~~{.terminal}
curl -s https://www.curly-lang.org/install-curly.sh | sh -s - --import-standard-keys
~~~~~

Compiling from source, for the curious
--------------------------------------

If you ever wondered "how does Curly come to be in the first place ?",
this is your lucky day. You too, with minimal effort, can experience
the heights of getting to compile this fine compiler, by running the
following commands :

~~~{.terminal}
git clone http://git.curly-lang.org/marc/curly
cd curly && stack build
~~~~
