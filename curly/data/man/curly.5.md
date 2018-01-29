% Curly(5) Languages | A lambda-calculus for the 21st century
% Marc Coiffier

Curly Calculus is a programming language derived from the venerable
lambda-calculus (in fact, downright copied from it) that offers
unmatched simplicity, portability and efficiency due to the
minimalistic nature of its semantics.

The syntax of Curly Calculus
============================

Before describing the formal syntax of Curly, it is better to start
with an example program to illustrate its features :

    #!/lib/module!#

    # Every Curly source file must start with a shebang line
    # identifying the type of file we are dealing with (a module in
    # this case). Oh, and lines that start with '#' are comments which
    # are ignored by the compiler.

    # You can define symbols and give them a value
    define const a b = a
    define compose f g x = f (g x)

    # Functions are applied to their arguments by writing them side by
    # side or by collating the function with its arguments between
    # parens.
    define compose2 f g x = f g(x)

    # You can create anonymous functions (lambdas) by wrapping the arguments and
    # expansion between '{}'s.
    define compose3 = {f g x: f g(x)}

    # When a name contains one or more '_'s, it defines a new syntax
    # matching its name. 
    define _+_ f g x = compose f(x) g(x)
    define _*_ = compose
    define trans x = 1+x*3

    # You can partially apply operators by writing '_'s in place of
    # their argument in an expression context.
    define trans' = 1+_*3
    define plus = _+_
    define inc = _+1
    
    # You can define local variables (or operators) by binding
    # parameters with expressions. You can bind variables even
    # in anonymous functions.
    define sqdist {_² x = x*x} a b = a²+b²
    define sqdist' = {{_² x = x*x} a b: a²+b²}

    # Once defined, you can export functions or operators, or group
    # them in submodules, which can themselves be further subdivised
    # if necesary
    export syntax{_+_ _*_} others{trans plus inc sqdist}

    # As a small extension, you can splice expressions within
    # literal strings with the '${...}' syntax.
    define message name = print "Hello, ${name} !"

    # Every expression has a type, and you can define new types with
    # constructors and destructors with the 'type' statement
    type Type a = unType : a
    define type_ t = unType t ...
    define :_ t k x = k unType(t x)
    define _of_ x t = unType t x
    define AnyType = Type _
    define typeof a {ta = AnyType} = const ta (a of ta)
    define forall k = k AnyType

    # Operators are left-associative by default, but you can make
    # them right-associative by appending a second '_' at the end
    # of the variable's name.
    define _->__ a b = typeof {x: ...(x of a) of b}

    type Pair : <_,_> a b = pairImpl : type {(forall) x: (a -> b -> x) -> x}
      <_,_> a b = Pair {f: f a b}
      fst p = unPair p {a _: a}
      snd p = unPair p {_ a: a}

    # You can define a type-indexed family of functions, allowing different
    # implementations to coexist under the same name.
    family _+_ : Num n => n -> n -> n
      _+_ = addInt
      _+_ = addString

    
      