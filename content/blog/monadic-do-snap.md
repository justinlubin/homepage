+++
title = "Do as I say: “monadic do” notation in Snap!"
date = 2020-01-13
+++

[Snap!](https://snap.berkeley.edu/)
is one of the most widely-used programming languages whose primary editor is
entirely block-based. For a while, I was skeptical of its expressive
capabilities, but, after finally playing around with it myself, I realized that
it has support for some extremely expressive constructs that enable embedded
domain-specific languages. As part of
[Sarah Chasins](https://schasins.com/)’
[CS 294: Building User-Centered Programming Tools](http://schasins.com/cs294-usable-programming-2020/)
class, I implemented a *monadic do* construct in Snap!, similar to
[monadic do notation in Haskell](https://en.wikibooks.org/wiki/Haskell/do_notation)
and
[binding operators in OCaml](https://caml.inria.fr/pub/docs/manual-ocaml/bindingops.html).

I explain more about how this construct works in the rest of this post (along with an example), but if you want to mess around with it live (and see the definitions of every block I use), you can do so
[here](https://snap.berkeley.edu/snap/snap.html#present:Username=justinlubin&ProjectName=monadic-do&editMode&noRun)!

### Implementation

To implement a *monadic do* construct, I had to implement four main features, each of which builds off the previous.

#### Feature 1: Basic Algebraic Data Type Support

I first introduced an “option” (i.e. nullable) type implemented under-the-hood as a singleton list:

![The Snap! representation of "None"](/blog-assets/monadic-do-snap/none.png)

![The Snap! representation of "Some"](/blog-assets/monadic-do-snap/some.png)

I also implemented pattern matching for options and lists:

![Pattern-matching on options](/blog-assets/monadic-do-snap/pattern-match-option.png)

![Pattern-matching on lists](/blog-assets/monadic-do-snap/pattern-match-list.png)

#### Feature 2: Functor and Monad Typeclasses

Next, I introduced blocks that allow for the definition of “anonymous” functor and monad instances:

![Functor typeclass definition](/blog-assets/monadic-do-snap/functor.png)

![Monad typeclass definition](/blog-assets/monadic-do-snap/monad.png)

These blocks can be used to create typeclass instances that can be assigned to a variable using a normal Snap! *set* block:

![Option and monad instances for list and option](/blog-assets/monadic-do-snap/instances.png)

Under the hood, I represent typeclasses as a product of functions, akin to [dictionary passing](http://okmij.org/ftp/Computation/typeclass.html#dict). I also provide “getter” blocks that simply return the correct function in a given typeclass dictionary:

![Accessing the map function of the list functor](/blog-assets/monadic-do-snap/map-list.png)

![Accessing the pure function of the list monad](/blog-assets/monadic-do-snap/pure-list.png)

![Accessing the join function of the list monad](/blog-assets/monadic-do-snap/join-list.png)

![Getting the functor instance for the list monad](/blog-assets/monadic-do-snap/functor-list.png)

Using these getters, I implemented some “typeclass functions” such as *bind* for arbitrary monads:

![A generic bind implementation for monads](/blog-assets/monadic-do-snap/bind-monad.png)

Unfortunately, these functions require explicitly passing in the monad as an argument. Which brings me to…

#### Feature 3: Monadic “Using” Notation

As the next step toward a nice *monadic do* construct, I introduced a block that allows its subexpressions to use a particular monad *implicitly*:

![The "using" block, demonstrated on the list monad](/blog-assets/monadic-do-snap/using-list.png)

It works by setting the global variable `current monad` to the supplied monad, running the block’s subexpression, and resetting `current monad` to its previous value. For example, using this block, I implemented a *return* block that simply calls the *pure* function of `current monad` with a given argument:

![A generic implementation of the return function for monads that uses the
"current monad" variable](/blog-assets/monadic-do-snap/return.png)

#### Feature 4: Monadic Let Bindings

Lastly, I introduced monadic let bindings akin to `x <- mx` in Haskell and `let* x = mx in …` in OCaml, which are sugar for the monadic bind operation:

![A monadic let binding](/blog-assets/monadic-do-snap/monadic-let.png)

These blocks only work in a *using* block because they rely on `current monad` being set properly (so that they can access its *bind* function).

To implement this block, I used an *upvar*, a surprisingly versatile Snap! construct that, rather than *consuming* the value of a variable passed in by a user, instead *provides* a variable to the user. (As an aside, I’ve found upvars to be a fantastic use of interesting/atypical programming language theory tailored to a specific domain that really improves developer experience.)

Using an upvar turned out to be a bit tricky in this context because it meant that I had to rely on mutation to set this provided variable properly, which is not how typical *monadic do* implementations desugar. In particular, I treat the body of the monadic let binding as an uninterpreted expression and perform the following desugaring:

{% center() %}
`let* x = mx in body` ↝ `mx >>= (fun output -> (x := output; body))`
{% end %}

Which, in Snap!, looks like this:

![Using an upvar to define monadic let bindings](/blog-assets/monadic-do-snap/upvar.png)

As a convenience, I also implemented *pure let bindings* to complement monadic let bindings:

![A pure let binding](/blog-assets/monadic-do-snap/pure-let.png)

### An Example

Using the list monad (which simulates a collection semantics for nondeterministic choice), I implemented a simple procedure that

1. “Nondeterministically” chooses a variable `x` from the list `[4, 2, 3]`, then
2. (Purely) sets `y` to twice the value of `x`, then
3. “Nondeterministically” chooses `z` from the list `[1, 2, …, y]`, and finally
4. Returns the triple `(x, y, z)`:

![An example list program](/blog-assets/monadic-do-snap/list-program.png)

When run, this program returns all possible outputs of the procedure just described:

![The output of the list program](/blog-assets/monadic-do-snap/list-output.png)
