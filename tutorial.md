
    
Because it massively overlaps with the `Prelude`, `Streaming.Prelude` must be imported qualified. The `Streaming` module, by contrast, is designed to be imported without qualification. Several important operations are put in the `Streaming.Prelude` because they overlap with the `Pipes` module and are in all cases semantically very close to their `Pipes` meaning. In the present tutorial, `Pipes` is not at issue; the examples will thus presuppose the following imports:


~~~
import Streaming  
import qualified Streaming.Prelude as S
import Streaming.Prelude (each, next, yield)
~~~

Occasionally we will see others, like

~~~
import qualified Control.Foldl as L -- cabal install foldl
~~~

Introducing a stream
--------------------

The simplest and most rudimentary way to construct a stream is with the `yield` statement:

~~~
>>> :t yield 
yield :: Monad m => a -> Stream (Of a) m ()

>>> S.print $ yield True
True

~~~

These can be sequenced with `>>` or do notation:

~~~

>>> S.print $ yield True >> yield False
True
False

>>> S.stdoutLn $ do {yield "hello"; yield "world"}
hello
world

~~~

`yield` statements are the basic building block of any hand-written definition `Stream (Of a) m r`, and will be discussed further below. 

While `yield` makes a singleton stream, `each` streams any pure, `Foldable` container of Haskell values into a nominally effectful stream:

~~~
>>> :t each [1..3::Int]
each [1..3::Int] :: Monad m => Stream (Of Int) m ()

>>> S.print $ each [1..3]
1
2
3
~~~

Even simple combinators can give us a genuinely effectful stream:

~~~
>>> :t S.replicateM 2 getLine
S.replicateM 2 getLine :: Stream (Of String) IO ()

>>> S.print $ S.replicateM 2 getLine
hello<Enter>
"hello"
world<Enter>
"world"
~~~

The simple textual IO operations, like `stdinLn`, `readFile` act line by line


~~~
>>> 

>>> runResourceT $ S.print $ S.readFile "hello.txt"
"hello"
"world"
~~~

(If you are not familiar with it, `runResourceT` here amounts basically `close_any_handle_opened_in`; it makes it possible
to define a semi-sensible `readFile` and `writeFile` without explicit use of handles. Note that these and similar functions in `Streaming.Prelude` are line-based, and hold regular
Haskell `String`s.)

Eliminating streams
-------------------

These are ways of introducing, 'constructing' or 'unfolding' a stream; what are some 
ways of eliminating them?  We have already been using `S.print`, which 
just prints all the elements, and keeps the final return value of the stream. Similarly `S.stdoutLn` eliminates a stream of strings by writing them on separate lines. 

The easiest way 'reduce' a stream is with standard folds like `S.sum`, `S.product`, `S.toList` and the like. `S.print` is a minimal such fold. 

~~~
>>> S.sum $ yield 1 >> yield 2
3 :> ()

>>> S.length $ yield 1 >> yield 2
2 :> ()

>>>  S.minimum $ yield 1 >> yield 2
Just 1 :> ()

>>> S.toList $ yield 1 >> yield 2
[1,2] :> ()

~~~

The underscored variants of standard folds like `S.sum_`, `S.product_` and so on drop the final return value. In simple cases like those above, we could as well have used them:

~~~
>>> S.sum_ $ yield 1 >> yield 2
3

>>> S.toList_ $ yield 1 >> yield 2
[1,2]
~~~

because above we were exiting streaming for good, and the streams we were folding just returned `()` anyway. We will quickly see why the 'official' versions of these simple folds preserve the final return value of the folded stream. 







This package pertains to the type 

    Stream f m r

In principle, `f` might be any functor and `m` any monad. But we are interested in a quite particuler range of functors. and on readings of `m` as as meeting some `MonadIO` constraint, or else as `IO` itself.  

In what follows, whatever fills the `f` position is called "the streamed functor" or "the form of the steps" or simply, "the functor". Whatever fills the `m` parameter is "the monad" or "the monad of effects" or the "effect" or "action" type.  Whatever fills the `r` position is the "return" or "exit" type.

Our focus in this tutorial a particular reading of the functor position:

    Stream (Of a) m r

this is the topic of `Streaming.Prelude`. This is the *stream of individual Haskell values derived from actions in some monad `m` and returning a value of type r*. The "streamed functor" `Of a` is almost as minimal as can be - the left-strict pair:

    data Of a r = !a :> r

We only prefer this to the standard Haskell pair

    data (,) a b = (a,b) 
    
because `ghc` has proven better able to optimize it in our use case.  The glue that holds together a typical Haskell pair, or our left-strict variant, is the glue that holds together the *successive phases* of a `Stream (Of a) m r`, linking each yielded element with the rest of the stream. That is, when we inspect a stream of the type 

    Stream (Of a) m r

we enter the monad of effects, e.g. `IO`, and there we either immediately hit upon the return or exit value - something of type `r`. In which case we are done streaming.  But we may also hit upon something of the type `Of a (Stream (Of a) m r)`. In that case we are "still streaming." The pattern for this case, if we are dealing with it directly, looks like so

    a :> rest

The first member of the pair - an individual `a` - is the present stream element, and it is linked by the pairing constructor to *the rest of the stream*. 

The function `next` 

    next :: Monad m => Stream (Of a) m r -> m (Either r (a, Stream (Of a) m r))
    
expresses these possibilities in terms of the standard base types `(,)` and `Either`. So to elimate all of the elements of a stream in order to get the return value, we might write:

    forget :: Monad m => Stream (Of a) m r -> m r
    forget str = do
       e <- next str
       case e of
         Left r -> return r
         Right (a, rest) -> forget rest

(This is `Streaming.Prelude.effects`.) I will return to `next` in a moment. We are at present only interested in the construction of a `Stream (Of a) ...` 

Note that `Stream` has a show instance which will work when we specialize `m` to `Identity`: 

~~~
>>> each [1..10] :: Stream (Of Int) Identity ()
Step (1 :> Step (2 :> Step (3 :> Step (4 :> Step (5 :> Step (6 :> Step (7 :> Step (8 :> Step (9 :> Step (10 :> Return ()))))))))))
~~~

This reveals the constructors which are hidden in the `Internal` module (here `Step` and `Return`)  but we can observe the similarity to the construction of a Haskell list.  The principal operational difference (apart from the fact that it has a final "return" value - here the vestigial `()`)  is that a `Stream (Of Int) Identity ()` is strict in its leaves. We can change that by moving from `Of Int` to `(,) Int`:

~~~
>>> :t lazily 
lazily :: Of a r -> (a, r)
>>> maps lazily (each [1..10]) :: Stream ((,) Int) Identity ()
Step (1,Step (2,Step (3,Step (4,Step (5,Step (6,Step (7,Step (8,Step (9,Step (10,Return ()))))))))))
~~~





