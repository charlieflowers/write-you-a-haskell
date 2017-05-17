-- Whew! Struggling here! My brain is so rusty regarding the haskell way of 
--  thinking! So I'm going to do a couple of things here: 
--  1. Write this so far WITHOUT the newtype.
--  2. break everything down into little tiny pieces, and make them not local
--   so I can work wiht the pieces at the repl.
--  see CharlieWipNano.hs

-- The monad type is: String -> [(a, String)]

runParser :: (String -> [(a, String)]) -> String -> a
runParser m s = 
    case m s of
        [(res, [])] -> res
        [(_, rs)]   -> error "Parser did not consume entire stream"
        _           -> error "Parser error"

 
-- See, item is just the same function, specialized to Char
item :: String -> [(Char, String)]
item = \s -> 
    case s of
        []      -> []
        (c:cs)  -> [(c, cs)]

-- This really makes it clear that item is a function. And we're just naming it. Which
--  of course means we can use this syntax.
itemAlt :: String -> [(Char, String)]
itemAlt []      = []
itemAlt (c:cs)  = [(c, cs)]

-- now, let's write a bind for this function.
-- reminder, bind is: m a -> (a -> m b) -> m b

bind :: (String -> [(a, String)]) -> (a -> (String -> [(b, String)]) -> (String -> [(b, String)]))

-- Now, Stephen doesn't make it totally clear what "bind magic" means for this 
--  particular monad. He talks about mapping over the list, so that is at least
--  part of it. But why? I don't think he's dealing with nondeterminism. So is
--  he only dealing with failure/success? I don't guess it matters. Usually,
--  to make a monad meaningful to the dev who will use it, you'd tell them why.
--  But instead, I've been told map is part of it. That could be nondeterminism
--  or "fake maybe", or both, or something else. Let's go with it for now.
--  What COULD multiple records mean? Only thing I can imagine is 
--  non-determinism. Clearly, missing means failure. And I see that "item" can
--  return empty list. 
--  His "combine" combinator returns the concatenation of parsing with p and 
--  parsing with q. "combine" is merely the plus of the monoid. 

-- Side note: one key to all this. NOTHING is tracking POSITION. Parsec does, but 
--  NOT our nanoparsec! We DON'T NEED TO, because the UNCONSUMED RESULT is 
--  returned. Combining 2 parsers is merely a matter of the second one working on 
--  the unconsumed input of the first one! 

-- So now, what does his "combine" combinator do? It REALLY DOES parse the SAME
--  text TWO WAYS. If you call "doTheParse foo", it tries to parse foo with a, and
--  it tries to parse foo with b. It then concats those lists together.
--
-- So, if you did `(combine charParser upperParser) "Foo"`, then:
    -- charParser would return [('F', "oo")], and 
    -- upperParser would return [('F', "oo")]
    -- then "combine" would give you: [('F', "oo"), ('F', "oo")]
 -- 
 -- This is how several papers define a monoid for parsers. What do they do 
 --  with `combine` aka `plus`? Stephen doesn't tell us. But the papers do.
 -- It is a non-deterministic "choice" operator! So that answers my question -- 
 --  Stephen IS going to deal with non-determinism, but apparently ONLY if 
 --  the "grammar author" specifically uses it when "defining the grammar".

 -- So, non-determinism IS part of the bind magic, so let's implement bind 
 --  with that in mind.

-- Go see Steven's "satisfy" function. He used bind for it.

 -- So, non-determinism IS part of the bind magic, so let's implement bind 
 --  with that in mind.

-- Let's get some requirements clatrified in my head. Say you have this parser:

-- parser = (combine charParser digitParser) >>= charParser
-- Remember, combine means try them both and lets consdier all paths moving fwd.
--
-- If you then called that fn as follows:
-- parser "9f"
--
-- What should you get?

-- I thought I knew what bind should do, but looking at his satisfy 
--  definition, now I'm not so sure. So let's go back to the parser combinator
--  paper. The paper EXACTLY matches steven's definitions (Functional Pearls).
--  It says that bind is a SEQUENCING operator for parsers. And indeed, it 
--  walks the list monad.

-- The paper gives this as an example that CONSUMES 3 CHARS, THROWS AWAY THE 
--  SECOND CHAR, AND RETURNS THE OTHER 2 AS A PAIR:

-- p = do {c <- item; item; d <- item; return (c, d)}

-- Now I'll desugar it:
--
-- p = item >>= \c -> 
--          item >>= \_ -> 
--            item >>= \d ->
--              return (c, d)    

-- HEY! Need to note one thing right now. The return call takes a TUPLE. That
--  is fine because "a" is a type variable. This is returning a 
-- Parser (Char, Char), so a equals (Char, Char).

-- This is all QUITE OBVIOUS, so much so I wonder why I needed examples. 
--  My brain is not nearly as comfortable thinking the haskell way as it used to 
--  be.

-- Let's flesh out this example from the paper and see if we need my other example
--  or not. Say you had the above parser, and you called it with 'bUb'

-- The first call to item would return [('b', "Ub")]
-- The second bind would have to consider EACH ITEM in the list a valid 
--  possibility, so it would bind d to EACH member of the list in turn.
--  Then, it would flatten all those results into one list. 
--  So, what gets bound to c is 'b'. 
--  The paper figures the call to the middle bind fn as:
--  (a, cs') <- p cs
-- which mmeans
--  ('b', "Ub") <- item cs
-- so a = 'b' and cs' = "Ub"
-- (1st lambda applied to 'b') "Ub"
-- c = 'b'
-- 
-- Sigh. This is hard. But it is a very interesting, educational kind of hard.
--  This drives home what you already knew ... each bind in a new lambda that runs
--  all the wayt to the end of the whole chain. Each bind means a new closure
--  capturing the values from its environment, which include the values fom 
--  "earlier" lambdas.
---
-- And this is GOOD. Yes, it stretches and punishes the brain to have to trace
--  through 10 levels of such lambdas. But the beautiful thing is, once you
--  fully understand bind and get comfprtable with it, you won't have to! You
--  will happily CAU"SE such huge chains to be generated. They will be doing
--  valuable work on your behalf. But you won't have to struggle with them.
--  You'll be working at a higher level of abstraction where they just make sense.
--
-- But for now, to cemet these ideas in my head, I need to walk through them.
--  I'm going to change my example, to make it MUCH EASIER on me. I'll do
--  a bind with only 2 levels in it.

-- p = item >>= \c -> 
--        return c

-- As I have changed it, it returns the result of the first item. Return is 
--  implemented in the paper as:
--  paperReturn a = Parser (\cs -> [(a, cs)])
-- So it is a funciton that takes a String and says "I matched this a, and the
--  leftovers were cs"  
-- 
-- Of course, I'm not using the newType, so my impl is:
--
-- return a = \cs -> [(a, cs)]
--
-- So, let's imagine calling p with "bar"
--
-- First, call item on "bar" and get result.
-- [('b', "ar")]
-- Now, bind the 'b' to c. You can already see that the last expression in 
--  p is "return c", so the result of the whole of p "bar" will be
--  return 'b', which is 
--  \cs -> [('b', cs)]. But you don't know what the bind operator will do to 
--  the result of the expression. So let's slog through it.
--
-- Bind is defined as:
-- p >>= f = (\cs -> concat [(f a) cs' |
--                        (a, cs') <- p cs])
-- Apply p to cs. That's p to "bar". p is item. So you have: 
-- p >>= f = (\cs -> concat [(f a) cs' |
--                        (a, cs') <- item "bar"])
-- item applied to "bar" is [('b', "ar")]
-- So a becomes 'b', and cs' becomes "ar".
-- Luckily there is only one item in the list, so merely substitute a and cs'
-- (\cs -> concat [(f 'b') "ar"])
-- f is (\c -> return c)
-- So substituting f:
-- (\cs -> concat [( (\c -> return c) 'b') "ar"])
--  and this part    ^^^^^^^^^^^^^^^^^^^^
--  simplifies to return 'b', so we have
-- (\cs -> concat [(return 'b') "ar"])
-- (return 'b') evaluates to (\xs -> [('b', xs)]), so we have
-- (\cs -> concat [(\xs -> [('b', xs)]) "ar" ])
-- and this part   ^^^^^^^^^^^^^^^^^^^^^^^^^ merely means pass "ar" to that lambda
-- which results in [('b', "ar")]
-- So we have: 
-- (\cs -> concat [ [('b', "ar")] ])
-- per definition of concat, that becomes:
-- (\cs -> [('b', "ar")])
--
-- OK, so this is surprising me a bit. The end result of (p "bar") is a 
-- PARSER FUNCTION! That should NOT surprise, because "once you're in a monad,
--  you don't get back out." At least not with bind you don't. 
--  So we get a function that IGNORES its input and returns [('b', "ar")]. The
--  only reason we want it to be a function is because now we can continue
--  right on combining  it with more parser functions. Hence the term "combinators"
--  after all!
--
-- Winding up with a function that is a glorified constant seems silly but it 
--  is only because I chose such a simple case. Now, let's add another level in 
--  there.
--
-- pp = item >>= \c -> 
--        item >>= \d -> 
--           return (c, d)
--
-- In this case, our ast is a tuple. Don't let that confuse you.
--
-- For the first substitution, we're binding "item" to a lambda with param "c".
-- So let's use the definition of bind for that:
(\cs -> concat [(f a) cs' |
    (a, cs') <- p cs])

-- We're going to apply that function to "bar". So we get:
concat [(f a) cs' | 
    (a, cs') <- p "bar"]
-- p, here .....^.. is item, so we have:
concat [(f a) cs' | 
    (a, cs') <- item "bar"]
-- This ........^^^^^^^^^^ is easily obtained by defiition of item
concat [(f a) cs' | 
    (a, cs') <- [('b', "ar")]]
-- Since list only has 1 item, a becomes 'b' and cs' is "ar"
concat [(f 'b') "ar" ]
-- f is: \c -> 
--          item >>= \d ->
--              return (c, d)
--
-- let's apply 'b' to f:
item >>= \d ->
    return ('b', d)
-- Let's work on that with the definition of bind.
(\cs -> concat [(f a) cs' |
    (a, cs') <- p cs])
-- we're going to apply "ar" to it, so cs = "ar". p is item.
concat [(f a) cs' |
    (a, cs') <- item "ar"]
-- This ........^^^^^^^^^ evals to ('a', "r"), so:
concat [(f a) cs' |
    (a, cs') <- ('a', "r"')]
-- per list comprehension, a = 'a' and cs' = "r"
concat [(f 'a') "r" ]
-- f is \d -> 
--    return ('b', d)
-- Let's apply 'a' to that:
return ('b', 'a')
-- Per definition of return, that is:
(\cs -> [( ('b', 'a') , cs )])    
-- let's apply "r" to that (CAREFULLY!)
[( ('b', 'a') , "r")]
-- So back on line 245, we had this:
concat       [(f 'a') "r" ]
-- and this   ^^^^^^^^^^^^ we figured out is [( ('b', 'a') , "r")], so:
concat       [ [( ('b', 'a') , "r")]  ]
--per def of concat, that is:
[(('b','a'),"r")]
-- This is a list. The list has only one tuple in it.
-- The tuple has an ast of ('b', 'a'), and a leftover of "r".
-- Now, back on line 227, we had this:
concat [(f 'b') "ar" ]
-- this ^^^^^^^^^^^^^  is what we just figured is [(('b','a'),"r")]
-- So we have:
concat [ [(('b','a'),"r")]    ]
-- per definition of concat, that is:
[(('b','a'),"r")]
-- This is a list. The list contains only one tuple. The tuple contains
--  an ast of ('b', 'a') and a leftover of "r".
--  THIS IS IT, AND I HAVE FINALLY SUCCESSFULLY HASHED OUT BIND.

-- So now that I have done it, describe how bind works:
-- The ast that resulted from the left fn in the bind gets passed to the 
--  right fn. That produces another fn, which happens to be a parser fn too.
--  The new parser fn gets applied to the leftovers. So it's simply a 
--  mechanical assembly line. Eventually, you're likely to end the chain
--  with a return. If you do, return produces a parser fn that ignores its
--  inputs and returns what you said to return, which is made up of whatever
--  you've built up.

--NEXT: Test this out and confirm by typing my definitions (just like steven's 
--  except no newType).








-- **************************************************************
-- EARLIER MISTAKES BELOW THIS LINE.


-- Below, here are some mistaken attempts. Main mistake was applying
--  the lambdea with \cs to the input, but keeping \cs -> around afterwards.

-- this part    ^^^^ means apply item to cs. cs is what is passed in, whic is "bar"
-- So we have:
(\cs -> concat [(f a) cs' |
    (a, cs') <- item "bar"])
-- item "bar" evals to [('b', "ar")]. So we sub 'b' for a and "ar" for cs'.
(\cs -> concat [(f 'b') "ar"])
-- f is: \c -> 
--          item >>= \d ->
--              return (c, d)
-- To apply 'b' to f gives: 
-- item >>= \d ->
--    return ('b', d)    
-- Now, let's apply "ar" to that. We will need to use the definition of bind
--  again. 
(\cs -> concat [(f a) cs' |
    (a, cs') <- p cs])
-- p cs means apply item to "ar", so:
concat [(f a) cs' |
    (a, cs') <- item "ar"]
-- item ar evals to [('a', "r")], so a = 'a' and cs' = "r". So:
concat [(f 'a') "r"]
-- f here is \d -> return ('b', d), let's apply 'a' to that:
return ('b', 'a')
-- by the definition of return, that is:
(\cs -> [( ('b', 'a'), cs)])
-- Let's apply "r" to THAT!
[( ('b', 'a'), "r")]
-- So, back up a few lines (236) we had this: concat [(f 'a') "r"]
-- and we figured out this part ......................^^^^^^^^^^^
-- so now we have: 
concat [ [( ('b', 'a'), "r")] ]
-- which per definition of concat is: 
[(('b','a'),"r")]
-- So this is a list. The list only has one tuple. That tuple has 
--  and ast of ('b', 'a'), and a leftover of "r".








-- So, a parser fn that ignores input and returns that it has parsed 'b' and 'a'
--  and has "r" leftover.
-- So, way back on line 221 (if things don't shift), we have this:
-- (\cs -> concat [(f 'b') "ar"])
-- and this part ..^^^^^^^^^^^^ was resolved to:
(\cs -> [(('b','a'),"r")])
-- now, for "hygeine" i'll rename this cs to xs
(\xs -> [(('b','a'),"r")])
-- and plugging that in, we get
(\cs -> concat [(\xs -> [(('b','a'),"r")])])
-- And this is a problem, because concat is being passed only one level of list.

-- Where did I go wrong? Let's do it again from the top, but more mechanically
--  this time, marking the substitution region with arrows and following it 
--  mechanically.

-- pp = item >>= \c -> 
--        item >>= \d -> 
--           return (c, d)
--
-- And we're calling: pp bar
--
-- Line 267 binds item to a lambda that takes \c as input. Let's use 
--  definition of bind for that:
(\cs -> concat [(f a) cs' |
    (a, cs') <- p cs])
-- we can sub   ^^^^ this.
-- p is item. cs is "bar". So:
(\cs -> concat [(f a) cs' |
    (a, cs') <- item "bar"])
-- This ........^^^^^^^^^^ evals to: [('b', "ar")]. So
(\cs -> concat [(f a) cs' |
    (a, cs') <- [('b', "ar")] ])
-- This simply means sub 'b' for a and "ar" for cs'. So
(\cs -> concat [(f 'b') "ar"])
-- Next target: .^^^^^
-- f is c -> item >>= \d -> return (c, d)
-- Applying 'b' means replace c with 'b'
-- item >>= \d -> return ('b', d)
-- Back on 285, we had: 
(\cs -> concat [(f 'b') "ar"])
-- and this     ^^^^^^^ is known, so sub it
(\cs -> concat [item >>= \d -> return ('b', d) "ar"])
-- This may be where I fucked up last time.
-- We really have to apply the definiton of bind to get anywhere next.
-- We have: (\cs -> concat [item >>= \d -> return ('b', d) "ar"])
-- and we are targeting     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- Definition of bind is:
(\cs -> concat [(f a) cs' |
    (a, cs') <- p cs])
-- in this bind,^^^^ is item "ar", so
(\cs -> concat [(f a) cs' |
    (a, cs') <- item "ar"])
-- And this     ^^^^^^^^^ is [('a', "r")], so
(\cs -> concat [(f a) cs' |
    (a, cs') <- [('a', "r")] ])    
-- So sub 'a' for a and "r" for cs'
(\cs -> concat [(f 'a') "r"])
-- And f is: \d -> return ('b', d) "ar". So:
(\cs -> concat [( (\d -> return ('b', d) "ar") 'a') "r"])
-- Target: .......^^^^^^^^^^^^^^^^^^^^^^^^^^^
 (\cs -> concat [( return ('b', "ar") ) 'a') "r"])
 -- Target: .......^^^^^^^^^^^^^^^^^^, with def of return
return a = (\xs -> [(a, xs)]) 
-- So the target is: (\xs -> [('b', "ar"), xs]). Thus: 
(\cs -> concat [( (\xs -> [('b', "ar"), xs])) 'a') "r"])
-- Remove unnecessary parens
(\cs -> concat [ (\xs -> [('b', "ar"), xs]) 'a') "r"])
-- Target: ......^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- One thing is clear: this is EXHAUSTING! There is no doubt if you 
--  can operate comfortably at the level of bind that a BUNCH of low
--  level details are happening for you!
-- I think my parentheses are messed up.










-- Applying 'a' to that gives
return ('b', 'a') "ar"
-- The definiton of return is: 
return a = (\xs -> [(a, cs)])
-- So,
return ('b', 'a') = \xs -> [('b', 'a'), xs]
-- So back at 309 we had this:
return ('b', 'a') "r"
-- ^^^^^^^^^^^^^^ that becomes \xs -> [('b', 'a'), xs], giving
\xs -> [('b', 'a'), xs] "r"
-- which evals to
[('b', 'a'), "r"] -- THIS IS IMPOSSIBLE. HETEROGENOUS LIST.
-- So back at 306, we had
(\cs -> concat [(f 'a') "r"])
-- and this     ^^^^^^^^^^^ became [('b', 'a'), "r"], giving
(\cs -> concat [[('b', 'a'), "r"]])






















