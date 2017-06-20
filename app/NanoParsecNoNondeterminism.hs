{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}


-- THIS IS A NEW FILE COPIED FROM NANOPSRSEC. THE IDEA IS TO IMPLEMENT A PARSER THAT DOES NOT OFFER NON-DETERMINISM, THAT HAS EXPLICIT FAILURE, AND TAKE IT 
--  UP THROUGH SATISFY, ALTERNATIVE, MANY AND MANY1.

-- The first question is, how exactly did Parsec get rid of non-determinism? All he seems to have said about it is "the <|> combinator, aka ALTERNATIVE, is left-biased
--  and will return the first succeeding parse tree". As if that were the only source of nondeterminism. But I feel like I recall having nondeterminism even in 
--  nanoparsec. Where was that? 

-- AHA, FOUND IT! THE FOLLOWING QUOTE SHOWS THAT THE NON-DETERMINISM *DOES INDEED* COME FROM THE CHOICE OPERATOR! 
-- -- This is how several papers define a monoid for parsers. What do they do 
-- --  with `combine` aka `plus`? Stephen doesn't tell us. But the papers do.
-- -- It is a non-deterministic "choice" operator! So that answers my question -- 
-- --  Stephen IS going to deal with non-determinism, but apparently ONLY if 
-- --  the "grammar author" specifically uses it when "defining the grammar".

-- OK, so Parsec makes the ALTERNATIVE operator like the (+++) DETERMINISTIC choice operator from the functional pearls paper. Left-biased and returns the first parse tree.
--  what about failure though?

-- Well failure was necessary to make <|> left-biased and deterministic, (which as an aside was necessary to move from LL(infinity) to LL(1)). The parser returns whether it 
--  HAS CONSUMED something or HAS NOT. And that is IN ADDITION to whether the result of the parse is OK or ERROR. Makes sense. Here's my first bit of code:

module NanoParsecDeterministic where

import Data.Char
import Control.Monad
import Control.Applicative

type Parser a       = String -> Consumed a

data Consumed a     = Consumed (Reply a)
                    | Empty (Reply a)
    deriving Show

data Reply a        = Success a String | Failure String
    deriving Show
-- ..........................^^^ that is the AST
-- .............................^^^^^^ that is the leftover text

runParser :: (Parser a) -> String -> a
runParser m s = 
    case m s of
        Consumed (Success ast [])       -> ast
        Consumed (Success _ leftover)   -> error "Parser did not consume entire stream"
        _                               -> error "Parser error"

-- item is for advancing the parser one character.
item :: Parser Char 
-- HERE IS THE FIRST QUESTION: If you pass ITEM an empty string, should it FAIL, or SUCCEED WITH NOTHING? In old approach, it 
--  returned [], which meant BOTH failure, or success with nothing. What does functional pearls have to say? It says it FAILS if 
--  nothiing is there.

item = \s ->
    case s of 
        []      -> Empty (Failure "hit eof!") 
        (c:cs)  -> Consumed (Success c cs)

-- So, if you give item empty input, it does not consume, and it fails. If you give it a list 
--  of chars (aka, a string), it will indicate that it found one char and consumed it, and 
--  the rest of the string was leftover.

-- here's return:
myReturn :: a -> Parser a
-- here's the NEXT big question!!! Should RETURN act as if it CONSUMED anything? I don't have enough info to know yet. But
--  I bet it should not. I bet you want this to always tell the truth about whether input was consumed, because it is about
--  avoiding accidental lookahead. So I'm going with that.
myReturn a = \cs -> Empty (Success a cs)

-- OK! Now getting to the good stuff! Let's write BIND!

-- item >>= \c -> item >>= \d -> myReturn (c,d) should, when applied to "dog", return ('d', 'o').

p `myBind` f = 
    -- Gotta return a function ....
    \cs -> 
        -- Call the left parser on the input
        let leftResult = p cs
        -- Now, leftResult is a CONSUMED. If it failed, fail. If it succeeded, we don't care if it consumed or not. We 
        --  need to get the right-side parser by executing a portion of the combinator machinery, and once we have it, 
        --  we need to apply the right-side parser to the leftovers.
        -- THIS IS WAY WAY WAY SIMPLER THAN THE TYPICAL HUTTON NONDETERMINISTIC LIST-BASED PARSERS!
        in case leftResult of 
            Consumed (Failure msg)                -> Consumed (Failure msg)
            Empty (Failure msg)                   -> Empty (Failure msg)
            Consumed (Success leftAst leftover)   -> callRightParser True leftAst leftover
            Empty (Success leftAst leftover)      -> callRightParser False leftAst leftover
            where
                callRightParser didLeftConsume leftAst leftover = 
                    let rightParser = f leftAst
                        rightResult = rightParser leftover
                        in 
                            case rightResult of 
                                Consumed (Failure msg)                        -> rightResult
                                Empty (Failure msg)                           -> if didLeftConsume then Consumed (Failure msg) else rightResult
                                Consumed (Success rightAst rightLeftover)     -> rightResult
                                Empty (Success rightAst rightLeftover)        -> if didLeftConsume then Consumed (Success rightAst rightLeftover) else rightResult

-- OK! This is UGLY AS SIN, and WAAAY TOO VERBOSE, due to some haskell rustiness. But it APPEARS CORRECT.

-- Here are some  tests: 
chain = item `myBind` \c -> item `myBind` \d -> myReturn (c,d)

example1 = chain "dog" -- Consumed (Success ('d', 'o') "g")

-- Now, what's next? What does write-you-a-haskell do next? It does FAILURE, OPTION and COMBINE next.
-- HOLY COW!!! This is starting to get EASY. The nondeterminism of the default Hutton was what made it a confusing learning process 
--  in the first place. If they want to help learners more, they should DEFINITELY remove that from the equation the first go around 
--  and then pedagogically build up to it.

-- I don't really need FAILURE ... it was implemented in the papers to create the monads. But I'm bypassing that for now.
--  I merely need to implement COMBINE and OPTION.

-- COMBINE DOESN'T MAKE SENSE EITHER!!! It would parse the input with p1 and p2, and concatenate the resulting lists. That is 
--  ALL ABOUT NONDETERMINISM. At this point, I assume Parsec does not have any equivalent to the COMBINE operator. Lemme check the
--  paper: indeed, it says nothing about it.

-- What does functional pearls say about COMBINE? Does it confirm it is about NONDETERMINISM? YES! It says "the (++) operator is a 
--  (non-deterministic) choice operator for parsers".

-- So, it is simply time to implement OPTION, and I think it will be really easy.
-- As mentioned at the very start of this file, OPTION is merely the (+++) operator from functional pearls. It is a LEFT-BIASED
--  choice operator which returns ONLY THE FIRST PARSE RESULT, which makes it DETERMINISTIC.

-- One thing to notice here: The OPTION combinator DOES NOT rely on SATISFY! It merely relies on the ability of each parser
--  to succeed or fail. Satisfy comes later!

-- So, OPTION means, try p1, but if it fails, go with p2. Here we go:

pleft `option` pright = 
    \cs -> let leftResult = pleft cs
    in case leftResult of
        Consumed (Failure _)                -> pright cs
        Empty (Failure _)                   -> pright cs
        _                                   -> leftResult

-- HAH! i can't test OPTION yet because all I have is ITEM, which always succeeds! And that means, LET'S INTRODUCE FAILURE!
failure :: Parser a
failure _ = Empty (Failure "failure always fails")

-- some expectations: 
-- (failure `option` item) "test" should be Consumed (Success 't' "est")
-- (failure `option` failure) "test" should be Empty (Failure "failure always fails")
-- (item `option` failure) "test" should be Consumed (Success 't' "est")
-- (chain `option` failure) "test" should be Consumed (Success ('t', 'e') "st")
-- (failure `option` chain) "test" should be Consumed (Success ('t', 'e') "st")

-- Now, one question I have: is OPTION EXACTLY the same as Parsec's <|> operator, or is <|> something different? Lemme check the Parsec paper. YES! OPTION
--  is the SAME THING as the Parsec paper's <|>. BUT -- I do notice that I failed to handle the "longest match" rule. If pleft succeeds without consuming anything, 
--  then you want to return the result of pright! Cool!

-- NOW! I have a deterministic choice operator, and it is time to write my own implementation of MANY! Sweet, this is the goal that drove all this.
-- Many is itself a combinator that takes in a parser function. It then returns a parser function. Internally, it will try the parser fn you passed,
--  and if it succeeds, keep on calling the parser you passed until it fails. It shouldn't be too hard.

-- WAIT!! Jumping the gun! Need a few more basic parsers first.

-- Like, SATISFY.

-- Let's refresh on the requirements for Satifsy, from Write You a Haskell.
satisfy :: (Char -> Bool) -> Parser Char

-- satisfy (`elem` "abc") "aXX" should be Consumed (Success 'a' "XX")
-- satisfy isUpper "abc" should be Empty (Failure "did not meet predicate")
-- satisfy isLower "abc" should be Consumed (Success 'a' "bc")
-- satisfy isDigit "abc" should be Empty (Failure "did not meet predicate")
-- satisfy isDigit "1bc" should be Consumed (Success '1' "bc")
-- satisfy isDigit "" should be Empty (Failure "hit eof!")

-- HEY, ANOTHER NOTE! I did this in plain old pointy style. I could have written SATISFY without mentioning CS, in point free style, at the level of combinators!
satisfy pred = \cs ->
    let parseResult = item cs in
    case parseResult of 
        -- The only thing that bugs me here is we're returning Empty, when actually we DID consume a char for a bit there. Should this return Consumed?
        -- Or, maybe we should not be using ITEM to perform our one char lookahead.
        Consumed (Success char _)   -> if pred char then parseResult else Empty (Failure "did not meet predicate")
        _                           -> parseResult

-- Now, let's make DIGIT. If the next char is a digit, parse it. Fail otherwise.
digit :: Parser Char
digit = satisfy isDigit

-- NOW! Let's write MANY!!!!
--
-- Examples:
-- many digit $ "12345AAA" should be Consumed (Success "12345" "AAA")
-- many digit $ "ABCDEF" should be Empty (Success "" "ABCDEF") -- Note, many ALWAYS succeeds
-- many digit $ "" should be Empty (Success "" "")

-- Right here, you can see why MANY can't be based on SATISFY alone. (I was interrupted here and lost my train of thought! Hope it comes back!)
-- Let me check functional pearls to see if this matches up. wELL, it doesn't offer any details. Let's see what "intro to parsing" has to say.
-- Yes, it's examples match. So let's do it.

many :: Parser a -> Parser a
many parser = many parser []

manyImpl :: Parser a -> [String] -> Parser a
manyImpl parser acc = \cs -> 
    let parserResult = parser cs in
        case parserResult of
            -- So this is interesting. I feel like I need to re-write BIND. And that makes me wonder if I'm missing something. 
            Empty (Failure msg)                         -> Consumed (Success acc cs) -- todo: Get consumed vs empty right here
            Consumed (Failure msg)                      -> Consumed (Success acc cs) 
            _                                           ->
                -- Next, just turn the whole let clause into a fn in a where that includes the let. And call that from the _ pattern above this line.
                let theRest = manyImpl parser acc $ leftover in
                    case theRest of 
                        Consumed (Success string leftover)    -> Consumed (Success char:string leftover)
                        _                                     -> error "not possible"
            

















                    

