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

data Reply a        = Success a String | Failure String
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
        (c:cs)  -> Consumed (Success 'c' cs)

-- So, if you give item empty input, it does not consume, and it fails. If you give it a list 
--  of chars (aka, a string), it will indicate that it found one char and consumed it, and 
--  the rest of the string was leftover.

-- here's return:
myReturn :: a -> Parser a
-- here's the NEXT big question!!! Should RETURN act as if it CONSUMED anything? I don't have enough info to know yet. But
--  I bet it should not. I bet you want this to always tell the truth about whether input was consumed, because it is about
--  avoiding accidental lookahead. So I'm going with that.
myReturn a = \cs -> Empty (Success a cs)