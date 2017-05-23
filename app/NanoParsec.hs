{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module NanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative

-- newtype Parser a = Parser { parse :: String -> [(a, String)]}

runParser :: (String -> [(a, String)]) -> String -> a
runParser m s = 
    case m s of
        [(res, [])] -> res
        [(_, rs)]   -> error "Parser did not consume entire stream"
        _           -> error "Parser error"

-- I see. So runParser is a harmless little helper. Stephen did that to make things
--  simpler, that's all. The "real" parsing stuff is done by parse.

-- So this thing we call "Parser" is really just a function that takes a 
--   string and returns an [(a, String])]. We've assigned a new name to that
--   type esentially.

-- The data record syntax means that we can call "parse foo", where foo is a 
--  parser, and it will give us the parse function. 

-- So "case parse m s of" is actually obtaining the value of the "parse" field
--  of the data type. The result is a fn from string to result. Then, calling 
--  that fn, passing "s" to it. Thus, parsing the string.

-- Then, we pattern match the result, and make the result dead simple (really,
--  too simple to be worthwhile.)

-- item is for advancing the parser one character.
item :: (String -> [(Char, String)]) 
-- That type sig means, I'll return a Char wrapped in the parser monad.
item = \s ->
    case s of 
        []      -> []
        (c:cs)  -> [(c,cs)]

-- So, if you give item empty input, it returns empty. If you give it a list 
--  of chars (aka, a string), it will indicate that it found one char, and 
--  the rest of the string was leftover.

-- Clearly, there are no magical mysteries so far.

-- here's return:
myReturn :: a -> (String -> [(a, String)])
myReturn a = \cs -> [(a, cs)]

-- now it's time for the bind operator. Since Steven chose a list monad (I wish
--  he hadn;t since Parsec doesn't), our bind operator has to map over the
--  members of the list and then flatten, just like any list monad. Clearly, "pure"
--  would merely wrap a single value into the result form. But wait ... maybe
--  steven doesn't plan to deal with ambiguous grammars. Maybe he chose list
--  simply so he could use empty to indicate failure. In which case, the list 
--  will either have one item or zero items.
-- bind :: Parser a -> (a -> Parser b) -> Parser b
-- mapFn :: (a, String) -> 


-- bind p f = Parser $ \s -> 
--    let mapFn (ast, _) = f ast
--    in map mapFn $ parse p s 

-- Whew! Struggling here! My brain is so rusty regarding the haskell way of 
--  thinking! So I'm going to do a couple of things here: 
--  1. Write this so far WITHOUT the newtype.
--  2. break everything down into little tiny pieces, and make them not local
--   so I can work wiht the pieces at the repl.
--  see CharlieWipNano.hs

-- OK, BACK AND READY TO ROLL.
-- Spoiler alert, I've already seen the defition of bind and become quite 
--  familiar with it. But I want to do it without list comprehension
--  anyway, so it will be a bit like writing it from scratch. I'll try the 
--  approach of making each little piece public and named too.

-- To compare my results, here's the REAL bind from the paper
bind :: (String-> [(a, String)]) -> (a -> (String-> [(b, String)])) -> (String-> [(b, String)])
p `bind` f = (\cs -> concat [(f a) cs' |
    (a, cs') <- p cs]) 

-- First, here's the type:
-- myBind :: (String-> [(a, String)]) -> (a -> (String-> [(b, String)])) -> (String-> [(b, String)])
-- Also, remember that 
-- item >>= \c -> item >>= \d -> return (c, d)
-- when applied to "bar", should return
-- [(('b','a'),"r")]
-- which is a list with one item that is a tuple. The tuple contains an ast
--   of ('b', 'a') and a leftover of "r".

p `myBind` f = 
    -- Gotta return a function, so let's start there:
    \cs -> 
        -- The idea is to CALL the first parser on the input.
        let leftResult = p cs
        -- Now leftResult is a list of tuples. You need to loop through
        --  each one, deconstruct it to get ast and leftover, and then
        --  call f with the ast. 
            mapResult = map (\(leftAst, leftRemains) -> 
                let rightParser = f leftAst
                    rightParserResult = rightParser leftRemains
                    in rightParserResult
                ) leftResult
                in concat mapResult

example = item `myBind` \c ->
    item `myBind` \d ->
         myReturn (c, d)

result = example "bar"

--I GOT IT!!! (Other comments below are from the struggles beforehand)






-- This is close. I'm getting: 
-- I get:       [([(('b','a'),"r")],"ar")]
-- should get:  [(('b','a'),"r")]

-- What should I get? 
hisReturn :: a -> (String -> [(a, String)])
hisReturn a = (\cs -> [(a, cs)])

hisBind :: (String -> [(a, String)]) -> (a -> (String -> [(b, String)])) -> (String -> [(b, String)])
p `hisBind` f = (\cs -> concat [(f a) cs' |
    (a, cs') <- p cs])

hisExample = item `hisBind` \c ->
    item `hisBind` \d ->
         hisReturn (c, d)

hisResult = hisExample "bar"         

-- I should be getting: [(('b','a'),"r")]

    
smallerExample = item `myBind` \c ->
    myReturn ('Z', c)

smallerResult = smallerExample "bar"

hisSmallerExample = item `hisBind` \c ->
    hisReturn ('Z', c)

hisSmallerResult = hisSmallerExample "bar" 
 