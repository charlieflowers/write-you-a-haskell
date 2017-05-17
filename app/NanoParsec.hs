{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module NanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a, String)]}

runParser :: Parser a -> String -> a
runParser m s = 
    case parse m s of
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
item :: Parser Char
-- That type sig means, I'll return a Char wrapped in the parser monad.
item = Parser $ \s ->
    case s of 
        []      -> []
        (c:cs)  -> [(c,cs)]

-- So, if you give item empty input, it returns empty. If you give it a list 
--  of chars (aka, a string), it will indicate that it found one char, and 
--  the rest of the string was leftover.

-- Clearly, there are no magical mysteries so far.

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


    