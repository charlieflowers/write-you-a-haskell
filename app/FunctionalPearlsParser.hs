-- Since Steven mostly took his nanoparser from the "Funcitonal Pearls" paper,
--  I'm typing in their parser here. But without the newtype and possibly with 
--  a few other modifications.

-- I'll use this to confirm my derivations and conclusions from CharlieWipNano.hs.

-- All parsers are functions of type String -> [(a, String)], except possibly specialized.
item :: String -> [(Char, String)]
item = (\cs -> case cs of 
    ""          -> []
    (c:cs)      -> [(c, cs)])

myReturn :: a -> (String -> [(a, String)])
myReturn a = (\cs -> [(a, cs)])

myBind :: (String -> [(a, String)]) -> (a -> (String -> [(b, String)])) -> (String -> [(b, String)])
p `myBind` f = (\cs -> concat [(f a) cs' |
    (a, cs') <- p cs])

-- That is all it takes to see the core in action and confirm my findings.

example1 = item `myBind` \c -> 
    myReturn c

result1 = example1 "bar" -- CONFIRMED!

example2 = item `myBind` \c -> 
    item `myBind` \d -> 
        myReturn (c, d)

result2 = example2 "bar"  -- CONFIRMED!





