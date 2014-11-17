-- {{{ Introduction
--
-- The `Writer` monad is for values that have another value attached that acts
-- as some sort of log. Writer makes it so we can do computation while keeping
-- track of log values, which will eventually be returned with the result.
--
-- For example, we often attach our values with strings that explain what is going
-- on, to make debugging easier. Consider a function that takes a number of bandits
-- and tells you whether they compose a big gang or not:
isBigGang' :: Int -> Bool
isBigGang' x = x > 9

-- Instead of just returning `True` or `False`, we want it to also return a log string 
-- that states what it did:
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9")

-- So now, we return a tuple were the first value is the actual value, and the second value
-- is the log value:
--
--    ghci> isBigGang 3
--    (False, "Compared gang size to 9.")
--
-- Let's develop a function that takes an `(a, String)` value and a function of type 
-- `a -> (b, String)`, and feeds the value into the function:
applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x,log) f = let (y,newLog) = f x in (y, log ++ newLog)

-- We can use applyLog to track our computation, e.g.:
--
--    ghci> (3, "Smallish gang.") `applyLog` isBigGang
--    (False, "Smallish gang.Compared gang size to 9")
--
-- So applyLog takes (3, "Smallish gang.") and the function isBigGang, and returns the result
-- of isBigGang along with the new log value.
--
-- }}}
--
-- {{{ Monoids to the rescue
--
-- 
--
-- }}}
--
-- {{{ The Writer type
--
--
--
-- }}}
--
-- {{{ Using do notation with Writer
--
--
--
-- }}}
--
-- {{{ Adding logging to programs
--
--
--
-- }}}
--
-- {{{ Inefficient list construction
--
--
--
-- }}}
--
-- {{{ Difference lists
--
--
--
-- }}}
--
-- {{{ Comparing Performance
--
--
-- 
-- }}}
