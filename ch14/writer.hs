import Data.Monoid
import Control.Monad.Writer -- Using do notation with Writer

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
-- We can edit the type of `applyLog` so it works for any sort of type that falls 
-- under the monoid typeclass:
applyLog' :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog' (x,log) f = let (y, log') = f x in (y, log `mappend` log')

-- Here's an example of how we could use our new, more flexible, apply log:
type Food = String
type Price = Sum Int

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _       = ("beer", Sum 30)


-- Now, our log value keeps track of how much we are spending:
--
--    ghci> ("beans", Sum 10) `applyLog` addDrink
--    ("milk", Sum {getSum = 35})
--
-- }}}
--
-- {{{ The Writer type
--
-- Above, we saw how a value with an attached monoid acts monadic. Let's examine
-- the actual Monad instance instance of these types of values:
--
--    newtype Writer w a = Writer { runWriter :: (a,w) }
--
--    instance (Monoid w) => Monad (Writer w) where
--       return x = Writer (x, mempty)
--       (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
--
-- Our implementation of >>= is nearly identical to that of applyLog', only now we wrap
-- everything in the Writer newtype. Obviously, return takes a value and returns a value
-- in the default minimal context by using the monoidic identity element. Let's test
-- return with a bunch of different monoids:
--
--    ghci> runWriter (return 3 :: Writer String Int)
--    (3, "")
--    ghci> runWriter (return 3 :: Writer (Sum Int) Int)
--    (3, Sum {getSum = 0})
--
-- Note that Writer does not have a show instance, so we have to use runWriter to view
-- the tuple contained inside.
--
-- }}}
--
-- {{{ Using do notation with Writer
--
-- Here's an example of using `do` notation with `Writer` to multiply two numbers:
logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
   a <- logNumber 3
   b <- logNumber 5
   return (a*b)

-- And if we run this:
--
--    ghci> runWriter multWithLog
--    (15, ["Got number: 3","Got number: 5"])
--
-- Pretty straightforward -- multWithLog chains together a number of operations on Writer,
-- and then the result is presented (with a log reflecting the process). Suppose we 
-- wanted to chain in a log value and nothing more, something like
--
--    Writer ((), "Hey check it out")
--
-- We can actually use the `tell` function in the middle of our `do` block to do this:
multWithLog' = Writer [String] Int
multWithLog' = do
   a <- logNumber 3
   b <- logNumber 5
   tell ["Gonna multiply these two"]
   return (a*b)

-- Running this function gives us the result:
--
--    ghci> runWriter multWithLog'
--    (15,["Got number: 3","Got number: 5","Gonna multiply these two"])
--
-- Because `tell` has a dummy value for its first component, were we to run it after
-- `return`, we'd have a function that returns
--    
--    ((),["Got number: 3","Got number: 5","Gonna multiply these two"])
--
-- }}}
--
-- {{{ Adding logging to programs
--
-- Let's test logging on a more "real" "world" example. We'll add it to a gcd
-- program:
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
   | b == 0 = do
       tell ["Finished with " ++ show a]
       return a
   | otherwise = do
       tell [show a ++ " mod " ++ show b ++ " = " show (a `mod` b)]
       gcd' b (a `mod` b)

-- If we use mapM_ and putStrLn to make the result kinda pretty, we actually have
-- a function that, aside from computing the gcd of two numbers, will actually 
-- return the steps of the euclidean algorithm:
--
--    ghci> mapM_ putStrLn $ snd $ runWriter (gcd' 24 10)
--    24 mod 10 = 4
--    10 mod 4 = 2
--    4 mod 2 = 0
--    Finished with 2
--
-- Very cool. The main takeway is that the Writer Monad can assist in transforming a normal
-- function into a  function with logging.
--
-- }}}
--
-- {{{ Inefficient list construction
--
-- We need to be careful with the writer monad, becuase it can be very slow if you 
-- use the wrong monoid. That's because lists use `++` for `mappend`, and using `++`
-- to add something to the end of the list can be very slow:
--
--    a ++ (b ++ (c ++ (d ++ (e ++ f))))     (FAST)
--    ((((a ++ b) ++ c) ++ d) ++ e) ++ f     (SUPR SLOW)
--
-- The latter option is slow because every time it wants to add the right part to the left,
-- it must cunstruct the left part all the way from the beginning.
--
--    The gcd' function we defined above used good list construction. Let's make an 
-- inefficient gcd function
badGcd :: Int -> Int -> Writer [String] Int
badGcd a b
   | b == 0 = do
       tell ["Finished with " ++ show a]
       return a
   | otherwise = do
       result <- badGcd b (a `mod` b)
       tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
       return result

-- This function does recursion first, and binds its result value to `result`. Then it adds
-- the current step to the log, but the current step goes at the end of the log that was
-- produced by the recursion. Finally, it presents the result of the recursion as the
-- final result.
--
-- }}}
--
-- {{{ Difference lists
--
-- Because lists can sometimes be inefficient w/ `++`, it's best to use a data structure
-- that always supports efficient appending. Difference list is one such option -- it's
-- like a list, but instead it's a function that takes a list and prepends another list
-- to it, e.g.:
--    
--    [1,2,3]                 (Standard list)
--    \xs -> [1,2,3] ++ xs    (Difference list equivalent)
-- 
-- Now, let's see the difference list equivalent of `++` (append):
--
--    f `append` g = \xs -> f (g xs)
--
-- For example, let f = \xs -> "foo" ++ xs, and let g = \ys = "bar" ++ ys. Then:
--
--    f `append` g = \ys -> "foo" ++ ("bar" ++ ys)
--
-- Easy. Let's make a newtype wrapper for difference lists, and give them monoid 
-- instances. We'll also make some helper functions that convert normal lists
-- to DiffLists and vice versa:
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList f = f []

instance Monoid (DiffList a) where
   mempty = DiffList (\xs -> [] ++ xs)
   (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))


--
-- }}}
--
-- {{{ Comparing Performance
--
-- Let's get an idea of how much difference lists improve performance. Let's create a simple 
-- function that counts down from some number to 0, but produces the log in reverse, so
-- that the numbers in the log are actually counted up:
finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
   tell (toDiffList ["0"])
finalCountDown x = do
   finalCountDown (x-1)
   tell (toDiffList [show x])

-- Load this is ghci, and it starts counting down (up?) pretty quickly. Now, let's do
-- the same function with normal lists:
finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = do
   tell ["0"]
finalCountDown' x = do
   finalCountDown (x-1)
   tell [show x]

-- Running this function w/ a large number takes forever. So difference lists are nice.
-- 
-- }}}
