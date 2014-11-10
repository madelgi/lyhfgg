import Data.Char
import Data.List

-- {{{ Introduction
--
-- If you ever find yourself binding the result of I/O to a name, and then applying
-- a function and calling it something else, consider using fmap. For example, here's
-- the 'wrong' way of doing things:
fmapping_io' = do line <- getLine
                  let line' = reverse line
                  let line'' = intersperse '-' line'
                  putStrLn line''

-- The above code takes a string and returns it, reversed w/ dashes in between each
-- character. The code below does the same thing (and additionally maps the characters
-- to their upper case equivalents), and does so in a much more concise manner:
fmapping_io = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
                 putStrLn line

-- }}}

-- {{{ Functions as functors
--
-- Functions are another instance of functors. Consider a function f,
--    
--    f: r --> a
--
-- This function can also be written as (->) r a (for example, we can write
-- 2 + 3 as (+) 2 3). Here's the implementation:
--
--    instance Functor ((->) r) where
--       fmap f g = (\x -> f (g x))
--
-- The above implementation looks a lot like function composition. Indeed, fmap's type
-- is
--    
--    fmap :: (a -> b) -> f a -> f b
--
-- Replace f with (->) r,
--
--    fmap :: (a -> b) -> (->) r a -> (->) r b
--
-- Writing it in a more intutive way, we get
--
--    fmap :: (a -> b) -> (r -> a) -> (r -> b)
--
-- So what does the above say? Given f : r -> a and g : a -> b. we have
--
--    fmap f g = f \circ g : r -> b,
--
-- which is precisely function composition. Indeed, we could write the implementation of
-- fmap as fmap = (.).
-- 
-- }}}
--
-- {{{ Defining fmap
--
--    Let's think about fmap generally. In the above case, it's acting as function composition.
-- In general, it has type fmap :: (a -> b) -> f a -> f b. In other words, it takes a function
-- and a functor, and then maps that function over the functor. For example
--
--    fmap (replicate 3) [1, 2] = [[1,1,1], [2,2,2]]
--
-- Here, our function is 'replicate 3', and the functor is the list functor. The replicate function
-- is mapped over each of of the elements within the list functor.
--
--    We are gradually honing in on the formal definition of a functor. A functor is a class with
-- associated function `fmap` that obeys the following laws:
--
--    (1) fmap id = id
--    (2) fmap (f . g) = (fmap f) . (fmap g)
-- 
-- Every instance of the functor class must obey these laws. As a demonstration, we verify that
-- these hold for fmap over Maybe:
--
--    fmap id Nothing = Nothing (by definition)
--    fmap id Just x = Just (id x) = Just x
--
-- Thus (1) is satisfied. Next, we show (2):
--
--    fmap (f . g) Nothing = Nothing 
--    fmap f (fmap g Nothing) = Nothing
--
--    fmap (f . g) (Just x) = Just (f . g x)
--                          = Just (f (g x))
--                          = fmap f (Just (g x))
--                          = fmap f (fmap g (Just x))
-- 
-- Thus fmap (f . g) = (fmap f) . (fmap g), as expected.
--
-- }}}
--
-- {{{ Example: Wolf in functor's clothing
--
-- In this section, we provide a pathological example of an instance of the Functor typeclass
-- that does not abey the functor laws
data CMaybe a = CNothing | CJust Int a 
   deriving (Show)

instance Functor CMaybe where
   fmap f CNothing = CNothing
   fmap f (CJust counter x) = CJust (counter + 1) (f x)

-- We can immediately see that this functor, because of how the counter works, is not well-behaved. 
-- Indeed, we can easily break the first functor law:
--
--    fmap id (CJust 0 "haha") = CJust 1 "haha"
--                            /= Cjust 0 "haha"
--                             = id (CJust 0 "haha")
--
-- All this goes to show that we can make instances of the Functor typeclass that do not obey the functor
-- laws. This is bad. While the laws may seem esoteric and unnecessary, we may make assumptions about
-- types that obey these laws, leading to code that is more abstract and extensible.
--
-- }}}
