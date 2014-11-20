import Data.Monoid

-- {{{ Introduction
--
-- Recall, typeclasses are used in Haskell to present an interface with some shared behavior. Think
-- of some of the basic examples we started with:
--
--    - Eq: Types whose values can be equated
--    - Ord: Types with values that can be ordered
--    - Show: Types with values that can be printed 
--    - etc
--
-- We eventually moved on to more interesting typeclasses, like Functors and Applicatives.
--
--    A monoid M is a set with a binary operation, *, that obeys the following two properties:
--
--       (1) There exists 1 in M such that for any x in M, 
--          
--             1 * x = x * 1 = 1 
--
--       (2) For all a, b, c in M,
--
--             a * (b * c) = (a * b) * c
--
-- In other words, a monoid is a group that need not contain inverses. Let's leave math world 
-- and return to CS world, and actually see how the Monoid typeclass is implemented in Haskell:
--
--    class Monoid m where
--       mempty :: m
--       mappend :: m -> m -> m
--       mdconcat :: [m] -> m
--       mconcat = foldr mappend mempty
-- 
-- Let's take a minute to think about how these correspond to the mathematical properties listed above.
-- `mempty` is the identity element. `mappend` is the binary function -- it takes two values of the same
-- type and returns a value of that type. The last function is `mconcat`, which takes a a list of
-- monoid values and reduces them to a single value by doing mappend between the list's elements. 
-- `mconcat` is defined out of convenience, being entirely determined by mempty and mappend. For this
-- reason, one need not define `mconcat` when creating a Monoid. Let's briefly write laws (1)
-- and (2) in Haskell-speak, just for fun. Let x, y, z be in M:
--
--    (1) mempty `mappend` x = x `mappend` mempty = x
--    (2) (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
--
-- Just as in the past, Haskell does NOT enforce these laws, so one has to take care when defining
-- monoids to ensure that they actually obey the requisite laws.
--
-- }}}
--
-- {{{ Lists are monoids
--
-- The list instance of Monoid is pretty straightforward:
--
--    instance Monoid [a] where
--       mempty = []
--       mappend = (++)
--
-- This functions exactly as one would expect, obeying laws (1) and (2) covered in the introduction.
--
-- }}}
--
-- {{{ Product and Sum
--
-- One way for numbers to be consider monoids is to have the binary function * be ordinary multiplication,
-- and let the identity be 1. Another way is to have + as the associative binary operation, with identity
-- 0. It is easy to see that the laws hold under each of the schemes. We can easily use the `newtype` 
-- wrapper to craft a Product type and a sum type:
-- 
--    newtype Product a = Product { getProduct :: a }
--       deriving (Eq, Ord, Read, Show, Bounded)
--
--    newtype Sum a = Sum { getSum :: a }
--       deriving (Eq, Ord, Read, Show, Bounded)
--
--    instance (Num a) => Monoid (Product a) where
--       mempty = Product 1
--       Product x `mappend` Product y = Product (x * y)
--
--    instance (Num a) => Monoid (Sum a) where
--       mempty = Sum 0
--       Sum x `mappend` Sum y = Sum (x + y) 
--
-- While it seems trivial to use the Product Monoid for actually multiplying numbers (ditto for Sum and
-- adding), we will see later some cases in which its uses are more apparent.
--
-- }}}
--
-- {{{ Any and ALL
--
-- Another type that can act like a monoid is Bool, and it can function as a Monoid in two distinct
-- ways. The first way is to have the OR function || act as *, and let False be the identity value.
-- The second way is to have && be the binary function, and let True be the identity value (somoe 
-- thinking about monoid laws immediately reveals why the identity values must be the way that 
-- they are)
--
--    Let's consider the first option. The `Any` newtype constructor is an instance of a Monoid in this
-- fashion, and it is defined like so:
--
--    newtype Any = Any { getAny :: Bool }
--       deriving (Eq, Ord, Read, Show, Bounded)
--
--    instance Monoid Any where
--       mempty = False
--       Any x `mappend` Any y = Any (x || y)
--
-- The second implementation is similar, and it is called `All`. We will not define it here.
--
-- }}}
--
-- {{{ The Ordering monoid
--
-- Recall the `Ordering` type. It's used as the result when comparing things, and it can have the 
-- values LT, EQ, and GT:
--
--    ghci> 1 `compare` 2
--    LT
--    ghci> 1 `compare` 1
--    EQ
--    ghci> 1 `compare` 0
--    GT
--
-- These three values, along with a certain instance of `mappend`  actually compose a monoid. It is defined 
-- like so:
--
--    instance Monoid Ordering where
--       mempty = EQ
--       LT `mappend` _ = LT
--       EQ `mappend` x = x
--       GT `mappend` _ = GT
--
-- While this seems kind of weird, we can think of this monoid as comparable to the manner in which
-- we compare words alphabetically (See pg. 258). It is worth noting that this monoid is not commutative, 
-- i.e. (letting `mappend` := *):
--
--    LT * GT = LT != GT = GT * LT
--
--    Let's consider a possible use for the monoid. Suppose we are writing a function that takes two strings,
-- compares their lengths, and returns x in Ordering. Instead of returning EQ for strings of the same
-- length, we want to then go to an alphabetical comparison. One way to write this is:
lengthCompare' :: String -> String -> Ordering
lengthCompare' x y = let a = length x `compare` length y
                         b = x `compare` y
                     in  if a == EQ then b else a

-- This is a totally reasonable way in which to write this function. However, understanding the way in 
-- which Ordering functions as a monoid, we may write this in a simpler manner:
lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (x `compare` y)

-- If the LHS of that expression is EQ, we get the result of the RHS. Otherwise, the result of the LHS
-- is returned -- this is due to the implementation of `mappend` in the Ordering monoid. We can string
-- these comparison rules together by precedence. For example, say after comparing for length, we want
-- to compare # of vowels. We just chain another comparison using `mappend`:
lengthVowelCompare :: String -> String -> Ordering
lengthVowelCompare x y = (length x `compare` length y) `mappend`
                         (vowels x `compare` vowels y) `mappend`
                         (x `compare` y)
   where vowels = length . filter (`elem` "aeiou")

-- So a general use case for the ordering monoid is now clear -- we may easily compare things by 
-- many different criteria, and then put those criteria in order themselves.
--
-- }}}
--
-- {{{ Maybe the monoid
--
-- There are a number of ways that Maybe can be treated as a monoid. We start with the 
-- default implementation:
--
--    instance Monoid a => Monoid (Maybe a) where
--       mempty = Nothing
--       Nothing `mappend` m = m
--       m `mappend` Nothing = m
--       Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
--
-- This is a relatively straightforward implementation. Note that we need that class 
-- constrain (Monoid a), as it allows us to use `mappend` on the values wrapped in 
-- Just. 
--
--    The natural followup question is how can we use Maybe as a monoid when we don't
-- know if the `a` in `Maybe a` is a monoid? The Data.Monoid package gives us an option,
-- with the `First` and `Last` types. We give the definition of the former:
--
--    newtype First a = First { getFirst :: Maybe a }}
--       deriving (Eq, Ord, Read, Show)
--
--    instance Monoid (First a) where
--       mempty = First Nothing
--       First (Just x) `mappend` x = First (Just x)
--       First Nothing `mappend` x = x
--
-- It is again pretty clear what is happening -- in the case that we have two non-mampty 
-- values being `mappend`'d, we completely ignore the second value. The `Last` type
-- is analagous, but it discards the first value.
--
-- }}}
