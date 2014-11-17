-- {{{ The newtype keyword
-- 
-- We know two methods of constructing types, at this point:
--
--    `data` keyword: used to construct our own algebraic data types
--    `type` keyword: use to give existing types synonyms
--
-- Now, we look at making new types out of existing data types by using the `newtype` keyword.
--
--    Recall that there are multiple ways for lists to be applicative functors. The default
-- implementation for <*> is to have it take every function out of the left-parameter list
-- and apply it to every value in the right parameter list, e.g.:
--
--    ghci> [(+1),(*100),(*5)] <*> [1,2,3]
--    [2,3,4,100,200,300,5,10,15]
--
-- Another possible implementation would be for functions to be applied to the values
-- sequentially, kinda like zipping two lists together. The `ZipList` type satisfies
-- this implementation:
--
--    ghci> getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3]
--    [2,200,15]
--
-- Let's think about how we would implement ZipList. Using the tools we have developed
-- thus far, one option is using the `data` keyword:
--
--    data ZipList a = ZipList [a]
--
-- A type that has just one value constructure and that value construct has just one field
-- that is a list of things. We can use record syntax to automatically get a function
-- that extracts a list from `ZipList`:
--
--    data ZipList a = ZipList { getZipList :: [a] }
--
-- This totally works. We had two ways of making lists an instance of Applicative, so we used
-- the `data` keyword to just wrap that type into the new `ZipList` type, and then proceed
-- to implement <*> in the second way.
--
--    The `newtype` keyword in Haskell is made precisely for these cases, where we want to take
-- one type and wrap it in something to present it as another type. The actual definition of
-- ZipList is as follows:
--
--    newtype ZipList a = ZipLIst { getZipList :: [a] }
--
-- Instead of `data`, the `newtype` keyword is used. In a sense, `newtype` is a specialized form
-- of `data`, used when all you want to do is wrap an existing type into a new type. Because
-- it is specialized in this manner, it is faster. Obviously, `data` is more broadly applicable.
-- The `newtype` keyword is restricted to one value constructor, and that value constructor can 
-- only have one field. With `data`, you can make data types that have several value constructors,
-- and each constructor can have zero or more fields:
data Profession = Fighter | Archer | Pianist

data Race = Human | Elf | Orc | Space | Rat

data PlayerCharacter = PlayerCharacter Race Profession

-- We can use the deriving keyword with `newtype`, same as with `data`. 
--
-- }}}
--
-- {{{ Using newtype to make type class instances
--
-- Recall `Maybe` as a Functor instance:
--
--    class Functor f where
--       fmap :: (a -> b) -> f a -> f b
--
--    instance Functor Maybe where
--       fmap _ Nothing = Nothing
--       fmap f (Just x) = Just (f x)
--
-- It's easy to implement a Maybe functor, because we basically just replace all f's in the 
-- definition of fmap with Maybe's, and things workout great.
--
--    What if we have a case where things don't workout as nicely. Let's say we want to make
-- a tuple an instance of Functor, such that the implementation of fmap applies a function to
-- the first value in a tuple, e.g.:
--    
--    ghci> fmap (+3) (2,1)
--    (5,1)
--
-- Doing this is non trivial, as only type constructors that take one parameter can be made 
-- instances of Functor, and tuples obviously take two parameters. To get around this, we 
-- use `newType` on our tupe in such a way that the second type parameter represents the type
-- of the first component in the tuple:
newtype Pair b a = Pair { getPair :: (a,b) }

instance Functor (Pair c) where
   fmap f (Pair (x,y)) = Pair (f x, y)

-- By making the second parameter represent the the type of the first component, we are able
-- to craft a grammatical instance of Functor. It works as expected:
--
--    ghci> getPair $ fmap (*100) (Pair (2,3))
--    (200,3)
--
-- }}}
--
-- {{{ On newtype laziness
--
-- We mentioned earlier that `newtype` is faster than `data`. Because `newtype` can only really
-- do one thing (turn an existing type into a new type), Haskell can represent the values of types
-- defined with `newtype` just like the original ones, though it has to note that a distinction
-- exists. This is what frequently makes `newtype` faster, and it also means newtype is lazy.
--
--    Recall what we mean when we say "lazy". Laziness re: functional programming means that only
-- when we actually print the result of our functions will any computation take place. Furthermore,
-- only the computations that are necessary for the function to execute will be carried out. In other
-- words, functions in Haskell avoid doing work unless they absolutely have to. The `undefined` value
-- in Haskell represents an erronous computation. If we try to evaluate it by printing to the terminal,
-- Haskell throws an exception:
--
--    ghci> undefined
--    *** Exception: Prelude.undefined
--
-- However, if we make a list that has undefined values in it but request the head of the list,
-- everything will be fine since Haskell doesn't nedd to evaluate any of the later elements in
-- the list:
--
--    ghci> head [3,4,5,undefined,2,undefined]
--    3
--
--    Consider the following type:
data CoolBool = CoolBool { getCoolBool :: Bool }

-- CoolBool is a standard algebraic data type w/ one value constructur, which has a filed of type
-- `Bool`. Let's make a function that pattern matches a `CoolBool` and returns the value "hello", 
-- regardless of what the `Bool` inside `CoolBool` is:
helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"
--
-- Instead of applying this function to a normal `CoolBool`, let's apply it to `undefined`:
--
--    ghci> helloMe undefined
--    "*** Exception: Prelude.undefined
--
-- Why do we encounter an exception? As discussed earlier, types defined with the `data` keyword
-- may have multiple value constructors (even though CoolBool only has one). So in order to see
-- if the value given to our function conforms to the `CoolBool _` pattern, Haskell must evaluate
-- the value just enough to see which constructor was used when we made the value. And evaluating
-- an `undefined` value always leads to an exception.
--
--    Now, we may take advantage of newtype's laziness. Defined a `CoolBool'`, but this time,
-- use `newtype` rather than `data`
newtype CoolBool' = CoolBool' {getCoolBool' :: Bool }

helloMe' :: CoolBool' -> String
helloMe' (CoolBool' _) = "hello"

-- Now, Haskell will not throw an exception when attempting to apply `helloMe'` to `undefined`:
--
--    ghci> helloMe' undefined
--    "hello"
--
-- }}}
--
-- {{{ `type` vs `newtype` vs `data`
--
-- This section is mostly review, but the last paragraph neatly summarizes the differences between
-- the three keywords:
--
--    * `data`: used for making your own data types. These can have as many constructors and fields
--              as you wish, and can be used to implement any algebraic data type.
--
--    * `type`: used for when you want your type signature to look cleaner and more descriptive.
--
--    * `newtype`: used to take an existing type and wrap it in a new type in order to make it an instance
--                 of a type class. Basically `data`, but can only have one constructor and one field*
--
-- * Even though from the programmer's perspective, this viewpoint is sufficient, it's good to keep in
-- mind that `data` and `newtype` work differently. As covered in the earlier section, the laziness
-- of newtype will sometimes lead to different evaluation schemes.
--
-- }}}
