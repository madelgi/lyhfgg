import qualified Data.Foldable as F
import Data.Monoid

-- Just like we can fold over lists, we can use monoids to assist us in
-- folding over almost any data structure. Because there are so many data
-- structures that play nice with folding, the `Foldable` type class was
-- introduced -- it is for things that can be folded up. Let's compare the
-- versiond of `foldr` in this module with the one in the standard library:
--
--    ghci> :t foldr
--    :: (a -> b -> b) -> b -> [a] -> b
--    ghci> :t F.foldr
--    :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b
--
-- So it's exactly the same, although now we aren't restrict to lists! Obviously,
-- anything that can be foldr'd can also be F.foldr'd (but not vice versa). Let's
-- see `F.foldr` in action:
--
--    ghci> F.foldl (+) 2 (Just 9)
--    Just 11
--    ghci> F.foldr (||) False (Just True)
--    Just True
--
-- Let's try to use this new expansion of fold(l || r) over a more interesting
-- data structure than Maybe.
--
--    Recall the tree data structure from chapter 7:
data Tree a = EmptyTree | Node a (Tree a) (Tree a)
   deriving (Show)

-- We can make the tree an instance of Foldable, allowing us to fold functions
-- over it. One way to do so is to directly implement `foldr`, which can be
-- tricky. A much easier way is to implement foldMap, which has the following
-- type signature:
--
--    foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
--
-- This function may seem somewhat mysterious, but let's make tree an instance
-- of Foldable to see how one might implement foldMap:
instance F.Foldable Tree where
   foldMap f EmptyTree = mempty
   foldMap f (Node x l r) = F.foldMap f l `mappend`
                            f x           `mappend`
                            F.foldMap f r

-- That's pretty easy! We simply recursively apply foldMap to each subtree, apply 
-- the function the node value, and then `mappend` these together in the obvious
-- manner. Now that tree is an instance of Foldable, let's see what it does to 
-- a test subtree:
--
-- visual:                5
--                      /  \
--                     /    \
--                    /      \
--                   3        9
--                  / \      / \
--                 1   6    8   10
testTree = Node 5
            (Node 3
               (Node 1 EmptyTree EmptyTree)
               (Node 6 EmptyTree EmptyTree)
            )
            (Node 9
               (Node 8 EmptyTree EmptyTree)
               (Node 10 EmptyTree EmptyTree)
            )

-- With a Foldable instance, we can do all of the folds that we could normally apply
-- to lists:
--
--    ghci> F.foldl (+) 0 testTree
--    42
--    ghci> F.foldl (*) 1 testTree
--    64800
--    ghci> getAny $ F.foldMap (\x -> Any $ x == 3) testTree
--    True
--    ghci> F.foldMap (\x -> [x]) testTree
--    [1,3,6,5,8,9,10]
