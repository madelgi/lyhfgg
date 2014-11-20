-- {{{ Code, code, code
--
-- This file contains some sample code to illustrate the utility of monads.
-- We model a tightrope walker who continually has birds land on their balance
-- pole. If the difference in birds on each side of the pole differs by 4 or 
-- more, our tightrope walker loses their balance. Let's begin by defining
-- two type synonyms, for convenience:
type Birds = Int
type Pole = (Birds, Birds)


-- The `Pole` type synonym is a tuple, where the first entry represents birds
-- on the left side of the pole, and the second entry represents brids on the right.
-- Next, we define two functions that take a pole and Birds as input, and then tweak
-- the number of birds on the left / right side of the pole by the given number of
-- birds:
landLeft' :: Birds -> Pole -> Pole
landLeft' n (left, right) = (left + n, right)

landRight' :: Birds -> Pole -> Pole
landRight' n (left, right) = (left, right + n)

-- Recall the `-:` operator, allowing us to apply functions as postfixes:
--
--    x -: f = f x
--
-- This operator allows us to string our landing functions together:
--
--    ghci> (0,0) -: landLeft' 1 -: landRight' 4 -: landLeft' (-1) -: landRight' (-2) 
--    (0,2)
--
-- }}}
--
-- {{{ I'll Fly Away
--
-- While the final result is fine, if we evaluate this string of functions piecewise,
-- we see that our tightrope walker lost his balance somewhere in the middle:
--
--    (0,0) ----> (1,0) ----> (1,4) --x--> (0,4) -----> (0,2) //
--
-- At (0,4), the walker loses their balance. Let's rewrite landLeft' and landRight', so
-- they can track when this occurs:
landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
   | abs (right - (left + n)) > 3 = Nothing
   | otherwise                    = Just (left + n, right)

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
   | abs (left - (right + n)) > 3 = Nothing
   | otherwise                    = Just (left, right + n)

-- Our addition is simple: use guards to check if the descrepency between the sides
-- is too large. If it is, return Nothing; if not, return Just [Result]. We are
-- using the Maybe monad to track "errors" (balance loss). Now, as soon as the 
-- tightrope walker loses balance, Nothing will be returned. Note that
--
--    landleft n :: Pole -> Maybe Pole
--
-- That is, when we chain operations together, we may now use the monadic "bind"
-- operator. For example, our earlier example can be rewritten like so:
--
--    ghci> landLeft 1 (0,0) >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)
--    Nothing
--
-- Rather than returning (0,2), our new chain returns Nothing, indicating the walker
-- fell at some point. Let zoom-in to one portion of the chain, to verify we are
-- satisfying the type signature of >>= (m a -> (a -> m b) -> m b):
--
--    landLeft 1 (0, 0) >>= landRight 4
--
-- landLeft 1 (0, 0) = Just (1, 0), which has type Maybe Pole. Likewise, landRight 4 is a function,
-- that takes a pole as an input and returns a Maybe Pole. The whole expression evaluates
-- to Maybe Pole. In other words, the expression makes sense. And it makes sense to continue to
-- string these operations together:
--
--    Maybe Pole >>= (Pole -> Maybe Pole) >>= (Pole -> Maybe Pole) >>= ...
--                            Maybe Pole  >>= (Pole -> Maybe Pole) >>= ...
--                                                                     ...
--                                                                     ...
--                                                              Maybe Pole
--
-- The key is that `>>=` allows us to feed a monadic value to a function that takes a normal one,
-- as the above chain displays.
--
-- }}}
--
-- {{{ Banana on a Wire
--
-- In this section, we devise a function that ignores the current number of birds on the pole,
-- and forces the walker to slip regardless:
--
banana :: Pole -> Maybe Pole
banana _ = Nothing

-- The above functions take a `Pole`, and always returns `Nothing`. Now if we chain this together
-- with our landing functions, our walker will always fall:
--
--    ghci> return (0,0) >>= landLeft 1 >>= landRight 2 >>= banana >>= landLeft 1
--    Nothing
--
-- Were `banana` not in the above chain, it would return `Just (2,2)`. Rather than making a kinda
-- useless function that totally ignores its input, we can just use `>>`:
--
--    (>>) :: (Monad m) => m a -> m b -> m b
--    m >> n = m >>= (\_ -> n)
--
-- So, a more sensical implementation of a `banana` is just the following:
--
--    ghci> return (0,0) >>= landLeft 1 >>= landRight 2 >> Nothing >>= landLeft 1
--                                                      ^
--                                                      '--- previously, this was `>>= banana`
--
-- }}}
