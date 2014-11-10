-- {{{ Introduction
--
-- In the previous section, we saw how fmap takes a function and a functor,
-- and then maps the function over the contents of the functor. What if we
-- have a a functor that contains a function and another functor, e.g.
-- Just (* 3) and Just 5. Is there a way to apply these two together to create
-- (ostensibly) Just 15? Not with fmap. Fmap doesn't work that way, so stop
-- trying to make it work that way. sry. 
--
--    Oh but you could look at applicative functors, which have the following 
-- two methods:
--
--    class (Functor f) => Applicative f where
--       pure :: a -> f a
--       (<*>) :: f (a -> b) -> f a -> f b
--
-- The <*> exists to solve the problem brought up in the previous paragraph. The
-- `pure` method wraps an `a` type in some context. We can examine a sample instance
-- of this class, to better wrap our heads around it:
--
--    instance Applicative Maybe where
--       pure = Just
--       Nothing <*> _ = Nothing
--       (Just f) <*> something = fmap f something
--
-- The above definition matches the general class defined above. `pure` takes an `a` and
-- wraps it in `Just`. The <*> operation does nothing if `Nothing` is one of the inputs. 
-- If (Just f) is the input, it rips the `f` out of its context and then applies it to
-- `something`.
--
-- }}}
--
-- {{{ IO as Applicative
--
-- IO is also an instance of an Applicative functor:
--
--    instance Applicative IO where
--       pure = return
--       a <*> b = do f <- a
--                    x <- b
--                    return (f x)
-- 
-- 
myAction :: IO String
myAction = do
   a <- getLine
   b <- getLine
   return $ a ++ b

myAction' :: IO String
myAction' = (++) <$> getLine <*> getLine
--
--
-- }}}
