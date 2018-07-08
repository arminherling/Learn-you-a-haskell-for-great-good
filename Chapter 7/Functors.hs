class Functor' f where
    fmap :: (a -> b) -> f a -> f b

instance Functor' [] where
    fmap = map

instance Functor' Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
