module Combinator (Consumer(..), anyC, optC, starC, plusC, valC) where

import Control.Applicative

-- Consumer ADT

newtype Consumer i o = Consumer { run :: i -> Maybe (o, i) }

instance Functor (Consumer i) where
    fmap f (Consumer l) =
        Consumer $ \input -> do
            (result, rest) <- l input
            Just (f result, rest)

instance Applicative (Consumer i) where
    pure x = Consumer $ \input -> Just (x, input)
    Consumer l1 <*> Consumer l2 =
        Consumer $ \input -> do
            (f, input') <- l1 input
            (x, input'') <- l2 input'
            Just (f x, input'')

instance Alternative (Consumer i) where
    empty = Consumer $ const Nothing
    Consumer l1 <|> Consumer l2 = Consumer $ \input -> l1 input <|> l2 input

anyC :: (a -> Consumer i o) -> [a] -> Consumer i o
anyC f xs = foldl1 (<|>) (map f xs)

optC :: Consumer i o -> Consumer i (Maybe o)
optC lexer@(Consumer l) = Consumer $ \input ->
    case l input of
        Just (x, input') -> Just (Just x, input)
        Nothing -> Just (Nothing, input)

starC :: Consumer i o -> Consumer i [o]
starC lexer@(Consumer l) = Consumer $ \input ->
    case l input of
        Just (x, input') -> run ((x:) <$> starC lexer) input'
        Nothing -> Just ([], input)

plusC :: Consumer i o -> Consumer i [o]
plusC lexer@(Consumer l) = Consumer $ \input -> do
   (first, input') <- l input
   (final, input'') <- run (starC lexer) input
   Just (final, input'')

valC :: (i -> Bool) -> Consumer [i] i
valC filter = Consumer f
    where
        f (y:ys)
            | filter y = Just (y, ys)
            | otherwise = Nothing
        f [] = Nothing


