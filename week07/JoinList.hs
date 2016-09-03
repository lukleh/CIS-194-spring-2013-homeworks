import Data.Monoid ((<>))
import Sized (Sized(..), getSize)

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

newtype Product a = Product a
  deriving (Eq, Ord, Num, Show)

getProduct :: Product a -> a
getProduct (Product a) = a

instance Num a => Monoid (Product a) where
  mempty  = Product 1
  mappend = (*)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a)
  | i == 0 = Just a
  | otherwise = Nothing
indexJ i (Append n jl1 jl2)
  | i > getSize n = Nothing
  | i < size0 = indexJ i jl1
  | otherwise = indexJ (i - size0) jl2
    where size0 = getSize . tag $ jl1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 jl = jl
dropj i (Append n jl1 jl2)
  | i > getSize n = Empty
  | i < size0 = dropJ i jl1 +++ jl2
  | otherwise = dropj (i - size0) jl2
    where size0 = getSize . tag $ jl1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ _ s@(Single _ _) = s
takeJ i a@(Append n jl1 jl2)
  | i >= getSize n = a
  | i <= size0 = takeJ i jl1
  | otherwise = jl1 +++ takeJ (i - size0) jl2
    where size0 = getSize . tag $ jl1
