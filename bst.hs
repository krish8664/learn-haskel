module DataTypes where
import Data.Maybe

---- BST Implementation

data BST = EmptyNode | Node {left::BST, value::Int, right::BST} deriving (Show)

inOrder :: BST -> [Int]
inOrder EmptyNode    = []
inOrder (Node l v r) = inOrder l ++ [v] ++ inOrder r

find :: BST -> Int -> Bool
find EmptyNode _ = False
find (Node l v r) x
  | x == v    = True
  | x > v     = find r x
  | otherwise = find l x

insert :: BST -> Int -> BST
insert EmptyNode x = Node EmptyNode x EmptyNode
insert node@(Node l v r) x
  | v > x     = Node (insert l x) v r
  | v < x     = Node l v (insert r x)
  | otherwise = node


fromList :: [Int] -> BST
fromList [] = EmptyNode
fromList (first : rest) = insert (fromList rest) first

------ Tree Map implementation

data TreeMap k v = EmptyMap | TMap (TreeMap k v) k v (TreeMap k v) deriving (Show)

insertTM :: Ord k => TreeMap k v -> k -> v -> TreeMap k v
insertTM EmptyMap nk nv = TMap EmptyMap nk nv EmptyMap
insertTM tm@(TMap l k v r) nk nv
  | k == nk = TMap l k nv r
  | nk < k = TMap (insertTM l nk nv) k v r
  | nk > k = TMap l k v (insertTM r nk nv)
  | otherwise = tm

get :: Ord k => TreeMap k v -> k -> Maybe v
get EmptyMap _ = Nothing
get (TMap l k v r) fk
  | k == fk = Just v
  | fk < k = get l fk
  | otherwise = get r fk

fromListTM :: Ord k => [(k, v)] -> TreeMap k v
fromListTM [] = EmptyMap
fromListTM ((k, v) : rest) = insertTM (fromListTM rest) k v


prop_testinsert :: Ord k => [(k,v)] -> k -> v -> Bool
prop_testinsert list k v = let map' = fromListTM list in if isJust (get (insertTM map' k v) k) then True else False
