{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s =
  case words s of
  ("E":y:z:xs) -> LogMessage (Error (read y :: Int)) (read z :: TimeStamp) (unwords xs)
  ("I":y:xs) -> LogMessage Info (read y :: TimeStamp) (unwords xs)
  ("W":y:xs) -> LogMessage Warning (read y :: TimeStamp) (unwords xs)
  _ -> Unknown s


parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s


insert :: LogMessage -> MessageTree -> MessageTree
insert lmsg Leaf = Node Leaf lmsg Leaf
insert lmsg@(LogMessage _ ts _) (Node mtl lmsg'@(LogMessage _ ts' _) mtr)
  | ts < ts' = Node (insert lmsg mtl) lmsg' mtr
  | otherwise = Node mtl lmsg (insert lmsg  mtr)
insert (Unknown _) mt = mt


build :: [LogMessage] -> MessageTree
build lmsgs = build' lmsgs Leaf

build' :: [LogMessage] -> MessageTree -> MessageTree
build' [] mt = mt
build' (x:xs) mt = build' xs (insert x mt)

inOrder :: MessageTree -> [LogMessage]
inOrder mt = inOrder' mt []

inOrder' :: MessageTree -> [LogMessage] -> [LogMessage]
inOrder' Leaf xs = xs
inOrder' (Node mtl lmsg mtr) xs = inOrder' mtr (lmsg : (inOrder' mtl xs))


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lmsgs = whatWentWrong' lmsgs []

whatWentWrong' :: [LogMessage] -> [String] -> [String]
whatWentWrong' (LogMessage (Error sev) _ s : xs) out
  | sev >= 50 = whatWentWrong' xs (s : out)
  | otherwise = whatWentWrong' xs out
whatWentWrong' (_ : xs) out = whatWentWrong' xs out
whatWentWrong' [] out = out
