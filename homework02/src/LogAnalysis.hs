{-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 02
--
----------------------------------------------------------------------

module LogAnalysis where

import Log

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> parseMessage "E 2 562 help help"
-- LogMessage (Error 2) 562 "help help"
-- >>> parseMessage "I 29 la la la"
-- LogMessage Info 29 "la la la"
-- >>> parseMessage "This is not in the right format"
-- Unknown "This is not in the right format"


parseMessage :: String -> LogMessage
parseMessage message = case words message of
  ("I" : time : xs) -> LogMessage Info (read time) (unwords xs)
  ("W" : time : xs) -> LogMessage Warning (read time) (unwords xs)
  ("E" : n : time : xs) -> LogMessage (Error $ read n) (read time) (unwords xs)
  msg -> Unknown (unwords msg)

parse :: String -> [LogMessage]
parse message = map parseMessage $ lines message

-- testParse parse 10 "src/error.log"

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- |
--
-- >>> insert (LogMessage (Error 2) 562 "help help") (Node Leaf (LogMessage (Error 2) 62 "help help") Leaf)
-- Node Leaf (LogMessage (Error 2) 62 "help help") (Node Leaf (LogMessage (Error 2) 562 "help help") Leaf)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTree = msgTree
insert logMsg Leaf = Node Leaf logMsg Leaf
insert logMsg (Node msgTi logM msgTd)
    | timeStamp logMsg <= timeStamp logM  = Node (insert logMsg msgTi) logM msgTd
    | otherwise = Node msgTi logM (insert logMsg msgTd)
   where timeStamp (LogMessage _ t _ ) = t
         timeStamp _ = 0


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> build [LogMessage (Error 2) 1 "help help", LogMessage (Error 2) 2 "help help"]
-- Node (Node Leaf (LogMessage (Error 2) 1 "help help") Leaf) (LogMessage (Error 2) 2 "help help") Leaf

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

{-build logMsg = foldr insert Leaf logMsg-}
{-build [] = Leaf
build  (logM: logMsg) = insert logM $ build logMsg-}

----------------------------------------------------------------------
-- Exercise 4
-------------------------------------------------------------------F i1) =---
-- |
--
-- >>> inOrder (Node (Node (Node Leaf (LogMessage (Error 2) 1 "help help") Leaf) (LogMessage (Error 2) 2 "help help") Leaf) (LogMessage (Error 2) 5 "help help") Leaf)
-- [LogMessage (Error 2) 1 "help help",LogMessage (Error 2) 2 "help help",LogMessage (Error 2) 5 "help help"]

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node msgTi logM msgTd) = inOrder msgTi ++ [logM] ++ inOrder msgTd

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

-- |
--
-- >>>
--

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong  = filterWantWWrong . inOrder . build 

filterWantWWrong :: [LogMessage] -> [String]
filterWantWWrong [] = []
filterWantWWrong (Unknown _ : _ ) = [] 
filterWantWWrong (LogMessage msgType _ msg :logMsg)
   | several msgType = msg : filterWantWWrong logMsg
   | otherwise = filterWantWWrong logMsg
  where several (Error n) = n >= 50
        several _ = False

{-whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong (logM :logMsg)
   | several logM = msg logM: whatWentWrong logMsg
   | otherwise = whatWentWrong logMsg
  where several (LogMessage (Error n) _ _ ) = n >= 50
        several _ = False
        msg (LogMessage _ _ m) = m
        msg _ = ""-}

--testWhatWentWrong parse whatWentWrong "src/sample.log"

{-E 70 3 Way too many pickles
E 65 1 Bad pickle-flange interaction detected
E 20 2 Too many pickles
E 99 10 Flange failed!-}

----------------------------------------------------------------------
-- Exercise 6 (Optional)
----------------------------------------------------------------------

{-whoDidIt :: String
whoDidIt = undefined-}
