-- Inf2d Assignment 1 2018-2019
-- Matriculation number:
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy)
import Debug.Trace
import TTTGame

gridLength_search::Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6



{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the  13th March 2018 at 3pm.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See the README for description of a user interface to test your code.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]

badNodesList::[Node]
-- This is your list of bad nodes. You should experimet with it to make sure your algorithm covers different cases.
badNodesList = []   --[(2,2),(1,2),(3,1)]

-- The maximum depth this search can reach
-- TODO: Fill in the maximum depth and justify your choice
maxDepth::Int
maxDepth= (gridLength_search * gridWidth_search) - length badNodesList
-- Why did you choose this number?
-- I chose this number because it represents the number of all the free cells. As a result, if there is a path, it has to be maximum this long.


-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.


next :: Branch -> [Branch]
next [] = []
next branch = [(x + d, y) : branch| d <- [-1,1], x+d>=1, x+d <=6, ((x+d, y) `elem` branch) == False, ((x+d,y) `elem` badNodesList) == False] ++
  [(x, y + d) : branch| d <- [-1,1], y+d>=1, y+d<=6, ((x, y+d) `elem` branch) == False, ((x,y+d) `elem` badNodesList) == False]
   where (x,y) = head branch



-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
 -- Note that this is the right type declaration for this function. You might have an old version of the Assignment PDF that names this wrongly.
checkArrival::Node -> Node -> Bool
checkArrival (d1,d2) (c1,c2) = d1 == c1 && d2 == c2


-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.

breadthFirstSearch::Node->(Branch -> [Branch])->[Branch]->[Node]->Maybe Branch

breadthFirstSearch destination next [] exploredList = Nothing
breadthFirstSearch destination next (branch:branches) exploredList
  |checkArrival destination (head branch) = Just branch
  |otherwise = if (head branch) `elem` exploredList then breadthFirstSearch destination next (branches) (exploredList)
     else breadthFirstSearch destination next (branches ++ (next branch)) (head(branch):exploredList)



-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.
depthFirstSearch::Node->(Branch -> [Branch])->[Branch]-> [Node]-> Maybe Branch
depthFirstSearch destination next [] exploredList = Nothing
depthFirstSearch destination next (branch:branches) exploredList
  |checkArrival destination (head branch) = Just branch
  |otherwise = if (head branch) `elem` exploredList then depthFirstSearch destination next (branches) (exploredList)
     else depthFirstSearch destination next ((next branch) ++ branches) (head(branch):exploredList)

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
depthLimitedSearch::Node->(Branch -> [Branch])->[Branch]-> Int-> Maybe Branch
depthLimitedSearch destination next [] d  = Nothing
depthLimitedSearch destination next (branch:branches) d
  |checkArrival destination (head branch) = Just branch
  |otherwise = if (length branch) > d then depthLimitedSearch destination next (branches) d else depthLimitedSearch destination next ((next branch) ++ branches) d


-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
-- Each time a solution is not found the depth should be increased.
iterDeepSearch:: Node-> (Branch -> [Branch])->Node -> Int-> Maybe Branch
iterDeepSearch destination next initialNode d
   | d > maxDepth = Nothing
   | answer_branch == Nothing = iterDeepSearch destination next initialNode (d+1) -- if no solution is found, increment depth limit
   | otherwise = answer_branch
      where answer_branch = depthLimitedSearch destination next [[initialNode]] d

-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the 'position' point and the 'destination'.

manhattan::Node->Node->Int
manhattan (p1,p2) (d1,d2) = abs(p1 - d1) + abs(p2 - d2)

-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

bestFirstSearch::Node->(Branch -> [Branch])->(Node->Int)->[Branch]-> [Node]-> Maybe Branch
bestFirstSearch destination next manhattan [] exploredList = Nothing
bestFirstSearch destination next manhattan (branch:branches) exploredList
  |checkArrival destination (head branch) = Just branch
  |otherwise = if ((head branch) `elem` exploredList) then bestFirstSearch destination next manhattan (sortBy (compareBranchesBFS destination) [b | b <- branches] ) (exploredList)
  else bestFirstSearch destination next manhattan (sortBy (compareBranchesBFS destination) [b | b <- (branches ++ (next branch))] ) (head(branch):exploredList)



-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

aStarSearch::Node->(Branch -> [Branch])->(Node->Int)->(Branch ->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch destination next manhattan cost [] exploredList = Nothing
aStarSearch destination next manhattan cost (branch:branches) exploredList
  |checkArrival destination (head branch) = Just branch
  |otherwise = if ((head branch) `elem` exploredList) then aStarSearch destination next manhattan cost (sortBy (compareBranchesAStar destination) [b | b <- branches] ) (exploredList)
  else aStarSearch destination next manhattan cost (sortBy (compareBranchesAStar destination) [b | b <- (branches ++ (next branch))] ) (head(branch):exploredList)



-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch  -> Int
cost branch = length branch


-- | Section 5: Games
-- See TTTGame.hs for more detail on the functions you will need to implement for both games' minimax and alphabeta searches.



-- | Section 5.1 Tic Tac Toe


-- | The eval function should be used to get the value of a terminal state.
-- A positive value (+1) is good for max player. The human player will be max.
-- A negative value (-1) is good for min player. The computer will be min.
-- A value 0 represents a draw.

eval :: Game -> Int
-- simply checks if player 1 has won, and if so returns 1, else check for player 0 and if so returns -1, else returns 0 as draw
eval game
 | checkWin game 1 = 1 -- human(maxPlayer) won
 | checkWin game 0 = -1 -- computer(minPlayer) won
 | otherwise = 0

-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.

minimax:: Game->Player->Int
minimax game player
 | terminal game = eval game -- terminal state
 | otherwise = if maxPlayer player
                then getMax (moves game player) (-1) player -- verify all actions and choose the highest score
                else getMin (moves game player) 1 player -- verify all actions and choose the lowest score
  where getMax availMoves bestScore player =
           if (null availMoves)
              then bestScore
                 else if ((minimax (head availMoves) (switch(player))) > bestScore)
                    then getMax (tail availMoves) (minimax (head availMoves) (switch(player))) player
                    else getMax (tail availMoves) bestScore player
        getMin availMoves bestScore player =
           if (null availMoves)
              then bestScore
                 else if ((minimax (head availMoves) (switch(player))) < bestScore)
                    then getMin (tail availMoves) (minimax (head availMoves) (switch(player))) player
                    else getMin (tail availMoves) bestScore player



-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state.

alphabeta:: Game->Player->Int
alphabeta game player
  | terminal game = eval game
  | otherwise = if maxPlayer player
                 then getMax_ab player game (-2) 2
                 else getMin_ab player game (-2) 2



-- | Section 5.2 Wild Tic Tac Toe



-- | The evalWild function should be used to get the value of a terminal state.
-- It should return 1 if either of the move types is in the correct winning position.
-- A value 0 represents a draw.

evalWild :: Game -> Int
-- simply gives the player who reached(!) the terminal state +1  if either the x's or the o's are in the correct position.
evalWild game =undefined



-- | The alphabetaWild function should return the minimax value using alphabeta pruning.
-- The evalWild function should be used to get the value of a terminal state. Note that this will now always return 1 for any player who reached the terminal state.
-- You will have to modify this output depending on the player. If a move by the max player sent(!) the game into a terminal state you should give a +1 reward.
-- If the min player sent the game into a terminal state you should give -1 reward.

alphabetaWild:: Game->Player->Int
alphabetaWild game player =undefined



-- | End of official assignment. However, if you want to also implement the minimax function to work for Wild Tic Tac Toe you can have a go at it here. This is NOT graded.


-- | The minimaxWild function should return the minimax value of the state (without alphabeta pruning).
-- The evalWild function should be used to get the value of a terminal state.

minimaxWild:: Game->Player->Int
minimaxWild game player =undefined



			-- | Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms here.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores

compareBranchesBFS:: Node -> Branch -> Branch -> Ordering             --this function comapres branches using the manhattan distance between the position point and the destiantion(for bestFirst search)
compareBranchesBFS destination br1 br2
  |manhattan (head br1) destination <= manhattan (head br2) destination = LT
  |otherwise = GT

compareBranchesAStar:: Node -> Branch -> Branch -> Ordering           --this function compares branches regarding both their manhattan distances and cost(for AStar search)
compareBranchesAStar destination br1 br2
  |manhattan (head br1) destination + cost br1 <= manhattan (head br2) destination + cost br2 = LT
  |otherwise = GT


--these are all helper functions for the alphabeta function
--maxRecord and minRecord go through all possible ways of the search tree choosing the maximum and minimum respectively

maxRecord :: Player -> [Game] -> Int -> Int -> Int -> Int
maxRecord p games a b x
   | null games = x
   | x' >= b = x'
   | otherwise = maxRecord p (tail games) a' b x'
       where x' = max x (getMin_ab (switch p) (head games) a b)
             a' = max a x'

minRecord :: Player -> [Game] -> Int -> Int -> Int -> Int
minRecord p games a b x
   | null games = x
   | x' <= a = x'
   | otherwise = minRecord p (tail games) a b' x'
       where x' = min x (getMax_ab (switch p) (head games) a b)
             b' = min b x'

getMin_ab :: Player -> Game -> Int -> Int -> Int
getMin_ab p game a b
   | terminal game = eval game
   | otherwise = minRecord p (moves game p) a b 2

getMax_ab :: Player -> Game -> Int -> Int -> Int
getMax_ab p game a b
   | terminal game = eval game
   | otherwise = maxRecord p (moves game p) a b (-2)
