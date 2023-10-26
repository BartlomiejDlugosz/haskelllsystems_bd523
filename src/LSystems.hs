module LSystems ( LSystem(LSystem), ColouredLine, Command(..)
                , angle, axiom, rules, lookupChar
                , expandOne, expand, move, parse, trace1, trace2
                , expandLSystem, commandMap ) where

import IC.Colour

type Rules a = [(Char, [a])]
data LSystem = LSystem {
        angle :: Float,
        axiom :: [Char],
        rules :: Rules Char
}
type Vertex = (Float, Float)
type TurtleState = (Vertex, Float)
data Command = F | L | R | B [Command] deriving (Show)
type ColouredLine = (Vertex, Vertex, Colour)

----------------------------------------------------------
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
-- angle :: LSystem -> Float
-- angle (LSystem angle' _ _) = angle'

-- -- Returns the axiom string for the given system.
-- axiom :: LSystem -> [Char]
-- axiom (LSystem _ axiom' _) = axiom'

-- -- Returns the set of rules for the given system.
-- rules :: LSystem -> Rules Char
-- rules (LSystem _ _ rules') = rules'

--
-- Pre: the character has a binding in the Rules list
--
lookupChar :: Rules a -> Char -> [a]
lookupChar r x = y
        where
                (_, y) = head [y | y <- r, fst y == x]

--
-- Expand command string s once using rule table r
--
expandOne :: Rules Char -> [Char] -> [Char]
expandOne _ [] = []
expandOne r (x:xs) = lookupChar r x ++ expandOne r xs

--
-- Expand command string s n times using rule table r
--
expand :: [Char] -> Int -> Rules Char -> [Char]
expand s 0 _ = s
expand s n r = expand exp (n - 1) r
        where
                exp = expandOne r s

-- Move a turtle.
--
-- F moves distance 1 in the current direction.
-- L rotates left according to the given angle.
-- R rotates right according to the given angle.
move :: Command -> Float -> TurtleState -> TurtleState
move F a ((x, y), angle) = ((x + cos rad, y + sin rad), angle)
        where
                rad = pi*(angle/180)
move L a (vertex, angle) = (vertex, angle + a)
move R a (vertex, angle) = (vertex, angle - a)
move _ _ state = state

getBrackets :: [Char] -> Int -> ([Char], [Char])
getBrackets (']':xs) 0 = ("", xs)
getBrackets (x:xs) n = (x:a, b)
        where
                (a, b) = getBrackets xs y
                y       | x == '[' = n + 1
                        | x == ']' = n - 1
                        | otherwise = n
                
parse :: Rules Command -> [Char] -> [Command]
parse _ [] = []
parse r (x:xs) 
        | x == '[' = B (parse r a) : parse r b
        | otherwise = lookupChar r x ++ parse r xs
        where
                (a, b) = getBrackets xs 0
                


trace1 :: [Command] -> Float -> Colour -> [ColouredLine]
trace1 [] _ _ = []
trace1 c f col = scanner ((0,0),90) c
        where
                scanner :: TurtleState -> [Command] -> [ColouredLine]
                scanner _ [] = []

                scanner initial ((B x):xs) = scanner initial x ++ scanner initial xs

                scanner initial@(vertex1, _) (x:xs)
                        | vertex1 == vertex2 = scanner state xs
                        | otherwise = (vertex1, vertex2, col) : scanner state xs
                        where
                                state@(vertex2, _) = move x f initial


-- This version uses an explicit stack of residual commands and turtle states
trace2 :: [Command] -> Float -> Colour -> [ColouredLine]
trace2 c f col = lines
        where
                (lines, _) = stackManager (executeCommands ((0,0),90) c)

                executeCommands :: TurtleState -> [Command] -> ([ColouredLine], [(TurtleState, [Command])])
                executeCommands _ [] = ([], [])

                executeCommands state ((B c):cs) = (first,(state, c) : second)
                        where
                                (first, second) = executeCommands state cs

                executeCommands state@(vertex1, _) (c:cs)
                        | vertex1 == vertex2 = (first, second)
                        | otherwise = ((vertex1, vertex2, col) : first, second)
                        where
                                s@(vertex2, _) = move c f state
                                (first, second) = executeCommands s cs

                stackManager :: ([ColouredLine], [(TurtleState, [Command])]) -> ([ColouredLine], [(TurtleState, [Command])])
                stackManager (lines, []) = (lines, [])
                stackManager (lines, (state, commands):xs) = stackManager (lines ++ lines2, xs ++ stack)
                        where
                                (lines2, stack) = executeCommands state commands

-- Provided Functions
------------------------------------------------------------------------------

expandLSystem :: LSystem -> Int -> [Command]
expandLSystem (LSystem _ axiom rs) n = parse commandMap (expand axiom n rs)

commandMap :: Rules Command
commandMap = [ ('M', [F])
             , ('N', [F])
             , ('X', [])
             , ('Y', [])
             , ('A', [])
             , ('+', [L])
             , ('-', [R])
             ]
