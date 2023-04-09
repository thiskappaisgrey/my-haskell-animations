{-# LANGUAGE FlexibleContexts #-}
module Main where
import Diagrams.Prelude hiding (Empty)
import Diagrams.TwoD.Layout.Tree
-- import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.Rasterific.CmdLine
-- import Control.Lens ((.~))
-- import Data.Active
import Data.Maybe (fromJust)
import Data.Tree (Tree)
 --       0                   ()  
 --     /   \               /    \
 --    1     4     =>      ()    ()    
 --   / \   / \           /  \   / \
 --  2   3  X X          ()  ()  X X
 -- / \ / \             / \  / \
 -- X X X X             X X  X X
empty :: BTree String
empty = leaf "X"
exampleTree :: BTree String
exampleTree = BNode "0" (BNode "1" (BNode "2" empty empty) (BNode "3" empty empty)) (BNode "4" empty empty)

Just t = symmLayoutBin exampleTree
-- myCircle :: Diagram B
-- myCircle = circle 1
exampleTreeDiagram :: Diagram B
exampleTreeDiagram = named (1 :: Int) $
  renderTree (\n ->  text n # fontSizeL 0.2 <> circle 0.2 # fc gold)
  (~~) t 
  
exampleTreeDiagramVoid :: Diagram B
exampleTreeDiagramVoid = named (2 :: Int) $
  renderTree (\n ->  text (if n /= "X" then "()" else "X") # fontSizeL 0.2 <> circle 0.2 # fc gold)
  (~~) t 
-- connectOutside' (with
--                                                 & gaps       .~ small
--                                                 & headLength .~ local 0.15) (1 :: Int) (2 :: Int)
diagram :: Diagram B
diagram = bg white . pad 1.1 . centerXY  $  exampleTreeDiagram  ||| myarrow ||| exampleTreeDiagramVoid 


sPt = p2 (0.5, -1.8)
ePt = p2 (2.0, -1.8)
-- We use small blue and red circles to mark the start and end points.
  
myarrow :: Diagram B  
myarrow =  arrowBetween' (with & headLength .~ veryLarge) sPt ePt



    --     ()
    --   //   \
    --  ()     ()
    -- // \   // \
    -- X   X  X  ()
    --          // \
    --          X   X
-- nfa tree
data MColor = Red | Yellow deriving (Show)
data MNode = MNode {
  d :: String,
  el :: String,
  color :: MColor
                 } deriving (Show)

-- 
data Dir = MLeft | MRight
type Thread = [Dir]   -- I don't need a full zipper b/c I don't need to backtrack


  
empty1 :: String -> BTree MNode
empty1 l = leaf (MNode "X" l Yellow) 
  
nfaTree :: BTree MNode
nfaTree = BNode (MNode "()" "r" Yellow) (BNode (MNode "()" "O" Yellow) (empty1 "O") (empty1 "I")) (BNode (MNode "()" "I" Yellow) (empty1 "O") (BNode (MNode "()" "I" Yellow) (empty1 "O") (empty1 "I")))

nfaTree' = highlightNodeNfaTree [MRight,  MRight] nfaTree

Just t1 = symmLayoutBin nfaTree'

highlightNodeNfaTree :: [Dir] -> BTree MNode -> BTree MNode
highlightNodeNfaTree _ Empty  = Empty
highlightNodeNfaTree (MLeft:l) (BNode (MNode d el _) lb rb) = BNode (MNode d el Yellow) (highlightNodeNfaTree l lb) rb
highlightNodeNfaTree (MRight:l) (BNode (MNode d el _) lb rb) = BNode (MNode d el Yellow) lb (highlightNodeNfaTree l rb)
highlightNodeNfaTree [] (BNode (MNode d el _) lb rb) = BNode (MNode d el Red) lb rb
  

  
mcolor :: (Ord a, Floating a) => MColor -> Colour a
mcolor Yellow = gold
mcolor Red = pink 


nfaTreeDiagram :: Diagram B
nfaTreeDiagram = named (1 :: Int) . font "monospace" . pad 1.1 . centerXY $
  renderTree' (\(MNode v _ c) ->  text v # fontSizeL 0.2 <> circle 0.2 # fc (mcolor c))
   (\((MNode _ _ _), a) ((MNode _ l2 _), b) ->
      let
         (x1, y1) = unp2 a
         (x2, y2) = unp2 b
      in
         (a ~~ b) <> alignedText 1 0 l2 # fontSizeL 0.1 # moveTo (p2 ((x1+x2)/2 - 0.1 , (y1+y2)/2)) 
   ) t1
-- nfaTreeDiagramF :: (Ord n0, Floating n0, Renderable n0) => Tree (MNode, P2 n0) -> QDiagram b0 V2 n0 Any

-- TODO attempt to animate the tree.. on inputs II
-- and IIO
-- mod <$> (floor <$> interval 0 100) <*> pure 7 - cycles numbers between 0 and 6
ii :: Active Thread
ii = discrete [[], [MRight], [MRight, MRight]]

-- f = (fromJust  . symmLayoutBin)
-- make the animation
-- the anim main kinda sucks..  
anim :: Animation B (V B) (N B)
anim = (pure nfaTreeDiagramF) <*> treeWithPos
  where
    tree = ((pure highlightNodeNfaTree) <*> ii <*> pure nfaTree)
    treeWithPos = (pure (fromJust . symmLayoutBin)) <*> tree
  
nfaTreeDiagramF t = bg white . font "monospace" . pad 1.1 . centerXY $
  renderTree' (\(MNode v _ c) -> text v # fontSizeL 0.2 <> circle 0.2 # fc (mcolor c))
   (\((MNode _ _ _), a) ((MNode _ l2 _), b) ->
      let
         (x1, y1) = unp2 a
         (x2, y2) = unp2 b
      in
         (a ~~ b) <> alignedText 1 0 l2 # fontSizeL 0.1 # moveTo (p2 ((x1+x2)/2 - 0.1 , (y1+y2)/2)) 
   ) t
-- II
animGif :: [(Diagram Rasterific, Int)]
animGif = let
  l = map (nfaTreeDiagramF . (fromJust . symmLayoutBin) . (flip highlightNodeNfaTree $ nfaTree))  [[], [MRight], [MRight, MRight]]
  in
  zip l [100, 100, 500]
-- OO - reject 
animGif1 :: [(Diagram Rasterific, Int)]
animGif1 = let
  l = map (nfaTreeDiagramF . (fromJust . symmLayoutBin) . (flip highlightNodeNfaTree $ nfaTree))  [[], [MLeft], [MLeft, MLeft]]
  in
  zip l [100, 100, 500]

main :: IO ()
main = gifMain animGif1
