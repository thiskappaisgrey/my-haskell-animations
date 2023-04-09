{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use bimap" #-}
module Main where

import Data.Text (Text, pack)
import Reanimate
import Reanimate.Animation
import Reanimate.Constants (screenHeight, screenWidth)
import Reanimate.Parameters (setFPS)
import Reanimate.Render (renderSvgs)
import System.Directory (createDirectory, doesDirectoryExist)
import System.Random

-- render animation for a simple AST like: 2 + 3 or 2 + 3 + 4 + 5

-- data Expr = Val Int | Add Expr Expr
type Point = (Double, Double)

data Expr a = Leaf a | Branch a (Expr a) (Expr a) deriving (Show)
data ExprPos a = LeafP Point a | BranchP Point a (ExprPos a) (ExprPos a) deriving (Show)

-- a "node" 
data ExprNode a = Node Point a


getPos :: ExprPos a -> Point
getPos (LeafP p _) = p
getPos (BranchP p _ _ _) = p

updatePos :: (Point -> Point) -> ExprPos a -> ExprPos a
updatePos f (LeafP p a) = LeafP (f p) a
updatePos f (BranchP p a b c) = BranchP (f p) a b c

changePos :: Point -> ExprPos a -> ExprPos a
changePos p (LeafP _ a) = LeafP p a
changePos p (BranchP _ a b c) = BranchP p a b c

class ToSVG a where
  toSVG :: a -> SVG

instance ToSVG Text where
  toSVG = mkText


myEnv :: Animation -> Animation
myEnv = mapA $ \svg -> mkGroup
  [ mkBackground "white"
  ,
    withFillOpacity 0 $
    withStrokeWidth 0.1 $
    withStrokeColor "black"
    (mkGroup [svg]) ]


main :: IO ()
main = reanimate $ myEnv (createTree `andThen` staticFrame 2 t)

t :: SVG
t = renderExprPosL (0, 3) $ exprToPosTree exTree

-- TODO draw the lines between the nodes to make a tree
createTree :: Animation
createTree =
  let
    textList = exprPosToList (2, 0) $ exprToPosText exTree
    treeList = exprPosToList (0, 3) $ exprToPosTree exTree
    l = zip textList treeList
    renderNode a (x, y) = translate x y $ toSVG a
    renderNode' (Node (x, y) a) = translate x y $ toSVG a
    tweenPts (x1, y1) (x2, y2) (x, y) time = (sigX1 time, sigX2 time)
      where
        sigX1 = fromToS x1 x2
        sigX2 = fromToS y1 y2
  in
        scene $ do
          -- basically, I need to repeat this for every node.
          varList <- mapM (\(x@(Node p1 n1), y@(Node p2 n2)) ->
                   do
                      v <- newVar p1
                      newSprite $ renderNode n1 <$> unVar v
                      return (v, x, y)
                      -- wait 1
                      -- -- TODO tween the variable instead
                      -- tweenVar v 2 (tweenPts p1 p2)
                      -- wait 1
                   ) l
          wait 1
          -- TODO I can draw a line between the points using a foldM probably
          mapM_ (\(v, Node p1 n1, Node p2 n2) ->
                   do
                     tweenVar v 1 (tweenPts p1 p2)
                     wait 1
                   ) varList
          wait 1
          

        

exTree :: Expr Text
exTree = Branch "-" (Branch "+" (Leaf "3") (Leaf "4")) (Leaf "5")


-- want to figure out position of the tree-frame actually
-- main :: IO ()
-- main = rawFrame $
--   mkGroup [
--   -- translate (-4) 0 $ mkText "3 + 4",
--   -- renderPosList  $ exprPosToList (-4, 0) $  exprToPosText exTree
--   -- renderExprPos (-4, 0) $ exprToPosText exTree
--    -- renderPosList  $ exprPosToList (2, 0) $ exprToPosTree exTree
--    -- withStrokeColor "grey" $ mkLine (0,0) (1,1)
--   renderExprPosL (0, 4) $ exprToPosTree exTree
--                           ]


rawTmp :: Animation -> IO ()
rawTmp ani = do
  setFPS 60
  dirName <- getDirName
  createDirectory dirName
  renderSvgs dirName 0 False ani
  where
    getDirName :: IO String
    getDirName = do
      i :: Int <- getStdRandom (randomR (0, 1000))
      let dirName = "./tmp" ++ show i
      b <- doesDirectoryExist dirName
      if b then getDirName else return dirName

rawFrame :: SVG -> IO ()
rawFrame frame = do
  let svg = renderSvg Nothing Nothing frame
  -- i :: Int <- getStdRandom (randomR (0, 1000))
  let i = 1
  let fileName = "./frame-" ++ show i ++ ".svg"
  print svg
  print fileName
  writeFile fileName svg



-- equilateral triangle - where c = sideLen
-- c = 2a, b = a * (sqrt 3) / 2

-- TODO I need to be able to draw lines between these too! So, maybe instead, make an IR that records the position of each + the "representation" of each node

-- uses relative position
exprToPosTree :: Expr a -> ExprPos a
exprToPosTree (Leaf a) = LeafP (0, 0) a
exprToPosTree (Branch t a b) =
  let -- position is relative to root node?
      sideLen = 4
      lb = changePos (-1 * sideLen / 2,-1 * sideLen * sqrt 3 / 2) $ exprToPosTree a
      rb = changePos (sideLen / 2,-1 * sideLen * sqrt 3 / 2) $ exprToPosTree b
   in BranchP (0, 0) t lb rb

-- the text case is a bit weird, since you need to double the offset if it's a branch
exprToPosText :: Expr a -> ExprPos a
exprToPosText (Leaf a) = LeafP (0, 0) a
exprToPosText (Branch t a b) =
  let -- position is relative to root node?
      textOffset = 3
      changePos' :: Point -> ExprPos a -> ExprPos a
      changePos' p (LeafP _ a) = LeafP p a
      changePos' p (BranchP _ a b c) = BranchP (p*2) a b c

      lb = changePos' (-textOffset, 0) $ exprToPosText a
      rb = changePos' (textOffset, 0) $ exprToPosText b
   in BranchP (0, 0) t lb rb


-- same render - kind of works but not really for text (misrenders).. that's OK though. I can just handle that separately
renderExprPos :: ToSVG a => Point -> ExprPos a -> SVG
renderExprPos (x, y) (LeafP (x1, y1) a) = translate (x1 + x) (y1 + y) $ toSVG a
renderExprPos (x, y) (BranchP (x1, y1) t a b) =
  mkGroup
    [ renderExprPos (x + x1, y + y1) a,
      renderExprPos (x+ x1, y + y1) b,
      translate (x + x1) (y + y1) $ toSVG t
    ]
  
-- Render, but lines - for the tree..
renderExprPosL :: ToSVG a => Point -> ExprPos a -> SVG
renderExprPosL (x, y) (LeafP (x1, y1) a) = translate (x1 + x) (y1 + y) $ toSVG a
renderExprPosL (x, y) (BranchP (x1, y1) t a b) =
  mkGroup
    [ renderExprPosL (x + x1, y + y1) a,
      renderExprPosL (x+ x1, y + y1) b,
      translate (x + x1) (y + y1) $ toSVG t,
      -- I need a slope if I need to shorten this line..
      withStrokeColor "grey" $ mkLine (x + x1, y + y1) (x + x1 + rx * 0.6, y + y1 + ry*0.6),
      withStrokeColor "grey" $ mkLine (x + x1, y + y1) (x + x1 + lx*0.6, y + y1 + ly*0.6)
    ]
    where
      ln = renderExprPosL (x + x1, y + y1) a
      rn = renderExprPosL (x+ x1, y + y1) b
      (lx, ly) = getPos a
      (rx, ry) = getPos b

-- TODO turn the tree into a list, and animate them 1 at a time? - each needs to be 
exprPosToList :: ToSVG a => Point -> ExprPos a -> [ExprNode a]
exprPosToList (x, y) (LeafP (x1, y1) a) = [Node (x1 + x ,y1 + y) a]
exprPosToList (x, y) (BranchP (x1, y1) t a b) = concat [
      exprPosToList (x + x1, y + y1) a,
      [Node (x + x1, y + y1) t],
      exprPosToList (x+ x1, y + y1) b
    ]

renderPosList :: ToSVG a => [ExprNode a] -> SVG
renderPosList  = mkGroup . map (\(Node (x, y) a) -> translate x y $ toSVG a)

-- TODO each needs to be a variable I think - for this to work..
-- TODO figure out how to animate the expression into a tree...
