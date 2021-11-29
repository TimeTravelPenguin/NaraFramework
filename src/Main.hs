module Main where

data Node t = Node t
  deriving (Show)

data Event t
  = Event (Node t) (Node t)
  | Composed (Event t) (Event t)
  deriving (Show)

newNode :: t -> Node t
newNode = Node

newArrow :: Node t -> Node t -> Event t
newArrow = Event

composeArrows :: Eq t => Event t -> Event t -> Event t
composeArrows from to =
  case (from, to) of
    (Event (Node a) (Node b), Event (Node c) (Node d)) ->
      if b == c
        then Event (Node a) (Node d)
        else Composed from to
    (Composed a b, Composed c d) ->
      composeArrows (composeArrows a b) (composeArrows c d)
    _ -> error "Error"

main :: IO ()
main = do
  let a = newNode "a"
  let b = newNode "b"
  let c = newNode "c"

  let ab = newArrow a b
  let bc = newArrow b c

  let ac = composeArrows ab bc

  print ab
  print bc
  print ac

  print "Done"