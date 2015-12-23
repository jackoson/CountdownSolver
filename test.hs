import Data.List
data Tree = Node Op Tree Tree | Leaf Double

data  Op = Add (Double->Double->Double) | Sub (Double->Double->Double) | Mul (Double->Double->Double) | Div (Double->Double->Double)

operators = [Add (+), Sub (-), Mul (*), Div (/)]

extractOp::Op->(Double->Double->Double)
extractOp (Add f)= f
extractOp (Sub f)= f
extractOp (Mul f)= f
extractOp (Div f)= f

instance Show Op where
  show (Add _) = "+"
  show (Sub _) = "-"
  show (Mul _) = "*"
  show (Div _) = "/"

instance Show Tree where
  --show = printTreeV 0
  show = printTreeH

printTreeV :: Int->Tree -> String
printTreeV x (Leaf i) = take x (repeat ' ') ++ show (truncate i)
printTreeV x (Node op l r) = take x (repeat ' ') ++ show op ++ "\n" ++ printTreeV (x+1) l ++ "\n" ++ printTreeV (x+1) r

printTreeH :: Tree -> String
printTreeH (Leaf x) = show (truncate x)
printTreeH (Node (Add f) l r) = "(" ++ (printTreeH l) ++ "+" ++ (printTreeH r) ++ ")"
printTreeH (Node (Sub f) l r) = "(" ++ (printTreeH l) ++ "-" ++ (printTreeH r) ++ ")"
printTreeH (Node (Mul f) l r) = "(" ++ (printTreeH l) ++ "*" ++ (printTreeH r) ++ ")"
printTreeH (Node (Div f) l r) = "(" ++ (printTreeH l) ++ "/" ++ (printTreeH r) ++ ")"

calcTree :: Tree -> Double
calcTree (Leaf x) =  x
calcTree (Node f l r) = (extractOp f) (calcTree l) (calcTree r)

constructTrees::[Double]->[Op]->[Tree]
constructTrees (x:[]) ops = [Leaf x]
constructTrees xs ops =  (concat.concat.concat) [let (a,b) = (splitAt i xs)
                          in [let ls  = (constructTrees a ops)
                                  rs = (constructTrees b ops)
                              in [[Node op l r | r<-rs] | l<-ls ] |op<-ops] | i<-[1..(length xs -1)] ]

powerset::[a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = xss /\/ map (x:) xss
                    where xss = powerset xs

(/\/)::[a] -> [a] -> [a]
[] /\/ ys = ys
(x:xs) /\/ ys = x : (ys /\/ xs)

constructAllLenghtTrees::[Double]->[Op]->[Tree]
constructAllLenghtTrees xs ops = concat [concat [constructTrees xs'' ops| xs''<-permutations xs'] | xs'<-powerset xs]

countdownSolution::[Double]->[Op]->Double->String
countdownSolution xs ops ans = let list = [show x|x<-(constructAllLenghtTrees xs ops), (calcTree x)==ans] in head list

getNumbers::IO([Double])
getNumbers = do
          number <- getLine
          if number == "end"
          then return []
          else do
            numbers <- getNumbers
            return $ (read number):numbers
main = do
    putStrLn "Please type each number followed by a new line. When finished type end and press enter"
    numbers <- getNumbers
    putStrLn "Great, now input target number"
    target <- getLine
    putStrLn $ countdownSolution numbers operators (read target)
