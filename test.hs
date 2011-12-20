
delimiters = [' ','\t','\n']
nameSpacePars = ['{','}']  -- limits the scope of names, names exist in 
objectPars = ['[',']']     -- an object is a sequence of objects or a name, can be true or false in expressions
exprPars = ['(',')']
andChar = '&'
orChar = '|'
xorChar = '^'
keychars = andChar:orChar:xorChar:nameSpacePars++objectPars

data Node = Branch [Node] | Leaf String deriving Show
data Id = Name Int | Key String deriving Show

tokenize :: String -> [String]
tokenize str = let tokenizeMore done token (next:rest) 
                        | next `elem` delimiters = if token=="" 
                                                 then tokenizeMore done "" rest 
                                                 else token:(tokenizeMore done "" rest)
                        | next `elem` keychars = if token=="" 
                                                 then [next]:(tokenizeMore done "" rest) 
                                                 else token:[next]:(tokenizeMore done "" rest)
                        | otherwise = tokenizeMore done (token ++ [next]) rest
                   tokenizeMore done token [] = if token=="" then done else done ++ [token]
               in tokenizeMore [] "" str

getTree :: String -> String -> [String] -> Node
getTree left right tokens = let treeify (first:rest)
                                   | first==left  = ((Branch $ fst $ treeify rest):(fst $ treeify $ snd $ treeify rest),snd $ treeify $ snd $ treeify rest)
                                   | first==right = ([],rest)
                                   | otherwise    = ((Leaf first):(fst $ treeify rest),snd $ treeify rest)
                                treeify [] = ([],[])
                            in Branch $ fst $ treeify tokens

indexIn :: Eq a => a -> [a] -> Int
indexIn e (first:rest) = if e == first then 0 else e `indexIn` rest + 1

getIds :: [String] -> [Id]
getIds tokens = let identify nextId knownNames (Branch nodes:rest) = (fst firstRes ++ fst restRes,snd restRes)
                                                                     where firstRes = identify nextId knownNames nodes
                                                                           restRes = identify (snd firstRes) knownNames rest
                    identify nextId knownNames (Leaf name:rest)    = 
                      ((if elem name [[x]|x<-keychars]
                        then Key name
                        else (Name (if name `elem` knownNames then name `indexIn` knownNames else length knownNames + 1)))
                         :(fst restRes),snd restRes)
                       where restRes = (identify nextId (if name `elem` knownNames then knownNames else name:knownNames) rest)
                    identify nextId knownNames []                    = ([],nextId)
                in fst $ identify 0 [] [getTree "{" "}" tokens]

--IO

main = do x <- readFile "test.z"
          putStrLn $ show $ tokenize x
          putStrLn $ show $ getIds $ tokenize x

printList :: [String] -> IO ()
printList [] = return ()
printList (first:rest) = do putStrLn first
                            printList rest
