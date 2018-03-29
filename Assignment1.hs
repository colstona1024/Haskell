main = do
 putStrLn "Hello, my name is Allen Colston"
 let colors = ["Red","Green","Blue","White","Black","Yellow","Magenta","Cyan","Gray","Salmon","Purple","Lime"]
 let months = ["January","February","March","April","May","June","July","August","September","October","November","December"]

 let combine = zip colors months

 print $ combine
 print $ head combine
 print $ last combine
 print $ combine !! 2
 print $ reverse combine