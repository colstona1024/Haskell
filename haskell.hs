doubleme x = x + x

doubleus x y = doubleme x + doubleme y

main :: IO()
main = do
	putStrLn doubleme 4
