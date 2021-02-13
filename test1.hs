
func :: Char -> String
func x
    | x == 'a' = "Actually its a"
    | x `elem` ['a'..'z'] = "Alpha"
    | x `elem` ['1'..'9'] = "Nums"
    | otherwise = "Others"
