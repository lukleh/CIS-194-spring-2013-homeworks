module Golf where

skips :: [a] -> [[a]]
skips [] = []
skips xs = map (every xs) [1..(length xs)]
    where every ns n = case drop (n -1) ns of
                    [] -> []
                    (y:ys) -> y : every ys n


localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | y > x && y > z = y : localMaxima (z:xs)
  | otherwise = localMaxima (z:xs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = let counts = map (length . (\n -> filter (== n) xs)) [0..9]
                   max_count = maximum counts
                   hist_map_list = map (\n -> map (\x -> if n < x then '*' else ' ') counts) [max_count,(max_count - 1)..0]
               in unlines hist_map_list ++ "==========\n0123456789\n"
