--main prediction function encapsulates markov chain functions--

--synthesize:: [ (channel, [key])] -> [(channel, [velocity])] -> [(channel, [tick])] -> [(channel, [(velocity, key, tick)])]

synthesize [] = []



--provides a square table of zeroes whose size is the length of the uniques of a list
--columns and rows are labelled by unique items in list
--matrix:: Num int => [int] -> [(int, int, int)]
matrix [] = []
matrix list = [(key1, key2, 0) | key1 <- (uniques list), key2 <- (uniques list)]


--uses matrix to create a matrix of zeroes and then replaces the zeroes with probabilites
--populate:: Num int => [int] -> [(int, int, int)] -> [(int, int, int)]
populate [] [(key1, key2, prop)] = [(key1, key2, prop)]
populate (h:t) [] = []
populate (h:t) [(key1, key2, prop)]
    | (length (h:t)) == 0 = [(key1, key2, prop)]
    | (length (h:t)) == 1 = (map (add1key1 h) [(key1, key2, prop)])
    | otherwise = populate t (map (add1key1key2 h (head t)) [(key1, key2, prop)])


--populate [] mat = mat
--populate (h:[]) [(key1, key2, prop)] = populate [] (map (add1key1 h) [(key1, key2, prop)])
--populate (h:n:t) [(key1, key2, prop)] = populate (n:t) (map (add1key1key2 h n) [(key1, key2, prop)])


--add1key1:: Eq a => a -> (a, a, a) -> (a, a, a)
add1key1 h (key1, key2, prop)
    | key1 == h = (key1, key2, (prop + 1))
    | otherwise = (key1, key2, prop)


add1key1key2 h n (key1, key2, prop)
    | (key1 == h && key2 == n) = (key1, key2, (prop + 1))
    | otherwise = (key1, key2, prop)



--returns only the unique items in a list
--uniques:: [a] -> [a]
uniques _ = [1, 2, 3]
