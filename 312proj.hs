--main prediction function encapsulates markov chain functions--



{- provides a square table of zeroes whose size is the length of the uniques of a list
   columns and rows are labelled by unique items in list
   -}
matrix :: (Eq b, Num c) => [b] -> [(b, b, c)]
matrix [] = []
matrix list = [(key1, key2, 0) | key1 <- (uniques list), key2 <- (uniques list)]

makenewlist list n newlist
    |n == 0 = newlist
    |newlist == [] = makenewlist list (n - 1) [(pickfirst list)]
    |otherwise = makenewlist list (n -1) ((picknext (head newlist) (options (head newlist) (proptoprob (populate list (matrix (list)))))) : newlist)

picknext item (head:opts)
    |opts == [] = second head
    |(third head) >= random = second head
    | otherwise = picknext item (proptoprob (opts))

addtothird n (a,b,c) = (a, b, (c + n))
third (a,b,c) = c
second (a,b,c) = b

sumcolumn d ((a,b,c):t)
    | t == [] = c
    | d == a = c + sumcolumn d t
    | otherwise = sumcolumn d t

--proptoprob ((a,b,c):t)
--    | t == [] = (a,b,c)
--    |otherwise = ((a,b, (c/ (sumcolumn a ((a,b,c):t)))): proptoprob t)

proptoprob ((a,b,c):t)
    | t == [] = [(a,b,c)]
    |otherwise = (a,b, (c/ (sumcolumn a ((a,b,c):t)))):(proptoprob t)

options item = filter (match1 item)

match1 item (key1, key2, prop) = item == key1

--pickfirst list = list!!(round ((length list) * random))

pickfirst list = list!!(round ((fromIntegral (length list)) * random))

random = 0.1


--uses matrix to create a matrix of zeroes and then replaces the zeroes with probabilites
--populate:: Eq int Num prop=> [int] -> [(int, int, prop)] -> [(int, int, prop)]
--goes through a list of keys, velocities, or ticks
--filters unique items in that list
--builds triplets such that each unique pair has a triplet, originally created with a zero
--goes through 

populate :: (Num a, Eq a) => [a] -> [(a,a,a)] -> [(a,a,a)]
populate [] lst = lst
populate (h:[]) lst = map (add1key1 h) lst
populate (h:h2:t) lst = populate (h2:t) (map (add1key1key2 h h2) lst)

{- special case of add1key1key2 where there is no key2
   -}
add1key1:: (Eq a, Num c) => a -> (a, b, c) -> (a, b, c)
add1key1 h (key1, key2, prop)
    | key1 == h = (key1, key2, (prop + 1))
    | otherwise = (key1, key2, prop)


{- add1key1key2 adds the number of times h,n returns a new 3-tuple with prop+1
   if (h,n) == (key1,key2)
   -}
add1key1key2 h n (key1, key2, prop)
    | (key1 == h && key2 == n) = (key1, key2, (prop + 1))
    | otherwise = (key1, key2, prop)



--returns only the unique items in a list
uniques:: Eq a => [a] -> [a]
uniques [] = []
uniques (h:t)
    | elem h t = uniques t
    | otherwise = h:(uniques t)
