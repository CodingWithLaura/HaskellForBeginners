myinc :: Int -> Int
myinc x	= x+1

quad :: Int -> Int
quad x = x*x

-- facultät
faculty :: Int -> Int
faculty 1 = 1
faculty x = x * faculty (x-1)
--bsp faculty 4 is ja 4 *(3 *(2* 1*))
-- suche in list
suche_in_liste_x :: [Int] -> Int -> Bool
suche_in_liste_x [] a = False
suche_in_liste_x (x:xs) a  |a == x    = True
                           |otherwise = suche_in_liste_x xs a


suche_in_liste_x_mit_index :: [Int] -> Int -> Int
suche_in_liste_x_mit_index [x] a |x == a =  1
                                 |otherwise = -1
suche_in_liste_x_mit_index (x:xs) a |x == a = 1
                                    |otherwise = let rest = (suche_in_liste_x_mit_index xs a)
                                                 in (if rest == -1 then (-1)
                                                                   else (rest +1))

ersetze_in_liste :: [Int] -> Int -> Int -> [Int]
ersetze_in_liste [] a b = []
ersetze_in_liste (x:xs) a b |x == a    = (b:(ersetze_in_liste xs a b))
                            |otherwise = (x:(ersetze_in_liste xs a b))

finde_min_liste :: [Int] -> Int
finde_min_liste [a] = a
finde_min_liste (x:xs) = let rest_min = (finde_min_liste xs)
                         in (if rest_min < x then rest_min
                                             else x)
                       
drehe_liste_um :: [Int] -> [Int]
drehe_liste_um [] = []
drehe_liste_um (x:xs) = (drehe_liste_um xs) ++ [x] 

schwellwert_liste :: [Int] -> Int -> Int -> [Int]
schwellwert_liste [] min max = []
schwellwert_liste (x:xs) min max | x < min =  schwellwert_liste xs min max
                                 | x > max =  schwellwert_liste xs min max
                                 | otherwise = x : schwellwert_liste xs min max       
                    
                   
replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
                | n <= 0    = []  
                | otherwise = x:(replicate' (n-1) x)                                   

