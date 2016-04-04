module Group where

u = [1,1,1,1,1,1,1]
u'= [1,-1,1,-1,1,1,-1]
v=  [4,2,1,0,-1,0,-1]
v' = prod v u'
altv = (minus vw $ summ v $ summ w w')
w = [5,1,-1,-1,0,1,1]
w' = prod w u'
vw = prod v w

misc [a,b,c,d,e,f,g] = [a,a,c,f,e,a,c]

alt :: [Integer] -> [Integer]
alt t =  map ((flip div) 2) $ minus (prod t t) (misc t) 
sym t = minus (prod t t) (alt t) 


conj = [1,10,20,30,24,15,20]

prod = zipWith (*)
summ = zipWith (+)
minus = zipWith (-)

chi = [u,u',v,v',alt v, w, w']

scalar a b =  (flip div) (sum conj) $ sum $ prod conj $ prod a b
norm t = scalar t t

main :: IO ()
main = do
	print $ map (scalar (prod v w)) chi
	print $ map (scalar (sym w)) chi
	print $ map (scalar (alt w)) chi

