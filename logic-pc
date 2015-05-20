
newPC :: (Eq a) => a -> [a] -> Maybe a 
newPC a [] = Nothing
newPC a b:ls 
	| a == b = Just b
	| otherwise = Nothing

newTestPC ::  (a -> Bool) -> [a] -> Maybe a
newTestPC f [] = Nothing
newTestPC f x:_
	| f x = Just x
	| otherwise = Nothing

newTestPC (\x -> a /= x) -- controlla se il primo carattere dello 'stream' è diverso da a

andPC f g [] = Nothing
andPC f g b:[] = Nothing
andPC f g b:c:ls = checkMaybe [f b, g c]

andPC' [] _ = error
andPC' f:fs [] = Nothing
andPC' f:fs b:ls = checkMaybe( (f b) : (andPC' fs ls) ) 


orPC f g [] = Nothing
orPC f g x:xs
	| f x /= Nothing = f x
	| otherwise = g x

orPC' [] _ = Nothing
orPC' _ [] = Nothing
orPC' f:fs x:xs = firstJust ((f x):(orPC' fs xs))
	where firstJust [] = Nothing
	      firstJust maybe:ms
			| maybe /= Nothing = maybe
			| otherwise = firstJust ms
	

notPC f Nothing = Just undefined
notPC f Just _  = Nothing

untilPC f [] = Nothing
untilPC f b:bs 
	| f b == Nothing = Nothing
	| otherwise = untilPC f bs

manyPC n f  = andPC' (replicate n f) 

between f g h bs = (andPC' [f,g,h] bs) !! 1 

checkMaybe = foldr1 f -- se nella lista ci sono solo dei Just porta fuori il Just altrimenti restituisce nothing
	where f :: Maybe [a] -> Maybe a -> Maybe [a]
	      f Nothing _ 	= Nothing
	      f _ 	Nothing	= Nothing
	      f Just xs Just x  = Just a:xs
