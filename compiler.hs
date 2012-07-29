import Text.Printf

compile str = do
	let code = rec str 0
	writeFile "test/helloworld.ll" (unlines code)

rec :: String -> Int -> [String]
rec str temp
	| str == [] = []
	| otherwise = code where
	(code1, nextTemp) = translate (head str) temp
	codeN = (rec (tail str) nextTemp)
	code = [code1] ++ codeN

--ptr++
translate :: Char -> Int -> (String, Int)	
translate '>' temp = (unlines[ printf "%%%d = load i8** %%ptr" temp,
	printf "%%%d = getelementptr inbounds i8* %%%d, i32 1" (temp+1) temp,
	printf "store i8* %%%d, i8** %%ptr" (temp+1)], temp+2)
translate '<' temp  = ("ptr--", temp+0)
--(*ptr)++
translate '+' temp = (unlines[printf "%%%d = load i8** %%ptr" temp,
	printf "%%%d = load i8* %%%d" (temp+1) temp,
	printf "%%%d = add nsw i8 %%%d, 1" (temp+2) (temp+1),
	printf "store i8 %%%d, i8* %%%d" (temp+2) temp], temp+3)
translate '-' temp = ("(*ptr)--",  temp+0)
--putchar(*ptr)
translate '.' temp = (unlines [printf "%%%d = load i8** %%ptr" temp,
	printf "%%%d = load i8* %%%d" (temp+1) temp,
	printf "%%%d = call i32 @putchar(i8 %%%d)" (temp+2) (temp+1)], temp+3)
--(*ptr) = getchar()
translate ',' temp = ("(*ptr) = getchar()", temp+0)
--while (*ptr){
translate '[' temp = ("while (*ptr){", temp+0)
--}
translate ']' temp = ("}", temp+0)
translate c temp = ("", temp+0)	

main = do  
    contents <- readFile "test/42.bf"  
    compile contents 
