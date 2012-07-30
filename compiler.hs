import Text.Printf

compile str = do
	let code = rec str 0 []
	writeFile "test/helloworld.ll" (unlines code)

rec :: String -> Int -> [Int]-> [String]
rec str temp label
	| str == [] = []
	| otherwise = code where
	(code1, nextTemp, nextLabel) = translate (head str) temp label
	codeN = (rec (tail str) nextTemp nextLabel)
	code = [code1] ++ codeN

--ptr++
translate :: Char -> Int -> [Int] -> (String, Int, [Int])	
translate '>' temp labels = (unlines[ printf "%%%d = load i8** %%ptr" temp,
	printf "%%%d = getelementptr inbounds i8* %%%d, i32 1" (temp+1) temp,
	printf "store i8* %%%d, i8** %%ptr" (temp+1)], temp+2, labels)
--ptr--
translate '<' temp labels = (unlines[ printf "%%%d = load i8** %%ptr" temp,
	printf "%%%d = getelementptr inbounds i8* %%%d, i32 -1" (temp+1) temp,
	printf "store i8* %%%d, i8** %%ptr" (temp+1)], temp+2, labels)
--(*ptr)++
translate '+' temp labels = (unlines[printf "%%%d = load i8** %%ptr" temp,
	printf "%%%d = load i8* %%%d" (temp+1) temp,
	printf "%%%d = add nsw i8 %%%d, 1" (temp+2) (temp+1),
	printf "store i8 %%%d, i8* %%%d" (temp+2) temp], temp+3, labels)
--(*ptr)--
translate '-' temp labels = (unlines[printf "%%%d = load i8** %%ptr" temp,
	printf "%%%d = load i8* %%%d" (temp+1) temp,
	printf "%%%d = sub nsw i8 %%%d, 1" (temp+2) (temp+1),
	printf "store i8 %%%d, i8* %%%d" (temp+2) temp], temp+3, labels)
--putchar(*ptr)
translate '.' temp labels = (unlines [printf "%%%d = load i8** %%ptr" temp,
	printf "%%%d = load i8* %%%d" (temp+1) temp,
	printf "%%%d = call i32 @putchar(i8 %%%d)" (temp+2) (temp+1)], temp+3, labels)
--(*ptr) = getchar()
translate ',' temp labels = ("(*ptr) = getchar()", temp+0, labels)
--while (*ptr){
translate '[' temp labels = (unlines [printf "br label %%beginLoop%d" temp,
	printf "beginLoop%d:" temp,
	printf "%%%d = load i8** %%ptr" (temp),
	printf "%%%d = load i8* %%%d" (temp+1) (temp),
	printf "%%%d = icmp eq i8 %%%d, 0" (temp+2) (temp+1),
	printf "br i1 %%%d, label %%endLoop%d, label %%%d" (temp+2) temp (temp+3)], temp+4, labels ++ [temp])
--}
-- fundera ut hur labels ska genereras. behöver [ peka på ] samtidigt som ] pekar på [ ???
-- 
translate ']' temp labels = (unlines [printf "br label %%beginLoop%d" (last labels),
	printf "br label %%endLoop%d" (last labels),
	printf  "endLoop%d:" (last labels)],temp+1, init labels)

translate c temp labels = ("", temp+0, labels)	

main = do  
    contents <- readFile "test/helloworld.bf"  
    compile contents 
