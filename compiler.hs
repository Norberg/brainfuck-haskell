
compile str = do
	let code = rec str 10
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
translate '>' temp = (unlines["%"++ show temp ++ " = load i8** %ptr",
	"%"++ show (temp+1) ++" = getelementptr inbounds i8* %"++ show temp ++", i32 1",
	"store i8* %" ++ show (temp+2) ++ ", i8** %ptr"], temp+2)
translate '<' temp  = ("ptr--", temp+0)
--(*ptr)++
translate '+' temp = (unlines["%3 = load i8** %ptr",
	"%4 = load i8* %3",
	"%5 = add nsw i8 %4, 1",
	"store i8 %5, i8* %3"], temp+3)
translate '-' temp = ("(*ptr)--",  temp+0)
--putchar(*ptr)
translate '.' temp = (unlines ["%6 = load i8** %ptr",
	"%7 = load i8* %6",
	"%8 = call i32 @putchar(i8 %7)"], temp+3)
--(*ptr) = getchar()
translate ',' temp = ("(*ptr) = getchar()", temp+0)
--while (*ptr){
translate '[' temp = ("while (*ptr){", temp+0)
--}
translate ']' temp = ("}", temp+0)
translate c temp = ("noop", temp+0)	

main = do  
    contents <- readFile "test/helloworld.bf"  
    compile contents 
