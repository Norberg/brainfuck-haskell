
compile str = do
	let temp = 0
	let code = (map translate str)
	writeFile "test/helloworld.ll" (unlines code)
--ptr++	
translate '>' = unlines["%1 = load i8** %ptr",
	"%2 = getelementptr inbounds i8* %1, i32 1",
	"store i8* %2, i8** %ptr"]
translate '<' = "ptr--"
--(*ptr)++
translate '+' = unlines["%3 = load i8** %ptr",
	"%4 = load i8* %3",
	"%5 = add nsw i8 %4, 1",
	"store i8 %5, i8* %3"]
translate '-' = "(*ptr)--"
--putchar(*ptr)
translate '.' = unlines ["%6 = load i8** %ptr",
	"%7 = load i8* %6",
	"%8 = call i32 @putchar(i8 %7)"]
--(*ptr) = getchar()
translate ',' = "(*ptr) = getchar()"
--while (*ptr){
translate '[' = "while (*ptr){"
--}
translate ']' = "}"
translate c = "noop"	

main = do  
    contents <- readFile "test/helloworld.bf"  
    compile contents 
