import System.Environment
import Text.Printf
import Control.Monad.State

data Term = Add Int
          | Sub Int
          | PtrAdd Int
          | PtrSub Int
          | PutChar
          | GetChar
          | BeginLoop
          | EndLoop
          | NoOp deriving(Show, Eq)

data Addr = Temp Int | StrAddr String deriving (Show)
type Src = Addr
type Dest = Addr
type Label = String
data Instruction = LoadByteValue Dest Src
         | LoadByteRef Dest Src
         | StoreByte Dest Src
         | GetPtr Dest Src
         | AddByte Dest Src Int
         | SubByte Dest Src Int
         | CallPutChar Src
         | CallGetChar Dest
         | DefineLabel Label
         | CompareByte Dest Src Int
         | ConditionalBranch Src Label Label
         | Branch Label deriving(Show)

data CompilerState = CompilerState{ tempCount :: Int
                        , labelCount :: Int
                        , labelStack :: [Int]
                        , instructions :: [Instruction]} deriving (Show)

initState :: CompilerState
initState = CompilerState 0 0 [] []

nextTemp :: State CompilerState Addr
nextTemp = do
	state <- get
	let next_temp = tempCount state
	let new_state = state{tempCount = next_temp + 1}
	put new_state
	return $ Temp next_temp

nextLabel ::  String -> State CompilerState Label
nextLabel str = do
	state <- get
	let next_label = labelCount state
	let new_state = state{ labelCount = next_label + 1,
                               labelStack = next_label : (labelStack state)}
	put new_state
	return $ str ++ show next_label

popLabel :: String -> State CompilerState String
popLabel str = do
	state <- get
	let label_stack = labelStack state
	let new_label = head label_stack
	let new_state = state {labelStack = tail label_stack}
	put new_state
	return $ str ++ show new_label

emit :: Instruction -> State CompilerState ()
emit instruction = do
	state <- get
	let current_instructions = instructions state
	let new_state = state{instructions = instruction:current_instructions}
	put new_state
	return ()

getInstructions :: State CompilerState [Instruction]
getInstructions = do
	state <- get
	return $ instructions state

compile str = do
	let ast = map charToTerm str
	putStrLn $ show ast
	let instrs = evalState (compileAst ast) initState
	putStrLn $ show $ reverse instrs

compileAst :: [Term] -> State CompilerState [Instruction]
compileAst terms = do
	a <- transform terms
	instrs <- getInstructions
	return instrs

transform :: [Term] -> State CompilerState ()
transform (Add n:terms) = do
	let ptr = StrAddr "ptr"
	ref <- nextTemp
	org <- nextTemp
	res <- nextTemp
	emit (LoadByteRef ref ptr)
	emit (LoadByteValue org ref)
	emit (AddByte res org n)
	emit (StoreByte ref res)
	evaluated <- transform terms
	return evaluated

--translate '+' temp labels = (unlines[printf "%%%d = load i8** %%ptr" temp,
--	printf "%%%d = load i8* %%%d" (temp+1) temp,
--	printf "%%%d = add nsw i8 %%%d, 1" (temp+2) (temp+1),
--	printf "store i8 %%%d, i8* %%%d" (temp+2) temp], temp+3, labels)
transform (not_implemented:terms) = do
	evaluated <- transform terms
	return evaluated

transform [] = return ()

--compile str = do
--	let code = rec str 0 []
--	writeFile "test/helloworld.ll" (unlines code)

charToTerm :: Char -> Term
charToTerm '+' = Add 1
charToTerm '-' = Sub 1
charToTerm '>' = PtrAdd 1
charToTerm '<' = PtrSub 1
charToTerm '.' = PutChar
charToTerm ',' = GetChar
charToTerm '[' = BeginLoop
charToTerm ']' = EndLoop
charToTerm x = NoOp

{-
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
translate ',' temp labels = (unlines [printf "%%%d = call i8 @getchar()" temp,
	printf "%%%d = load i8** %%ptr" (temp+1),
	printf "store i8 %%%d, i8* %%%d" temp (temp+1)], temp+2, labels)
--while (*ptr){
translate '[' temp labels = (unlines [printf "br label %%beginLoop%d" temp,
	printf "beginLoop%d:" temp,
	printf "%%%d = load i8** %%ptr" (temp),
	printf "%%%d = load i8* %%%d" (temp+1) (temp),
	printf "%%%d = icmp eq i8 %%%d, 0" (temp+2) (temp+1),
	printf "br i1 %%%d, label %%endLoop%d, label %%%d" (temp+2) temp (temp+3)], temp+4, labels ++ [temp])
translate ']' temp labels = (unlines [printf "br label %%beginLoop%d" (last labels),
	printf "br label %%endLoop%d" (last labels),
	printf  "endLoop%d:" (last labels)],temp+1, init labels)

translate c temp labels = ("", temp+0, labels)

--}
main = do
	argv <- getArgs
	contents <- readFile (head argv)
	compile contents
