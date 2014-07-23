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
         | StorePtr Dest Src
         | GetPtr Dest Src Int
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

currentLabel ::  String -> State CompilerState Label
currentLabel str = do
	state <- get
	let label_stack = labelStack state
	let current_label = head label_stack
	return $ str ++ show current_label

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
	--putStrLn $ show ast
	let instrs = evalState (compileAst ast) initState
	let code = map instrToAsm $ reverse instrs
	--putStrLn $ show instrs
	--putStrLn $ show code
	writeFile "test/helloworld.ll" (unlines code)

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
	emit $ LoadByteRef ref ptr
	emit $ LoadByteValue org ref
	emit $ AddByte res org n
	emit $ StoreByte ref res
	evaluated <- transform terms
	return evaluated

transform (Sub n:terms) = do
	let ptr = StrAddr "ptr"
	ref <- nextTemp
	org <- nextTemp
	res <- nextTemp
	emit $ LoadByteRef ref ptr
	emit $ LoadByteValue org ref
	emit $ SubByte res org n
	emit $ StoreByte ref res
	evaluated <- transform terms
	return evaluated

transform (PutChar : terms) = do
	let ptr = StrAddr "ptr"
	ref <- nextTemp
	val <- nextTemp
	dummy <- nextTemp
	emit $ LoadByteRef ref ptr
	emit $ LoadByteValue val ref
	emit $ CallPutChar val
	evaluated <- transform terms
	return evaluated

transform (GetChar : terms) = do
	let ptr = StrAddr "ptr"
	val <- nextTemp
	ref <- nextTemp
	emit $ CallGetChar val
	emit $ LoadByteRef ref ptr
	emit $ StoreByte ref val
	evaluated <- transform terms
	return evaluated

transform (PtrAdd n : terms) = do
	let ptr = StrAddr "ptr"
	ref <- nextTemp
	val <- nextTemp
	emit $ LoadByteRef ref ptr
	emit $ GetPtr val ref n
	emit $ StorePtr ptr val
	evaluated <- transform terms
	return evaluated

transform (PtrSub n : terms) = do
	let ptr = StrAddr "ptr"
	ref <- nextTemp
	val <- nextTemp
	emit $ LoadByteRef ref ptr
	emit $ GetPtr val ref (-n)
	emit $ StorePtr ptr val
	evaluated <- transform terms
	return evaluated

transform (BeginLoop : terms) = do
	let ptr = StrAddr "ptr"
	begin <- nextLabel "beginLoop"
	end <- currentLabel "endLoop"
	continue <- currentLabel "continueLoop"
	ref <- nextTemp
	val <- nextTemp
	res <- nextTemp
	emit $ Branch begin
	emit $ DefineLabel begin
	emit $ LoadByteRef ref ptr
	emit $ LoadByteValue val ref
	emit $ CompareByte res val 0
	emit $ ConditionalBranch res end continue
	emit $ DefineLabel continue
	evaluated <- transform terms
	return evaluated

transform (EndLoop : terms) = do
	let ptr = StrAddr "ptr"
	end <- currentLabel "endLoop"
	begin <- popLabel "beginLoop"
	emit $ Branch begin
	emit $ DefineLabel end
	evaluated <- transform terms
	return evaluated

transform (NoOp:terms) = do
	evaluated <- transform terms
	return evaluated

transform [] = return ()

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

asmAddr :: Addr -> String
asmAddr (Temp n) = '%' : show n
asmAddr (StrAddr addr) = '%' : addr

asmAssign :: Addr -> String -> Addr -> String
asmAssign dest instr src = (asmAddr dest) ++ " = " ++ instr ++ " " ++ (asmAddr src)

instrToAsm :: Instruction -> String
instrToAsm (LoadByteRef dest src) = asmAssign dest "load i8**" src
instrToAsm (LoadByteValue dest src) = asmAssign dest  "load i8*" src
instrToAsm (AddByte dest src n) = (asmAssign dest "add nsw i8"  src) ++ ", " ++ (show n)
instrToAsm (SubByte dest src n) = (asmAssign dest "sub nsw i8"  src) ++ ", " ++ (show n)
instrToAsm (StoreByte dest src) = "store i8 " ++ (asmAddr src) ++ ", i8*" ++ (asmAddr dest)
instrToAsm (StorePtr dest src) = "store i8* " ++ (asmAddr src) ++ ", i8**" ++ (asmAddr dest)
instrToAsm (CallPutChar src) = "call i32 @putchar(i8 " ++ (asmAddr src) ++ ")"
instrToAsm (CallGetChar dest) = (asmAddr dest) ++ " = call i8 @getchar()"
instrToAsm (GetPtr dest src n) = (asmAssign dest "getelementptr inbounds i8*" src) ++ ", i32 " ++
	show (n)
instrToAsm (DefineLabel label) = label ++ ":"
instrToAsm (CompareByte dest src val) = (asmAssign dest "icmp eq i8" src) ++ ", " ++ show val
instrToAsm (ConditionalBranch val true false) = "br i1 " ++ (asmAddr val) ++ ", " ++ "label %" ++ true ++ ", label %" ++ false
instrToAsm (Branch label) = "br label %" ++ label

main = do
	argv <- getArgs
	contents <- readFile (head argv)
	compile contents
