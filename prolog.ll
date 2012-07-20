declare i32 @putchar(i8 zeroext) nounwind
@data = global [30000 x i8] zeroinitializer
define i32 @main() nounwind {
entry:
%ptr = alloca i8*
store i8* getelementptr inbounds ([30000 x i8]* @data, i32 0, i32 0), i8** %ptr
