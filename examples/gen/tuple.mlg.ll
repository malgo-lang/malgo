; ModuleID = './examples/tuple.mlg'
source_filename = "./examples/tuple.mlg"

@0 = unnamed_addr constant [15 x i8] c" is the answer\00"

declare {}* @print_int(i64)

define {}* @print_int1(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

declare {}* @println(i8*)

define {}* @println2(i8*) {
  %2 = call {}* @println(i8* %0)
  ret {}* %2
}

declare {}* @newline()

define {}* @newline3() {
  %1 = call {}* @newline()
  ret {}* %1
}

define i64 @fst_int4({ i64, i8* }*) {
  %2 = getelementptr { i64, i8* }, { i64, i8* }* %0, i32 0, i32 0
  %3 = load i64, i64* %2
  ret i64 %3
}

define i8* @snd_str5({ i64, i8* }*) {
  %2 = getelementptr { i64, i8* }, { i64, i8* }* %0, i32 0, i32 1
  %3 = load i8*, i8** %2
  ret i8* %3
}

declare i8* @GC_malloc(i64)

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 ptrtoint ({ i64, i8* }* getelementptr inbounds ({ i64, i8* }, { i64, i8* }* null, i32 1) to i64))
  %2 = bitcast i8* %1 to { i64, i8* }*
  %3 = getelementptr { i64, i8* }, { i64, i8* }* %2, i32 0, i32 0
  store i64 42, i64* %3
  %4 = getelementptr { i64, i8* }, { i64, i8* }* %2, i32 0, i32 1
  store i8* getelementptr inbounds ([15 x i8], [15 x i8]* @0, i32 0, i32 0), i8** %4
  %5 = call i64 @fst_int4({ i64, i8* }* %2)
  %6 = call {}* @print_int1(i64 %5)
  %7 = call i8* @snd_str5({ i64, i8* }* %2)
  %8 = call {}* @println2(i8* %7)
  ret i32 0
}
