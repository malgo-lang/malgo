; ModuleID = './examples/tuple.mlg'
source_filename = "./examples/tuple.mlg"

@0 = global [15 x i8] c" is the answer\00"

declare i8* @GC_malloc(i64)

define i32 @main33() {
  %1 = bitcast [15 x i8]* @0 to i8*
  %2 = call i8* @GC_malloc(i64 ptrtoint ({ i64, i8* }* getelementptr inbounds ({ i64, i8* }, { i64, i8* }* null, i32 1) to i64))
  %3 = bitcast i8* %2 to { i64, i8* }*
  %4 = getelementptr { i64, i8* }, { i64, i8* }* %3, i32 0, i32 0
  store i64 42, i64* %4
  %5 = call i8* @GC_malloc(i64 0)
  %6 = bitcast i8* %5 to {}*
  %7 = getelementptr { i64, i8* }, { i64, i8* }* %3, i32 0, i32 1
  store i8* %1, i8** %7
  %8 = call i8* @GC_malloc(i64 0)
  %9 = bitcast i8* %8 to {}*
  %10 = call i64 @fst_int35({ i64, i8* }* %3)
  %11 = call {}* @print_int38(i64 %10)
  %12 = call i8* @snd_str34({ i64, i8* }* %3)
  %13 = call {}* @println37(i8* %12)
  ret i32 0
}

define i8* @snd_str34({ i64, i8* }*) {
  %2 = getelementptr { i64, i8* }, { i64, i8* }* %0, i32 0, i32 1
  %3 = load i8*, i8** %2
  ret i8* %3
}

define i64 @fst_int35({ i64, i8* }*) {
  %2 = getelementptr { i64, i8* }, { i64, i8* }* %0, i32 0, i32 0
  %3 = load i64, i64* %2
  ret i64 %3
}

declare {}* @newline()

define {}* @newline36() {
  %1 = call {}* @newline()
  ret {}* %1
}

declare {}* @println(i8*)

define {}* @println37(i8*) {
  %2 = call {}* @println(i8* %0)
  ret {}* %2
}

declare {}* @print_int(i64)

define {}* @print_int38(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

define i32 @main() {
  %1 = call i32 @main33()
  ret i32 0
}
