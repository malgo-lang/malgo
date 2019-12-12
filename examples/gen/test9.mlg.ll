; ModuleID = './examples/test9.mlg'
source_filename = "./examples/test9.mlg"

declare i8* @GC_malloc(i64)

define i64 @f.11(i8*, {}*) {
  %3 = bitcast i8* %0 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { i64 (i8*, {}*)*, i8* }*
  %8 = getelementptr { i64 (i8*, {}*)*, i8* }, { i64 (i8*, {}*)*, i8* }* %7, i32 0, i32 0
  store i64 (i8*, {}*)* @f.11, i64 (i8*, {}*)** %8
  %9 = getelementptr { i64 (i8*, {}*)*, i8* }, { i64 (i8*, {}*)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %9
  ret i64 %5
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %2 = bitcast i8* %1 to { i64 }*
  %3 = getelementptr { i64 }, { i64 }* %2, i32 0, i32 0
  store i64 42, i64* %3
  %4 = bitcast { i64 }* %2 to i8*
  %5 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %6 = bitcast i8* %5 to { i64 (i8*, {}*)*, i8* }*
  %7 = getelementptr { i64 (i8*, {}*)*, i8* }, { i64 (i8*, {}*)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, {}*)* @f.11, i64 (i8*, {}*)** %7
  %8 = getelementptr { i64 (i8*, {}*)*, i8* }, { i64 (i8*, {}*)*, i8* }* %6, i32 0, i32 1
  store i8* %4, i8** %8
  %9 = call i8* @GC_malloc(i64 0)
  %10 = bitcast i8* %9 to {}*
  %11 = getelementptr { i64 (i8*, {}*)*, i8* }, { i64 (i8*, {}*)*, i8* }* %6, i32 0, i32 0
  %12 = load i64 (i8*, {}*)*, i64 (i8*, {}*)** %11
  %13 = getelementptr { i64 (i8*, {}*)*, i8* }, { i64 (i8*, {}*)*, i8* }* %6, i32 0, i32 1
  %14 = load i8*, i8** %13
  %15 = call i64 %12(i8* %14, {}* %10)
  %16 = add i64 %15, 42
  ret i32 0
}
