; ModuleID = './examples/cls.mlg'
source_filename = "./examples/cls.mlg"

declare i8* @GC_malloc(i64)

define i64 @add_inner2(i8*, i64) {
  %3 = bitcast i8* %0 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { i64 (i8*, i64)*, i8* }*
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 0
  store i64 (i8*, i64)* @add_inner2, i64 (i8*, i64)** %8
  %9 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = add i64 %5, %1
  ret i64 %10
}

define { i64 (i8*, i64)*, i8* }* @add0(i64) {
  %2 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %3 = bitcast i8* %2 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  store i64 %0, i64* %4
  %5 = bitcast { i64 }* %3 to i8*
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { i64 (i8*, i64)*, i8* }*
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 0
  store i64 (i8*, i64)* @add_inner2, i64 (i8*, i64)** %8
  %9 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %5, i8** %9
  ret { i64 (i8*, i64)*, i8* }* %7
}

define i32 @main() {
  %1 = call { i64 (i8*, i64)*, i8* }* @add0(i64 3)
  %2 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %1, i32 0, i32 0
  %3 = load i64 (i8*, i64)*, i64 (i8*, i64)** %2
  %4 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %1, i32 0, i32 1
  %5 = load i8*, i8** %4
  %6 = call i64 %3(i8* %5, i64 4)
  ret i32 0
}
