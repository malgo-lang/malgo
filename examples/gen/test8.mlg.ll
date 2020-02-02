; ModuleID = './examples/test8.mlg'
source_filename = "./examples/test8.mlg"

declare i8* @GC_malloc(i64)

define i64 @g2(i8*, i64) {
  %3 = bitcast i8* %0 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { i64 (i8*, i64)*, i8* }*
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 0
  store i64 (i8*, i64)* @f1, i64 (i8*, i64)** %8
  %9 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %11 = bitcast i8* %10 to { i64 (i8*, i64)*, i8* }*
  %12 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %11, i32 0, i32 0
  store i64 (i8*, i64)* @g2, i64 (i8*, i64)** %12
  %13 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %0, i8** %13
  %14 = call i64 @f1(i8* %0, i64 %5)
  %15 = add i64 %14, %1
  ret i64 %15
}

define i64 @f1(i8*, i64) {
  %3 = bitcast i8* %0 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { i64 (i8*, i64)*, i8* }*
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 0
  store i64 (i8*, i64)* @f1, i64 (i8*, i64)** %8
  %9 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %11 = bitcast i8* %10 to { i64 (i8*, i64)*, i8* }*
  %12 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %11, i32 0, i32 0
  store i64 (i8*, i64)* @g2, i64 (i8*, i64)** %12
  %13 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %0, i8** %13
  %14 = add i64 %1, %5
  ret i64 %14
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %2 = bitcast i8* %1 to { i64 }*
  %3 = getelementptr { i64 }, { i64 }* %2, i32 0, i32 0
  store i64 42, i64* %3
  %4 = bitcast { i64 }* %2 to i8*
  %5 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }*
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, i64)* @f1, i64 (i8*, i64)** %7
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %4, i8** %8
  %9 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %10 = bitcast i8* %9 to { i64 }*
  %11 = getelementptr { i64 }, { i64 }* %10, i32 0, i32 0
  store i64 42, i64* %11
  %12 = bitcast { i64 }* %10 to i8*
  %13 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %14 = bitcast i8* %13 to { i64 (i8*, i64)*, i8* }*
  %15 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %14, i32 0, i32 0
  store i64 (i8*, i64)* @g2, i64 (i8*, i64)** %15
  %16 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %14, i32 0, i32 1
  store i8* %12, i8** %16
  %17 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %14, i32 0, i32 0
  %18 = load i64 (i8*, i64)*, i64 (i8*, i64)** %17
  %19 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %14, i32 0, i32 1
  %20 = load i8*, i8** %19
  %21 = call i64 %18(i8* %20, i64 4)
  %22 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  %23 = load i64 (i8*, i64)*, i64 (i8*, i64)** %22
  %24 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  %25 = load i8*, i8** %24
  %26 = call i64 %23(i8* %25, i64 5)
  %27 = add i64 %21, %26
  ret i32 0
}
