; ModuleID = './examples/test5.mlg'
source_filename = "./examples/test5.mlg"

declare i8* @GC_malloc(i64)

define i64 @f.20(i8*, i64, i64) {
  %4 = bitcast i8* %0 to { i64 }*
  %5 = getelementptr { i64 }, { i64 }* %4, i32 0, i32 0
  %6 = load i64, i64* %5
  %7 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %8 = bitcast i8* %7 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*
  %9 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %8, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @k.15, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %9
  %10 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %8, i32 0, i32 1
  store i8* %0, i8** %10
  %11 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %12 = bitcast i8* %11 to { i64 (i8*, i64, i64)*, i8* }*
  %13 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %12, i32 0, i32 0
  store i64 (i8*, i64, i64)* @f.20, i64 (i8*, i64, i64)** %13
  %14 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %12, i32 0, i32 1
  store i8* %0, i8** %14
  %15 = call { i64 (i8*, i64)*, i8* }* @k.15(i8* %0, i64 %1)
  %16 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %15, i32 0, i32 0
  %17 = load i64 (i8*, i64)*, i64 (i8*, i64)** %16
  %18 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %15, i32 0, i32 1
  %19 = load i8*, i8** %18
  %20 = call i64 %17(i8* %19, i64 %2)
  ret i64 %20
}

define { i64 (i8*, i64)*, i8* }* @k.15(i8*, i64) {
  %3 = bitcast i8* %0 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*
  %8 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %7, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @k.15, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %8
  %9 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %11 = bitcast i8* %10 to { i64 (i8*, i64, i64)*, i8* }*
  %12 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %11, i32 0, i32 0
  store i64 (i8*, i64, i64)* @f.20, i64 (i8*, i64, i64)** %12
  %13 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %0, i8** %13
  %14 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %15 = bitcast i8* %14 to { i64 }*
  %16 = getelementptr { i64 }, { i64 }* %15, i32 0, i32 0
  store i64 %5, i64* %16
  %17 = bitcast { i64 }* %15 to i8*
  %18 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %19 = bitcast i8* %18 to { i64 (i8*, i64)*, i8* }*
  %20 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %19, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.22", i64 (i8*, i64)** %20
  %21 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %19, i32 0, i32 1
  store i8* %17, i8** %21
  ret { i64 (i8*, i64)*, i8* }* %19
}

define i64 @"$lambda.22"(i8*, i64) {
  %3 = bitcast i8* %0 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { i64 (i8*, i64)*, i8* }*
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.22", i64 (i8*, i64)** %8
  %9 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = add i64 %1, %5
  ret i64 %10
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %2 = bitcast i8* %1 to { i64 }*
  %3 = getelementptr { i64 }, { i64 }* %2, i32 0, i32 0
  store i64 42, i64* %3
  %4 = bitcast { i64 }* %2 to i8*
  %5 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %6 = bitcast i8* %5 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*
  %7 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @k.15, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %7
  %8 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %4, i8** %8
  %9 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %10 = bitcast i8* %9 to { i64 }*
  %11 = getelementptr { i64 }, { i64 }* %10, i32 0, i32 0
  store i64 42, i64* %11
  %12 = bitcast { i64 }* %10 to i8*
  %13 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %14 = bitcast i8* %13 to { i64 (i8*, i64, i64)*, i8* }*
  %15 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %14, i32 0, i32 0
  store i64 (i8*, i64, i64)* @f.20, i64 (i8*, i64, i64)** %15
  %16 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %14, i32 0, i32 1
  store i8* %12, i8** %16
  %17 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %14, i32 0, i32 0
  %18 = load i64 (i8*, i64, i64)*, i64 (i8*, i64, i64)** %17
  %19 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %14, i32 0, i32 1
  %20 = load i8*, i8** %19
  %21 = call i64 %18(i8* %20, i64 3, i64 4)
  ret i32 0
}
