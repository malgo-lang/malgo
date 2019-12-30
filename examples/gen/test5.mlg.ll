; ModuleID = './examples/test5.mlg'
source_filename = "./examples/test5.mlg"

declare i8* @GC_malloc(i64)

define i64 @"$lambda28"(i8*, i64) {
  %3 = bitcast i8* %0 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { i64 (i8*, i64)*, i8* }*
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda28", i64 (i8*, i64)** %8
  %9 = call i8* @GC_malloc(i64 0)
  %10 = bitcast i8* %9 to {}*
  %11 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = call i8* @GC_malloc(i64 0)
  %13 = bitcast i8* %12 to {}*
  %14 = add i64 %1, %5
  ret i64 %14
}

define { i64 (i8*, i64)*, i8* }* @k27(i8*, i64) {
  %3 = bitcast i8* %0 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*
  %8 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %7, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @k27, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %8
  %9 = call i8* @GC_malloc(i64 0)
  %10 = bitcast i8* %9 to {}*
  %11 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = call i8* @GC_malloc(i64 0)
  %13 = bitcast i8* %12 to {}*
  %14 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %15 = bitcast i8* %14 to { i64 (i8*, i64, i64)*, i8* }*
  %16 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %15, i32 0, i32 0
  store i64 (i8*, i64, i64)* @f26, i64 (i8*, i64, i64)** %16
  %17 = call i8* @GC_malloc(i64 0)
  %18 = bitcast i8* %17 to {}*
  %19 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %15, i32 0, i32 1
  store i8* %0, i8** %19
  %20 = call i8* @GC_malloc(i64 0)
  %21 = bitcast i8* %20 to {}*
  %22 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %23 = bitcast i8* %22 to { i64 }*
  %24 = getelementptr { i64 }, { i64 }* %23, i32 0, i32 0
  store i64 %5, i64* %24
  %25 = call i8* @GC_malloc(i64 0)
  %26 = bitcast i8* %25 to {}*
  %27 = bitcast { i64 }* %23 to i8*
  %28 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %29 = bitcast i8* %28 to { i64 (i8*, i64)*, i8* }*
  %30 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %29, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda28", i64 (i8*, i64)** %30
  %31 = call i8* @GC_malloc(i64 0)
  %32 = bitcast i8* %31 to {}*
  %33 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %29, i32 0, i32 1
  store i8* %27, i8** %33
  %34 = call i8* @GC_malloc(i64 0)
  %35 = bitcast i8* %34 to {}*
  ret { i64 (i8*, i64)*, i8* }* %29
}

define i64 @f26(i8*, i64, i64) {
  %4 = bitcast i8* %0 to { i64 }*
  %5 = getelementptr { i64 }, { i64 }* %4, i32 0, i32 0
  %6 = load i64, i64* %5
  %7 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %8 = bitcast i8* %7 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*
  %9 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %8, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @k27, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %8, i32 0, i32 1
  store i8* %0, i8** %12
  %13 = call i8* @GC_malloc(i64 0)
  %14 = bitcast i8* %13 to {}*
  %15 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %16 = bitcast i8* %15 to { i64 (i8*, i64, i64)*, i8* }*
  %17 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %16, i32 0, i32 0
  store i64 (i8*, i64, i64)* @f26, i64 (i8*, i64, i64)** %17
  %18 = call i8* @GC_malloc(i64 0)
  %19 = bitcast i8* %18 to {}*
  %20 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %16, i32 0, i32 1
  store i8* %0, i8** %20
  %21 = call i8* @GC_malloc(i64 0)
  %22 = bitcast i8* %21 to {}*
  %23 = call { i64 (i8*, i64)*, i8* }* @k27(i8* %0, i64 %1)
  %24 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %23, i32 0, i32 0
  %25 = load i64 (i8*, i64)*, i64 (i8*, i64)** %24
  %26 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %23, i32 0, i32 1
  %27 = load i8*, i8** %26
  %28 = call i64 %25(i8* %27, i64 %2)
  ret i64 %28
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %2 = bitcast i8* %1 to { i64 }*
  %3 = getelementptr { i64 }, { i64 }* %2, i32 0, i32 0
  store i64 42, i64* %3
  %4 = call i8* @GC_malloc(i64 0)
  %5 = bitcast i8* %4 to {}*
  %6 = bitcast { i64 }* %2 to i8*
  %7 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %8 = bitcast i8* %7 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*
  %9 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %8, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @k27, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %8, i32 0, i32 1
  store i8* %6, i8** %12
  %13 = call i8* @GC_malloc(i64 0)
  %14 = bitcast i8* %13 to {}*
  %15 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %16 = bitcast i8* %15 to { i64 }*
  %17 = getelementptr { i64 }, { i64 }* %16, i32 0, i32 0
  store i64 42, i64* %17
  %18 = call i8* @GC_malloc(i64 0)
  %19 = bitcast i8* %18 to {}*
  %20 = bitcast { i64 }* %16 to i8*
  %21 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %22 = bitcast i8* %21 to { i64 (i8*, i64, i64)*, i8* }*
  %23 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %22, i32 0, i32 0
  store i64 (i8*, i64, i64)* @f26, i64 (i8*, i64, i64)** %23
  %24 = call i8* @GC_malloc(i64 0)
  %25 = bitcast i8* %24 to {}*
  %26 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %22, i32 0, i32 1
  store i8* %20, i8** %26
  %27 = call i8* @GC_malloc(i64 0)
  %28 = bitcast i8* %27 to {}*
  %29 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %22, i32 0, i32 0
  %30 = load i64 (i8*, i64, i64)*, i64 (i8*, i64, i64)** %29
  %31 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %22, i32 0, i32 1
  %32 = load i8*, i8** %31
  %33 = call i64 %30(i8* %32, i64 3, i64 4)
  ret i32 0
}
