; ModuleID = './examples/map.mlg'
source_filename = "./examples/map.mlg"

declare {}* @print_int(i64)

define {}* @print_int0(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

declare i8* @GC_malloc(i64)

define { i64, i64 }* @map1(i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*) {
  %4 = bitcast i8* %0 to {}*
  %5 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %6 = bitcast i8* %5 to { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }*
  %7 = getelementptr { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }, { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }* %6, i32 0, i32 0
  store { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)* @map1, { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)** %7
  %8 = getelementptr { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }, { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %10 = bitcast i8* %9 to { i64 (i8*, i64)*, i8* }*
  %11 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %10, i32 0, i32 0
  store i64 (i8*, i64)* @add422, i64 (i8*, i64)** %11
  %12 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %10, i32 0, i32 1
  store i8* %0, i8** %12
  %13 = getelementptr { i64, i64 }, { i64, i64 }* %2, i32 0, i32 0
  %14 = load i64, i64* %13
  %15 = getelementptr { i64, i64 }, { i64, i64 }* %2, i32 0, i32 1
  %16 = load i64, i64* %15
  %17 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %1, i32 0, i32 0
  %18 = load i64 (i8*, i64)*, i64 (i8*, i64)** %17
  %19 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %1, i32 0, i32 1
  %20 = load i8*, i8** %19
  %21 = call i64 %18(i8* %20, i64 %14)
  %22 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %1, i32 0, i32 0
  %23 = load i64 (i8*, i64)*, i64 (i8*, i64)** %22
  %24 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %1, i32 0, i32 1
  %25 = load i8*, i8** %24
  %26 = call i64 %23(i8* %25, i64 %16)
  %27 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %28 = bitcast i8* %27 to { i64, i64 }*
  %29 = getelementptr { i64, i64 }, { i64, i64 }* %28, i32 0, i32 0
  store i64 %21, i64* %29
  %30 = getelementptr { i64, i64 }, { i64, i64 }* %28, i32 0, i32 1
  store i64 %26, i64* %30
  ret { i64, i64 }* %28
}

define i64 @add422(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }*
  %6 = getelementptr { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }, { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }* %5, i32 0, i32 0
  store { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)* @map1, { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)** %6
  %7 = getelementptr { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }, { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %7
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i64 (i8*, i64)*, i8* }*
  %10 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 0
  store i64 (i8*, i64)* @add422, i64 (i8*, i64)** %10
  %11 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = add i64 %1, 42
  ret i64 %12
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  %3 = bitcast {}* %2 to i8*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }*
  %6 = getelementptr { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }, { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }* %5, i32 0, i32 0
  store { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)* @map1, { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)** %6
  %7 = getelementptr { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }, { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }* %5, i32 0, i32 1
  store i8* %3, i8** %7
  %8 = call i8* @GC_malloc(i64 0)
  %9 = bitcast i8* %8 to {}*
  %10 = bitcast {}* %9 to i8*
  %11 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %12 = bitcast i8* %11 to { i64 (i8*, i64)*, i8* }*
  %13 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %12, i32 0, i32 0
  store i64 (i8*, i64)* @add422, i64 (i8*, i64)** %13
  %14 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %12, i32 0, i32 1
  store i8* %10, i8** %14
  %15 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %16 = bitcast i8* %15 to { i64, i64 }*
  %17 = getelementptr { i64, i64 }, { i64, i64 }* %16, i32 0, i32 0
  store i64 1, i64* %17
  %18 = getelementptr { i64, i64 }, { i64, i64 }* %16, i32 0, i32 1
  store i64 2, i64* %18
  %19 = getelementptr { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }, { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }* %5, i32 0, i32 0
  %20 = load { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)** %19
  %21 = getelementptr { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }, { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }* %5, i32 0, i32 1
  %22 = load i8*, i8** %21
  %23 = call { i64, i64 }* %20(i8* %22, { i64 (i8*, i64)*, i8* }* %12, { i64, i64 }* %16)
  %24 = getelementptr { i64, i64 }, { i64, i64 }* %23, i32 0, i32 0
  %25 = load i64, i64* %24
  %26 = getelementptr { i64, i64 }, { i64, i64 }* %23, i32 0, i32 1
  %27 = load i64, i64* %26
  %28 = call {}* @print_int0(i64 %25)
  ret i32 0
}
