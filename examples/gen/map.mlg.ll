; ModuleID = './examples/map.mlg'
source_filename = "./examples/map.mlg"

declare i8* @GC_malloc(i64)

define i32 @main35() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  %3 = bitcast {}* %2 to i8*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }*
  %6 = getelementptr { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }, { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }* %5, i32 0, i32 0
  store { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)* @map37, { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }, { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }* %5, i32 0, i32 1
  store i8* %3, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = call i8* @GC_malloc(i64 0)
  %13 = bitcast i8* %12 to {}*
  %14 = bitcast {}* %13 to i8*
  %15 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %16 = bitcast i8* %15 to { i64 (i8*, i64)*, i8* }*
  %17 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %16, i32 0, i32 0
  store i64 (i8*, i64)* @add4236, i64 (i8*, i64)** %17
  %18 = call i8* @GC_malloc(i64 0)
  %19 = bitcast i8* %18 to {}*
  %20 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %16, i32 0, i32 1
  store i8* %14, i8** %20
  %21 = call i8* @GC_malloc(i64 0)
  %22 = bitcast i8* %21 to {}*
  %23 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %24 = bitcast i8* %23 to { i64, i64 }*
  %25 = getelementptr { i64, i64 }, { i64, i64 }* %24, i32 0, i32 0
  store i64 1, i64* %25
  %26 = call i8* @GC_malloc(i64 0)
  %27 = bitcast i8* %26 to {}*
  %28 = getelementptr { i64, i64 }, { i64, i64 }* %24, i32 0, i32 1
  store i64 2, i64* %28
  %29 = call i8* @GC_malloc(i64 0)
  %30 = bitcast i8* %29 to {}*
  %31 = getelementptr { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }, { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }* %5, i32 0, i32 0
  %32 = load { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)** %31
  %33 = getelementptr { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }, { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }* %5, i32 0, i32 1
  %34 = load i8*, i8** %33
  %35 = call { i64, i64 }* %32(i8* %34, { i64 (i8*, i64)*, i8* }* %16, { i64, i64 }* %24)
  %36 = getelementptr { i64, i64 }, { i64, i64 }* %35, i32 0, i32 0
  %37 = load i64, i64* %36
  %38 = call {}* @print_int38(i64 %37)
  ret i32 0
}

define i64 @add4236(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }*
  %6 = getelementptr { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }, { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }* %5, i32 0, i32 0
  store { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)* @map37, { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }, { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { i64 (i8*, i64)*, i8* }*
  %14 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %13, i32 0, i32 0
  store i64 (i8*, i64)* @add4236, i64 (i8*, i64)** %14
  %15 = call i8* @GC_malloc(i64 0)
  %16 = bitcast i8* %15 to {}*
  %17 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %13, i32 0, i32 1
  store i8* %0, i8** %17
  %18 = call i8* @GC_malloc(i64 0)
  %19 = bitcast i8* %18 to {}*
  %20 = add i64 %1, 42
  ret i64 %20
}

define { i64, i64 }* @map37(i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*) {
  %4 = bitcast i8* %0 to {}*
  %5 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %6 = bitcast i8* %5 to { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }*
  %7 = getelementptr { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }, { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }* %6, i32 0, i32 0
  store { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)* @map37, { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)** %7
  %8 = call i8* @GC_malloc(i64 0)
  %9 = bitcast i8* %8 to {}*
  %10 = getelementptr { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }, { { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %10
  %11 = call i8* @GC_malloc(i64 0)
  %12 = bitcast i8* %11 to {}*
  %13 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %14 = bitcast i8* %13 to { i64 (i8*, i64)*, i8* }*
  %15 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %14, i32 0, i32 0
  store i64 (i8*, i64)* @add4236, i64 (i8*, i64)** %15
  %16 = call i8* @GC_malloc(i64 0)
  %17 = bitcast i8* %16 to {}*
  %18 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %14, i32 0, i32 1
  store i8* %0, i8** %18
  %19 = call i8* @GC_malloc(i64 0)
  %20 = bitcast i8* %19 to {}*
  %21 = getelementptr { i64, i64 }, { i64, i64 }* %2, i32 0, i32 0
  %22 = load i64, i64* %21
  %23 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %1, i32 0, i32 0
  %24 = load i64 (i8*, i64)*, i64 (i8*, i64)** %23
  %25 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %1, i32 0, i32 1
  %26 = load i8*, i8** %25
  %27 = call i64 %24(i8* %26, i64 %22)
  %28 = getelementptr { i64, i64 }, { i64, i64 }* %2, i32 0, i32 1
  %29 = load i64, i64* %28
  %30 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %1, i32 0, i32 0
  %31 = load i64 (i8*, i64)*, i64 (i8*, i64)** %30
  %32 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %1, i32 0, i32 1
  %33 = load i8*, i8** %32
  %34 = call i64 %31(i8* %33, i64 %29)
  %35 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %36 = bitcast i8* %35 to { i64, i64 }*
  %37 = getelementptr { i64, i64 }, { i64, i64 }* %36, i32 0, i32 0
  store i64 %27, i64* %37
  %38 = call i8* @GC_malloc(i64 0)
  %39 = bitcast i8* %38 to {}*
  %40 = getelementptr { i64, i64 }, { i64, i64 }* %36, i32 0, i32 1
  store i64 %34, i64* %40
  %41 = call i8* @GC_malloc(i64 0)
  %42 = bitcast i8* %41 to {}*
  ret { i64, i64 }* %36
}

declare {}* @print_int(i64)

define {}* @print_int38(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

define i32 @main() {
  %1 = call i32 @main35()
  ret i32 0
}
