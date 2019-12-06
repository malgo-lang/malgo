; ModuleID = './examples/curry.mlg'
source_filename = "./examples/curry.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

define i64 @main.53() {
  %1 = getelementptr {}, {}* null, i64 1
  %2 = ptrtoint {}* %1 to i64
  %3 = call i8* @malloc_gc(i64 %2)
  %4 = bitcast i8* %3 to {}*
  %5 = bitcast {}* %4 to i8*
  %6 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* null, i64 1
  %7 = ptrtoint { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %6 to i64
  %8 = call i8* @malloc_gc(i64 %7)
  %9 = bitcast i8* %8 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*
  %10 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %9, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @"$lambda.20", { i64 (i8*, i64)*, i8* }* (i8*, i64)** %10
  %11 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %5, i8** %11
  %12 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %9, i32 0, i32 0
  %13 = load { i64 (i8*, i64)*, i8* }* (i8*, i64)*, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %12
  %14 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %9, i32 0, i32 1
  %15 = load i8*, i8** %14
  %16 = call { i64 (i8*, i64)*, i8* }* %13(i8* %15, i64 1)
  %17 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %16, i32 0, i32 0
  %18 = load i64 (i8*, i64)*, i64 (i8*, i64)** %17
  %19 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %16, i32 0, i32 1
  %20 = load i8*, i8** %19
  %21 = call i64 %18(i8* %20, i64 2)
  ret i64 %21
}

define { i64 (i8*, i64)*, i8* }* @"$lambda.20"(i8*, i64) {
  %3 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*
  %7 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @"$lambda.20", { i64 (i8*, i64)*, i8* }* (i8*, i64)** %7
  %8 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to {}*
  %10 = getelementptr { i64 }, { i64 }* null, i64 1
  %11 = ptrtoint { i64 }* %10 to i64
  %12 = call i8* @malloc_gc(i64 %11)
  %13 = bitcast i8* %12 to { i64 }*
  %14 = getelementptr { i64 }, { i64 }* %13, i32 0, i32 0
  store i64 %1, i64* %14
  %15 = bitcast { i64 }* %13 to i8*
  %16 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %17 = ptrtoint { i64 (i8*, i64)*, i8* }* %16 to i64
  %18 = call i8* @malloc_gc(i64 %17)
  %19 = bitcast i8* %18 to { i64 (i8*, i64)*, i8* }*
  %20 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %19, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.22", i64 (i8*, i64)** %20
  %21 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %19, i32 0, i32 1
  store i8* %15, i8** %21
  ret { i64 (i8*, i64)*, i8* }* %19
}

declare i64 @add_i64(i64, i64)

define i64 @"$lambda.22"(i8*, i64) {
  %3 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i64 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }*
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.22", i64 (i8*, i64)** %7
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i64 }*
  %10 = getelementptr { i64 }, { i64 }* %9, i32 0, i32 0
  %11 = load i64, i64* %10
  %12 = call i64 @add_i64(i64 %11, i64 %1)
  ret i64 %12
}

define i32 @main() {
  call void @init_gc()
  %1 = call i64 @main.53()
  ret i32 0
}
