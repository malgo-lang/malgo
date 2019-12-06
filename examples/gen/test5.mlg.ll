; ModuleID = './examples/test5.mlg'
source_filename = "./examples/test5.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

define i64 @main.149() {
  %1 = getelementptr { i64 }, { i64 }* null, i64 1
  %2 = ptrtoint { i64 }* %1 to i64
  %3 = call i8* @malloc_gc(i64 %2)
  %4 = bitcast i8* %3 to { i64 }*
  %5 = getelementptr { i64 }, { i64 }* %4, i32 0, i32 0
  store i64 42, i64* %5
  %6 = bitcast { i64 }* %4 to i8*
  %7 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* null, i64 1
  %8 = ptrtoint { i64 (i8*, i64, i64)*, i8* }* %7 to i64
  %9 = call i8* @malloc_gc(i64 %8)
  %10 = bitcast i8* %9 to { i64 (i8*, i64, i64)*, i8* }*
  %11 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %10, i32 0, i32 0
  store i64 (i8*, i64, i64)* @f.41, i64 (i8*, i64, i64)** %11
  %12 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %10, i32 0, i32 1
  store i8* %6, i8** %12
  %13 = getelementptr { i64 }, { i64 }* null, i64 1
  %14 = ptrtoint { i64 }* %13 to i64
  %15 = call i8* @malloc_gc(i64 %14)
  %16 = bitcast i8* %15 to { i64 }*
  %17 = getelementptr { i64 }, { i64 }* %16, i32 0, i32 0
  store i64 42, i64* %17
  %18 = bitcast { i64 }* %16 to i8*
  %19 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* null, i64 1
  %20 = ptrtoint { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %19 to i64
  %21 = call i8* @malloc_gc(i64 %20)
  %22 = bitcast i8* %21 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*
  %23 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %22, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @k.32, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %23
  %24 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %22, i32 0, i32 1
  store i8* %18, i8** %24
  %25 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %10, i32 0, i32 0
  %26 = load i64 (i8*, i64, i64)*, i64 (i8*, i64, i64)** %25
  %27 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %10, i32 0, i32 1
  %28 = load i8*, i8** %27
  %29 = call i64 %26(i8* %28, i64 3, i64 4)
  ret i64 %29
}

define { i64 (i8*, i64)*, i8* }* @k.32(i8*, i64) {
  %3 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*
  %7 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @k.32, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %7
  %8 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i64 }*
  %10 = getelementptr { i64 }, { i64 }* %9, i32 0, i32 0
  %11 = load i64, i64* %10
  %12 = getelementptr { { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* }, { { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* }* null, i64 1
  %13 = ptrtoint { { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* }* %12 to i64
  %14 = call i8* @malloc_gc(i64 %13)
  %15 = bitcast i8* %14 to { { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* }*
  %16 = getelementptr { { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* }, { { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* }* %15, i32 0, i32 0
  store { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %6, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }** %16
  %17 = bitcast { { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* }* %15 to i8*
  %18 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* null, i64 1
  %19 = ptrtoint { i64 (i8*, i64, i64)*, i8* }* %18 to i64
  %20 = call i8* @malloc_gc(i64 %19)
  %21 = bitcast i8* %20 to { i64 (i8*, i64, i64)*, i8* }*
  %22 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %21, i32 0, i32 0
  store i64 (i8*, i64, i64)* @f.34, i64 (i8*, i64, i64)** %22
  %23 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %21, i32 0, i32 1
  store i8* %17, i8** %23
  %24 = getelementptr { i64 }, { i64 }* null, i64 1
  %25 = ptrtoint { i64 }* %24 to i64
  %26 = call i8* @malloc_gc(i64 %25)
  %27 = bitcast i8* %26 to { i64 }*
  %28 = getelementptr { i64 }, { i64 }* %27, i32 0, i32 0
  store i64 %11, i64* %28
  %29 = bitcast { i64 }* %27 to i8*
  %30 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %31 = ptrtoint { i64 (i8*, i64)*, i8* }* %30 to i64
  %32 = call i8* @malloc_gc(i64 %31)
  %33 = bitcast i8* %32 to { i64 (i8*, i64)*, i8* }*
  %34 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %33, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.38", i64 (i8*, i64)** %34
  %35 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %33, i32 0, i32 1
  store i8* %29, i8** %35
  ret { i64 (i8*, i64)*, i8* }* %33
}

declare i64 @add_i64(i64, i64)

define i64 @"$lambda.38"(i8*, i64) {
  %3 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i64 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }*
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.38", i64 (i8*, i64)** %7
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i64 }*
  %10 = getelementptr { i64 }, { i64 }* %9, i32 0, i32 0
  %11 = load i64, i64* %10
  %12 = call i64 @add_i64(i64 %1, i64 %11)
  ret i64 %12
}

define i64 @f.34(i8*, i64, i64) {
  %4 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* null, i64 1
  %5 = ptrtoint { i64 (i8*, i64, i64)*, i8* }* %4 to i64
  %6 = call i8* @malloc_gc(i64 %5)
  %7 = bitcast i8* %6 to { i64 (i8*, i64, i64)*, i8* }*
  %8 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %7, i32 0, i32 0
  store i64 (i8*, i64, i64)* @f.34, i64 (i8*, i64, i64)** %8
  %9 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = bitcast i8* %0 to { { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* }*
  %11 = getelementptr { { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* }, { { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* }* %10, i32 0, i32 0
  %12 = load { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }** %11
  %13 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %12, i32 0, i32 0
  %14 = load { i64 (i8*, i64)*, i8* }* (i8*, i64)*, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %13
  %15 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %12, i32 0, i32 1
  %16 = load i8*, i8** %15
  %17 = call { i64 (i8*, i64)*, i8* }* %14(i8* %16, i64 %1)
  %18 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %17, i32 0, i32 0
  %19 = load i64 (i8*, i64)*, i64 (i8*, i64)** %18
  %20 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %17, i32 0, i32 1
  %21 = load i8*, i8** %20
  %22 = call i64 %19(i8* %21, i64 %2)
  ret i64 %22
}

define i64 @f.41(i8*, i64, i64) {
  %4 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* null, i64 1
  %5 = ptrtoint { i64 (i8*, i64, i64)*, i8* }* %4 to i64
  %6 = call i8* @malloc_gc(i64 %5)
  %7 = bitcast i8* %6 to { i64 (i8*, i64, i64)*, i8* }*
  %8 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %7, i32 0, i32 0
  store i64 (i8*, i64, i64)* @f.41, i64 (i8*, i64, i64)** %8
  %9 = getelementptr { i64 (i8*, i64, i64)*, i8* }, { i64 (i8*, i64, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = bitcast i8* %0 to { i64 }*
  %11 = getelementptr { i64 }, { i64 }* %10, i32 0, i32 0
  %12 = load i64, i64* %11
  %13 = getelementptr { i64 }, { i64 }* null, i64 1
  %14 = ptrtoint { i64 }* %13 to i64
  %15 = call i8* @malloc_gc(i64 %14)
  %16 = bitcast i8* %15 to { i64 }*
  %17 = getelementptr { i64 }, { i64 }* %16, i32 0, i32 0
  store i64 %12, i64* %17
  %18 = bitcast { i64 }* %16 to i8*
  %19 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* null, i64 1
  %20 = ptrtoint { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %19 to i64
  %21 = call i8* @malloc_gc(i64 %20)
  %22 = bitcast i8* %21 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*
  %23 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %22, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @k.44, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %23
  %24 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %22, i32 0, i32 1
  store i8* %18, i8** %24
  %25 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %22, i32 0, i32 0
  %26 = load { i64 (i8*, i64)*, i8* }* (i8*, i64)*, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %25
  %27 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %22, i32 0, i32 1
  %28 = load i8*, i8** %27
  %29 = call { i64 (i8*, i64)*, i8* }* %26(i8* %28, i64 %1)
  %30 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %29, i32 0, i32 0
  %31 = load i64 (i8*, i64)*, i64 (i8*, i64)** %30
  %32 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %29, i32 0, i32 1
  %33 = load i8*, i8** %32
  %34 = call i64 %31(i8* %33, i64 %2)
  ret i64 %34
}

define { i64 (i8*, i64)*, i8* }* @k.44(i8*, i64) {
  %3 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*
  %7 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @k.44, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %7
  %8 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i64 }*
  %10 = getelementptr { i64 }, { i64 }* %9, i32 0, i32 0
  %11 = load i64, i64* %10
  %12 = getelementptr { i64 }, { i64 }* null, i64 1
  %13 = ptrtoint { i64 }* %12 to i64
  %14 = call i8* @malloc_gc(i64 %13)
  %15 = bitcast i8* %14 to { i64 }*
  %16 = getelementptr { i64 }, { i64 }* %15, i32 0, i32 0
  store i64 %11, i64* %16
  %17 = bitcast { i64 }* %15 to i8*
  %18 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %19 = ptrtoint { i64 (i8*, i64)*, i8* }* %18 to i64
  %20 = call i8* @malloc_gc(i64 %19)
  %21 = bitcast i8* %20 to { i64 (i8*, i64)*, i8* }*
  %22 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %21, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.46", i64 (i8*, i64)** %22
  %23 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %21, i32 0, i32 1
  store i8* %17, i8** %23
  ret { i64 (i8*, i64)*, i8* }* %21
}

define i64 @"$lambda.46"(i8*, i64) {
  %3 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i64 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }*
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.46", i64 (i8*, i64)** %7
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i64 }*
  %10 = getelementptr { i64 }, { i64 }* %9, i32 0, i32 0
  %11 = load i64, i64* %10
  %12 = call i64 @add_i64(i64 %1, i64 %11)
  ret i64 %12
}

define i32 @main() {
  call void @init_gc()
  %1 = call i64 @main.149()
  ret i32 0
}
