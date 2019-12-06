; ModuleID = './examples/map.mlg'
source_filename = "./examples/map.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

define {} @main.97() {
  %1 = getelementptr {}, {}* null, i64 1
  %2 = ptrtoint {}* %1 to i64
  %3 = call i8* @malloc_gc(i64 %2)
  %4 = bitcast i8* %3 to {}*
  %5 = bitcast {}* %4 to i8*
  %6 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %7 = ptrtoint { i64 (i8*, i64)*, i8* }* %6 to i64
  %8 = call i8* @malloc_gc(i64 %7)
  %9 = bitcast i8* %8 to { i64 (i8*, i64)*, i8* }*
  %10 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 0
  store i64 (i8*, i64)* @add42.63, i64 (i8*, i64)** %10
  %11 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %5, i8** %11
  %12 = getelementptr { i64, i64 }, { i64, i64 }* null, i64 1
  %13 = ptrtoint { i64, i64 }* %12 to i64
  %14 = call i8* @malloc_gc(i64 %13)
  %15 = bitcast i8* %14 to { i64, i64 }*
  %16 = getelementptr { i64, i64 }, { i64, i64 }* %15, i32 0, i32 0
  store i64 1, i64* %16
  %17 = getelementptr { i64, i64 }, { i64, i64 }* %15, i32 0, i32 1
  store i64 2, i64* %17
  %18 = call { i64, i64 }* @map.52({ i64 (i8*, i64)*, i8* }* %9, { i64, i64 }* %15)
  %19 = getelementptr { i64, i64 }, { i64, i64 }* %18, i32 0, i32 0
  %20 = load i64, i64* %19
  %21 = call {} @print_int.49(i64 %20)
  ret {} %21
}

define { i64, i64 }* @map.52({ i64 (i8*, i64)*, i8* }*, { i64, i64 }*) {
  %3 = getelementptr { i64, i64 }, { i64, i64 }* %1, i32 0, i32 0
  %4 = load i64, i64* %3
  %5 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %0, i32 0, i32 0
  %6 = load i64 (i8*, i64)*, i64 (i8*, i64)** %5
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %0, i32 0, i32 1
  %8 = load i8*, i8** %7
  %9 = call i64 %6(i8* %8, i64 %4)
  %10 = getelementptr { i64, i64 }, { i64, i64 }* %1, i32 0, i32 1
  %11 = load i64, i64* %10
  %12 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %0, i32 0, i32 0
  %13 = load i64 (i8*, i64)*, i64 (i8*, i64)** %12
  %14 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %0, i32 0, i32 1
  %15 = load i8*, i8** %14
  %16 = call i64 %13(i8* %15, i64 %11)
  %17 = getelementptr { i64, i64 }, { i64, i64 }* null, i64 1
  %18 = ptrtoint { i64, i64 }* %17 to i64
  %19 = call i8* @malloc_gc(i64 %18)
  %20 = bitcast i8* %19 to { i64, i64 }*
  %21 = getelementptr { i64, i64 }, { i64, i64 }* %20, i32 0, i32 0
  store i64 %9, i64* %21
  %22 = getelementptr { i64, i64 }, { i64, i64 }* %20, i32 0, i32 1
  store i64 %16, i64* %22
  ret { i64, i64 }* %20
}

declare i64 @add_i64(i64, i64)

define i64 @add42.55(i64) {
  %2 = call i64 @add_i64(i64 %0, i64 42)
  ret i64 %2
}

define i64 @add42.63(i8*, i64) {
  %3 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i64 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }*
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, i64)* @add42.63, i64 (i8*, i64)** %7
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to {}*
  %10 = call i64 @add_i64(i64 %1, i64 42)
  ret i64 %10
}

define { i64, i64 }* @map.65({ i64 (i8*, i64)*, i8* }*, { i64, i64 }*) {
  %3 = getelementptr { i64, i64 }, { i64, i64 }* %1, i32 0, i32 0
  %4 = load i64, i64* %3
  %5 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %0, i32 0, i32 0
  %6 = load i64 (i8*, i64)*, i64 (i8*, i64)** %5
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %0, i32 0, i32 1
  %8 = load i8*, i8** %7
  %9 = call i64 %6(i8* %8, i64 %4)
  %10 = getelementptr { i64, i64 }, { i64, i64 }* %1, i32 0, i32 1
  %11 = load i64, i64* %10
  %12 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %0, i32 0, i32 0
  %13 = load i64 (i8*, i64)*, i64 (i8*, i64)** %12
  %14 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %0, i32 0, i32 1
  %15 = load i8*, i8** %14
  %16 = call i64 %13(i8* %15, i64 %11)
  %17 = getelementptr { i64, i64 }, { i64, i64 }* null, i64 1
  %18 = ptrtoint { i64, i64 }* %17 to i64
  %19 = call i8* @malloc_gc(i64 %18)
  %20 = bitcast i8* %19 to { i64, i64 }*
  %21 = getelementptr { i64, i64 }, { i64, i64 }* %20, i32 0, i32 0
  store i64 %9, i64* %21
  %22 = getelementptr { i64, i64 }, { i64, i64 }* %20, i32 0, i32 1
  store i64 %16, i64* %22
  ret { i64, i64 }* %20
}

declare {} @print_int(i64)

define {} @print_int.49(i64) {
  %2 = call {} @print_int(i64 %0)
  ret {} %2
}

define i32 @main() {
  call void @init_gc()
  %1 = call {} @main.97()
  ret i32 0
}
