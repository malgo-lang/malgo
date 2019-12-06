; ModuleID = './examples/test8.mlg'
source_filename = "./examples/test8.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

declare i64 @add_i64(i64, i64)

define i64 @main.109() {
  %1 = getelementptr { i64 }, { i64 }* null, i64 1
  %2 = ptrtoint { i64 }* %1 to i64
  %3 = call i8* @malloc_gc(i64 %2)
  %4 = bitcast i8* %3 to { i64 }*
  %5 = getelementptr { i64 }, { i64 }* %4, i32 0, i32 0
  store i64 42, i64* %5
  %6 = bitcast { i64 }* %4 to i8*
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %8 = ptrtoint { i64 (i8*, i64)*, i8* }* %7 to i64
  %9 = call i8* @malloc_gc(i64 %8)
  %10 = bitcast i8* %9 to { i64 (i8*, i64)*, i8* }*
  %11 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %10, i32 0, i32 0
  store i64 (i8*, i64)* @g.42, i64 (i8*, i64)** %11
  %12 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %10, i32 0, i32 1
  store i8* %6, i8** %12
  %13 = getelementptr { i64 }, { i64 }* null, i64 1
  %14 = ptrtoint { i64 }* %13 to i64
  %15 = call i8* @malloc_gc(i64 %14)
  %16 = bitcast i8* %15 to { i64 }*
  %17 = getelementptr { i64 }, { i64 }* %16, i32 0, i32 0
  store i64 42, i64* %17
  %18 = bitcast { i64 }* %16 to i8*
  %19 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %20 = ptrtoint { i64 (i8*, i64)*, i8* }* %19 to i64
  %21 = call i8* @malloc_gc(i64 %20)
  %22 = bitcast i8* %21 to { i64 (i8*, i64)*, i8* }*
  %23 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %22, i32 0, i32 0
  store i64 (i8*, i64)* @f.35, i64 (i8*, i64)** %23
  %24 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %22, i32 0, i32 1
  store i8* %18, i8** %24
  %25 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %10, i32 0, i32 0
  %26 = load i64 (i8*, i64)*, i64 (i8*, i64)** %25
  %27 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %10, i32 0, i32 1
  %28 = load i8*, i8** %27
  %29 = call i64 %26(i8* %28, i64 4)
  %30 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %22, i32 0, i32 0
  %31 = load i64 (i8*, i64)*, i64 (i8*, i64)** %30
  %32 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %22, i32 0, i32 1
  %33 = load i8*, i8** %32
  %34 = call i64 %31(i8* %33, i64 5)
  %35 = call i64 @add_i64(i64 %29, i64 %34)
  ret i64 %35
}

define i64 @f.35(i8*, i64) {
  %3 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i64 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }*
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, i64)* @f.35, i64 (i8*, i64)** %7
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i64 }*
  %10 = getelementptr { i64 }, { i64 }* %9, i32 0, i32 0
  %11 = load i64, i64* %10
  %12 = getelementptr { i64, { i64 (i8*, i64)*, i8* }* }, { i64, { i64 (i8*, i64)*, i8* }* }* null, i64 1
  %13 = ptrtoint { i64, { i64 (i8*, i64)*, i8* }* }* %12 to i64
  %14 = call i8* @malloc_gc(i64 %13)
  %15 = bitcast i8* %14 to { i64, { i64 (i8*, i64)*, i8* }* }*
  %16 = getelementptr { i64, { i64 (i8*, i64)*, i8* }* }, { i64, { i64 (i8*, i64)*, i8* }* }* %15, i32 0, i32 0
  store i64 %11, i64* %16
  %17 = getelementptr { i64, { i64 (i8*, i64)*, i8* }* }, { i64, { i64 (i8*, i64)*, i8* }* }* %15, i32 0, i32 1
  store { i64 (i8*, i64)*, i8* }* %6, { i64 (i8*, i64)*, i8* }** %17
  %18 = bitcast { i64, { i64 (i8*, i64)*, i8* }* }* %15 to i8*
  %19 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %20 = ptrtoint { i64 (i8*, i64)*, i8* }* %19 to i64
  %21 = call i8* @malloc_gc(i64 %20)
  %22 = bitcast i8* %21 to { i64 (i8*, i64)*, i8* }*
  %23 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %22, i32 0, i32 0
  store i64 (i8*, i64)* @g.37, i64 (i8*, i64)** %23
  %24 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %22, i32 0, i32 1
  store i8* %18, i8** %24
  %25 = call i64 @add_i64(i64 %1, i64 %11)
  ret i64 %25
}

define i64 @g.37(i8*, i64) {
  %3 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i64 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }*
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, i64)* @g.37, i64 (i8*, i64)** %7
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i64, { i64 (i8*, i64)*, i8* }* }*
  %10 = getelementptr { i64, { i64 (i8*, i64)*, i8* }* }, { i64, { i64 (i8*, i64)*, i8* }* }* %9, i32 0, i32 0
  %11 = load i64, i64* %10
  %12 = getelementptr { i64, { i64 (i8*, i64)*, i8* }* }, { i64, { i64 (i8*, i64)*, i8* }* }* %9, i32 0, i32 1
  %13 = load { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %12
  %14 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %13, i32 0, i32 0
  %15 = load i64 (i8*, i64)*, i64 (i8*, i64)** %14
  %16 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %13, i32 0, i32 1
  %17 = load i8*, i8** %16
  %18 = call i64 %15(i8* %17, i64 %11)
  %19 = call i64 @add_i64(i64 %18, i64 %1)
  ret i64 %19
}

define i64 @g.42(i8*, i64) {
  %3 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i64 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }*
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, i64)* @g.42, i64 (i8*, i64)** %7
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 1
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
  store i64 (i8*, i64)* @f.44, i64 (i8*, i64)** %22
  %23 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %21, i32 0, i32 1
  store i8* %17, i8** %23
  %24 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %21, i32 0, i32 0
  %25 = load i64 (i8*, i64)*, i64 (i8*, i64)** %24
  %26 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %21, i32 0, i32 1
  %27 = load i8*, i8** %26
  %28 = call i64 %25(i8* %27, i64 %11)
  %29 = call i64 @add_i64(i64 %28, i64 %1)
  ret i64 %29
}

define i64 @f.44(i8*, i64) {
  %3 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i64 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }*
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, i64)* @f.44, i64 (i8*, i64)** %7
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
  %1 = call i64 @main.109()
  ret i32 0
}
