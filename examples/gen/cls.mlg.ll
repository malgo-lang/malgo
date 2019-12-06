; ModuleID = './examples/cls.mlg'
source_filename = "./examples/cls.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

define i64 @main.39() {
  %1 = call { i64 (i8*, i64)*, i8* }* @add.22(i64 3)
  %2 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %1, i32 0, i32 0
  %3 = load i64 (i8*, i64)*, i64 (i8*, i64)** %2
  %4 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %1, i32 0, i32 1
  %5 = load i8*, i8** %4
  %6 = call i64 %3(i8* %5, i64 4)
  ret i64 %6
}

define { i64 (i8*, i64)*, i8* }* @add.22(i64) {
  %2 = getelementptr { i64 }, { i64 }* null, i64 1
  %3 = ptrtoint { i64 }* %2 to i64
  %4 = call i8* @malloc_gc(i64 %3)
  %5 = bitcast i8* %4 to { i64 }*
  %6 = getelementptr { i64 }, { i64 }* %5, i32 0, i32 0
  store i64 %0, i64* %6
  %7 = bitcast { i64 }* %5 to i8*
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %9 = ptrtoint { i64 (i8*, i64)*, i8* }* %8 to i64
  %10 = call i8* @malloc_gc(i64 %9)
  %11 = bitcast i8* %10 to { i64 (i8*, i64)*, i8* }*
  %12 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %11, i32 0, i32 0
  store i64 (i8*, i64)* @add_inner.24, i64 (i8*, i64)** %12
  %13 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %7, i8** %13
  ret { i64 (i8*, i64)*, i8* }* %11
}

declare i64 @add_i64(i64, i64)

define i64 @add_inner.24(i8*, i64) {
  %3 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i64 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }*
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, i64)* @add_inner.24, i64 (i8*, i64)** %7
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
  %1 = call i64 @main.39()
  ret i32 0
}
