; ModuleID = './examples/test9.mlg'
source_filename = "./examples/test9.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

declare i64 @add_i64(i64, i64)

define i64 @main.37() {
  %1 = getelementptr { i64 }, { i64 }* null, i64 1
  %2 = ptrtoint { i64 }* %1 to i64
  %3 = call i8* @malloc_gc(i64 %2)
  %4 = bitcast i8* %3 to { i64 }*
  %5 = getelementptr { i64 }, { i64 }* %4, i32 0, i32 0
  store i64 42, i64* %5
  %6 = bitcast { i64 }* %4 to i8*
  %7 = getelementptr { i64 (i8*, {})*, i8* }, { i64 (i8*, {})*, i8* }* null, i64 1
  %8 = ptrtoint { i64 (i8*, {})*, i8* }* %7 to i64
  %9 = call i8* @malloc_gc(i64 %8)
  %10 = bitcast i8* %9 to { i64 (i8*, {})*, i8* }*
  %11 = getelementptr { i64 (i8*, {})*, i8* }, { i64 (i8*, {})*, i8* }* %10, i32 0, i32 0
  store i64 (i8*, {})* @f.23, i64 (i8*, {})** %11
  %12 = getelementptr { i64 (i8*, {})*, i8* }, { i64 (i8*, {})*, i8* }* %10, i32 0, i32 1
  store i8* %6, i8** %12
  %13 = getelementptr { i64 (i8*, {})*, i8* }, { i64 (i8*, {})*, i8* }* %10, i32 0, i32 0
  %14 = load i64 (i8*, {})*, i64 (i8*, {})** %13
  %15 = getelementptr { i64 (i8*, {})*, i8* }, { i64 (i8*, {})*, i8* }* %10, i32 0, i32 1
  %16 = load i8*, i8** %15
  %17 = call i64 %14(i8* %16, {} undef)
  %18 = call i64 @add_i64(i64 %17, i64 42)
  ret i64 %18
}

define i64 @f.23(i8*, {}) {
  %3 = getelementptr { i64 (i8*, {})*, i8* }, { i64 (i8*, {})*, i8* }* null, i64 1
  %4 = ptrtoint { i64 (i8*, {})*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i64 (i8*, {})*, i8* }*
  %7 = getelementptr { i64 (i8*, {})*, i8* }, { i64 (i8*, {})*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, {})* @f.23, i64 (i8*, {})** %7
  %8 = getelementptr { i64 (i8*, {})*, i8* }, { i64 (i8*, {})*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i64 }*
  %10 = getelementptr { i64 }, { i64 }* %9, i32 0, i32 0
  %11 = load i64, i64* %10
  ret i64 %11
}

define i32 @main() {
  call void @init_gc()
  %1 = call i64 @main.37()
  ret i32 0
}
