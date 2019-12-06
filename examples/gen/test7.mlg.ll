; ModuleID = './examples/test7.mlg'
source_filename = "./examples/test7.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

define i64 @main.46() {
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
  store i64 (i8*, i64)* @f.26, i64 (i8*, i64)** %11
  %12 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %10, i32 0, i32 1
  store i8* %6, i8** %12
  %13 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %10, i32 0, i32 0
  %14 = load i64 (i8*, i64)*, i64 (i8*, i64)** %13
  %15 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %10, i32 0, i32 1
  %16 = load i8*, i8** %15
  %17 = call i64 %14(i8* %16, i64 3)
  ret i64 %17
}

declare i1 @le_i64(i64, i64)

declare i64 @sub_i64(i64, i64)

define i64 @f.26(i8*, i64) {
  %3 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i64 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }*
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, i64)* @f.26, i64 (i8*, i64)** %7
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i64 }*
  %10 = getelementptr { i64 }, { i64 }* %9, i32 0, i32 0
  %11 = load i64, i64* %10
  %12 = call i1 @le_i64(i64 %1, i64 1)
  %13 = alloca i64
  br i1 %12, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store i64 %11, i64* %13
  br label %endif_0

else_0:                                           ; preds = %2
  %14 = call i64 @sub_i64(i64 %1, i64 1)
  %15 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  %16 = load i64 (i8*, i64)*, i64 (i8*, i64)** %15
  %17 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  %18 = load i8*, i8** %17
  %19 = call i64 %16(i8* %18, i64 %14)
  store i64 %19, i64* %13
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %20 = load i64, i64* %13
  ret i64 %20
}

define i32 @main() {
  call void @init_gc()
  %1 = call i64 @main.46()
  ret i32 0
}
