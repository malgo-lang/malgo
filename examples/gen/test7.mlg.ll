; ModuleID = './examples/test7.mlg'
source_filename = "./examples/test7.mlg"

declare i8* @GC_malloc(i64)

define i64 @f.11(i8*, i64) {
  %3 = bitcast i8* %0 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { i64 (i8*, i64)*, i8* }*
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 0
  store i64 (i8*, i64)* @f.11, i64 (i8*, i64)** %8
  %9 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = icmp sle i64 %1, 1
  %11 = alloca i64
  br i1 %10, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store i64 %5, i64* %11
  br label %endif_0

else_0:                                           ; preds = %2
  %12 = sub i64 %1, 1
  %13 = call i64 @f.11(i8* %0, i64 %12)
  store i64 %13, i64* %11
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %14 = load i64, i64* %11
  ret i64 %14
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %2 = bitcast i8* %1 to { i64 }*
  %3 = getelementptr { i64 }, { i64 }* %2, i32 0, i32 0
  store i64 42, i64* %3
  %4 = bitcast { i64 }* %2 to i8*
  %5 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }*
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, i64)* @f.11, i64 (i8*, i64)** %7
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %4, i8** %8
  %9 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  %10 = load i64 (i8*, i64)*, i64 (i8*, i64)** %9
  %11 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  %12 = load i8*, i8** %11
  %13 = call i64 %10(i8* %12, i64 3)
  ret i32 0
}
