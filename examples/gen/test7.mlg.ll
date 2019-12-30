; ModuleID = './examples/test7.mlg'
source_filename = "./examples/test7.mlg"

declare i8* @GC_malloc(i64)

define i64 @f18(i8*, i64) {
  %3 = bitcast i8* %0 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { i64 (i8*, i64)*, i8* }*
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 0
  store i64 (i8*, i64)* @f18, i64 (i8*, i64)** %8
  %9 = call i8* @GC_malloc(i64 0)
  %10 = bitcast i8* %9 to {}*
  %11 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = call i8* @GC_malloc(i64 0)
  %13 = bitcast i8* %12 to {}*
  %14 = icmp sle i64 %1, 1
  %15 = alloca i64
  br i1 %14, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store i64 %5, i64* %15
  br label %end_0

else_0:                                           ; preds = %2
  %16 = sub i64 %1, 1
  %17 = call i64 @f18(i8* %0, i64 %16)
  store i64 %17, i64* %15
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %18 = load i64, i64* %15
  ret i64 %18
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %2 = bitcast i8* %1 to { i64 }*
  %3 = getelementptr { i64 }, { i64 }* %2, i32 0, i32 0
  store i64 42, i64* %3
  %4 = call i8* @GC_malloc(i64 0)
  %5 = bitcast i8* %4 to {}*
  %6 = bitcast { i64 }* %2 to i8*
  %7 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %8 = bitcast i8* %7 to { i64 (i8*, i64)*, i8* }*
  %9 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %8, i32 0, i32 0
  store i64 (i8*, i64)* @f18, i64 (i8*, i64)** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %8, i32 0, i32 1
  store i8* %6, i8** %12
  %13 = call i8* @GC_malloc(i64 0)
  %14 = bitcast i8* %13 to {}*
  %15 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %8, i32 0, i32 0
  %16 = load i64 (i8*, i64)*, i64 (i8*, i64)** %15
  %17 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %8, i32 0, i32 1
  %18 = load i8*, i8** %17
  %19 = call i64 %16(i8* %18, i64 3)
  ret i32 0
}
