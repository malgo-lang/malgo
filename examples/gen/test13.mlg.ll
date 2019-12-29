; ModuleID = './examples/test13.mlg'
source_filename = "./examples/test13.mlg"

declare i8* @GC_malloc(i64)

define i32 @main15() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  %3 = bitcast {}* %2 to i8*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { {}* (i8*, i64)*, i8* }*
  %6 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store {}* (i8*, i64)* @f16, {}* (i8*, i64)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %3, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 0
  %13 = load {}* (i8*, i64)*, {}* (i8*, i64)** %12
  %14 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 1
  %15 = load i8*, i8** %14
  %16 = call {}* %13(i8* %15, i64 5)
  ret i32 0
}

define {}* @f16(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { {}* (i8*, i64)*, i8* }*
  %6 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store {}* (i8*, i64)* @f16, {}* (i8*, i64)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = icmp slt i64 %1, 0
  %13 = alloca {}*
  br i1 %12, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %14 = call i8* @GC_malloc(i64 0)
  %15 = bitcast i8* %14 to {}*
  store {}* %15, {}** %13
  br label %end_0

else_0:                                           ; preds = %2
  %16 = call i8* @GC_malloc(i64 0)
  %17 = bitcast i8* %16 to {}*
  %18 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %19 = bitcast i8* %18 to { { {}* (i8*, i64)*, i8* }*, {}* }*
  %20 = getelementptr { { {}* (i8*, i64)*, i8* }*, {}* }, { { {}* (i8*, i64)*, i8* }*, {}* }* %19, i32 0, i32 0
  store { {}* (i8*, i64)*, i8* }* %5, { {}* (i8*, i64)*, i8* }** %20
  %21 = call i8* @GC_malloc(i64 0)
  %22 = bitcast i8* %21 to {}*
  %23 = getelementptr { { {}* (i8*, i64)*, i8* }*, {}* }, { { {}* (i8*, i64)*, i8* }*, {}* }* %19, i32 0, i32 1
  store {}* %17, {}** %23
  %24 = call i8* @GC_malloc(i64 0)
  %25 = bitcast i8* %24 to {}*
  %26 = getelementptr { { {}* (i8*, i64)*, i8* }*, {}* }, { { {}* (i8*, i64)*, i8* }*, {}* }* %19, i32 0, i32 1
  %27 = load {}*, {}** %26
  store {}* %27, {}** %13
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %28 = load {}*, {}** %13
  ret {}* %28
}

define i32 @main() {
  %1 = call i32 @main15()
  ret i32 0
}
