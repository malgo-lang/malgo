; ModuleID = './examples/test13.mlg'
source_filename = "./examples/test13.mlg"

declare i8* @GC_malloc(i64)

define {}* @f0(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { {}* (i8*, i64)*, i8* }*
  %6 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store {}* (i8*, i64)* @f0, {}* (i8*, i64)** %6
  %7 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %7
  %8 = icmp slt i64 %1, 0
  %9 = alloca {}*
  br i1 %8, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  store {}* %11, {}** %9
  br label %end_0

else_0:                                           ; preds = %2
  %12 = call i8* @GC_malloc(i64 0)
  %13 = bitcast i8* %12 to {}*
  %14 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %15 = bitcast i8* %14 to { { {}* (i8*, i64)*, i8* }*, {}* }*
  %16 = getelementptr { { {}* (i8*, i64)*, i8* }*, {}* }, { { {}* (i8*, i64)*, i8* }*, {}* }* %15, i32 0, i32 0
  store { {}* (i8*, i64)*, i8* }* %5, { {}* (i8*, i64)*, i8* }** %16
  %17 = getelementptr { { {}* (i8*, i64)*, i8* }*, {}* }, { { {}* (i8*, i64)*, i8* }*, {}* }* %15, i32 0, i32 1
  store {}* %13, {}** %17
  %18 = getelementptr { { {}* (i8*, i64)*, i8* }*, {}* }, { { {}* (i8*, i64)*, i8* }*, {}* }* %15, i32 0, i32 0
  %19 = load { {}* (i8*, i64)*, i8* }*, { {}* (i8*, i64)*, i8* }** %18
  %20 = getelementptr { { {}* (i8*, i64)*, i8* }*, {}* }, { { {}* (i8*, i64)*, i8* }*, {}* }* %15, i32 0, i32 1
  %21 = load {}*, {}** %20
  store {}* %21, {}** %9
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %22 = load {}*, {}** %9
  ret {}* %22
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  %3 = bitcast {}* %2 to i8*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { {}* (i8*, i64)*, i8* }*
  %6 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store {}* (i8*, i64)* @f0, {}* (i8*, i64)** %6
  %7 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %3, i8** %7
  %8 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 0
  %9 = load {}* (i8*, i64)*, {}* (i8*, i64)** %8
  %10 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 1
  %11 = load i8*, i8** %10
  %12 = call {}* %9(i8* %11, i64 5)
  ret i32 0
}
