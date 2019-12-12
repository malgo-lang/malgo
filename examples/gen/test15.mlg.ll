; ModuleID = './examples/test15.mlg'
source_filename = "./examples/test15.mlg"

declare i8* @GC_malloc(i64)

define {}* @g.21(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { {}* (i8*, i64)*, i8* }*
  %6 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store {}* (i8*, i64)* @f.14, {}* (i8*, i64)** %6
  %7 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %7
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { {}* (i8*, i64)*, i8* }*
  %10 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %9, i32 0, i32 0
  store {}* (i8*, i64)* @g.21, {}* (i8*, i64)** %10
  %11 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = icmp slt i64 %1, 0
  %13 = alloca {}*
  br i1 %12, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %14 = call i8* @GC_malloc(i64 0)
  %15 = bitcast i8* %14 to {}*
  store {}* %15, {}** %13
  br label %endif_0

else_0:                                           ; preds = %2
  %16 = call i8* @GC_malloc(i64 0)
  %17 = bitcast i8* %16 to {}*
  %18 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %19 = bitcast i8* %18 to { { {}* (i8*, i64)*, i8* }*, {}* }*
  %20 = getelementptr { { {}* (i8*, i64)*, i8* }*, {}* }, { { {}* (i8*, i64)*, i8* }*, {}* }* %19, i32 0, i32 0
  store { {}* (i8*, i64)*, i8* }* %5, { {}* (i8*, i64)*, i8* }** %20
  %21 = getelementptr { { {}* (i8*, i64)*, i8* }*, {}* }, { { {}* (i8*, i64)*, i8* }*, {}* }* %19, i32 0, i32 1
  store {}* %17, {}** %21
  %22 = getelementptr { { {}* (i8*, i64)*, i8* }*, {}* }, { { {}* (i8*, i64)*, i8* }*, {}* }* %19, i32 0, i32 0
  %23 = load { {}* (i8*, i64)*, i8* }*, { {}* (i8*, i64)*, i8* }** %22
  %24 = sub i64 %1, 1
  %25 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %23, i32 0, i32 0
  %26 = load {}* (i8*, i64)*, {}* (i8*, i64)** %25
  %27 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %23, i32 0, i32 1
  %28 = load i8*, i8** %27
  %29 = call {}* %26(i8* %28, i64 %24)
  store {}* %29, {}** %13
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %30 = load {}*, {}** %13
  ret {}* %30
}

define {}* @f.14(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { {}* (i8*, i64)*, i8* }*
  %6 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store {}* (i8*, i64)* @f.14, {}* (i8*, i64)** %6
  %7 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %7
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { {}* (i8*, i64)*, i8* }*
  %10 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %9, i32 0, i32 0
  store {}* (i8*, i64)* @g.21, {}* (i8*, i64)** %10
  %11 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = icmp slt i64 %1, 0
  %13 = alloca {}*
  br i1 %12, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %14 = call i8* @GC_malloc(i64 0)
  %15 = bitcast i8* %14 to {}*
  store {}* %15, {}** %13
  br label %endif_0

else_0:                                           ; preds = %2
  %16 = call i8* @GC_malloc(i64 0)
  %17 = bitcast i8* %16 to {}*
  %18 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %19 = bitcast i8* %18 to { { {}* (i8*, i64)*, i8* }*, {}* }*
  %20 = getelementptr { { {}* (i8*, i64)*, i8* }*, {}* }, { { {}* (i8*, i64)*, i8* }*, {}* }* %19, i32 0, i32 0
  store { {}* (i8*, i64)*, i8* }* %9, { {}* (i8*, i64)*, i8* }** %20
  %21 = getelementptr { { {}* (i8*, i64)*, i8* }*, {}* }, { { {}* (i8*, i64)*, i8* }*, {}* }* %19, i32 0, i32 1
  store {}* %17, {}** %21
  %22 = getelementptr { { {}* (i8*, i64)*, i8* }*, {}* }, { { {}* (i8*, i64)*, i8* }*, {}* }* %19, i32 0, i32 0
  %23 = load { {}* (i8*, i64)*, i8* }*, { {}* (i8*, i64)*, i8* }** %22
  %24 = sub i64 %1, 1
  %25 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %23, i32 0, i32 0
  %26 = load {}* (i8*, i64)*, {}* (i8*, i64)** %25
  %27 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %23, i32 0, i32 1
  %28 = load i8*, i8** %27
  %29 = call {}* %26(i8* %28, i64 %24)
  store {}* %29, {}** %13
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %30 = load {}*, {}** %13
  ret {}* %30
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  %3 = bitcast {}* %2 to i8*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { {}* (i8*, i64)*, i8* }*
  %6 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store {}* (i8*, i64)* @f.14, {}* (i8*, i64)** %6
  %7 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %3, i8** %7
  %8 = call i8* @GC_malloc(i64 0)
  %9 = bitcast i8* %8 to {}*
  %10 = bitcast {}* %9 to i8*
  %11 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %12 = bitcast i8* %11 to { {}* (i8*, i64)*, i8* }*
  %13 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %12, i32 0, i32 0
  store {}* (i8*, i64)* @g.21, {}* (i8*, i64)** %13
  %14 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %12, i32 0, i32 1
  store i8* %10, i8** %14
  %15 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 0
  %16 = load {}* (i8*, i64)*, {}* (i8*, i64)** %15
  %17 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 1
  %18 = load i8*, i8** %17
  %19 = call {}* %16(i8* %18, i64 5)
  ret i32 0
}
