; ModuleID = './examples/test15.mlg'
source_filename = "./examples/test15.mlg"

declare i8* @GC_malloc(i64)

define i32 @main38() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  %3 = bitcast {}* %2 to i8*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { {}* (i8*, i64)*, i8* }*
  %6 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store {}* (i8*, i64)* @f40, {}* (i8*, i64)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %3, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = call i8* @GC_malloc(i64 0)
  %13 = bitcast i8* %12 to {}*
  %14 = bitcast {}* %13 to i8*
  %15 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %16 = bitcast i8* %15 to { {}* (i8*, i64)*, i8* }*
  %17 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %16, i32 0, i32 0
  store {}* (i8*, i64)* @g39, {}* (i8*, i64)** %17
  %18 = call i8* @GC_malloc(i64 0)
  %19 = bitcast i8* %18 to {}*
  %20 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %16, i32 0, i32 1
  store i8* %14, i8** %20
  %21 = call i8* @GC_malloc(i64 0)
  %22 = bitcast i8* %21 to {}*
  %23 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 0
  %24 = load {}* (i8*, i64)*, {}* (i8*, i64)** %23
  %25 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 1
  %26 = load i8*, i8** %25
  %27 = call {}* %24(i8* %26, i64 5)
  ret i32 0
}

define {}* @g39(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { {}* (i8*, i64)*, i8* }*
  %6 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store {}* (i8*, i64)* @f40, {}* (i8*, i64)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { {}* (i8*, i64)*, i8* }*
  %14 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %13, i32 0, i32 0
  store {}* (i8*, i64)* @g39, {}* (i8*, i64)** %14
  %15 = call i8* @GC_malloc(i64 0)
  %16 = bitcast i8* %15 to {}*
  %17 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %13, i32 0, i32 1
  store i8* %0, i8** %17
  %18 = call i8* @GC_malloc(i64 0)
  %19 = bitcast i8* %18 to {}*
  %20 = icmp slt i64 %1, 0
  %21 = alloca {}*
  br i1 %20, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  store {}* %23, {}** %21
  br label %end_0

else_0:                                           ; preds = %2
  %24 = call i8* @GC_malloc(i64 0)
  %25 = bitcast i8* %24 to {}*
  %26 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %27 = bitcast i8* %26 to { { {}* (i8*, i64)*, i8* }*, {}* }*
  %28 = getelementptr { { {}* (i8*, i64)*, i8* }*, {}* }, { { {}* (i8*, i64)*, i8* }*, {}* }* %27, i32 0, i32 0
  store { {}* (i8*, i64)*, i8* }* %5, { {}* (i8*, i64)*, i8* }** %28
  %29 = call i8* @GC_malloc(i64 0)
  %30 = bitcast i8* %29 to {}*
  %31 = getelementptr { { {}* (i8*, i64)*, i8* }*, {}* }, { { {}* (i8*, i64)*, i8* }*, {}* }* %27, i32 0, i32 1
  store {}* %25, {}** %31
  %32 = call i8* @GC_malloc(i64 0)
  %33 = bitcast i8* %32 to {}*
  %34 = getelementptr { { {}* (i8*, i64)*, i8* }*, {}* }, { { {}* (i8*, i64)*, i8* }*, {}* }* %27, i32 0, i32 0
  %35 = load { {}* (i8*, i64)*, i8* }*, { {}* (i8*, i64)*, i8* }** %34
  %36 = sub i64 %1, 1
  %37 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %35, i32 0, i32 0
  %38 = load {}* (i8*, i64)*, {}* (i8*, i64)** %37
  %39 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %35, i32 0, i32 1
  %40 = load i8*, i8** %39
  %41 = call {}* %38(i8* %40, i64 %36)
  store {}* %41, {}** %21
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %42 = load {}*, {}** %21
  ret {}* %42
}

define {}* @f40(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { {}* (i8*, i64)*, i8* }*
  %6 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store {}* (i8*, i64)* @f40, {}* (i8*, i64)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { {}* (i8*, i64)*, i8* }*
  %14 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %13, i32 0, i32 0
  store {}* (i8*, i64)* @g39, {}* (i8*, i64)** %14
  %15 = call i8* @GC_malloc(i64 0)
  %16 = bitcast i8* %15 to {}*
  %17 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %13, i32 0, i32 1
  store i8* %0, i8** %17
  %18 = call i8* @GC_malloc(i64 0)
  %19 = bitcast i8* %18 to {}*
  %20 = icmp slt i64 %1, 0
  %21 = alloca {}*
  br i1 %20, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  store {}* %23, {}** %21
  br label %end_0

else_0:                                           ; preds = %2
  %24 = call i8* @GC_malloc(i64 0)
  %25 = bitcast i8* %24 to {}*
  %26 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %27 = bitcast i8* %26 to { { {}* (i8*, i64)*, i8* }*, {}* }*
  %28 = getelementptr { { {}* (i8*, i64)*, i8* }*, {}* }, { { {}* (i8*, i64)*, i8* }*, {}* }* %27, i32 0, i32 0
  store { {}* (i8*, i64)*, i8* }* %13, { {}* (i8*, i64)*, i8* }** %28
  %29 = call i8* @GC_malloc(i64 0)
  %30 = bitcast i8* %29 to {}*
  %31 = getelementptr { { {}* (i8*, i64)*, i8* }*, {}* }, { { {}* (i8*, i64)*, i8* }*, {}* }* %27, i32 0, i32 1
  store {}* %25, {}** %31
  %32 = call i8* @GC_malloc(i64 0)
  %33 = bitcast i8* %32 to {}*
  %34 = getelementptr { { {}* (i8*, i64)*, i8* }*, {}* }, { { {}* (i8*, i64)*, i8* }*, {}* }* %27, i32 0, i32 0
  %35 = load { {}* (i8*, i64)*, i8* }*, { {}* (i8*, i64)*, i8* }** %34
  %36 = sub i64 %1, 1
  %37 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %35, i32 0, i32 0
  %38 = load {}* (i8*, i64)*, {}* (i8*, i64)** %37
  %39 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %35, i32 0, i32 1
  %40 = load i8*, i8** %39
  %41 = call {}* %38(i8* %40, i64 %36)
  store {}* %41, {}** %21
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %42 = load {}*, {}** %21
  ret {}* %42
}

define i32 @main() {
  %1 = call i32 @main38()
  ret i32 0
}
