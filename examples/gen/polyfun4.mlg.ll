; ModuleID = './examples/polyfun4.mlg'
source_filename = "./examples/polyfun4.mlg"

define i8* @snd0({ i8*, i8* }*) {
  %2 = getelementptr { i8*, i8* }, { i8*, i8* }* %0, i32 0, i32 0
  %3 = load i8*, i8** %2
  %4 = getelementptr { i8*, i8* }, { i8*, i8* }* %0, i32 0, i32 1
  %5 = load i8*, i8** %4
  ret i8* %5
}

declare i8* @GC_malloc(i64)

define { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* @"$lambda32"(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }*
  %6 = getelementptr { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)* @"$lambda32", { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)** %6
  %7 = getelementptr { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %7
  %8 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %9 = bitcast i8* %8 to { i64 }*
  %10 = getelementptr { i64 }, { i64 }* %9, i32 0, i32 0
  store i64 %1, i64* %10
  %11 = bitcast { i64 }* %9 to i8*
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }*
  %14 = getelementptr { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }, { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %13, i32 0, i32 0
  store { i64, { i8**, i64 }* }* (i8*, i8*)* @"$lambda31", { i64, { i8**, i64 }* }* (i8*, i8*)** %14
  %15 = getelementptr { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }, { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %13, i32 0, i32 1
  store i8* %11, i8** %15
  ret { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %13
}

define { i64, { i8**, i64 }* }* @"$lambda31"(i8*, i8*) {
  %3 = bitcast i8* %0 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }*
  %8 = getelementptr { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }, { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %7, i32 0, i32 0
  store { i64, { i8**, i64 }* }* (i8*, i8*)* @"$lambda31", { i64, { i8**, i64 }* }* (i8*, i8*)** %8
  %9 = getelementptr { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }, { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = mul i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), 1
  %11 = call i8* @GC_malloc(i64 %10)
  %12 = bitcast i8* %11 to i8**
  %13 = call i8* @GC_malloc(i64 ptrtoint ({ i8**, i64 }* getelementptr inbounds ({ i8**, i64 }, { i8**, i64 }* null, i32 1) to i64))
  %14 = bitcast i8* %13 to { i8**, i64 }*
  %15 = getelementptr { i8**, i64 }, { i8**, i64 }* %14, i32 0, i32 0
  store i8** %12, i8*** %15
  %16 = getelementptr { i8**, i64 }, { i8**, i64 }* %14, i32 0, i32 1
  store i64 1, i64* %16
  %17 = alloca i64
  store i64 0, i64* %17
  br label %cond_0

cond_0:                                           ; preds = %body_0, %2
  %18 = load i64, i64* %17
  %19 = icmp slt i64 %18, 1
  br i1 %19, label %body_0, label %end_0

body_0:                                           ; preds = %cond_0
  %20 = getelementptr { i8**, i64 }, { i8**, i64 }* %14, i32 0, i32 0
  %21 = load i8**, i8*** %20
  %22 = getelementptr i8*, i8** %21, i64 %18
  store i8* %1, i8** %22
  %23 = add i64 %18, 1
  store i64 %23, i64* %17
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %24 = call i8* @GC_malloc(i64 ptrtoint ({ i64, { i8**, i64 }* }* getelementptr inbounds ({ i64, { i8**, i64 }* }, { i64, { i8**, i64 }* }* null, i32 1) to i64))
  %25 = bitcast i8* %24 to { i64, { i8**, i64 }* }*
  %26 = getelementptr { i64, { i8**, i64 }* }, { i64, { i8**, i64 }* }* %25, i32 0, i32 0
  store i64 %5, i64* %26
  %27 = getelementptr { i64, { i8**, i64 }* }, { i64, { i8**, i64 }* }* %25, i32 0, i32 1
  store { i8**, i64 }* %14, { i8**, i64 }** %27
  ret { i64, { i8**, i64 }* }* %25
}

define i64 @"$lambda24"(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i64 (i8*, i64)*, i8* }*
  %6 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda24", i64 (i8*, i64)** %6
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %7
  ret i64 %1
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  %3 = bitcast {}* %2 to i8*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i64 (i8*, i64)*, i8* }*
  %6 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda24", i64 (i8*, i64)** %6
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %3, i8** %7
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 0
  %9 = load i64 (i8*, i64)*, i64 (i8*, i64)** %8
  %10 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 1
  %11 = load i8*, i8** %10
  %12 = call i64 %9(i8* %11, i64 1)
  %13 = call i8* @GC_malloc(i64 0)
  %14 = bitcast i8* %13 to {}*
  %15 = bitcast {}* %14 to i8*
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }*
  %18 = getelementptr { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }* %17, i32 0, i32 0
  store { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)* @"$lambda32", { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)** %18
  %19 = getelementptr { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }* %17, i32 0, i32 1
  store i8* %15, i8** %19
  %20 = call i8* @GC_malloc(i64 ptrtoint ({ i64, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }* }* getelementptr inbounds ({ i64, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }* }, { i64, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }* }* null, i32 1) to i64))
  %21 = bitcast i8* %20 to { i64, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }* }*
  %22 = getelementptr { i64, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }* }, { i64, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }* }* %21, i32 0, i32 0
  store i64 %12, i64* %22
  %23 = getelementptr { i64, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }* }, { i64, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }* }* %21, i32 0, i32 1
  store { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }* %17, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }** %23
  %24 = getelementptr { i64, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }* }, { i64, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }* }* %21, i32 0, i32 0
  %25 = load i64, i64* %24
  %26 = getelementptr { i64, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }* }, { i64, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }* }* %21, i32 0, i32 1
  %27 = load { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }*, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }** %26
  %28 = getelementptr { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }* %27, i32 0, i32 0
  %29 = load { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)** %28
  %30 = getelementptr { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }, { { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, i8* }* %27, i32 0, i32 1
  %31 = load i8*, i8** %30
  %32 = call { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %29(i8* %31, i64 1)
  %33 = call i8* @GC_malloc(i64 0)
  %34 = bitcast i8* %33 to {}*
  ret i32 0
}
