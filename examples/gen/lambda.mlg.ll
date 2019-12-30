; ModuleID = './examples/lambda.mlg'
source_filename = "./examples/lambda.mlg"

declare {}* @print_int(i64)

define {}* @print_int45(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

declare {}* @newline()

define {}* @newline44() {
  %1 = call {}* @newline()
  ret {}* %1
}

declare i8* @GC_malloc(i64)

define i64 @"$lambda43"(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i64 (i8*, i64)*, i8* }*
  %6 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda43", i64 (i8*, i64)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = add i64 %1, 1
  ret i64 %12
}

define i64 @"$lambda42"(i8*, i64) {
  %3 = bitcast i8* %0 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { i64 (i8*, i64)*, i8* }*
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda42", i64 (i8*, i64)** %8
  %9 = call i8* @GC_malloc(i64 0)
  %10 = bitcast i8* %9 to {}*
  %11 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = call i8* @GC_malloc(i64 0)
  %13 = bitcast i8* %12 to {}*
  %14 = add i64 %5, %1
  ret i64 %14
}

define { i64 (i8*, i64)*, i8* }* @"$lambda41"(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*
  %6 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @"$lambda41", { i64 (i8*, i64)*, i8* }* (i8*, i64)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %13 = bitcast i8* %12 to { i64 }*
  %14 = getelementptr { i64 }, { i64 }* %13, i32 0, i32 0
  store i64 %1, i64* %14
  %15 = call i8* @GC_malloc(i64 0)
  %16 = bitcast i8* %15 to {}*
  %17 = bitcast { i64 }* %13 to i8*
  %18 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %19 = bitcast i8* %18 to { i64 (i8*, i64)*, i8* }*
  %20 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %19, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda42", i64 (i8*, i64)** %20
  %21 = call i8* @GC_malloc(i64 0)
  %22 = bitcast i8* %21 to {}*
  %23 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %19, i32 0, i32 1
  store i8* %17, i8** %23
  %24 = call i8* @GC_malloc(i64 0)
  %25 = bitcast i8* %24 to {}*
  ret { i64 (i8*, i64)*, i8* }* %19
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  %3 = bitcast {}* %2 to i8*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i64 (i8*, i64)*, i8* }*
  %6 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda43", i64 (i8*, i64)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %3, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = call i8* @GC_malloc(i64 0)
  %13 = bitcast i8* %12 to {}*
  %14 = bitcast {}* %13 to i8*
  %15 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %16 = bitcast i8* %15 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*
  %17 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %16, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @"$lambda41", { i64 (i8*, i64)*, i8* }* (i8*, i64)** %17
  %18 = call i8* @GC_malloc(i64 0)
  %19 = bitcast i8* %18 to {}*
  %20 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %16, i32 0, i32 1
  store i8* %14, i8** %20
  %21 = call i8* @GC_malloc(i64 0)
  %22 = bitcast i8* %21 to {}*
  %23 = alloca i64
  br i1 true, label %then_0, label %else_0

then_0:                                           ; preds = %0
  store i64 42, i64* %23
  br label %end_0

else_0:                                           ; preds = %0
  store i64 48, i64* %23
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %24 = load i64, i64* %23
  %25 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 0
  %26 = load i64 (i8*, i64)*, i64 (i8*, i64)** %25
  %27 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 1
  %28 = load i8*, i8** %27
  %29 = call i64 %26(i8* %28, i64 41)
  %30 = call {}* @print_int45(i64 %29)
  %31 = call {}* @newline44()
  %32 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %16, i32 0, i32 0
  %33 = load { i64 (i8*, i64)*, i8* }* (i8*, i64)*, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %32
  %34 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %16, i32 0, i32 1
  %35 = load i8*, i8** %34
  %36 = call { i64 (i8*, i64)*, i8* }* %33(i8* %35, i64 2)
  %37 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %36, i32 0, i32 0
  %38 = load i64 (i8*, i64)*, i64 (i8*, i64)** %37
  %39 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %36, i32 0, i32 1
  %40 = load i8*, i8** %39
  %41 = call i64 %38(i8* %40, i64 3)
  %42 = call {}* @print_int45(i64 %41)
  %43 = call {}* @newline44()
  ret i32 0
}
