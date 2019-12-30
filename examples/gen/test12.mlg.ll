; ModuleID = './examples/test12.mlg'
source_filename = "./examples/test12.mlg"

declare i8* @GC_malloc(i64)

define i1 @even37(i8*, i64) {
  %3 = bitcast i8* %0 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { i1 (i8*, i64)*, i8* }*
  %8 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %7, i32 0, i32 0
  store i1 (i8*, i64)* @even37, i1 (i8*, i64)** %8
  %9 = call i8* @GC_malloc(i64 0)
  %10 = bitcast i8* %9 to {}*
  %11 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = call i8* @GC_malloc(i64 0)
  %13 = bitcast i8* %12 to {}*
  %14 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %15 = bitcast i8* %14 to { i1 (i8*, i64)*, i8* }*
  %16 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %15, i32 0, i32 0
  store i1 (i8*, i64)* @odd36, i1 (i8*, i64)** %16
  %17 = call i8* @GC_malloc(i64 0)
  %18 = bitcast i8* %17 to {}*
  %19 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %15, i32 0, i32 1
  store i8* %0, i8** %19
  %20 = call i8* @GC_malloc(i64 0)
  %21 = bitcast i8* %20 to {}*
  %22 = icmp eq i64 %1, %5
  %23 = alloca i1
  br i1 %22, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store i1 true, i1* %23
  br label %end_0

else_0:                                           ; preds = %2
  %24 = sub i64 %1, 1
  %25 = call i1 @odd36(i8* %0, i64 %24)
  store i1 %25, i1* %23
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %26 = load i1, i1* %23
  ret i1 %26
}

define i1 @odd36(i8*, i64) {
  %3 = bitcast i8* %0 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { i1 (i8*, i64)*, i8* }*
  %8 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %7, i32 0, i32 0
  store i1 (i8*, i64)* @even37, i1 (i8*, i64)** %8
  %9 = call i8* @GC_malloc(i64 0)
  %10 = bitcast i8* %9 to {}*
  %11 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = call i8* @GC_malloc(i64 0)
  %13 = bitcast i8* %12 to {}*
  %14 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %15 = bitcast i8* %14 to { i1 (i8*, i64)*, i8* }*
  %16 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %15, i32 0, i32 0
  store i1 (i8*, i64)* @odd36, i1 (i8*, i64)** %16
  %17 = call i8* @GC_malloc(i64 0)
  %18 = bitcast i8* %17 to {}*
  %19 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %15, i32 0, i32 1
  store i8* %0, i8** %19
  %20 = call i8* @GC_malloc(i64 0)
  %21 = bitcast i8* %20 to {}*
  %22 = icmp eq i64 %1, %5
  %23 = alloca i1
  br i1 %22, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store i1 false, i1* %23
  br label %end_0

else_0:                                           ; preds = %2
  %24 = sub i64 %1, 1
  %25 = call i1 @even37(i8* %0, i64 %24)
  store i1 %25, i1* %23
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %26 = load i1, i1* %23
  ret i1 %26
}

declare {}* @print_bool(i1)

define {}* @print_bool35(i1) {
  %2 = call {}* @print_bool(i1 %0)
  ret {}* %2
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %2 = bitcast i8* %1 to { i64 }*
  %3 = getelementptr { i64 }, { i64 }* %2, i32 0, i32 0
  store i64 0, i64* %3
  %4 = call i8* @GC_malloc(i64 0)
  %5 = bitcast i8* %4 to {}*
  %6 = bitcast { i64 }* %2 to i8*
  %7 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %8 = bitcast i8* %7 to { i1 (i8*, i64)*, i8* }*
  %9 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %8, i32 0, i32 0
  store i1 (i8*, i64)* @even37, i1 (i8*, i64)** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %8, i32 0, i32 1
  store i8* %6, i8** %12
  %13 = call i8* @GC_malloc(i64 0)
  %14 = bitcast i8* %13 to {}*
  %15 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %16 = bitcast i8* %15 to { i64 }*
  %17 = getelementptr { i64 }, { i64 }* %16, i32 0, i32 0
  store i64 0, i64* %17
  %18 = call i8* @GC_malloc(i64 0)
  %19 = bitcast i8* %18 to {}*
  %20 = bitcast { i64 }* %16 to i8*
  %21 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %22 = bitcast i8* %21 to { i1 (i8*, i64)*, i8* }*
  %23 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %22, i32 0, i32 0
  store i1 (i8*, i64)* @odd36, i1 (i8*, i64)** %23
  %24 = call i8* @GC_malloc(i64 0)
  %25 = bitcast i8* %24 to {}*
  %26 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %22, i32 0, i32 1
  store i8* %20, i8** %26
  %27 = call i8* @GC_malloc(i64 0)
  %28 = bitcast i8* %27 to {}*
  %29 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %22, i32 0, i32 0
  %30 = load i1 (i8*, i64)*, i1 (i8*, i64)** %29
  %31 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %22, i32 0, i32 1
  %32 = load i8*, i8** %31
  %33 = call i1 %30(i8* %32, i64 34)
  %34 = call {}* @print_bool35(i1 %33)
  ret i32 0
}
