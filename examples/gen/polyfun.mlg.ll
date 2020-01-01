; ModuleID = './examples/polyfun.mlg'
source_filename = "./examples/polyfun.mlg"

define i64 @fo187(i8*, i64) {
  %3 = bitcast i8* %0 to { i8* (i8*, i8*)*, i8* }*
  %4 = inttoptr i64 %1 to i8*
  %5 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i32 0, i32 0
  %6 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %5
  %7 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i32 0, i32 1
  %8 = load i8*, i8** %7
  %9 = call i8* %6(i8* %8, i8* %4)
  %10 = ptrtoint i8* %9 to i64
  ret i64 %10
}

declare i8* @GC_malloc(i64)

define { i64, { i8**, i64 }* }* @fo156(i8*, i8*) {
  %3 = bitcast i8* %0 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }*
  %4 = getelementptr { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %3, i32 0, i32 0
  %5 = load { i8*, { i8**, i64 }* }* (i8*, i8*)*, { i8*, { i8**, i64 }* }* (i8*, i8*)** %4
  %6 = getelementptr { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %3, i32 0, i32 1
  %7 = load i8*, i8** %6
  %8 = call { i8*, { i8**, i64 }* }* %5(i8* %7, i8* %1)
  %9 = call i8* @GC_malloc(i64 ptrtoint ({ i64, { i8**, i64 }* }* getelementptr inbounds ({ i64, { i8**, i64 }* }, { i64, { i8**, i64 }* }* null, i32 1) to i64))
  %10 = bitcast i8* %9 to { i64, { i8**, i64 }* }*
  %11 = getelementptr { i8*, { i8**, i64 }* }, { i8*, { i8**, i64 }* }* %8, i32 0, i32 0
  %12 = load i8*, i8** %11
  %13 = ptrtoint i8* %12 to i64
  %14 = getelementptr { i64, { i8**, i64 }* }, { i64, { i8**, i64 }* }* %10, i32 0, i32 0
  store i64 %13, i64* %14
  %15 = call i8* @GC_malloc(i64 0)
  %16 = bitcast i8* %15 to {}*
  %17 = getelementptr { i8*, { i8**, i64 }* }, { i8*, { i8**, i64 }* }* %8, i32 0, i32 0
  %18 = load i8*, i8** %17
  %19 = bitcast i8* %18 to { i8**, i64 }*
  %20 = getelementptr { i8**, i64 }, { i8**, i64 }* %19, i32 0, i32 0
  %21 = load i8**, i8*** %20
  %22 = getelementptr { i8**, i64 }, { i8**, i64 }* %19, i32 0, i32 1
  %23 = load i64, i64* %22
  %24 = mul i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), %23
  %25 = call i8* @GC_malloc(i64 %24)
  %26 = bitcast i8* %25 to i8**
  %27 = call i8* @GC_malloc(i64 ptrtoint ({ i8**, i64 }* getelementptr inbounds ({ i8**, i64 }, { i8**, i64 }* null, i32 1) to i64))
  %28 = bitcast i8* %27 to { i8**, i64 }*
  %29 = getelementptr { i8**, i64 }, { i8**, i64 }* %28, i32 0, i32 0
  store i8** %26, i8*** %29
  %30 = getelementptr { i8**, i64 }, { i8**, i64 }* %28, i32 0, i32 1
  store i64 %23, i64* %30
  %31 = getelementptr { i8**, i64 }, { i8**, i64 }* %28, i32 0, i32 0
  %32 = load i8**, i8*** %31
  %33 = getelementptr { i8**, i64 }, { i8**, i64 }* %28, i32 0, i32 1
  store i64 %23, i64* %33
  %34 = call i8* @GC_malloc(i64 0)
  %35 = bitcast i8* %34 to {}*
  %36 = alloca i64
  store i64 0, i64* %36
  br label %cond_0

cond_0:                                           ; preds = %body_0, %2
  %37 = load i64, i64* %36
  %38 = icmp slt i64 %37, %23
  br i1 %38, label %body_0, label %end_0

body_0:                                           ; preds = %cond_0
  %39 = getelementptr i8*, i8** %21, i64 %37
  %40 = load i8*, i8** %39
  %41 = getelementptr i8*, i8** %32, i64 %37
  store i8* %40, i8** %41
  %42 = call i8* @GC_malloc(i64 0)
  %43 = bitcast i8* %42 to {}*
  %44 = add i64 %37, 1
  store i64 %44, i64* %36
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %45 = call i8* @GC_malloc(i64 0)
  %46 = bitcast i8* %45 to {}*
  %47 = getelementptr { i64, { i8**, i64 }* }, { i64, { i8**, i64 }* }* %10, i32 0, i32 1
  store { i8**, i64 }* %28, { i8**, i64 }** %47
  %48 = call i8* @GC_malloc(i64 0)
  %49 = bitcast i8* %48 to {}*
  ret { i64, { i8**, i64 }* }* %10
}

define i8* @fo140(i8*, i8*) {
  %3 = bitcast i8* %0 to { i8* (i8*, i8*)*, i8* }*
  %4 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i32 0, i32 0
  %5 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %4
  %6 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i32 0, i32 1
  %7 = load i8*, i8** %6
  %8 = call i8* %5(i8* %7, i8* %1)
  ret i8* %8
}

define i8* @id60(i8*, i8*) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i8* (i8*, i8*)*, i8* }*
  %6 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 0
  store i8* (i8*, i8*)* @id60, i8* (i8*, i8*)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { i64 (i8*, i64)*, i8* }*
  %14 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %13, i32 0, i32 0
  store i64 (i8*, i64)* @addOne59, i64 (i8*, i64)** %14
  %15 = call i8* @GC_malloc(i64 0)
  %16 = bitcast i8* %15 to {}*
  %17 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %13, i32 0, i32 1
  store i8* %0, i8** %17
  %18 = call i8* @GC_malloc(i64 0)
  %19 = bitcast i8* %18 to {}*
  ret i8* %1
}

define i64 @addOne59(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i8* (i8*, i8*)*, i8* }*
  %6 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 0
  store i8* (i8*, i8*)* @id60, i8* (i8*, i8*)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { i64 (i8*, i64)*, i8* }*
  %14 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %13, i32 0, i32 0
  store i64 (i8*, i64)* @addOne59, i64 (i8*, i64)** %14
  %15 = call i8* @GC_malloc(i64 0)
  %16 = bitcast i8* %15 to {}*
  %17 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %13, i32 0, i32 1
  store i8* %0, i8** %17
  %18 = call i8* @GC_malloc(i64 0)
  %19 = bitcast i8* %18 to {}*
  %20 = add i64 %1, 1
  ret i64 %20
}

define { i8*, { i8**, i64 }* }* @"$lambda58"(i8*, i8*) {
  %3 = bitcast i8* %0 to { i8* }*
  %4 = getelementptr { i8* }, { i8* }* %3, i32 0, i32 0
  %5 = load i8*, i8** %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }*
  %8 = getelementptr { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %7, i32 0, i32 0
  store { i8*, { i8**, i64 }* }* (i8*, i8*)* @"$lambda58", { i8*, { i8**, i64 }* }* (i8*, i8*)** %8
  %9 = call i8* @GC_malloc(i64 0)
  %10 = bitcast i8* %9 to {}*
  %11 = getelementptr { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = call i8* @GC_malloc(i64 0)
  %13 = bitcast i8* %12 to {}*
  %14 = mul i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), 1
  %15 = call i8* @GC_malloc(i64 %14)
  %16 = bitcast i8* %15 to i8**
  %17 = call i8* @GC_malloc(i64 ptrtoint ({ i8**, i64 }* getelementptr inbounds ({ i8**, i64 }, { i8**, i64 }* null, i32 1) to i64))
  %18 = bitcast i8* %17 to { i8**, i64 }*
  %19 = getelementptr { i8**, i64 }, { i8**, i64 }* %18, i32 0, i32 0
  store i8** %16, i8*** %19
  %20 = getelementptr { i8**, i64 }, { i8**, i64 }* %18, i32 0, i32 1
  store i64 1, i64* %20
  %21 = alloca i64
  store i64 0, i64* %21
  br label %cond_0

cond_0:                                           ; preds = %body_0, %2
  %22 = load i64, i64* %21
  %23 = icmp slt i64 %22, 1
  br i1 %23, label %body_0, label %end_0

body_0:                                           ; preds = %cond_0
  %24 = getelementptr { i8**, i64 }, { i8**, i64 }* %18, i32 0, i32 0
  %25 = load i8**, i8*** %24
  %26 = getelementptr i8*, i8** %25, i64 %22
  store i8* %1, i8** %26
  %27 = call i8* @GC_malloc(i64 0)
  %28 = bitcast i8* %27 to {}*
  %29 = add i64 %22, 1
  store i64 %29, i64* %21
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %30 = call i8* @GC_malloc(i64 0)
  %31 = bitcast i8* %30 to {}*
  %32 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %33 = bitcast i8* %32 to { i8*, { i8**, i64 }* }*
  %34 = getelementptr { i8*, { i8**, i64 }* }, { i8*, { i8**, i64 }* }* %33, i32 0, i32 0
  store i8* %5, i8** %34
  %35 = call i8* @GC_malloc(i64 0)
  %36 = bitcast i8* %35 to {}*
  %37 = getelementptr { i8*, { i8**, i64 }* }, { i8*, { i8**, i64 }* }* %33, i32 0, i32 1
  store { i8**, i64 }* %18, { i8**, i64 }** %37
  %38 = call i8* @GC_malloc(i64 0)
  %39 = bitcast i8* %38 to {}*
  ret { i8*, { i8**, i64 }* }* %33
}

define { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* @"$lambda57"(i8*, i8*) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }*
  %6 = getelementptr { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }, { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }* %5, i32 0, i32 0
  store { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)* @"$lambda57", { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }, { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = call i8* @GC_malloc(i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64))
  %13 = bitcast i8* %12 to { i8* }*
  %14 = getelementptr { i8* }, { i8* }* %13, i32 0, i32 0
  store i8* %1, i8** %14
  %15 = call i8* @GC_malloc(i64 0)
  %16 = bitcast i8* %15 to {}*
  %17 = bitcast { i8* }* %13 to i8*
  %18 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %19 = bitcast i8* %18 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }*
  %20 = getelementptr { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %19, i32 0, i32 0
  store { i8*, { i8**, i64 }* }* (i8*, i8*)* @"$lambda58", { i8*, { i8**, i64 }* }* (i8*, i8*)** %20
  %21 = call i8* @GC_malloc(i64 0)
  %22 = bitcast i8* %21 to {}*
  %23 = getelementptr { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %19, i32 0, i32 1
  store i8* %17, i8** %23
  %24 = call i8* @GC_malloc(i64 0)
  %25 = bitcast i8* %24 to {}*
  ret { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %19
}

declare {}* @print_int(i64)

define {}* @print_int56(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  %3 = bitcast {}* %2 to i8*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i8* (i8*, i8*)*, i8* }*
  %6 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 0
  store i8* (i8*, i8*)* @id60, i8* (i8*, i8*)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 1
  store i8* %3, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = call i8* @GC_malloc(i64 0)
  %13 = bitcast i8* %12 to {}*
  %14 = bitcast {}* %13 to i8*
  %15 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %16 = bitcast i8* %15 to { i64 (i8*, i64)*, i8* }*
  %17 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %16, i32 0, i32 0
  store i64 (i8*, i64)* @addOne59, i64 (i8*, i64)** %17
  %18 = call i8* @GC_malloc(i64 0)
  %19 = bitcast i8* %18 to {}*
  %20 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %16, i32 0, i32 1
  store i8* %14, i8** %20
  %21 = call i8* @GC_malloc(i64 0)
  %22 = bitcast i8* %21 to {}*
  %23 = mul i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), 1
  %24 = call i8* @GC_malloc(i64 %23)
  %25 = bitcast i8* %24 to { i64 (i8*, i64)*, i8* }**
  %26 = call i8* @GC_malloc(i64 ptrtoint ({ { i64 (i8*, i64)*, i8* }**, i64 }* getelementptr inbounds ({ { i64 (i8*, i64)*, i8* }**, i64 }, { { i64 (i8*, i64)*, i8* }**, i64 }* null, i32 1) to i64))
  %27 = bitcast i8* %26 to { { i64 (i8*, i64)*, i8* }**, i64 }*
  %28 = getelementptr { { i64 (i8*, i64)*, i8* }**, i64 }, { { i64 (i8*, i64)*, i8* }**, i64 }* %27, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }** %25, { i64 (i8*, i64)*, i8* }*** %28
  %29 = getelementptr { { i64 (i8*, i64)*, i8* }**, i64 }, { { i64 (i8*, i64)*, i8* }**, i64 }* %27, i32 0, i32 1
  store i64 1, i64* %29
  %30 = alloca i64
  store i64 0, i64* %30
  br label %cond_0

cond_0:                                           ; preds = %body_0, %0
  %31 = load i64, i64* %30
  %32 = icmp slt i64 %31, 1
  br i1 %32, label %body_0, label %end_0

body_0:                                           ; preds = %cond_0
  %33 = getelementptr { { i64 (i8*, i64)*, i8* }**, i64 }, { { i64 (i8*, i64)*, i8* }**, i64 }* %27, i32 0, i32 0
  %34 = load { i64 (i8*, i64)*, i8* }**, { i64 (i8*, i64)*, i8* }*** %33
  %35 = getelementptr { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %34, i64 %31
  store { i64 (i8*, i64)*, i8* }* %16, { i64 (i8*, i64)*, i8* }** %35
  %36 = call i8* @GC_malloc(i64 0)
  %37 = bitcast i8* %36 to {}*
  %38 = add i64 %31, 1
  store i64 %38, i64* %30
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %39 = call i8* @GC_malloc(i64 0)
  %40 = bitcast i8* %39 to {}*
  %41 = call i8* @GC_malloc(i64 0)
  %42 = bitcast i8* %41 to {}*
  %43 = bitcast {}* %42 to i8*
  %44 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %45 = bitcast i8* %44 to { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }*
  %46 = getelementptr { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }, { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }* %45, i32 0, i32 0
  store { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)* @"$lambda57", { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)** %46
  %47 = call i8* @GC_malloc(i64 0)
  %48 = bitcast i8* %47 to {}*
  %49 = getelementptr { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }, { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }* %45, i32 0, i32 1
  store i8* %43, i8** %49
  %50 = call i8* @GC_malloc(i64 0)
  %51 = bitcast i8* %50 to {}*
  %52 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 0
  %53 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %52
  %54 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 1
  %55 = load i8*, i8** %54
  %56 = bitcast { i8* (i8*, i8*)*, i8* }* %5 to i8*
  %57 = call i8* %53(i8* %55, i8* %56)
  %58 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %59 = bitcast i8* %58 to { i8* (i8*, i8*)*, i8* }*
  %60 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %59, i32 0, i32 0
  store i8* (i8*, i8*)* @fo140, i8* (i8*, i8*)** %60
  %61 = call i8* @GC_malloc(i64 0)
  %62 = bitcast i8* %61 to {}*
  %63 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %59, i32 0, i32 1
  store i8* %57, i8** %63
  %64 = call i8* @GC_malloc(i64 0)
  %65 = bitcast i8* %64 to {}*
  %66 = getelementptr { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }, { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }* %45, i32 0, i32 0
  %67 = load { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)** %66
  %68 = getelementptr { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }, { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }* %45, i32 0, i32 1
  %69 = load i8*, i8** %68
  %70 = inttoptr i64 1 to i8*
  %71 = call { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %67(i8* %69, i8* %70)
  %72 = bitcast { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %71 to i8*
  %73 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %74 = bitcast i8* %73 to { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }*
  %75 = getelementptr { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }, { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %74, i32 0, i32 0
  store { i64, { i8**, i64 }* }* (i8*, i8*)* @fo156, { i64, { i8**, i64 }* }* (i8*, i8*)** %75
  %76 = call i8* @GC_malloc(i64 0)
  %77 = bitcast i8* %76 to {}*
  %78 = getelementptr { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }, { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %74, i32 0, i32 1
  store i8* %72, i8** %78
  %79 = call i8* @GC_malloc(i64 0)
  %80 = bitcast i8* %79 to {}*
  %81 = getelementptr { { i64 (i8*, i64)*, i8* }**, i64 }, { { i64 (i8*, i64)*, i8* }**, i64 }* %27, i32 0, i32 0
  %82 = load { i64 (i8*, i64)*, i8* }**, { i64 (i8*, i64)*, i8* }*** %81
  %83 = bitcast { i8* (i8*, i8*)*, i8* }* %5 to i8*
  %84 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %85 = bitcast i8* %84 to { i64 (i8*, i64)*, i8* }*
  %86 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %85, i32 0, i32 0
  store i64 (i8*, i64)* @fo187, i64 (i8*, i64)** %86
  %87 = call i8* @GC_malloc(i64 0)
  %88 = bitcast i8* %87 to {}*
  %89 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %85, i32 0, i32 1
  store i8* %83, i8** %89
  %90 = call i8* @GC_malloc(i64 0)
  %91 = bitcast i8* %90 to {}*
  %92 = getelementptr { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %82, i64 0
  store { i64 (i8*, i64)*, i8* }* %85, { i64 (i8*, i64)*, i8* }** %92
  %93 = call i8* @GC_malloc(i64 0)
  %94 = bitcast i8* %93 to {}*
  %95 = getelementptr { { i64 (i8*, i64)*, i8* }**, i64 }, { { i64 (i8*, i64)*, i8* }**, i64 }* %27, i32 0, i32 0
  %96 = load { i64 (i8*, i64)*, i8* }**, { i64 (i8*, i64)*, i8* }*** %95
  %97 = getelementptr { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %96, i64 0
  %98 = load { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %97
  %99 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %98, i32 0, i32 0
  %100 = load i64 (i8*, i64)*, i64 (i8*, i64)** %99
  %101 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %98, i32 0, i32 1
  %102 = load i8*, i8** %101
  %103 = call i64 %100(i8* %102, i64 1)
  %104 = call {}* @print_int56(i64 %103)
  ret i32 0
}
