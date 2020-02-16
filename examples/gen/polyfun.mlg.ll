; ModuleID = './examples/polyfun.mlg'
source_filename = "./examples/polyfun.mlg"

define i64 @"$fo125"(i8*, i64) {
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

define { i64, { i8**, i64 }* }* @"$fo109"(i8*, i8*) {
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
  %15 = getelementptr { i8*, { i8**, i64 }* }, { i8*, { i8**, i64 }* }* %8, i32 0, i32 1
  %16 = load { i8**, i64 }*, { i8**, i64 }** %15
  %17 = getelementptr { i64, { i8**, i64 }* }, { i64, { i8**, i64 }* }* %10, i32 0, i32 1
  store { i8**, i64 }* %16, { i8**, i64 }** %17
  ret { i64, { i8**, i64 }* }* %10
}

define i8* @"$fo95"(i8*, i8*) {
  %3 = bitcast i8* %0 to { i8* (i8*, i8*)*, i8* }*
  %4 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i32 0, i32 0
  %5 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %4
  %6 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i32 0, i32 1
  %7 = load i8*, i8** %6
  %8 = call i8* %5(i8* %7, i8* %1)
  ret i8* %8
}

declare {}* @print_int(i64)

define {}* @print_int10(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

define i8* @id0(i8*, i8*) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i8* (i8*, i8*)*, i8* }*
  %6 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 0
  store i8* (i8*, i8*)* @id0, i8* (i8*, i8*)** %6
  %7 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %7
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i64 (i8*, i64)*, i8* }*
  %10 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 0
  store i64 (i8*, i64)* @addOne1, i64 (i8*, i64)** %10
  %11 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %11
  ret i8* %1
}

define i64 @addOne1(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i8* (i8*, i8*)*, i8* }*
  %6 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 0
  store i8* (i8*, i8*)* @id0, i8* (i8*, i8*)** %6
  %7 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %7
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i64 (i8*, i64)*, i8* }*
  %10 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 0
  store i64 (i8*, i64)* @addOne1, i64 (i8*, i64)** %10
  %11 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = add i64 %1, 1
  ret i64 %12
}

define { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* @"$lambda34"(i8*, i8*) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }*
  %6 = getelementptr { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }, { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }* %5, i32 0, i32 0
  store { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)* @"$lambda34", { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)** %6
  %7 = getelementptr { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }, { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %7
  %8 = call i8* @GC_malloc(i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64))
  %9 = bitcast i8* %8 to { i8* }*
  %10 = getelementptr { i8* }, { i8* }* %9, i32 0, i32 0
  store i8* %1, i8** %10
  %11 = bitcast { i8* }* %9 to i8*
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }*
  %14 = getelementptr { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %13, i32 0, i32 0
  store { i8*, { i8**, i64 }* }* (i8*, i8*)* @"$lambda33", { i8*, { i8**, i64 }* }* (i8*, i8*)** %14
  %15 = getelementptr { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %13, i32 0, i32 1
  store i8* %11, i8** %15
  ret { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %13
}

define { i8*, { i8**, i64 }* }* @"$lambda33"(i8*, i8*) {
  %3 = bitcast i8* %0 to { i8* }*
  %4 = getelementptr { i8* }, { i8* }* %3, i32 0, i32 0
  %5 = load i8*, i8** %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }*
  %8 = getelementptr { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %7, i32 0, i32 0
  store { i8*, { i8**, i64 }* }* (i8*, i8*)* @"$lambda33", { i8*, { i8**, i64 }* }* (i8*, i8*)** %8
  %9 = getelementptr { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %7, i32 0, i32 1
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
  %24 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %25 = bitcast i8* %24 to { i8*, { i8**, i64 }* }*
  %26 = getelementptr { i8*, { i8**, i64 }* }, { i8*, { i8**, i64 }* }* %25, i32 0, i32 0
  store i8* %5, i8** %26
  %27 = getelementptr { i8*, { i8**, i64 }* }, { i8*, { i8**, i64 }* }* %25, i32 0, i32 1
  store { i8**, i64 }* %14, { i8**, i64 }** %27
  ret { i8*, { i8**, i64 }* }* %25
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  %3 = bitcast {}* %2 to i8*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i8* (i8*, i8*)*, i8* }*
  %6 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 0
  store i8* (i8*, i8*)* @id0, i8* (i8*, i8*)** %6
  %7 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 1
  store i8* %3, i8** %7
  %8 = call i8* @GC_malloc(i64 0)
  %9 = bitcast i8* %8 to {}*
  %10 = bitcast {}* %9 to i8*
  %11 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %12 = bitcast i8* %11 to { i64 (i8*, i64)*, i8* }*
  %13 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %12, i32 0, i32 0
  store i64 (i8*, i64)* @addOne1, i64 (i8*, i64)** %13
  %14 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %12, i32 0, i32 1
  store i8* %10, i8** %14
  %15 = mul i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), 1
  %16 = call i8* @GC_malloc(i64 %15)
  %17 = bitcast i8* %16 to { i64 (i8*, i64)*, i8* }**
  %18 = call i8* @GC_malloc(i64 ptrtoint ({ { i64 (i8*, i64)*, i8* }**, i64 }* getelementptr inbounds ({ { i64 (i8*, i64)*, i8* }**, i64 }, { { i64 (i8*, i64)*, i8* }**, i64 }* null, i32 1) to i64))
  %19 = bitcast i8* %18 to { { i64 (i8*, i64)*, i8* }**, i64 }*
  %20 = getelementptr { { i64 (i8*, i64)*, i8* }**, i64 }, { { i64 (i8*, i64)*, i8* }**, i64 }* %19, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }** %17, { i64 (i8*, i64)*, i8* }*** %20
  %21 = getelementptr { { i64 (i8*, i64)*, i8* }**, i64 }, { { i64 (i8*, i64)*, i8* }**, i64 }* %19, i32 0, i32 1
  store i64 1, i64* %21
  %22 = alloca i64
  store i64 0, i64* %22
  br label %cond_0

cond_0:                                           ; preds = %body_0, %0
  %23 = load i64, i64* %22
  %24 = icmp slt i64 %23, 1
  br i1 %24, label %body_0, label %end_0

body_0:                                           ; preds = %cond_0
  %25 = getelementptr { { i64 (i8*, i64)*, i8* }**, i64 }, { { i64 (i8*, i64)*, i8* }**, i64 }* %19, i32 0, i32 0
  %26 = load { i64 (i8*, i64)*, i8* }**, { i64 (i8*, i64)*, i8* }*** %25
  %27 = getelementptr { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %26, i64 %23
  store { i64 (i8*, i64)*, i8* }* %12, { i64 (i8*, i64)*, i8* }** %27
  %28 = add i64 %23, 1
  store i64 %28, i64* %22
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %29 = call i8* @GC_malloc(i64 0)
  %30 = bitcast i8* %29 to {}*
  %31 = bitcast {}* %30 to i8*
  %32 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %33 = bitcast i8* %32 to { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }*
  %34 = getelementptr { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }, { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }* %33, i32 0, i32 0
  store { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)* @"$lambda34", { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)** %34
  %35 = getelementptr { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }, { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }* %33, i32 0, i32 1
  store i8* %31, i8** %35
  %36 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 0
  %37 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %36
  %38 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 1
  %39 = load i8*, i8** %38
  %40 = bitcast { i8* (i8*, i8*)*, i8* }* %5 to i8*
  %41 = call i8* %37(i8* %39, i8* %40)
  %42 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %43 = bitcast i8* %42 to { i8* (i8*, i8*)*, i8* }*
  %44 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %43, i32 0, i32 0
  store i8* (i8*, i8*)* @"$fo95", i8* (i8*, i8*)** %44
  %45 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %43, i32 0, i32 1
  store i8* %41, i8** %45
  %46 = getelementptr { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }, { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }* %33, i32 0, i32 0
  %47 = load { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)** %46
  %48 = getelementptr { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }, { { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, i8* }* %33, i32 0, i32 1
  %49 = load i8*, i8** %48
  %50 = inttoptr i64 1 to i8*
  %51 = call { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %47(i8* %49, i8* %50)
  %52 = bitcast { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %51 to i8*
  %53 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %54 = bitcast i8* %53 to { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }*
  %55 = getelementptr { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }, { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %54, i32 0, i32 0
  store { i64, { i8**, i64 }* }* (i8*, i8*)* @"$fo109", { i64, { i8**, i64 }* }* (i8*, i8*)** %55
  %56 = getelementptr { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }, { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %54, i32 0, i32 1
  store i8* %52, i8** %56
  %57 = getelementptr { { i64 (i8*, i64)*, i8* }**, i64 }, { { i64 (i8*, i64)*, i8* }**, i64 }* %19, i32 0, i32 0
  %58 = load { i64 (i8*, i64)*, i8* }**, { i64 (i8*, i64)*, i8* }*** %57
  %59 = bitcast { i8* (i8*, i8*)*, i8* }* %5 to i8*
  %60 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %61 = bitcast i8* %60 to { i64 (i8*, i64)*, i8* }*
  %62 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %61, i32 0, i32 0
  store i64 (i8*, i64)* @"$fo125", i64 (i8*, i64)** %62
  %63 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %61, i32 0, i32 1
  store i8* %59, i8** %63
  %64 = getelementptr { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %58, i64 0
  store { i64 (i8*, i64)*, i8* }* %61, { i64 (i8*, i64)*, i8* }** %64
  %65 = getelementptr { { i64 (i8*, i64)*, i8* }**, i64 }, { { i64 (i8*, i64)*, i8* }**, i64 }* %19, i32 0, i32 0
  %66 = load { i64 (i8*, i64)*, i8* }**, { i64 (i8*, i64)*, i8* }*** %65
  %67 = getelementptr { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %66, i64 0
  %68 = load { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %67
  %69 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %68, i32 0, i32 0
  %70 = load i64 (i8*, i64)*, i64 (i8*, i64)** %69
  %71 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %68, i32 0, i32 1
  %72 = load i8*, i8** %71
  %73 = call i64 %70(i8* %72, i64 1)
  %74 = call {}* @print_int10(i64 %73)
  ret i32 0
}
