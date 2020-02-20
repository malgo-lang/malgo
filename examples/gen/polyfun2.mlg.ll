; ModuleID = './examples/polyfun2.mlg'
source_filename = "./examples/polyfun2.mlg"

define i8* @"$fo164"(i8*, i8*) {
  %3 = bitcast i8* %0 to { i64 (i8*, i8*)*, i8* }*
  %4 = getelementptr { i64 (i8*, i8*)*, i8* }, { i64 (i8*, i8*)*, i8* }* %3, i32 0, i32 0
  %5 = load i64 (i8*, i8*)*, i64 (i8*, i8*)** %4
  %6 = getelementptr { i64 (i8*, i8*)*, i8* }, { i64 (i8*, i8*)*, i8* }* %3, i32 0, i32 1
  %7 = load i8*, i8** %6
  %8 = call i64 %5(i8* %7, i8* %1)
  %9 = inttoptr i64 %8 to i8*
  ret i8* %9
}

define i64 @"$fo113"(i8*, i8*) {
  %3 = bitcast i8* %0 to { i64 (i8*, i64)*, i8* }*
  %4 = ptrtoint i8* %1 to i64
  %5 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %3, i32 0, i32 0
  %6 = load i64 (i8*, i64)*, i64 (i8*, i64)** %5
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %3, i32 0, i32 1
  %8 = load i8*, i8** %7
  %9 = call i64 %6(i8* %8, i64 %4)
  ret i64 %9
}

declare i8* @GC_malloc(i64)

define i8* @"$fo86"(i8*, i8*) {
  %3 = bitcast i8* %0 to { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }*
  %4 = bitcast i8* %1 to { i8*, i8* }*
  %5 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %6 = bitcast i8* %5 to { { i8* (i8*, i8*)*, i8* }*, i8* }*
  %7 = getelementptr { i8*, i8* }, { i8*, i8* }* %4, i32 0, i32 0
  %8 = load i8*, i8** %7
  %9 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %10 = bitcast i8* %9 to { i8* (i8*, i8*)*, i8* }*
  %11 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %10, i32 0, i32 0
  store i8* (i8*, i8*)* @"$fo93", i8* (i8*, i8*)** %11
  %12 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %10, i32 0, i32 1
  store i8* %8, i8** %12
  %13 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %6, i32 0, i32 0
  store { i8* (i8*, i8*)*, i8* }* %10, { i8* (i8*, i8*)*, i8* }** %13
  %14 = getelementptr { i8*, i8* }, { i8*, i8* }* %4, i32 0, i32 1
  %15 = load i8*, i8** %14
  %16 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %6, i32 0, i32 1
  store i8* %15, i8** %16
  %17 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %3, i32 0, i32 0
  %18 = load i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %17
  %19 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %3, i32 0, i32 1
  %20 = load i8*, i8** %19
  %21 = call i8* %18(i8* %20, { { i8* (i8*, i8*)*, i8* }*, i8* }* %6)
  ret i8* %21
}

define i8* @"$fo93"(i8*, i8*) {
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

define i8* @id1(i8*, i8*) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }*
  %6 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 0
  store i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)* @f0, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %6
  %7 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %7
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i8* (i8*, i8*)*, i8* }*
  %10 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %9, i32 0, i32 0
  store i8* (i8*, i8*)* @id1, i8* (i8*, i8*)** %10
  %11 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { i64 (i8*, i64)*, i8* }*
  %14 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %13, i32 0, i32 0
  store i64 (i8*, i64)* @addOne2, i64 (i8*, i64)** %14
  %15 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %13, i32 0, i32 1
  store i8* %0, i8** %15
  ret i8* %1
}

define i8* @f0(i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }*
  %6 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 0
  store i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)* @f0, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %6
  %7 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %7
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i8* (i8*, i8*)*, i8* }*
  %10 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %9, i32 0, i32 0
  store i8* (i8*, i8*)* @id1, i8* (i8*, i8*)** %10
  %11 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { i64 (i8*, i64)*, i8* }*
  %14 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %13, i32 0, i32 0
  store i64 (i8*, i64)* @addOne2, i64 (i8*, i64)** %14
  %15 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %13, i32 0, i32 1
  store i8* %0, i8** %15
  %16 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %1, i32 0, i32 0
  %17 = load { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }** %16
  %18 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %1, i32 0, i32 1
  %19 = load i8*, i8** %18
  %20 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %17, i32 0, i32 0
  %21 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %20
  %22 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %17, i32 0, i32 1
  %23 = load i8*, i8** %22
  %24 = call i8* %21(i8* %23, i8* %19)
  ret i8* %24
}

define i64 @addOne2(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }*
  %6 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 0
  store i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)* @f0, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %6
  %7 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %7
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i8* (i8*, i8*)*, i8* }*
  %10 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %9, i32 0, i32 0
  store i8* (i8*, i8*)* @id1, i8* (i8*, i8*)** %10
  %11 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { i64 (i8*, i64)*, i8* }*
  %14 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %13, i32 0, i32 0
  store i64 (i8*, i64)* @addOne2, i64 (i8*, i64)** %14
  %15 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %13, i32 0, i32 1
  store i8* %0, i8** %15
  %16 = add i64 %1, 1
  ret i64 %16
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  %3 = bitcast {}* %2 to i8*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }*
  %6 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 0
  store i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)* @f0, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %6
  %7 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 1
  store i8* %3, i8** %7
  %8 = bitcast { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5 to i8*
  %9 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %10 = bitcast i8* %9 to { i8* (i8*, i8*)*, i8* }*
  %11 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %10, i32 0, i32 0
  store i8* (i8*, i8*)* @"$fo86", i8* (i8*, i8*)** %11
  %12 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %10, i32 0, i32 1
  store i8* %8, i8** %12
  %13 = call i8* @GC_malloc(i64 0)
  %14 = bitcast i8* %13 to {}*
  %15 = bitcast {}* %14 to i8*
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { i8* (i8*, i8*)*, i8* }*
  %18 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %17, i32 0, i32 0
  store i8* (i8*, i8*)* @id1, i8* (i8*, i8*)** %18
  %19 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %17, i32 0, i32 1
  store i8* %15, i8** %19
  %20 = call i8* @GC_malloc(i64 0)
  %21 = bitcast i8* %20 to {}*
  %22 = bitcast {}* %21 to i8*
  %23 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %24 = bitcast i8* %23 to { i64 (i8*, i64)*, i8* }*
  %25 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %24, i32 0, i32 0
  store i64 (i8*, i64)* @addOne2, i64 (i8*, i64)** %25
  %26 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %24, i32 0, i32 1
  store i8* %22, i8** %26
  %27 = bitcast { i64 (i8*, i64)*, i8* }* %24 to i8*
  %28 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %29 = bitcast i8* %28 to { i64 (i8*, i8*)*, i8* }*
  %30 = getelementptr { i64 (i8*, i8*)*, i8* }, { i64 (i8*, i8*)*, i8* }* %29, i32 0, i32 0
  store i64 (i8*, i8*)* @"$fo113", i64 (i8*, i8*)** %30
  %31 = getelementptr { i64 (i8*, i8*)*, i8* }, { i64 (i8*, i8*)*, i8* }* %29, i32 0, i32 1
  store i8* %27, i8** %31
  %32 = call i8* @GC_malloc(i64 ptrtoint ({ { i8* (i8*, i8*)*, i8* }*, i64 }* getelementptr inbounds ({ { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* null, i32 1) to i64))
  %33 = bitcast i8* %32 to { { i8* (i8*, i8*)*, i8* }*, i64 }*
  %34 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* %33, i32 0, i32 0
  store { i8* (i8*, i8*)*, i8* }* %17, { i8* (i8*, i8*)*, i8* }** %34
  %35 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* %33, i32 0, i32 1
  store i64 1, i64* %35
  %36 = mul i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), 1
  %37 = call i8* @GC_malloc(i64 %36)
  %38 = bitcast i8* %37 to { { i8* (i8*, i8*)*, i8* }*, i64 }**
  %39 = call i8* @GC_malloc(i64 ptrtoint ({ { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* getelementptr inbounds ({ { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* null, i32 1) to i64))
  %40 = bitcast i8* %39 to { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }*
  %41 = getelementptr { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* %40, i32 0, i32 0
  store { { i8* (i8*, i8*)*, i8* }*, i64 }** %38, { { i8* (i8*, i8*)*, i8* }*, i64 }*** %41
  %42 = getelementptr { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* %40, i32 0, i32 1
  store i64 1, i64* %42
  %43 = alloca i64
  store i64 0, i64* %43
  br label %cond_0

cond_0:                                           ; preds = %body_0, %0
  %44 = load i64, i64* %43
  %45 = icmp slt i64 %44, 1
  br i1 %45, label %body_0, label %end_0

body_0:                                           ; preds = %cond_0
  %46 = getelementptr { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* %40, i32 0, i32 0
  %47 = load { { i8* (i8*, i8*)*, i8* }*, i64 }**, { { i8* (i8*, i8*)*, i8* }*, i64 }*** %46
  %48 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }*, { { i8* (i8*, i8*)*, i8* }*, i64 }** %47, i64 %44
  store { { i8* (i8*, i8*)*, i8* }*, i64 }* %33, { { i8* (i8*, i8*)*, i8* }*, i64 }** %48
  %49 = add i64 %44, 1
  store i64 %49, i64* %43
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %50 = call i8* @GC_malloc(i64 ptrtoint ({ { i64 (i8*, i8*)*, i8* }*, i64 }* getelementptr inbounds ({ { i64 (i8*, i8*)*, i8* }*, i64 }, { { i64 (i8*, i8*)*, i8* }*, i64 }* null, i32 1) to i64))
  %51 = bitcast i8* %50 to { { i64 (i8*, i8*)*, i8* }*, i64 }*
  %52 = getelementptr { { i64 (i8*, i8*)*, i8* }*, i64 }, { { i64 (i8*, i8*)*, i8* }*, i64 }* %51, i32 0, i32 0
  store { i64 (i8*, i8*)*, i8* }* %29, { i64 (i8*, i8*)*, i8* }** %52
  %53 = getelementptr { { i64 (i8*, i8*)*, i8* }*, i64 }, { { i64 (i8*, i8*)*, i8* }*, i64 }* %51, i32 0, i32 1
  store i64 1, i64* %53
  %54 = mul i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), 1
  %55 = call i8* @GC_malloc(i64 %54)
  %56 = bitcast i8* %55 to { { i64 (i8*, i8*)*, i8* }*, i64 }**
  %57 = call i8* @GC_malloc(i64 ptrtoint ({ { { i64 (i8*, i8*)*, i8* }*, i64 }**, i64 }* getelementptr inbounds ({ { { i64 (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i8*)*, i8* }*, i64 }**, i64 }* null, i32 1) to i64))
  %58 = bitcast i8* %57 to { { { i64 (i8*, i8*)*, i8* }*, i64 }**, i64 }*
  %59 = getelementptr { { { i64 (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i8*)*, i8* }*, i64 }**, i64 }* %58, i32 0, i32 0
  store { { i64 (i8*, i8*)*, i8* }*, i64 }** %56, { { i64 (i8*, i8*)*, i8* }*, i64 }*** %59
  %60 = getelementptr { { { i64 (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i8*)*, i8* }*, i64 }**, i64 }* %58, i32 0, i32 1
  store i64 1, i64* %60
  %61 = alloca i64
  store i64 0, i64* %61
  br label %cond_1

cond_1:                                           ; preds = %body_1, %end_0
  %62 = load i64, i64* %61
  %63 = icmp slt i64 %62, 1
  br i1 %63, label %body_1, label %end_1

body_1:                                           ; preds = %cond_1
  %64 = getelementptr { { { i64 (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i8*)*, i8* }*, i64 }**, i64 }* %58, i32 0, i32 0
  %65 = load { { i64 (i8*, i8*)*, i8* }*, i64 }**, { { i64 (i8*, i8*)*, i8* }*, i64 }*** %64
  %66 = getelementptr { { i64 (i8*, i8*)*, i8* }*, i64 }*, { { i64 (i8*, i8*)*, i8* }*, i64 }** %65, i64 %62
  store { { i64 (i8*, i8*)*, i8* }*, i64 }* %51, { { i64 (i8*, i8*)*, i8* }*, i64 }** %66
  %67 = add i64 %62, 1
  store i64 %67, i64* %61
  br label %cond_1

end_1:                                            ; preds = %cond_1
  %68 = getelementptr { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* %40, i32 0, i32 0
  %69 = load { { i8* (i8*, i8*)*, i8* }*, i64 }**, { { i8* (i8*, i8*)*, i8* }*, i64 }*** %68
  %70 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }*, { { i8* (i8*, i8*)*, i8* }*, i64 }** %69, i64 0
  %71 = load { { i8* (i8*, i8*)*, i8* }*, i64 }*, { { i8* (i8*, i8*)*, i8* }*, i64 }** %70
  %72 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %10, i32 0, i32 0
  %73 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %72
  %74 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %10, i32 0, i32 1
  %75 = load i8*, i8** %74
  %76 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %77 = bitcast i8* %76 to { i8*, i8* }*
  %78 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* %71, i32 0, i32 0
  %79 = load { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }** %78
  %80 = bitcast { i8* (i8*, i8*)*, i8* }* %79 to i8*
  %81 = getelementptr { i8*, i8* }, { i8*, i8* }* %77, i32 0, i32 0
  store i8* %80, i8** %81
  %82 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* %71, i32 0, i32 1
  %83 = load i64, i64* %82
  %84 = inttoptr i64 %83 to i8*
  %85 = getelementptr { i8*, i8* }, { i8*, i8* }* %77, i32 0, i32 1
  store i8* %84, i8** %85
  %86 = bitcast { i8*, i8* }* %77 to i8*
  %87 = call i8* %73(i8* %75, i8* %86)
  %88 = ptrtoint i8* %87 to i64
  %89 = call {}* @print_int10(i64 %88)
  %90 = getelementptr { { { i64 (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i8*)*, i8* }*, i64 }**, i64 }* %58, i32 0, i32 0
  %91 = load { { i64 (i8*, i8*)*, i8* }*, i64 }**, { { i64 (i8*, i8*)*, i8* }*, i64 }*** %90
  %92 = getelementptr { { i64 (i8*, i8*)*, i8* }*, i64 }*, { { i64 (i8*, i8*)*, i8* }*, i64 }** %91, i64 0
  %93 = load { { i64 (i8*, i8*)*, i8* }*, i64 }*, { { i64 (i8*, i8*)*, i8* }*, i64 }** %92
  %94 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %10, i32 0, i32 0
  %95 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %94
  %96 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %10, i32 0, i32 1
  %97 = load i8*, i8** %96
  %98 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %99 = bitcast i8* %98 to { i8*, i8* }*
  %100 = getelementptr { { i64 (i8*, i8*)*, i8* }*, i64 }, { { i64 (i8*, i8*)*, i8* }*, i64 }* %93, i32 0, i32 0
  %101 = load { i64 (i8*, i8*)*, i8* }*, { i64 (i8*, i8*)*, i8* }** %100
  %102 = bitcast { i64 (i8*, i8*)*, i8* }* %101 to i8*
  %103 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %104 = bitcast i8* %103 to { i8* (i8*, i8*)*, i8* }*
  %105 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %104, i32 0, i32 0
  store i8* (i8*, i8*)* @"$fo164", i8* (i8*, i8*)** %105
  %106 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %104, i32 0, i32 1
  store i8* %102, i8** %106
  %107 = bitcast { i8* (i8*, i8*)*, i8* }* %104 to i8*
  %108 = getelementptr { i8*, i8* }, { i8*, i8* }* %99, i32 0, i32 0
  store i8* %107, i8** %108
  %109 = getelementptr { { i64 (i8*, i8*)*, i8* }*, i64 }, { { i64 (i8*, i8*)*, i8* }*, i64 }* %93, i32 0, i32 1
  %110 = load i64, i64* %109
  %111 = inttoptr i64 %110 to i8*
  %112 = getelementptr { i8*, i8* }, { i8*, i8* }* %99, i32 0, i32 1
  store i8* %111, i8** %112
  %113 = bitcast { i8*, i8* }* %99 to i8*
  %114 = call i8* %95(i8* %97, i8* %113)
  %115 = ptrtoint i8* %114 to i64
  %116 = call {}* @print_int10(i64 %115)
  ret i32 0
}
