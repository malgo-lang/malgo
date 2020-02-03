; ModuleID = './examples/polyfun1.mlg'
source_filename = "./examples/polyfun1.mlg"

define i8* @"$fo86"(i8*, i8*) {
  %3 = bitcast i8* %0 to { i64 (i8*, i64)*, i8* }*
  %4 = ptrtoint i8* %1 to i64
  %5 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %3, i32 0, i32 0
  %6 = load i64 (i8*, i64)*, i64 (i8*, i64)** %5
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %3, i32 0, i32 1
  %8 = load i8*, i8** %7
  %9 = call i64 %6(i8* %8, i64 %4)
  %10 = inttoptr i64 %9 to i8*
  ret i8* %10
}

declare i8* @GC_malloc(i64)

define i64 @succ1(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }*
  %6 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 0
  store i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)* @f0, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %6
  %7 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %7
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i64 (i8*, i64)*, i8* }*
  %10 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 0
  store i64 (i8*, i64)* @succ1, i64 (i8*, i64)** %10
  %11 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = add i64 %1, 1
  ret i64 %12
}

declare {}* @print_int(i64)

define {}* @print_int7(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
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
  %9 = bitcast i8* %8 to { i64 (i8*, i64)*, i8* }*
  %10 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 0
  store i64 (i8*, i64)* @succ1, i64 (i8*, i64)** %10
  %11 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %1, i32 0, i32 0
  %13 = load { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }** %12
  %14 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %1, i32 0, i32 1
  %15 = load i8*, i8** %14
  %16 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %13, i32 0, i32 0
  %17 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %16
  %18 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %13, i32 0, i32 1
  %19 = load i8*, i8** %18
  %20 = call i8* %17(i8* %19, i8* %15)
  ret i8* %20
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
  %8 = call i8* @GC_malloc(i64 0)
  %9 = bitcast i8* %8 to {}*
  %10 = bitcast {}* %9 to i8*
  %11 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %12 = bitcast i8* %11 to { i64 (i8*, i64)*, i8* }*
  %13 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %12, i32 0, i32 0
  store i64 (i8*, i64)* @succ1, i64 (i8*, i64)** %13
  %14 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %12, i32 0, i32 1
  store i8* %10, i8** %14
  %15 = call i8* @GC_malloc(i64 ptrtoint ({ { i64 (i8*, i64)*, i8* }*, i64 }* getelementptr inbounds ({ { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* null, i32 1) to i64))
  %16 = bitcast i8* %15 to { { i64 (i8*, i64)*, i8* }*, i64 }*
  %17 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %16, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* %12, { i64 (i8*, i64)*, i8* }** %17
  %18 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %16, i32 0, i32 1
  store i64 1, i64* %18
  %19 = mul i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), 1
  %20 = call i8* @GC_malloc(i64 %19)
  %21 = bitcast i8* %20 to { { i64 (i8*, i64)*, i8* }*, i64 }**
  %22 = call i8* @GC_malloc(i64 ptrtoint ({ { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* getelementptr inbounds ({ { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* null, i32 1) to i64))
  %23 = bitcast i8* %22 to { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }*
  %24 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %23, i32 0, i32 0
  store { { i64 (i8*, i64)*, i8* }*, i64 }** %21, { { i64 (i8*, i64)*, i8* }*, i64 }*** %24
  %25 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %23, i32 0, i32 1
  store i64 1, i64* %25
  %26 = alloca i64
  store i64 0, i64* %26
  br label %cond_0

cond_0:                                           ; preds = %body_0, %0
  %27 = load i64, i64* %26
  %28 = icmp slt i64 %27, 1
  br i1 %28, label %body_0, label %end_0

body_0:                                           ; preds = %cond_0
  %29 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %23, i32 0, i32 0
  %30 = load { { i64 (i8*, i64)*, i8* }*, i64 }**, { { i64 (i8*, i64)*, i8* }*, i64 }*** %29
  %31 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %30, i64 %27
  store { { i64 (i8*, i64)*, i8* }*, i64 }* %16, { { i64 (i8*, i64)*, i8* }*, i64 }** %31
  %32 = add i64 %27, 1
  store i64 %32, i64* %26
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %33 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %23, i32 0, i32 0
  %34 = load { { i64 (i8*, i64)*, i8* }*, i64 }**, { { i64 (i8*, i64)*, i8* }*, i64 }*** %33
  %35 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %34, i64 0
  %36 = load { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %35
  %37 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 0
  %38 = load i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %37
  %39 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 1
  %40 = load i8*, i8** %39
  %41 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %42 = bitcast i8* %41 to { { i8* (i8*, i8*)*, i8* }*, i8* }*
  %43 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %36, i32 0, i32 0
  %44 = load { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %43
  %45 = bitcast { i64 (i8*, i64)*, i8* }* %44 to i8*
  %46 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %47 = bitcast i8* %46 to { i8* (i8*, i8*)*, i8* }*
  %48 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %47, i32 0, i32 0
  store i8* (i8*, i8*)* @"$fo86", i8* (i8*, i8*)** %48
  %49 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %47, i32 0, i32 1
  store i8* %45, i8** %49
  %50 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %42, i32 0, i32 0
  store { i8* (i8*, i8*)*, i8* }* %47, { i8* (i8*, i8*)*, i8* }** %50
  %51 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %36, i32 0, i32 1
  %52 = load i64, i64* %51
  %53 = inttoptr i64 %52 to i8*
  %54 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %42, i32 0, i32 1
  store i8* %53, i8** %54
  %55 = call i8* %38(i8* %40, { { i8* (i8*, i8*)*, i8* }*, i8* }* %42)
  %56 = ptrtoint i8* %55 to i64
  %57 = call {}* @print_int7(i64 %56)
  ret i32 0
}
