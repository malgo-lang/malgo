; ModuleID = './examples/polyfun2.mlg'
source_filename = "./examples/polyfun2.mlg"

define i8* @"$fo161"(i8*, i8*) {
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

define i8* @"$fo137"(i8*, i8*) {
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

define i64 @"$fo107"(i8*, i64) {
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

declare {}* @print_int(i64)

define {}* @print_int10(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

declare i8* @GC_malloc(i64)

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
  %8 = call i8* @GC_malloc(i64 0)
  %9 = bitcast i8* %8 to {}*
  %10 = bitcast {}* %9 to i8*
  %11 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %12 = bitcast i8* %11 to { i8* (i8*, i8*)*, i8* }*
  %13 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %12, i32 0, i32 0
  store i8* (i8*, i8*)* @id1, i8* (i8*, i8*)** %13
  %14 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %12, i32 0, i32 1
  store i8* %10, i8** %14
  %15 = call i8* @GC_malloc(i64 0)
  %16 = bitcast i8* %15 to {}*
  %17 = bitcast {}* %16 to i8*
  %18 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %19 = bitcast i8* %18 to { i64 (i8*, i64)*, i8* }*
  %20 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %19, i32 0, i32 0
  store i64 (i8*, i64)* @addOne2, i64 (i8*, i64)** %20
  %21 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %19, i32 0, i32 1
  store i8* %17, i8** %21
  %22 = call i8* @GC_malloc(i64 ptrtoint ({ { i8* (i8*, i8*)*, i8* }*, i64 }* getelementptr inbounds ({ { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* null, i32 1) to i64))
  %23 = bitcast i8* %22 to { { i8* (i8*, i8*)*, i8* }*, i64 }*
  %24 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* %23, i32 0, i32 0
  store { i8* (i8*, i8*)*, i8* }* %12, { i8* (i8*, i8*)*, i8* }** %24
  %25 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* %23, i32 0, i32 1
  store i64 1, i64* %25
  %26 = mul i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), 1
  %27 = call i8* @GC_malloc(i64 %26)
  %28 = bitcast i8* %27 to { { i8* (i8*, i8*)*, i8* }*, i64 }**
  %29 = call i8* @GC_malloc(i64 ptrtoint ({ { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* getelementptr inbounds ({ { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* null, i32 1) to i64))
  %30 = bitcast i8* %29 to { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }*
  %31 = getelementptr { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* %30, i32 0, i32 0
  store { { i8* (i8*, i8*)*, i8* }*, i64 }** %28, { { i8* (i8*, i8*)*, i8* }*, i64 }*** %31
  %32 = getelementptr { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* %30, i32 0, i32 1
  store i64 1, i64* %32
  %33 = alloca i64
  store i64 0, i64* %33
  br label %cond_0

cond_0:                                           ; preds = %body_0, %0
  %34 = load i64, i64* %33
  %35 = icmp slt i64 %34, 1
  br i1 %35, label %body_0, label %end_0

body_0:                                           ; preds = %cond_0
  %36 = getelementptr { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* %30, i32 0, i32 0
  %37 = load { { i8* (i8*, i8*)*, i8* }*, i64 }**, { { i8* (i8*, i8*)*, i8* }*, i64 }*** %36
  %38 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }*, { { i8* (i8*, i8*)*, i8* }*, i64 }** %37, i64 %34
  store { { i8* (i8*, i8*)*, i8* }*, i64 }* %23, { { i8* (i8*, i8*)*, i8* }*, i64 }** %38
  %39 = add i64 %34, 1
  store i64 %39, i64* %33
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %40 = getelementptr { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* %30, i32 0, i32 0
  %41 = load { { i8* (i8*, i8*)*, i8* }*, i64 }**, { { i8* (i8*, i8*)*, i8* }*, i64 }*** %40
  %42 = getelementptr { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* %30, i32 0, i32 1
  %43 = load i64, i64* %42
  %44 = mul i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), %43
  %45 = call i8* @GC_malloc(i64 %44)
  %46 = bitcast i8* %45 to { { i64 (i8*, i64)*, i8* }*, i64 }**
  %47 = call i8* @GC_malloc(i64 ptrtoint ({ { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* getelementptr inbounds ({ { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* null, i32 1) to i64))
  %48 = bitcast i8* %47 to { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }*
  %49 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %48, i32 0, i32 0
  store { { i64 (i8*, i64)*, i8* }*, i64 }** %46, { { i64 (i8*, i64)*, i8* }*, i64 }*** %49
  %50 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %48, i32 0, i32 1
  store i64 %43, i64* %50
  %51 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %48, i32 0, i32 0
  %52 = load { { i64 (i8*, i64)*, i8* }*, i64 }**, { { i64 (i8*, i64)*, i8* }*, i64 }*** %51
  %53 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %48, i32 0, i32 1
  store i64 %43, i64* %53
  %54 = alloca i64
  store i64 0, i64* %54
  br label %cond_1

cond_1:                                           ; preds = %body_1, %end_0
  %55 = load i64, i64* %54
  %56 = icmp slt i64 %55, %43
  br i1 %56, label %body_1, label %end_1

body_1:                                           ; preds = %cond_1
  %57 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }*, { { i8* (i8*, i8*)*, i8* }*, i64 }** %41, i64 %55
  %58 = load { { i8* (i8*, i8*)*, i8* }*, i64 }*, { { i8* (i8*, i8*)*, i8* }*, i64 }** %57
  %59 = call i8* @GC_malloc(i64 ptrtoint ({ { i64 (i8*, i64)*, i8* }*, i64 }* getelementptr inbounds ({ { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* null, i32 1) to i64))
  %60 = bitcast i8* %59 to { { i64 (i8*, i64)*, i8* }*, i64 }*
  %61 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* %58, i32 0, i32 0
  %62 = load { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }** %61
  %63 = bitcast { i8* (i8*, i8*)*, i8* }* %62 to i8*
  %64 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %65 = bitcast i8* %64 to { i64 (i8*, i64)*, i8* }*
  %66 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %65, i32 0, i32 0
  store i64 (i8*, i64)* @"$fo107", i64 (i8*, i64)** %66
  %67 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %65, i32 0, i32 1
  store i8* %63, i8** %67
  %68 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %60, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* %65, { i64 (i8*, i64)*, i8* }** %68
  %69 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* %58, i32 0, i32 1
  %70 = load i64, i64* %69
  %71 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %60, i32 0, i32 1
  store i64 %70, i64* %71
  %72 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %52, i64 %55
  store { { i64 (i8*, i64)*, i8* }*, i64 }* %60, { { i64 (i8*, i64)*, i8* }*, i64 }** %72
  %73 = add i64 %55, 1
  store i64 %73, i64* %54
  br label %cond_1

end_1:                                            ; preds = %cond_1
  %74 = call i8* @GC_malloc(i64 ptrtoint ({ { i64 (i8*, i64)*, i8* }*, i64 }* getelementptr inbounds ({ { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* null, i32 1) to i64))
  %75 = bitcast i8* %74 to { { i64 (i8*, i64)*, i8* }*, i64 }*
  %76 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %75, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* %19, { i64 (i8*, i64)*, i8* }** %76
  %77 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %75, i32 0, i32 1
  store i64 1, i64* %77
  %78 = mul i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), 1
  %79 = call i8* @GC_malloc(i64 %78)
  %80 = bitcast i8* %79 to { { i64 (i8*, i64)*, i8* }*, i64 }**
  %81 = call i8* @GC_malloc(i64 ptrtoint ({ { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* getelementptr inbounds ({ { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* null, i32 1) to i64))
  %82 = bitcast i8* %81 to { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }*
  %83 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %82, i32 0, i32 0
  store { { i64 (i8*, i64)*, i8* }*, i64 }** %80, { { i64 (i8*, i64)*, i8* }*, i64 }*** %83
  %84 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %82, i32 0, i32 1
  store i64 1, i64* %84
  %85 = alloca i64
  store i64 0, i64* %85
  br label %cond_2

cond_2:                                           ; preds = %body_2, %end_1
  %86 = load i64, i64* %85
  %87 = icmp slt i64 %86, 1
  br i1 %87, label %body_2, label %end_2

body_2:                                           ; preds = %cond_2
  %88 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %82, i32 0, i32 0
  %89 = load { { i64 (i8*, i64)*, i8* }*, i64 }**, { { i64 (i8*, i64)*, i8* }*, i64 }*** %88
  %90 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %89, i64 %86
  store { { i64 (i8*, i64)*, i8* }*, i64 }* %75, { { i64 (i8*, i64)*, i8* }*, i64 }** %90
  %91 = add i64 %86, 1
  store i64 %91, i64* %85
  br label %cond_2

end_2:                                            ; preds = %cond_2
  %92 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %48, i32 0, i32 0
  %93 = load { { i64 (i8*, i64)*, i8* }*, i64 }**, { { i64 (i8*, i64)*, i8* }*, i64 }*** %92
  %94 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %93, i64 0
  %95 = load { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %94
  %96 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 0
  %97 = load i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %96
  %98 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 1
  %99 = load i8*, i8** %98
  %100 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %101 = bitcast i8* %100 to { { i8* (i8*, i8*)*, i8* }*, i8* }*
  %102 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %95, i32 0, i32 0
  %103 = load { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %102
  %104 = bitcast { i64 (i8*, i64)*, i8* }* %103 to i8*
  %105 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %106 = bitcast i8* %105 to { i8* (i8*, i8*)*, i8* }*
  %107 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %106, i32 0, i32 0
  store i8* (i8*, i8*)* @"$fo137", i8* (i8*, i8*)** %107
  %108 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %106, i32 0, i32 1
  store i8* %104, i8** %108
  %109 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %101, i32 0, i32 0
  store { i8* (i8*, i8*)*, i8* }* %106, { i8* (i8*, i8*)*, i8* }** %109
  %110 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %95, i32 0, i32 1
  %111 = load i64, i64* %110
  %112 = inttoptr i64 %111 to i8*
  %113 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %101, i32 0, i32 1
  store i8* %112, i8** %113
  %114 = call i8* %97(i8* %99, { { i8* (i8*, i8*)*, i8* }*, i8* }* %101)
  %115 = ptrtoint i8* %114 to i64
  %116 = call {}* @print_int10(i64 %115)
  %117 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %82, i32 0, i32 0
  %118 = load { { i64 (i8*, i64)*, i8* }*, i64 }**, { { i64 (i8*, i64)*, i8* }*, i64 }*** %117
  %119 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %118, i64 0
  %120 = load { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %119
  %121 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 0
  %122 = load i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %121
  %123 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 1
  %124 = load i8*, i8** %123
  %125 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %126 = bitcast i8* %125 to { { i8* (i8*, i8*)*, i8* }*, i8* }*
  %127 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %120, i32 0, i32 0
  %128 = load { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %127
  %129 = bitcast { i64 (i8*, i64)*, i8* }* %128 to i8*
  %130 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %131 = bitcast i8* %130 to { i8* (i8*, i8*)*, i8* }*
  %132 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %131, i32 0, i32 0
  store i8* (i8*, i8*)* @"$fo161", i8* (i8*, i8*)** %132
  %133 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %131, i32 0, i32 1
  store i8* %129, i8** %133
  %134 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %126, i32 0, i32 0
  store { i8* (i8*, i8*)*, i8* }* %131, { i8* (i8*, i8*)*, i8* }** %134
  %135 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %120, i32 0, i32 1
  %136 = load i64, i64* %135
  %137 = inttoptr i64 %136 to i8*
  %138 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %126, i32 0, i32 1
  store i8* %137, i8** %138
  %139 = call i8* %122(i8* %124, { { i8* (i8*, i8*)*, i8* }*, i8* }* %126)
  %140 = ptrtoint i8* %139 to i64
  %141 = call {}* @print_int10(i64 %140)
  ret i32 0
}
