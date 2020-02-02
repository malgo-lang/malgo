; ModuleID = './examples/polyfun2.mlg'
source_filename = "./examples/polyfun2.mlg"

define i8* @"$fo128"(i8*, i8*) {
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

define i8* @"$fo113"(i8*, i8*) {
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

declare {}* @print_int(i64)

define {}* @print_int9(i64) {
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
  %22 = call i8* @GC_malloc(i64 ptrtoint ({ { i64 (i8*, i64)*, i8* }*, i64 }* getelementptr inbounds ({ { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* null, i32 1) to i64))
  %23 = bitcast i8* %22 to { { i64 (i8*, i64)*, i8* }*, i64 }*
  %24 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %23, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* %19, { i64 (i8*, i64)*, i8* }** %24
  %25 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %23, i32 0, i32 1
  store i64 1, i64* %25
  %26 = mul i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), 1
  %27 = call i8* @GC_malloc(i64 %26)
  %28 = bitcast i8* %27 to { { i64 (i8*, i64)*, i8* }*, i64 }**
  %29 = call i8* @GC_malloc(i64 ptrtoint ({ { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* getelementptr inbounds ({ { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* null, i32 1) to i64))
  %30 = bitcast i8* %29 to { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }*
  %31 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %30, i32 0, i32 0
  store { { i64 (i8*, i64)*, i8* }*, i64 }** %28, { { i64 (i8*, i64)*, i8* }*, i64 }*** %31
  %32 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %30, i32 0, i32 1
  store i64 1, i64* %32
  %33 = alloca i64
  store i64 0, i64* %33
  br label %cond_0

cond_0:                                           ; preds = %body_0, %0
  %34 = load i64, i64* %33
  %35 = icmp slt i64 %34, 1
  br i1 %35, label %body_0, label %end_0

body_0:                                           ; preds = %cond_0
  %36 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %30, i32 0, i32 0
  %37 = load { { i64 (i8*, i64)*, i8* }*, i64 }**, { { i64 (i8*, i64)*, i8* }*, i64 }*** %36
  %38 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %37, i64 %34
  store { { i64 (i8*, i64)*, i8* }*, i64 }* %23, { { i64 (i8*, i64)*, i8* }*, i64 }** %38
  %39 = add i64 %34, 1
  store i64 %39, i64* %33
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %40 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %30, i32 0, i32 0
  %41 = load { { i64 (i8*, i64)*, i8* }*, i64 }**, { { i64 (i8*, i64)*, i8* }*, i64 }*** %40
  %42 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %41, i64 0
  %43 = load { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %42
  %44 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 0
  %45 = load i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %44
  %46 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 1
  %47 = load i8*, i8** %46
  %48 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %49 = bitcast i8* %48 to { { i8* (i8*, i8*)*, i8* }*, i8* }*
  %50 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %43, i32 0, i32 0
  %51 = load { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %50
  %52 = bitcast { i64 (i8*, i64)*, i8* }* %51 to i8*
  %53 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %54 = bitcast i8* %53 to { i8* (i8*, i8*)*, i8* }*
  %55 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %54, i32 0, i32 0
  store i8* (i8*, i8*)* @"$fo113", i8* (i8*, i8*)** %55
  %56 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %54, i32 0, i32 1
  store i8* %52, i8** %56
  %57 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %49, i32 0, i32 0
  store { i8* (i8*, i8*)*, i8* }* %54, { i8* (i8*, i8*)*, i8* }** %57
  %58 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %43, i32 0, i32 0
  %59 = load { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %58
  %60 = bitcast { i64 (i8*, i64)*, i8* }* %59 to i8*
  %61 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %62 = bitcast i8* %61 to { i8* (i8*, i8*)*, i8* }*
  %63 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %62, i32 0, i32 0
  store i8* (i8*, i8*)* @"$fo128", i8* (i8*, i8*)** %63
  %64 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %62, i32 0, i32 1
  store i8* %60, i8** %64
  %65 = bitcast { i8* (i8*, i8*)*, i8* }* %62 to i8*
  %66 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %49, i32 0, i32 1
  store i8* %65, i8** %66
  %67 = call i8* %45(i8* %47, { { i8* (i8*, i8*)*, i8* }*, i8* }* %49)
  %68 = ptrtoint i8* %67 to i64
  %69 = call {}* @print_int9(i64 %68)
  ret i32 0
}
