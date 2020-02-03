; ModuleID = './examples/polyfun3.mlg'
source_filename = "./examples/polyfun3.mlg"

define i8* @"$fo111"(i8*, i8*) {
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

define i64 @"$fo85"(i8*, i64) {
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

define {}* @print_int7(i64) {
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
  %12 = bitcast i8* %11 to { i8* (i8*, i8*)*, i8* }*
  %13 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %12, i32 0, i32 0
  store i8* (i8*, i8*)* @id1, i8* (i8*, i8*)** %13
  %14 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %12, i32 0, i32 1
  store i8* %10, i8** %14
  %15 = call i8* @GC_malloc(i64 ptrtoint ({ { i8* (i8*, i8*)*, i8* }*, i64 }* getelementptr inbounds ({ { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* null, i32 1) to i64))
  %16 = bitcast i8* %15 to { { i8* (i8*, i8*)*, i8* }*, i64 }*
  %17 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* %16, i32 0, i32 0
  store { i8* (i8*, i8*)*, i8* }* %12, { i8* (i8*, i8*)*, i8* }** %17
  %18 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* %16, i32 0, i32 1
  store i64 1, i64* %18
  %19 = mul i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), 1
  %20 = call i8* @GC_malloc(i64 %19)
  %21 = bitcast i8* %20 to { { i8* (i8*, i8*)*, i8* }*, i64 }**
  %22 = call i8* @GC_malloc(i64 ptrtoint ({ { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* getelementptr inbounds ({ { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* null, i32 1) to i64))
  %23 = bitcast i8* %22 to { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }*
  %24 = getelementptr { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* %23, i32 0, i32 0
  store { { i8* (i8*, i8*)*, i8* }*, i64 }** %21, { { i8* (i8*, i8*)*, i8* }*, i64 }*** %24
  %25 = getelementptr { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* %23, i32 0, i32 1
  store i64 1, i64* %25
  %26 = alloca i64
  store i64 0, i64* %26
  br label %cond_0

cond_0:                                           ; preds = %body_0, %0
  %27 = load i64, i64* %26
  %28 = icmp slt i64 %27, 1
  br i1 %28, label %body_0, label %end_0

body_0:                                           ; preds = %cond_0
  %29 = getelementptr { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* %23, i32 0, i32 0
  %30 = load { { i8* (i8*, i8*)*, i8* }*, i64 }**, { { i8* (i8*, i8*)*, i8* }*, i64 }*** %29
  %31 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }*, { { i8* (i8*, i8*)*, i8* }*, i64 }** %30, i64 %27
  store { { i8* (i8*, i8*)*, i8* }*, i64 }* %16, { { i8* (i8*, i8*)*, i8* }*, i64 }** %31
  %32 = add i64 %27, 1
  store i64 %32, i64* %26
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %33 = getelementptr { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* %23, i32 0, i32 0
  %34 = load { { i8* (i8*, i8*)*, i8* }*, i64 }**, { { i8* (i8*, i8*)*, i8* }*, i64 }*** %33
  %35 = getelementptr { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }, { { { i8* (i8*, i8*)*, i8* }*, i64 }**, i64 }* %23, i32 0, i32 1
  %36 = load i64, i64* %35
  %37 = mul i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), %36
  %38 = call i8* @GC_malloc(i64 %37)
  %39 = bitcast i8* %38 to { { i64 (i8*, i64)*, i8* }*, i64 }**
  %40 = call i8* @GC_malloc(i64 ptrtoint ({ { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* getelementptr inbounds ({ { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* null, i32 1) to i64))
  %41 = bitcast i8* %40 to { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }*
  %42 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %41, i32 0, i32 0
  store { { i64 (i8*, i64)*, i8* }*, i64 }** %39, { { i64 (i8*, i64)*, i8* }*, i64 }*** %42
  %43 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %41, i32 0, i32 1
  store i64 %36, i64* %43
  %44 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %41, i32 0, i32 0
  %45 = load { { i64 (i8*, i64)*, i8* }*, i64 }**, { { i64 (i8*, i64)*, i8* }*, i64 }*** %44
  %46 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %41, i32 0, i32 1
  store i64 %36, i64* %46
  %47 = alloca i64
  store i64 0, i64* %47
  br label %cond_1

cond_1:                                           ; preds = %body_1, %end_0
  %48 = load i64, i64* %47
  %49 = icmp slt i64 %48, %36
  br i1 %49, label %body_1, label %end_1

body_1:                                           ; preds = %cond_1
  %50 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }*, { { i8* (i8*, i8*)*, i8* }*, i64 }** %34, i64 %48
  %51 = load { { i8* (i8*, i8*)*, i8* }*, i64 }*, { { i8* (i8*, i8*)*, i8* }*, i64 }** %50
  %52 = call i8* @GC_malloc(i64 ptrtoint ({ { i64 (i8*, i64)*, i8* }*, i64 }* getelementptr inbounds ({ { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* null, i32 1) to i64))
  %53 = bitcast i8* %52 to { { i64 (i8*, i64)*, i8* }*, i64 }*
  %54 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* %51, i32 0, i32 0
  %55 = load { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }** %54
  %56 = bitcast { i8* (i8*, i8*)*, i8* }* %55 to i8*
  %57 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %58 = bitcast i8* %57 to { i64 (i8*, i64)*, i8* }*
  %59 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %58, i32 0, i32 0
  store i64 (i8*, i64)* @"$fo85", i64 (i8*, i64)** %59
  %60 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %58, i32 0, i32 1
  store i8* %56, i8** %60
  %61 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %53, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* %58, { i64 (i8*, i64)*, i8* }** %61
  %62 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* %51, i32 0, i32 1
  %63 = load i64, i64* %62
  %64 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %53, i32 0, i32 1
  store i64 %63, i64* %64
  %65 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %45, i64 %48
  store { { i64 (i8*, i64)*, i8* }*, i64 }* %53, { { i64 (i8*, i64)*, i8* }*, i64 }** %65
  %66 = add i64 %48, 1
  store i64 %66, i64* %47
  br label %cond_1

end_1:                                            ; preds = %cond_1
  %67 = getelementptr { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }, { { { i64 (i8*, i64)*, i8* }*, i64 }**, i64 }* %41, i32 0, i32 0
  %68 = load { { i64 (i8*, i64)*, i8* }*, i64 }**, { { i64 (i8*, i64)*, i8* }*, i64 }*** %67
  %69 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %68, i64 0
  %70 = load { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %69
  %71 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 0
  %72 = load i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %71
  %73 = getelementptr { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }, { i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* }* %5, i32 0, i32 1
  %74 = load i8*, i8** %73
  %75 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %76 = bitcast i8* %75 to { { i8* (i8*, i8*)*, i8* }*, i8* }*
  %77 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %70, i32 0, i32 0
  %78 = load { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %77
  %79 = bitcast { i64 (i8*, i64)*, i8* }* %78 to i8*
  %80 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %81 = bitcast i8* %80 to { i8* (i8*, i8*)*, i8* }*
  %82 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %81, i32 0, i32 0
  store i8* (i8*, i8*)* @"$fo111", i8* (i8*, i8*)** %82
  %83 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %81, i32 0, i32 1
  store i8* %79, i8** %83
  %84 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %76, i32 0, i32 0
  store { i8* (i8*, i8*)*, i8* }* %81, { i8* (i8*, i8*)*, i8* }** %84
  %85 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %70, i32 0, i32 1
  %86 = load i64, i64* %85
  %87 = inttoptr i64 %86 to i8*
  %88 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %76, i32 0, i32 1
  store i8* %87, i8** %88
  %89 = call i8* %72(i8* %74, { { i8* (i8*, i8*)*, i8* }*, i8* }* %76)
  %90 = ptrtoint i8* %89 to i64
  %91 = call {}* @print_int7(i64 %90)
  ret i32 0
}
