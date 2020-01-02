; ModuleID = './examples/polytuple.mlg'
source_filename = "./examples/polytuple.mlg"

declare i8* @GC_malloc(i64)

define {}* @f21({ i8**, i64 }*, i8*) {
  %3 = getelementptr { i8**, i64 }, { i8**, i64 }* %0, i32 0, i32 0
  %4 = load i8**, i8*** %3
  %5 = getelementptr i8*, i8** %4, i64 0
  store i8* %1, i8** %5
  %6 = call i8* @GC_malloc(i64 0)
  %7 = bitcast i8* %6 to {}*
  ret {}* %7
}

define i32 @main() {
  %1 = mul i64 ptrtoint (i64* getelementptr inbounds (i64, i64* null, i32 1) to i64), 10
  %2 = call i8* @GC_malloc(i64 %1)
  %3 = bitcast i8* %2 to i64*
  %4 = call i8* @GC_malloc(i64 ptrtoint ({ i64*, i64 }* getelementptr inbounds ({ i64*, i64 }, { i64*, i64 }* null, i32 1) to i64))
  %5 = bitcast i8* %4 to { i64*, i64 }*
  %6 = getelementptr { i64*, i64 }, { i64*, i64 }* %5, i32 0, i32 0
  store i64* %3, i64** %6
  %7 = getelementptr { i64*, i64 }, { i64*, i64 }* %5, i32 0, i32 1
  store i64 10, i64* %7
  %8 = alloca i64
  store i64 0, i64* %8
  br label %cond_0

cond_0:                                           ; preds = %body_0, %0
  %9 = load i64, i64* %8
  %10 = icmp slt i64 %9, 10
  br i1 %10, label %body_0, label %end_0

body_0:                                           ; preds = %cond_0
  %11 = getelementptr { i64*, i64 }, { i64*, i64 }* %5, i32 0, i32 0
  %12 = load i64*, i64** %11
  %13 = getelementptr i64, i64* %12, i64 %9
  store i64 1, i64* %13
  %14 = call i8* @GC_malloc(i64 0)
  %15 = bitcast i8* %14 to {}*
  %16 = add i64 %9, 1
  store i64 %16, i64* %8
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %17 = call i8* @GC_malloc(i64 0)
  %18 = bitcast i8* %17 to {}*
  %19 = getelementptr { i64*, i64 }, { i64*, i64 }* %5, i32 0, i32 0
  %20 = load i64*, i64** %19
  %21 = getelementptr { i64*, i64 }, { i64*, i64 }* %5, i32 0, i32 1
  %22 = load i64, i64* %21
  %23 = mul i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), %22
  %24 = call i8* @GC_malloc(i64 %23)
  %25 = bitcast i8* %24 to i8**
  %26 = call i8* @GC_malloc(i64 ptrtoint ({ i8**, i64 }* getelementptr inbounds ({ i8**, i64 }, { i8**, i64 }* null, i32 1) to i64))
  %27 = bitcast i8* %26 to { i8**, i64 }*
  %28 = getelementptr { i8**, i64 }, { i8**, i64 }* %27, i32 0, i32 0
  store i8** %25, i8*** %28
  %29 = getelementptr { i8**, i64 }, { i8**, i64 }* %27, i32 0, i32 1
  store i64 %22, i64* %29
  %30 = getelementptr { i8**, i64 }, { i8**, i64 }* %27, i32 0, i32 0
  %31 = load i8**, i8*** %30
  %32 = getelementptr { i8**, i64 }, { i8**, i64 }* %27, i32 0, i32 1
  store i64 %22, i64* %32
  %33 = call i8* @GC_malloc(i64 0)
  %34 = bitcast i8* %33 to {}*
  %35 = alloca i64
  store i64 0, i64* %35
  br label %cond_1

cond_1:                                           ; preds = %body_1, %end_0
  %36 = load i64, i64* %35
  %37 = icmp slt i64 %36, %22
  br i1 %37, label %body_1, label %end_1

body_1:                                           ; preds = %cond_1
  %38 = getelementptr i64, i64* %20, i64 %36
  %39 = load i64, i64* %38
  %40 = inttoptr i64 %39 to i8*
  %41 = getelementptr i8*, i8** %31, i64 %36
  store i8* %40, i8** %41
  %42 = call i8* @GC_malloc(i64 0)
  %43 = bitcast i8* %42 to {}*
  %44 = add i64 %36, 1
  store i64 %44, i64* %35
  br label %cond_1

end_1:                                            ; preds = %cond_1
  %45 = call i8* @GC_malloc(i64 0)
  %46 = bitcast i8* %45 to {}*
  %47 = inttoptr i64 10 to i8*
  %48 = call {}* @f21({ i8**, i64 }* %27, i8* %47)
  %49 = call i8* @GC_malloc(i64 0)
  %50 = bitcast i8* %49 to {}*
  ret i32 0
}
