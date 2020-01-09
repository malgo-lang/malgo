; ModuleID = './examples/polytuple.mlg'
source_filename = "./examples/polytuple.mlg"

define {}* @f0({ i8**, i64 }*, i8*) {
  %3 = getelementptr { i8**, i64 }, { i8**, i64 }* %0, i32 0, i32 0
  %4 = load i8**, i8*** %3
  %5 = getelementptr i8*, i8** %4, i64 0
  store i8* %1, i8** %5
  ret {}* undef
}

declare i8* @GC_malloc(i64)

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
  %14 = add i64 %9, 1
  store i64 %14, i64* %8
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %15 = getelementptr { i64*, i64 }, { i64*, i64 }* %5, i32 0, i32 0
  %16 = load i64*, i64** %15
  %17 = getelementptr { i64*, i64 }, { i64*, i64 }* %5, i32 0, i32 1
  %18 = load i64, i64* %17
  %19 = mul i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), %18
  %20 = call i8* @GC_malloc(i64 %19)
  %21 = bitcast i8* %20 to i8**
  %22 = call i8* @GC_malloc(i64 ptrtoint ({ i8**, i64 }* getelementptr inbounds ({ i8**, i64 }, { i8**, i64 }* null, i32 1) to i64))
  %23 = bitcast i8* %22 to { i8**, i64 }*
  %24 = getelementptr { i8**, i64 }, { i8**, i64 }* %23, i32 0, i32 0
  store i8** %21, i8*** %24
  %25 = getelementptr { i8**, i64 }, { i8**, i64 }* %23, i32 0, i32 1
  store i64 %18, i64* %25
  %26 = getelementptr { i8**, i64 }, { i8**, i64 }* %23, i32 0, i32 0
  %27 = load i8**, i8*** %26
  %28 = getelementptr { i8**, i64 }, { i8**, i64 }* %23, i32 0, i32 1
  store i64 %18, i64* %28
  %29 = alloca i64
  store i64 0, i64* %29
  br label %cond_1

cond_1:                                           ; preds = %body_1, %end_0
  %30 = load i64, i64* %29
  %31 = icmp slt i64 %30, %18
  br i1 %31, label %body_1, label %end_1

body_1:                                           ; preds = %cond_1
  %32 = getelementptr i64, i64* %16, i64 %30
  %33 = load i64, i64* %32
  %34 = inttoptr i64 %33 to i8*
  %35 = getelementptr i8*, i8** %27, i64 %30
  store i8* %34, i8** %35
  %36 = add i64 %30, 1
  store i64 %36, i64* %29
  br label %cond_1

end_1:                                            ; preds = %cond_1
  %37 = inttoptr i64 10 to i8*
  %38 = call {}* @f0({ i8**, i64 }* %23, i8* %37)
  %39 = call i8* @GC_malloc(i64 0)
  %40 = bitcast i8* %39 to {}*
  ret i32 0
}
