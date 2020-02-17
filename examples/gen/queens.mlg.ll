; ModuleID = './examples/queens.mlg'
source_filename = "./examples/queens.mlg"

@0 = unnamed_addr constant [3 x i8] c" O\00"
@1 = unnamed_addr constant [3 x i8] c" .\00"
@2 = unnamed_addr constant [2 x i8] c"\0A\00"
@3 = unnamed_addr constant [2 x i8] c"\0A\00"

declare i8* @GC_malloc(i64)

define {}* @try7(i8*, i64) {
  %3 = bitcast i8* %0 to { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }*
  %4 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %3, i32 0, i32 0
  %5 = load { i64*, i64 }*, { i64*, i64 }** %4
  %6 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %3, i32 0, i32 1
  %7 = load { i64*, i64 }*, { i64*, i64 }** %6
  %8 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %3, i32 0, i32 2
  %9 = load { i64*, i64 }*, { i64*, i64 }** %8
  %10 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %3, i32 0, i32 3
  %11 = load i64, i64* %10
  %12 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %3, i32 0, i32 4
  %13 = load { i64*, i64 }*, { i64*, i64 }** %12
  %14 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %15 = bitcast i8* %14 to { {}* (i8*)*, i8* }*
  %16 = getelementptr { {}* (i8*)*, i8* }, { {}* (i8*)*, i8* }* %15, i32 0, i32 0
  store {}* (i8*)* @printboard6, {}* (i8*)** %16
  %17 = getelementptr { {}* (i8*)*, i8* }, { {}* (i8*)*, i8* }* %15, i32 0, i32 1
  store i8* %0, i8** %17
  %18 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %19 = bitcast i8* %18 to { {}* (i8*, i64)*, i8* }*
  %20 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %19, i32 0, i32 0
  store {}* (i8*, i64)* @try7, {}* (i8*, i64)** %20
  %21 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %19, i32 0, i32 1
  store i8* %0, i8** %21
  %22 = call i8* @GC_malloc(i64 ptrtoint ({ i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }* getelementptr inbounds ({ i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }, { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }* null, i32 1) to i64))
  %23 = bitcast i8* %22 to { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }*
  %24 = getelementptr { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }, { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }* %23, i32 0, i32 0
  store i64 %1, i64* %24
  %25 = getelementptr { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }, { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }* %23, i32 0, i32 1
  store { i64*, i64 }* %5, { i64*, i64 }** %25
  %26 = getelementptr { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }, { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }* %23, i32 0, i32 2
  store { i64*, i64 }* %7, { i64*, i64 }** %26
  %27 = getelementptr { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }, { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }* %23, i32 0, i32 3
  store { i64*, i64 }* %9, { i64*, i64 }** %27
  %28 = getelementptr { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }, { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }* %23, i32 0, i32 4
  store i64 %11, i64* %28
  %29 = getelementptr { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }, { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }* %23, i32 0, i32 5
  store { i64*, i64 }* %13, { i64*, i64 }** %29
  %30 = getelementptr { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }, { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }* %23, i32 0, i32 6
  store { {}* (i8*, i64)*, i8* }* %19, { {}* (i8*, i64)*, i8* }** %30
  %31 = bitcast { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }* %23 to i8*
  %32 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %33 = bitcast i8* %32 to { {}* (i8*, i64)*, i8* }*
  %34 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %33, i32 0, i32 0
  store {}* (i8*, i64)* @loop13, {}* (i8*, i64)** %34
  %35 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %33, i32 0, i32 1
  store i8* %31, i8** %35
  %36 = icmp eq i64 %1, %11
  %37 = alloca {}*
  br i1 %36, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %38 = call {}* @printboard6(i8* %0)
  store {}* %38, {}** %37
  br label %end_0

else_0:                                           ; preds = %2
  %39 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %33, i32 0, i32 0
  %40 = load {}* (i8*, i64)*, {}* (i8*, i64)** %39
  %41 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %33, i32 0, i32 1
  %42 = load i8*, i8** %41
  %43 = call {}* %40(i8* %42, i64 0)
  store {}* %43, {}** %37
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %44 = load {}*, {}** %37
  ret {}* %44
}

define {}* @printboard6(i8*) {
  %2 = bitcast i8* %0 to { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }*
  %3 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %2, i32 0, i32 0
  %4 = load { i64*, i64 }*, { i64*, i64 }** %3
  %5 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %2, i32 0, i32 1
  %6 = load { i64*, i64 }*, { i64*, i64 }** %5
  %7 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %2, i32 0, i32 2
  %8 = load { i64*, i64 }*, { i64*, i64 }** %7
  %9 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %2, i32 0, i32 3
  %10 = load i64, i64* %9
  %11 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %2, i32 0, i32 4
  %12 = load { i64*, i64 }*, { i64*, i64 }** %11
  %13 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %14 = bitcast i8* %13 to { {}* (i8*)*, i8* }*
  %15 = getelementptr { {}* (i8*)*, i8* }, { {}* (i8*)*, i8* }* %14, i32 0, i32 0
  store {}* (i8*)* @printboard6, {}* (i8*)** %15
  %16 = getelementptr { {}* (i8*)*, i8* }, { {}* (i8*)*, i8* }* %14, i32 0, i32 1
  store i8* %0, i8** %16
  %17 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %18 = bitcast i8* %17 to { {}* (i8*, i64)*, i8* }*
  %19 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %18, i32 0, i32 0
  store {}* (i8*, i64)* @try7, {}* (i8*, i64)** %19
  %20 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %18, i32 0, i32 1
  store i8* %0, i8** %20
  %21 = call i8* @GC_malloc(i64 ptrtoint ({ { i64*, i64 }*, i64 }* getelementptr inbounds ({ { i64*, i64 }*, i64 }, { { i64*, i64 }*, i64 }* null, i32 1) to i64))
  %22 = bitcast i8* %21 to { { i64*, i64 }*, i64 }*
  %23 = getelementptr { { i64*, i64 }*, i64 }, { { i64*, i64 }*, i64 }* %22, i32 0, i32 0
  store { i64*, i64 }* %4, { i64*, i64 }** %23
  %24 = getelementptr { { i64*, i64 }*, i64 }, { { i64*, i64 }*, i64 }* %22, i32 0, i32 1
  store i64 %10, i64* %24
  %25 = bitcast { { i64*, i64 }*, i64 }* %22 to i8*
  %26 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %27 = bitcast i8* %26 to { {}* (i8*, i64)*, i8* }*
  %28 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %27, i32 0, i32 0
  store {}* (i8*, i64)* @loopi8, {}* (i8*, i64)** %28
  %29 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %27, i32 0, i32 1
  store i8* %25, i8** %29
  %30 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %27, i32 0, i32 0
  %31 = load {}* (i8*, i64)*, {}* (i8*, i64)** %30
  %32 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %27, i32 0, i32 1
  %33 = load i8*, i8** %32
  %34 = call {}* %31(i8* %33, i64 0)
  ret {}* %34
}

declare {}* @print(i8*)

define {}* @print0(i8*) {
  %2 = call {}* @print(i8* %0)
  ret {}* %2
}

define {}* @loopj10(i8*, i64) {
  %3 = bitcast i8* %0 to { { i64*, i64 }*, i64, i64 }*
  %4 = getelementptr { { i64*, i64 }*, i64, i64 }, { { i64*, i64 }*, i64, i64 }* %3, i32 0, i32 0
  %5 = load { i64*, i64 }*, { i64*, i64 }** %4
  %6 = getelementptr { { i64*, i64 }*, i64, i64 }, { { i64*, i64 }*, i64, i64 }* %3, i32 0, i32 1
  %7 = load i64, i64* %6
  %8 = getelementptr { { i64*, i64 }*, i64, i64 }, { { i64*, i64 }*, i64, i64 }* %3, i32 0, i32 2
  %9 = load i64, i64* %8
  %10 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %11 = bitcast i8* %10 to { {}* (i8*, i64)*, i8* }*
  %12 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %11, i32 0, i32 0
  store {}* (i8*, i64)* @loopj10, {}* (i8*, i64)** %12
  %13 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %0, i8** %13
  %14 = icmp slt i64 %1, %9
  %15 = alloca {}*
  br i1 %14, label %then_0, label %else_1

then_0:                                           ; preds = %2
  %16 = getelementptr { i64*, i64 }, { i64*, i64 }* %5, i32 0, i32 0
  %17 = load i64*, i64** %16
  %18 = getelementptr i64, i64* %17, i64 %7
  %19 = load i64, i64* %18
  %20 = icmp eq i64 %19, %1
  %21 = alloca i8*
  br i1 %20, label %then_1, label %else_0

then_1:                                           ; preds = %then_0
  store i8* getelementptr inbounds ([3 x i8], [3 x i8]* @0, i32 0, i32 0), i8** %21
  br label %end_0

else_0:                                           ; preds = %then_0
  store i8* getelementptr inbounds ([3 x i8], [3 x i8]* @1, i32 0, i32 0), i8** %21
  br label %end_0

end_0:                                            ; preds = %else_0, %then_1
  %22 = load i8*, i8** %21
  %23 = call {}* @print0(i8* %22)
  %24 = add i64 %1, 1
  %25 = call {}* @loopj10(i8* %0, i64 %24)
  store {}* %25, {}** %15
  br label %end_1

else_1:                                           ; preds = %2
  %26 = call {}* @print0(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @2, i32 0, i32 0))
  store {}* %26, {}** %15
  br label %end_1

end_1:                                            ; preds = %else_1, %end_0
  %27 = load {}*, {}** %15
  ret {}* %27
}

define {}* @loopi8(i8*, i64) {
  %3 = bitcast i8* %0 to { { i64*, i64 }*, i64 }*
  %4 = getelementptr { { i64*, i64 }*, i64 }, { { i64*, i64 }*, i64 }* %3, i32 0, i32 0
  %5 = load { i64*, i64 }*, { i64*, i64 }** %4
  %6 = getelementptr { { i64*, i64 }*, i64 }, { { i64*, i64 }*, i64 }* %3, i32 0, i32 1
  %7 = load i64, i64* %6
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { {}* (i8*, i64)*, i8* }*
  %10 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %9, i32 0, i32 0
  store {}* (i8*, i64)* @loopi8, {}* (i8*, i64)** %10
  %11 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = icmp slt i64 %1, %7
  %13 = alloca {}*
  br i1 %12, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %14 = call i8* @GC_malloc(i64 ptrtoint ({ { i64*, i64 }*, i64, i64 }* getelementptr inbounds ({ { i64*, i64 }*, i64, i64 }, { { i64*, i64 }*, i64, i64 }* null, i32 1) to i64))
  %15 = bitcast i8* %14 to { { i64*, i64 }*, i64, i64 }*
  %16 = getelementptr { { i64*, i64 }*, i64, i64 }, { { i64*, i64 }*, i64, i64 }* %15, i32 0, i32 0
  store { i64*, i64 }* %5, { i64*, i64 }** %16
  %17 = getelementptr { { i64*, i64 }*, i64, i64 }, { { i64*, i64 }*, i64, i64 }* %15, i32 0, i32 1
  store i64 %1, i64* %17
  %18 = getelementptr { { i64*, i64 }*, i64, i64 }, { { i64*, i64 }*, i64, i64 }* %15, i32 0, i32 2
  store i64 %7, i64* %18
  %19 = bitcast { { i64*, i64 }*, i64, i64 }* %15 to i8*
  %20 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %21 = bitcast i8* %20 to { {}* (i8*, i64)*, i8* }*
  %22 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %21, i32 0, i32 0
  store {}* (i8*, i64)* @loopj10, {}* (i8*, i64)** %22
  %23 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %21, i32 0, i32 1
  store i8* %19, i8** %23
  %24 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %21, i32 0, i32 0
  %25 = load {}* (i8*, i64)*, {}* (i8*, i64)** %24
  %26 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %21, i32 0, i32 1
  %27 = load i8*, i8** %26
  %28 = call {}* %25(i8* %27, i64 0)
  %29 = add i64 %1, 1
  %30 = call {}* @loopi8(i8* %0, i64 %29)
  store {}* %30, {}** %13
  br label %end_0

else_0:                                           ; preds = %2
  %31 = call {}* @print0(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @3, i32 0, i32 0))
  store {}* %31, {}** %13
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %32 = load {}*, {}** %13
  ret {}* %32
}

define {}* @loop13(i8*, i64) {
  %3 = bitcast i8* %0 to { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }*
  %4 = getelementptr { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }, { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = getelementptr { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }, { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }* %3, i32 0, i32 1
  %7 = load { i64*, i64 }*, { i64*, i64 }** %6
  %8 = getelementptr { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }, { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }* %3, i32 0, i32 2
  %9 = load { i64*, i64 }*, { i64*, i64 }** %8
  %10 = getelementptr { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }, { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }* %3, i32 0, i32 3
  %11 = load { i64*, i64 }*, { i64*, i64 }** %10
  %12 = getelementptr { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }, { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }* %3, i32 0, i32 4
  %13 = load i64, i64* %12
  %14 = getelementptr { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }, { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }* %3, i32 0, i32 5
  %15 = load { i64*, i64 }*, { i64*, i64 }** %14
  %16 = getelementptr { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }, { i64, { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }*, { {}* (i8*, i64)*, i8* }* }* %3, i32 0, i32 6
  %17 = load { {}* (i8*, i64)*, i8* }*, { {}* (i8*, i64)*, i8* }** %16
  %18 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %19 = bitcast i8* %18 to { {}* (i8*, i64)*, i8* }*
  %20 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %19, i32 0, i32 0
  store {}* (i8*, i64)* @loop13, {}* (i8*, i64)** %20
  %21 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %19, i32 0, i32 1
  store i8* %0, i8** %21
  %22 = icmp slt i64 %1, %13
  %23 = alloca {}*
  br i1 %22, label %then_0, label %else_1

then_0:                                           ; preds = %2
  %24 = getelementptr { i64*, i64 }, { i64*, i64 }* %15, i32 0, i32 0
  %25 = load i64*, i64** %24
  %26 = getelementptr i64, i64* %25, i64 %1
  %27 = load i64, i64* %26
  %28 = icmp eq i64 %27, 0
  %29 = add i64 %1, %5
  %30 = getelementptr { i64*, i64 }, { i64*, i64 }* %9, i32 0, i32 0
  %31 = load i64*, i64** %30
  %32 = getelementptr i64, i64* %31, i64 %29
  %33 = load i64, i64* %32
  %34 = icmp eq i64 %33, 0
  %35 = and i1 %28, %34
  %36 = add i64 %1, 7
  %37 = sub i64 %36, %5
  %38 = getelementptr { i64*, i64 }, { i64*, i64 }* %11, i32 0, i32 0
  %39 = load i64*, i64** %38
  %40 = getelementptr i64, i64* %39, i64 %37
  %41 = load i64, i64* %40
  %42 = icmp eq i64 %41, 0
  %43 = and i1 %35, %42
  %44 = alloca {}*
  br i1 %43, label %then_1, label %else_0

then_1:                                           ; preds = %then_0
  %45 = getelementptr { i64*, i64 }, { i64*, i64 }* %15, i32 0, i32 0
  %46 = load i64*, i64** %45
  %47 = getelementptr i64, i64* %46, i64 %1
  store i64 1, i64* %47
  %48 = add i64 %1, %5
  %49 = getelementptr { i64*, i64 }, { i64*, i64 }* %9, i32 0, i32 0
  %50 = load i64*, i64** %49
  %51 = getelementptr i64, i64* %50, i64 %48
  store i64 1, i64* %51
  %52 = add i64 %1, 7
  %53 = sub i64 %52, %5
  %54 = getelementptr { i64*, i64 }, { i64*, i64 }* %11, i32 0, i32 0
  %55 = load i64*, i64** %54
  %56 = getelementptr i64, i64* %55, i64 %53
  store i64 1, i64* %56
  %57 = getelementptr { i64*, i64 }, { i64*, i64 }* %7, i32 0, i32 0
  %58 = load i64*, i64** %57
  %59 = getelementptr i64, i64* %58, i64 %5
  store i64 %1, i64* %59
  %60 = add i64 %5, 1
  %61 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %17, i32 0, i32 0
  %62 = load {}* (i8*, i64)*, {}* (i8*, i64)** %61
  %63 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %17, i32 0, i32 1
  %64 = load i8*, i8** %63
  %65 = call {}* %62(i8* %64, i64 %60)
  %66 = getelementptr { i64*, i64 }, { i64*, i64 }* %15, i32 0, i32 0
  %67 = load i64*, i64** %66
  %68 = getelementptr i64, i64* %67, i64 %1
  store i64 0, i64* %68
  %69 = add i64 %1, %5
  %70 = getelementptr { i64*, i64 }, { i64*, i64 }* %9, i32 0, i32 0
  %71 = load i64*, i64** %70
  %72 = getelementptr i64, i64* %71, i64 %69
  store i64 0, i64* %72
  %73 = add i64 %1, 7
  %74 = sub i64 %73, %5
  %75 = getelementptr { i64*, i64 }, { i64*, i64 }* %11, i32 0, i32 0
  %76 = load i64*, i64** %75
  %77 = getelementptr i64, i64* %76, i64 %74
  store i64 0, i64* %77
  %78 = add i64 %1, 1
  %79 = call {}* @loop13(i8* %0, i64 %78)
  store {}* %79, {}** %44
  br label %end_0

else_0:                                           ; preds = %then_0
  %80 = add i64 %1, 1
  %81 = call {}* @loop13(i8* %0, i64 %80)
  store {}* %81, {}** %44
  br label %end_0

end_0:                                            ; preds = %else_0, %then_1
  %82 = load {}*, {}** %44
  store {}* %82, {}** %23
  br label %end_1

else_1:                                           ; preds = %2
  %83 = call i8* @GC_malloc(i64 0)
  %84 = bitcast i8* %83 to {}*
  store {}* %84, {}** %23
  br label %end_1

end_1:                                            ; preds = %else_1, %end_0
  %85 = load {}*, {}** %23
  ret {}* %85
}

define i32 @main() {
  %1 = mul i64 ptrtoint (i64* getelementptr inbounds (i64, i64* null, i32 1) to i64), 8
  %2 = call i8* @GC_malloc(i64 %1)
  %3 = bitcast i8* %2 to i64*
  %4 = call i8* @GC_malloc(i64 ptrtoint ({ i64*, i64 }* getelementptr inbounds ({ i64*, i64 }, { i64*, i64 }* null, i32 1) to i64))
  %5 = bitcast i8* %4 to { i64*, i64 }*
  %6 = getelementptr { i64*, i64 }, { i64*, i64 }* %5, i32 0, i32 0
  store i64* %3, i64** %6
  %7 = getelementptr { i64*, i64 }, { i64*, i64 }* %5, i32 0, i32 1
  store i64 8, i64* %7
  %8 = alloca i64
  store i64 0, i64* %8
  br label %cond_0

cond_0:                                           ; preds = %body_0, %0
  %9 = load i64, i64* %8
  %10 = icmp slt i64 %9, 8
  br i1 %10, label %body_0, label %end_0

body_0:                                           ; preds = %cond_0
  %11 = getelementptr { i64*, i64 }, { i64*, i64 }* %5, i32 0, i32 0
  %12 = load i64*, i64** %11
  %13 = getelementptr i64, i64* %12, i64 %9
  store i64 0, i64* %13
  %14 = add i64 %9, 1
  store i64 %14, i64* %8
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %15 = mul i64 ptrtoint (i64* getelementptr inbounds (i64, i64* null, i32 1) to i64), 8
  %16 = call i8* @GC_malloc(i64 %15)
  %17 = bitcast i8* %16 to i64*
  %18 = call i8* @GC_malloc(i64 ptrtoint ({ i64*, i64 }* getelementptr inbounds ({ i64*, i64 }, { i64*, i64 }* null, i32 1) to i64))
  %19 = bitcast i8* %18 to { i64*, i64 }*
  %20 = getelementptr { i64*, i64 }, { i64*, i64 }* %19, i32 0, i32 0
  store i64* %17, i64** %20
  %21 = getelementptr { i64*, i64 }, { i64*, i64 }* %19, i32 0, i32 1
  store i64 8, i64* %21
  %22 = alloca i64
  store i64 0, i64* %22
  br label %cond_1

cond_1:                                           ; preds = %body_1, %end_0
  %23 = load i64, i64* %22
  %24 = icmp slt i64 %23, 8
  br i1 %24, label %body_1, label %end_1

body_1:                                           ; preds = %cond_1
  %25 = getelementptr { i64*, i64 }, { i64*, i64 }* %19, i32 0, i32 0
  %26 = load i64*, i64** %25
  %27 = getelementptr i64, i64* %26, i64 %23
  store i64 0, i64* %27
  %28 = add i64 %23, 1
  store i64 %28, i64* %22
  br label %cond_1

end_1:                                            ; preds = %cond_1
  %29 = add i64 8, 8
  %30 = sub i64 %29, 1
  %31 = mul i64 ptrtoint (i64* getelementptr inbounds (i64, i64* null, i32 1) to i64), %30
  %32 = call i8* @GC_malloc(i64 %31)
  %33 = bitcast i8* %32 to i64*
  %34 = call i8* @GC_malloc(i64 ptrtoint ({ i64*, i64 }* getelementptr inbounds ({ i64*, i64 }, { i64*, i64 }* null, i32 1) to i64))
  %35 = bitcast i8* %34 to { i64*, i64 }*
  %36 = getelementptr { i64*, i64 }, { i64*, i64 }* %35, i32 0, i32 0
  store i64* %33, i64** %36
  %37 = getelementptr { i64*, i64 }, { i64*, i64 }* %35, i32 0, i32 1
  store i64 %30, i64* %37
  %38 = alloca i64
  store i64 0, i64* %38
  br label %cond_2

cond_2:                                           ; preds = %body_2, %end_1
  %39 = load i64, i64* %38
  %40 = icmp slt i64 %39, %30
  br i1 %40, label %body_2, label %end_2

body_2:                                           ; preds = %cond_2
  %41 = getelementptr { i64*, i64 }, { i64*, i64 }* %35, i32 0, i32 0
  %42 = load i64*, i64** %41
  %43 = getelementptr i64, i64* %42, i64 %39
  store i64 0, i64* %43
  %44 = add i64 %39, 1
  store i64 %44, i64* %38
  br label %cond_2

end_2:                                            ; preds = %cond_2
  %45 = add i64 8, 8
  %46 = sub i64 %45, 1
  %47 = mul i64 ptrtoint (i64* getelementptr inbounds (i64, i64* null, i32 1) to i64), %46
  %48 = call i8* @GC_malloc(i64 %47)
  %49 = bitcast i8* %48 to i64*
  %50 = call i8* @GC_malloc(i64 ptrtoint ({ i64*, i64 }* getelementptr inbounds ({ i64*, i64 }, { i64*, i64 }* null, i32 1) to i64))
  %51 = bitcast i8* %50 to { i64*, i64 }*
  %52 = getelementptr { i64*, i64 }, { i64*, i64 }* %51, i32 0, i32 0
  store i64* %49, i64** %52
  %53 = getelementptr { i64*, i64 }, { i64*, i64 }* %51, i32 0, i32 1
  store i64 %46, i64* %53
  %54 = alloca i64
  store i64 0, i64* %54
  br label %cond_3

cond_3:                                           ; preds = %body_3, %end_2
  %55 = load i64, i64* %54
  %56 = icmp slt i64 %55, %46
  br i1 %56, label %body_3, label %end_3

body_3:                                           ; preds = %cond_3
  %57 = getelementptr { i64*, i64 }, { i64*, i64 }* %51, i32 0, i32 0
  %58 = load i64*, i64** %57
  %59 = getelementptr i64, i64* %58, i64 %55
  store i64 0, i64* %59
  %60 = add i64 %55, 1
  store i64 %60, i64* %54
  br label %cond_3

end_3:                                            ; preds = %cond_3
  %61 = call i8* @GC_malloc(i64 ptrtoint ({ { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* getelementptr inbounds ({ { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* null, i32 1) to i64))
  %62 = bitcast i8* %61 to { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }*
  %63 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %62, i32 0, i32 0
  store { i64*, i64 }* %19, { i64*, i64 }** %63
  %64 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %62, i32 0, i32 1
  store { i64*, i64 }* %35, { i64*, i64 }** %64
  %65 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %62, i32 0, i32 2
  store { i64*, i64 }* %51, { i64*, i64 }** %65
  %66 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %62, i32 0, i32 3
  store i64 8, i64* %66
  %67 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %62, i32 0, i32 4
  store { i64*, i64 }* %5, { i64*, i64 }** %67
  %68 = bitcast { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %62 to i8*
  %69 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %70 = bitcast i8* %69 to { {}* (i8*)*, i8* }*
  %71 = getelementptr { {}* (i8*)*, i8* }, { {}* (i8*)*, i8* }* %70, i32 0, i32 0
  store {}* (i8*)* @printboard6, {}* (i8*)** %71
  %72 = getelementptr { {}* (i8*)*, i8* }, { {}* (i8*)*, i8* }* %70, i32 0, i32 1
  store i8* %68, i8** %72
  %73 = call i8* @GC_malloc(i64 ptrtoint ({ { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* getelementptr inbounds ({ { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* null, i32 1) to i64))
  %74 = bitcast i8* %73 to { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }*
  %75 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %74, i32 0, i32 0
  store { i64*, i64 }* %19, { i64*, i64 }** %75
  %76 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %74, i32 0, i32 1
  store { i64*, i64 }* %35, { i64*, i64 }** %76
  %77 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %74, i32 0, i32 2
  store { i64*, i64 }* %51, { i64*, i64 }** %77
  %78 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %74, i32 0, i32 3
  store i64 8, i64* %78
  %79 = getelementptr { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }, { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %74, i32 0, i32 4
  store { i64*, i64 }* %5, { i64*, i64 }** %79
  %80 = bitcast { { i64*, i64 }*, { i64*, i64 }*, { i64*, i64 }*, i64, { i64*, i64 }* }* %74 to i8*
  %81 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %82 = bitcast i8* %81 to { {}* (i8*, i64)*, i8* }*
  %83 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %82, i32 0, i32 0
  store {}* (i8*, i64)* @try7, {}* (i8*, i64)** %83
  %84 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %82, i32 0, i32 1
  store i8* %80, i8** %84
  %85 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %82, i32 0, i32 0
  %86 = load {}* (i8*, i64)*, {}* (i8*, i64)** %85
  %87 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %82, i32 0, i32 1
  %88 = load i8*, i8** %87
  %89 = call {}* %86(i8* %88, i64 0)
  ret i32 0
}
