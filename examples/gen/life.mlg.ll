; ModuleID = './examples/life.mlg'
source_filename = "./examples/life.mlg"

declare i8* @GC_malloc(i64)

define {}* @loop.224(i8*, i64) {
  %3 = bitcast i8* %0 to { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }*
  %4 = getelementptr { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }, { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }* %3, i32 0, i32 0
  %5 = load i1*, i1** %4
  %6 = getelementptr { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }, { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }* %3, i32 0, i32 1
  %7 = load { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }** %6
  %8 = getelementptr { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }, { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }* %3, i32 0, i32 2
  %9 = load { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }** %8
  %10 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %11 = bitcast i8* %10 to { {}* (i8*, i64)*, i8* }*
  %12 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %11, i32 0, i32 0
  store {}* (i8*, i64)* @loop.224, {}* (i8*, i64)** %12
  %13 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %0, i8** %13
  %14 = icmp sgt i64 %1, 0
  %15 = alloca {}*
  br i1 %14, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %16 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %7, i32 0, i32 0
  %17 = load {}* (i8*, i1*)*, {}* (i8*, i1*)** %16
  %18 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %7, i32 0, i32 1
  %19 = load i8*, i8** %18
  %20 = call {}* %17(i8* %19, i1* %5)
  %21 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %9, i32 0, i32 0
  %22 = load {}* (i8*, i1*)*, {}* (i8*, i1*)** %21
  %23 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %9, i32 0, i32 1
  %24 = load i8*, i8** %23
  %25 = call {}* %22(i8* %24, i1* %5)
  %26 = call {}* @newline.47()
  %27 = call {}* @sleep.50(i64 1)
  %28 = sub i64 %1, 1
  %29 = call {}* @loop.224(i8* %0, i64 %28)
  store {}* %29, {}** %15
  br label %endif_0

else_0:                                           ; preds = %2
  %30 = call i8* @GC_malloc(i64 0)
  %31 = bitcast i8* %30 to {}*
  store {}* %31, {}** %15
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %32 = load {}*, {}** %15
  ret {}* %32
}

define {}* @update_cells.210(i8*, i1*) {
  %3 = bitcast i8* %0 to { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %4 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 0
  %5 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %4
  %6 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 1
  %7 = load { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }** %6
  %8 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 2
  %9 = load i64, i64* %8
  %10 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 3
  %11 = load i64, i64* %10
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { {}* (i8*, i1*)*, i8* }*
  %14 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %13, i32 0, i32 0
  store {}* (i8*, i1*)* @update_cells.210, {}* (i8*, i1*)** %14
  %15 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %13, i32 0, i32 1
  store i8* %0, i8** %15
  %16 = mul i64 %9, %11
  %17 = call i1* @copy_bool_array.51(i1* %1, i64 %16)
  %18 = call i8* @GC_malloc(i64 ptrtoint ({ i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* getelementptr inbounds ({ i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %19 = bitcast i8* %18 to { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %20 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %19, i32 0, i32 0
  store i1* %1, i1** %20
  %21 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %19, i32 0, i32 1
  store i1* %17, i1** %21
  %22 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %19, i32 0, i32 2
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %5, { i1 (i8*, i1*, i64, i64)*, i8* }** %22
  %23 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %19, i32 0, i32 3
  store { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %7, { {}* (i8*, i1*, i64, i64, i1)*, i8* }** %23
  %24 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %19, i32 0, i32 4
  store i64 %9, i64* %24
  %25 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %19, i32 0, i32 5
  store i64 %11, i64* %25
  %26 = bitcast { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %19 to i8*
  %27 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %28 = bitcast i8* %27 to { {}* (i8*, i64)*, i8* }*
  %29 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %28, i32 0, i32 0
  store {}* (i8*, i64)* @go_y.208, {}* (i8*, i64)** %29
  %30 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %28, i32 0, i32 1
  store i8* %26, i8** %30
  %31 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %28, i32 0, i32 0
  %32 = load {}* (i8*, i64)*, {}* (i8*, i64)** %31
  %33 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %28, i32 0, i32 1
  %34 = load i8*, i8** %33
  %35 = call {}* %32(i8* %34, i64 0)
  ret {}* %35
}

define {}* @go_y.208(i8*, i64) {
  %3 = bitcast i8* %0 to { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %4 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 0
  %5 = load i1*, i1** %4
  %6 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 1
  %7 = load i1*, i1** %6
  %8 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 2
  %9 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %8
  %10 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 3
  %11 = load { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }** %10
  %12 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 4
  %13 = load i64, i64* %12
  %14 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 5
  %15 = load i64, i64* %14
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { {}* (i8*, i64)*, i8* }*
  %18 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %17, i32 0, i32 0
  store {}* (i8*, i64)* @go_y.208, {}* (i8*, i64)** %18
  %19 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %17, i32 0, i32 1
  store i8* %0, i8** %19
  %20 = call i8* @GC_malloc(i64 ptrtoint ({ i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* getelementptr inbounds ({ i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %21 = bitcast i8* %20 to { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %22 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %21, i32 0, i32 0
  store i1* %5, i1** %22
  %23 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %21, i32 0, i32 1
  store i1* %7, i1** %23
  %24 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %21, i32 0, i32 2
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %9, { i1 (i8*, i1*, i64, i64)*, i8* }** %24
  %25 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %21, i32 0, i32 3
  store { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %11, { {}* (i8*, i1*, i64, i64, i1)*, i8* }** %25
  %26 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %21, i32 0, i32 4
  store i64 %13, i64* %26
  %27 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %21, i32 0, i32 5
  store i64 %1, i64* %27
  %28 = bitcast { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %21 to i8*
  %29 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %30 = bitcast i8* %29 to { {}* (i8*, i64)*, i8* }*
  %31 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %30, i32 0, i32 0
  store {}* (i8*, i64)* @go_x.202, {}* (i8*, i64)** %31
  %32 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %30, i32 0, i32 1
  store i8* %28, i8** %32
  %33 = icmp sge i64 %1, %15
  %34 = alloca {}*
  br i1 %33, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %35 = call i8* @GC_malloc(i64 0)
  %36 = bitcast i8* %35 to {}*
  store {}* %36, {}** %34
  br label %endif_0

else_0:                                           ; preds = %2
  %37 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %30, i32 0, i32 0
  %38 = load {}* (i8*, i64)*, {}* (i8*, i64)** %37
  %39 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %30, i32 0, i32 1
  %40 = load i8*, i8** %39
  %41 = call {}* %38(i8* %40, i64 0)
  %42 = add i64 %1, 1
  %43 = call {}* @go_y.208(i8* %0, i64 %42)
  store {}* %43, {}** %34
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %44 = load {}*, {}** %34
  ret {}* %44
}

define {}* @go_x.202(i8*, i64) {
  %3 = bitcast i8* %0 to { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %4 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 0
  %5 = load i1*, i1** %4
  %6 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 1
  %7 = load i1*, i1** %6
  %8 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 2
  %9 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %8
  %10 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 3
  %11 = load { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }** %10
  %12 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 4
  %13 = load i64, i64* %12
  %14 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 5
  %15 = load i64, i64* %14
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { {}* (i8*, i64)*, i8* }*
  %18 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %17, i32 0, i32 0
  store {}* (i8*, i64)* @go_x.202, {}* (i8*, i64)** %18
  %19 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %17, i32 0, i32 1
  store i8* %0, i8** %19
  %20 = icmp sge i64 %1, %13
  %21 = alloca {}*
  br i1 %20, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  store {}* %23, {}** %21
  br label %endif_0

else_0:                                           ; preds = %2
  %24 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %9, i32 0, i32 0
  %25 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %24
  %26 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %9, i32 0, i32 1
  %27 = load i8*, i8** %26
  %28 = call i1 %25(i8* %27, i1* %7, i64 %1, i64 %15)
  %29 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %11, i32 0, i32 0
  %30 = load {}* (i8*, i1*, i64, i64, i1)*, {}* (i8*, i1*, i64, i64, i1)** %29
  %31 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %11, i32 0, i32 1
  %32 = load i8*, i8** %31
  %33 = call {}* %30(i8* %32, i1* %5, i64 %1, i64 %15, i1 %28)
  %34 = add i64 %1, 1
  %35 = call {}* @go_x.202(i8* %0, i64 %34)
  store {}* %35, {}** %21
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %36 = load {}*, {}** %21
  ret {}* %36
}

define i1 @next_state.183(i8*, i1*, i64, i64) {
  %5 = bitcast i8* %0 to { { i1 (i8*, i1*, i64, i64)*, i8* }* }*
  %6 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }* }, { { i1 (i8*, i1*, i64, i64)*, i8* }* }* %5, i32 0, i32 0
  %7 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %6
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %10 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %9, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @next_state.183, i1 (i8*, i1*, i64, i64)** %10
  %11 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = mul i64 1, ptrtoint (i64* getelementptr inbounds (i64, i64* null, i32 1) to i64)
  %13 = call i8* @GC_malloc(i64 %12)
  %14 = bitcast i8* %13 to i64*
  %15 = sub i64 %2, 1
  %16 = add i64 %3, 1
  %17 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %18 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %17
  %19 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %20 = load i8*, i8** %19
  %21 = call i1 %18(i8* %20, i1* %1, i64 %15, i64 %16)
  %22 = call i64 @to_int.128(i1 %21)
  %23 = add i64 %3, 1
  %24 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %25 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %24
  %26 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %27 = load i8*, i8** %26
  %28 = call i1 %25(i8* %27, i1* %1, i64 %2, i64 %23)
  %29 = call i64 @to_int.128(i1 %28)
  %30 = add i64 %22, %29
  %31 = add i64 %2, 1
  %32 = add i64 %3, 1
  %33 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %34 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %33
  %35 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %36 = load i8*, i8** %35
  %37 = call i1 %34(i8* %36, i1* %1, i64 %31, i64 %32)
  %38 = call i64 @to_int.128(i1 %37)
  %39 = add i64 %30, %38
  %40 = getelementptr i64, i64* %14, i64 0
  store i64 %39, i64* %40
  %41 = sub i64 %2, 1
  %42 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %43 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %42
  %44 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %45 = load i8*, i8** %44
  %46 = call i1 %43(i8* %45, i1* %1, i64 %41, i64 %3)
  %47 = call i64 @to_int.128(i1 %46)
  %48 = add i64 %2, 1
  %49 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %50 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %49
  %51 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %52 = load i8*, i8** %51
  %53 = call i1 %50(i8* %52, i1* %1, i64 %48, i64 %3)
  %54 = call i64 @to_int.128(i1 %53)
  %55 = add i64 %47, %54
  %56 = getelementptr i64, i64* %14, i64 0
  %57 = load i64, i64* %56
  %58 = add i64 %55, %57
  %59 = getelementptr i64, i64* %14, i64 0
  store i64 %58, i64* %59
  %60 = sub i64 %2, 1
  %61 = sub i64 %3, 1
  %62 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %63 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %62
  %64 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %65 = load i8*, i8** %64
  %66 = call i1 %63(i8* %65, i1* %1, i64 %60, i64 %61)
  %67 = call i64 @to_int.128(i1 %66)
  %68 = sub i64 %3, 1
  %69 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %70 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %69
  %71 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %72 = load i8*, i8** %71
  %73 = call i1 %70(i8* %72, i1* %1, i64 %2, i64 %68)
  %74 = call i64 @to_int.128(i1 %73)
  %75 = add i64 %67, %74
  %76 = add i64 %2, 1
  %77 = sub i64 %3, 1
  %78 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %79 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %78
  %80 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %81 = load i8*, i8** %80
  %82 = call i1 %79(i8* %81, i1* %1, i64 %76, i64 %77)
  %83 = call i64 @to_int.128(i1 %82)
  %84 = add i64 %75, %83
  %85 = getelementptr i64, i64* %14, i64 0
  %86 = load i64, i64* %85
  %87 = add i64 %84, %86
  %88 = getelementptr i64, i64* %14, i64 0
  store i64 %87, i64* %88
  %89 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %90 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %89
  %91 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %92 = load i8*, i8** %91
  %93 = call i1 %90(i8* %92, i1* %1, i64 %2, i64 %3)
  %94 = alloca i1
  br i1 %93, label %then_0, label %else_0

then_0:                                           ; preds = %4
  %95 = getelementptr i64, i64* %14, i64 0
  %96 = load i64, i64* %95
  %97 = icmp eq i64 %96, 2
  %98 = getelementptr i64, i64* %14, i64 0
  %99 = load i64, i64* %98
  %100 = icmp eq i64 %99, 3
  %101 = or i1 %97, %100
  store i1 %101, i1* %94
  br label %endif_0

else_0:                                           ; preds = %4
  %102 = getelementptr i64, i64* %14, i64 0
  %103 = load i64, i64* %102
  %104 = icmp eq i64 %103, 3
  store i1 %104, i1* %94
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %105 = load i1, i1* %94
  ret i1 %105
}

define i64 @to_int.128(i1) {
  %2 = alloca i64
  br i1 %0, label %then_0, label %else_0

then_0:                                           ; preds = %1
  store i64 1, i64* %2
  br label %endif_0

else_0:                                           ; preds = %1
  store i64 0, i64* %2
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %3 = load i64, i64* %2
  ret i64 %3
}

define {}* @print_cells.124(i8*, i1*) {
  %3 = bitcast i8* %0 to { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }*
  %4 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 0
  %5 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %4
  %6 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 1
  %7 = load i64, i64* %6
  %8 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 2
  %9 = load i64, i64* %8
  %10 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %11 = bitcast i8* %10 to { {}* (i8*, i1*)*, i8* }*
  %12 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %11, i32 0, i32 0
  store {}* (i8*, i1*)* @print_cells.124, {}* (i8*, i1*)** %12
  %13 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %11, i32 0, i32 1
  store i8* %0, i8** %13
  %14 = call i8* @GC_malloc(i64 ptrtoint ({ i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* getelementptr inbounds ({ i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %15 = bitcast i8* %14 to { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }*
  %16 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %15, i32 0, i32 0
  store i1* %1, i1** %16
  %17 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %15, i32 0, i32 1
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %5, { i1 (i8*, i1*, i64, i64)*, i8* }** %17
  %18 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %15, i32 0, i32 2
  store i64 %7, i64* %18
  %19 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %15, i32 0, i32 3
  store i64 %9, i64* %19
  %20 = bitcast { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %15 to i8*
  %21 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %22 = bitcast i8* %21 to { {}* (i8*, i64)*, i8* }*
  %23 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %22, i32 0, i32 0
  store {}* (i8*, i64)* @go_y.122, {}* (i8*, i64)** %23
  %24 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %22, i32 0, i32 1
  store i8* %20, i8** %24
  %25 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %22, i32 0, i32 0
  %26 = load {}* (i8*, i64)*, {}* (i8*, i64)** %25
  %27 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %22, i32 0, i32 1
  %28 = load i8*, i8** %27
  %29 = call {}* %26(i8* %28, i64 0)
  ret {}* %29
}

define {}* @go_y.122(i8*, i64) {
  %3 = bitcast i8* %0 to { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }*
  %4 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 0
  %5 = load i1*, i1** %4
  %6 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 1
  %7 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %6
  %8 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 2
  %9 = load i64, i64* %8
  %10 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 3
  %11 = load i64, i64* %10
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { {}* (i8*, i64)*, i8* }*
  %14 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %13, i32 0, i32 0
  store {}* (i8*, i64)* @go_y.122, {}* (i8*, i64)** %14
  %15 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %13, i32 0, i32 1
  store i8* %0, i8** %15
  %16 = call i8* @GC_malloc(i64 ptrtoint ({ i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* getelementptr inbounds ({ i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %17 = bitcast i8* %16 to { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }*
  %18 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %17, i32 0, i32 0
  store i1* %5, i1** %18
  %19 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %17, i32 0, i32 1
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %7, { i1 (i8*, i1*, i64, i64)*, i8* }** %19
  %20 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %17, i32 0, i32 2
  store i64 %9, i64* %20
  %21 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %17, i32 0, i32 3
  store i64 %1, i64* %21
  %22 = bitcast { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %17 to i8*
  %23 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %24 = bitcast i8* %23 to { {}* (i8*, i64)*, i8* }*
  %25 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %24, i32 0, i32 0
  store {}* (i8*, i64)* @go_x.115, {}* (i8*, i64)** %25
  %26 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %24, i32 0, i32 1
  store i8* %22, i8** %26
  %27 = icmp sge i64 %1, %11
  %28 = alloca {}*
  br i1 %27, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %29 = call i8* @GC_malloc(i64 0)
  %30 = bitcast i8* %29 to {}*
  store {}* %30, {}** %28
  br label %endif_0

else_0:                                           ; preds = %2
  %31 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %24, i32 0, i32 0
  %32 = load {}* (i8*, i64)*, {}* (i8*, i64)** %31
  %33 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %24, i32 0, i32 1
  %34 = load i8*, i8** %33
  %35 = call {}* %32(i8* %34, i64 0)
  %36 = call {}* @newline.47()
  %37 = add i64 %1, 1
  %38 = call {}* @go_y.122(i8* %0, i64 %37)
  store {}* %38, {}** %28
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %39 = load {}*, {}** %28
  ret {}* %39
}

define {}* @go_x.115(i8*, i64) {
  %3 = bitcast i8* %0 to { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }*
  %4 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 0
  %5 = load i1*, i1** %4
  %6 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 1
  %7 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %6
  %8 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 2
  %9 = load i64, i64* %8
  %10 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 3
  %11 = load i64, i64* %10
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { {}* (i8*, i64)*, i8* }*
  %14 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %13, i32 0, i32 0
  store {}* (i8*, i64)* @go_x.115, {}* (i8*, i64)** %14
  %15 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %13, i32 0, i32 1
  store i8* %0, i8** %15
  %16 = icmp sge i64 %1, %9
  %17 = alloca {}*
  br i1 %16, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %18 = call i8* @GC_malloc(i64 0)
  %19 = bitcast i8* %18 to {}*
  store {}* %19, {}** %17
  br label %endif_0

else_0:                                           ; preds = %2
  %20 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %21 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %20
  %22 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %23 = load i8*, i8** %22
  %24 = call i1 %21(i8* %23, i1* %5, i64 %1, i64 %11)
  %25 = call i8 @to_char.98(i1 %24)
  %26 = call {}* @print_char.46(i8 %25)
  %27 = add i64 %1, 1
  %28 = call {}* @go_x.115(i8* %0, i64 %27)
  store {}* %28, {}** %17
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %29 = load {}*, {}** %17
  ret {}* %29
}

define i8 @to_char.98(i1) {
  %2 = alloca i8
  br i1 %0, label %then_0, label %else_0

then_0:                                           ; preds = %1
  store i8 35, i8* %2
  br label %endif_0

else_0:                                           ; preds = %1
  store i8 95, i8* %2
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %3 = load i8, i8* %2
  ret i8 %3
}

define {}* @init_cells.94(i8*, i1*, i64) {
  %4 = bitcast i8* %0 to { i64, i64 }*
  %5 = getelementptr { i64, i64 }, { i64, i64 }* %4, i32 0, i32 0
  %6 = load i64, i64* %5
  %7 = getelementptr { i64, i64 }, { i64, i64 }* %4, i32 0, i32 1
  %8 = load i64, i64* %7
  %9 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %10 = bitcast i8* %9 to { {}* (i8*, i1*, i64)*, i8* }*
  %11 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %10, i32 0, i32 0
  store {}* (i8*, i1*, i64)* @init_cells.94, {}* (i8*, i1*, i64)** %11
  %12 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %10, i32 0, i32 1
  store i8* %0, i8** %12
  %13 = mul i64 %6, %8
  %14 = icmp sge i64 %2, %13
  %15 = alloca {}*
  br i1 %14, label %then_0, label %else_0

then_0:                                           ; preds = %3
  %16 = call i8* @GC_malloc(i64 0)
  %17 = bitcast i8* %16 to {}*
  store {}* %17, {}** %15
  br label %endif_0

else_0:                                           ; preds = %3
  %18 = call i1 @rand_bool.49()
  %19 = getelementptr i1, i1* %1, i64 %2
  store i1 %18, i1* %19
  %20 = add i64 %2, 1
  %21 = call {}* @init_cells.94(i8* %0, i1* %1, i64 %20)
  store {}* %21, {}** %15
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %22 = load {}*, {}** %15
  ret {}* %22
}

define {}* @set.83(i8*, i1*, i64, i64, i1) {
  %6 = bitcast i8* %0 to { i64 }*
  %7 = getelementptr { i64 }, { i64 }* %6, i32 0, i32 0
  %8 = load i64, i64* %7
  %9 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %10 = bitcast i8* %9 to { {}* (i8*, i1*, i64, i64, i1)*, i8* }*
  %11 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %10, i32 0, i32 0
  store {}* (i8*, i1*, i64, i64, i1)* @set.83, {}* (i8*, i1*, i64, i64, i1)** %11
  %12 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %10, i32 0, i32 1
  store i8* %0, i8** %12
  %13 = mul i64 %3, %8
  %14 = add i64 %13, %2
  %15 = getelementptr i1, i1* %1, i64 %14
  store i1 %4, i1* %15
  ret {}* undef
}

define i1 @view.74(i8*, i1*, i64, i64) {
  %5 = bitcast i8* %0 to { i64, i64 }*
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %5, i32 0, i32 0
  %7 = load i64, i64* %6
  %8 = getelementptr { i64, i64 }, { i64, i64 }* %5, i32 0, i32 1
  %9 = load i64, i64* %8
  %10 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %11 = bitcast i8* %10 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %12 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %11, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @view.74, i1 (i8*, i1*, i64, i64)** %12
  %13 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %0, i8** %13
  %14 = icmp sge i64 %2, %7
  %15 = icmp sge i64 %3, %9
  %16 = or i1 %14, %15
  %17 = icmp slt i64 %2, 0
  %18 = or i1 %16, %17
  %19 = icmp slt i64 %3, 0
  %20 = or i1 %18, %19
  %21 = alloca i1
  br i1 %20, label %then_0, label %else_0

then_0:                                           ; preds = %4
  store i1 false, i1* %21
  br label %endif_0

else_0:                                           ; preds = %4
  %22 = mul i64 %3, %7
  %23 = add i64 %22, %2
  %24 = getelementptr i1, i1* %1, i64 %23
  %25 = load i1, i1* %24
  store i1 %25, i1* %21
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %26 = load i1, i1* %21
  ret i1 %26
}

declare {}* @pulsar(i1*)

define {}* @pulsar.52(i1*) {
  %2 = call {}* @pulsar(i1* %0)
  ret {}* %2
}

declare i1* @copy_bool_array(i1*, i64)

define i1* @copy_bool_array.51(i1*, i64) {
  %3 = call i1* @copy_bool_array(i1* %0, i64 %1)
  ret i1* %3
}

declare {}* @malgo_sleep(i64)

define {}* @sleep.50(i64) {
  %2 = call {}* @malgo_sleep(i64 %0)
  ret {}* %2
}

declare i1 @rand_bool()

define i1 @rand_bool.49() {
  %1 = call i1 @rand_bool()
  ret i1 %1
}

declare {}* @gen_seed()

define {}* @gen_seed.48() {
  %1 = call {}* @gen_seed()
  ret {}* %1
}

declare {}* @newline()

define {}* @newline.47() {
  %1 = call {}* @newline()
  ret {}* %1
}

declare {}* @print_char(i8)

define {}* @print_char.46(i8) {
  %2 = call {}* @print_char(i8 %0)
  ret {}* %2
}

define i32 @main() {
  %1 = mul i64 50, 20
  %2 = mul i64 %1, ptrtoint (i1* getelementptr inbounds (i1, i1* null, i32 1) to i64)
  %3 = call i8* @GC_malloc(i64 %2)
  %4 = bitcast i8* %3 to i1*
  %5 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %6 = bitcast i8* %5 to { i64, i64 }*
  %7 = getelementptr { i64, i64 }, { i64, i64 }* %6, i32 0, i32 0
  store i64 50, i64* %7
  %8 = getelementptr { i64, i64 }, { i64, i64 }* %6, i32 0, i32 1
  store i64 20, i64* %8
  %9 = bitcast { i64, i64 }* %6 to i8*
  %10 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %11 = bitcast i8* %10 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %12 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %11, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @view.74, i1 (i8*, i1*, i64, i64)** %12
  %13 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %9, i8** %13
  %14 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %15 = bitcast i8* %14 to { i64 }*
  %16 = getelementptr { i64 }, { i64 }* %15, i32 0, i32 0
  store i64 50, i64* %16
  %17 = bitcast { i64 }* %15 to i8*
  %18 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %19 = bitcast i8* %18 to { {}* (i8*, i1*, i64, i64, i1)*, i8* }*
  %20 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %19, i32 0, i32 0
  store {}* (i8*, i1*, i64, i64, i1)* @set.83, {}* (i8*, i1*, i64, i64, i1)** %20
  %21 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %19, i32 0, i32 1
  store i8* %17, i8** %21
  %22 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %23 = bitcast i8* %22 to { i64, i64 }*
  %24 = getelementptr { i64, i64 }, { i64, i64 }* %23, i32 0, i32 0
  store i64 50, i64* %24
  %25 = getelementptr { i64, i64 }, { i64, i64 }* %23, i32 0, i32 1
  store i64 20, i64* %25
  %26 = bitcast { i64, i64 }* %23 to i8*
  %27 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %28 = bitcast i8* %27 to { {}* (i8*, i1*, i64)*, i8* }*
  %29 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %28, i32 0, i32 0
  store {}* (i8*, i1*, i64)* @init_cells.94, {}* (i8*, i1*, i64)** %29
  %30 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %28, i32 0, i32 1
  store i8* %26, i8** %30
  %31 = call i8* @GC_malloc(i64 ptrtoint ({ { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* getelementptr inbounds ({ { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %32 = bitcast i8* %31 to { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }*
  %33 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %32, i32 0, i32 0
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %11, { i1 (i8*, i1*, i64, i64)*, i8* }** %33
  %34 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %32, i32 0, i32 1
  store i64 50, i64* %34
  %35 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %32, i32 0, i32 2
  store i64 20, i64* %35
  %36 = bitcast { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %32 to i8*
  %37 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %38 = bitcast i8* %37 to { {}* (i8*, i1*)*, i8* }*
  %39 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %38, i32 0, i32 0
  store {}* (i8*, i1*)* @print_cells.124, {}* (i8*, i1*)** %39
  %40 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %38, i32 0, i32 1
  store i8* %36, i8** %40
  %41 = call i8* @GC_malloc(i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64))
  %42 = bitcast i8* %41 to { { i1 (i8*, i1*, i64, i64)*, i8* }* }*
  %43 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }* }, { { i1 (i8*, i1*, i64, i64)*, i8* }* }* %42, i32 0, i32 0
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %11, { i1 (i8*, i1*, i64, i64)*, i8* }** %43
  %44 = bitcast { { i1 (i8*, i1*, i64, i64)*, i8* }* }* %42 to i8*
  %45 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %46 = bitcast i8* %45 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %47 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %46, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @next_state.183, i1 (i8*, i1*, i64, i64)** %47
  %48 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %46, i32 0, i32 1
  store i8* %44, i8** %48
  %49 = call i8* @GC_malloc(i64 ptrtoint ({ { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* getelementptr inbounds ({ { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %50 = bitcast i8* %49 to { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %51 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %50, i32 0, i32 0
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %46, { i1 (i8*, i1*, i64, i64)*, i8* }** %51
  %52 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %50, i32 0, i32 1
  store { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %19, { {}* (i8*, i1*, i64, i64, i1)*, i8* }** %52
  %53 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %50, i32 0, i32 2
  store i64 50, i64* %53
  %54 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %50, i32 0, i32 3
  store i64 20, i64* %54
  %55 = bitcast { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %50 to i8*
  %56 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %57 = bitcast i8* %56 to { {}* (i8*, i1*)*, i8* }*
  %58 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %57, i32 0, i32 0
  store {}* (i8*, i1*)* @update_cells.210, {}* (i8*, i1*)** %58
  %59 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %57, i32 0, i32 1
  store i8* %55, i8** %59
  %60 = call {}* @gen_seed.48()
  %61 = call {}* @pulsar.52(i1* %4)
  %62 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 3))
  %63 = bitcast i8* %62 to { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }*
  %64 = getelementptr { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }, { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }* %63, i32 0, i32 0
  store i1* %4, i1** %64
  %65 = getelementptr { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }, { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }* %63, i32 0, i32 1
  store { {}* (i8*, i1*)*, i8* }* %38, { {}* (i8*, i1*)*, i8* }** %65
  %66 = getelementptr { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }, { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }* %63, i32 0, i32 2
  store { {}* (i8*, i1*)*, i8* }* %57, { {}* (i8*, i1*)*, i8* }** %66
  %67 = bitcast { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }* %63 to i8*
  %68 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %69 = bitcast i8* %68 to { {}* (i8*, i64)*, i8* }*
  %70 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %69, i32 0, i32 0
  store {}* (i8*, i64)* @loop.224, {}* (i8*, i64)** %70
  %71 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %69, i32 0, i32 1
  store i8* %67, i8** %71
  %72 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %69, i32 0, i32 0
  %73 = load {}* (i8*, i64)*, {}* (i8*, i64)** %72
  %74 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %69, i32 0, i32 1
  %75 = load i8*, i8** %74
  %76 = call {}* %73(i8* %75, i64 50)
  ret i32 0
}
