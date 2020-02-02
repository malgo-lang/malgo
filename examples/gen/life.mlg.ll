; ModuleID = './examples/life.mlg'
source_filename = "./examples/life.mlg"

declare i8* @GC_malloc(i64)

define i1 @view10(i8*, { i1*, i64 }*, i64, i64) {
  %5 = bitcast i8* %0 to { i64, i64 }*
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %5, i32 0, i32 0
  %7 = load i64, i64* %6
  %8 = getelementptr { i64, i64 }, { i64, i64 }* %5, i32 0, i32 1
  %9 = load i64, i64* %8
  %10 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %11 = bitcast i8* %10 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %12 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %11, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %12
  %13 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %0, i8** %13
  %14 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %15 = bitcast i8* %14 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*
  %16 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %15, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %16
  %17 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %15, i32 0, i32 1
  store i8* %0, i8** %17
  %18 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %19 = bitcast i8* %18 to { {}* (i8*, { i1*, i64 }*, i64)*, i8* }*
  %20 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %19, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %20
  %21 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %19, i32 0, i32 1
  store i8* %0, i8** %21
  %22 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %23 = bitcast i8* %22 to { i8 (i8*, i1)*, i8* }*
  %24 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %23, i32 0, i32 0
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %24
  %25 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %23, i32 0, i32 1
  store i8* %0, i8** %25
  %26 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %27 = bitcast i8* %26 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %28 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %27, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %28
  %29 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %27, i32 0, i32 1
  store i8* %0, i8** %29
  %30 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %31 = bitcast i8* %30 to { i64 (i8*, i1)*, i8* }*
  %32 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %31, i32 0, i32 0
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %32
  %33 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %31, i32 0, i32 1
  store i8* %0, i8** %33
  %34 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %35 = bitcast i8* %34 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %36 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %35, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %36
  %37 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %35, i32 0, i32 1
  store i8* %0, i8** %37
  %38 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %39 = bitcast i8* %38 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %40 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %39, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %40
  %41 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %39, i32 0, i32 1
  store i8* %0, i8** %41
  %42 = icmp sge i64 %2, %7
  %43 = icmp sge i64 %3, %9
  %44 = or i1 %42, %43
  %45 = icmp slt i64 %2, 0
  %46 = or i1 %44, %45
  %47 = icmp slt i64 %3, 0
  %48 = or i1 %46, %47
  %49 = alloca i1
  br i1 %48, label %then_0, label %else_0

then_0:                                           ; preds = %4
  store i1 false, i1* %49
  br label %end_0

else_0:                                           ; preds = %4
  %50 = mul i64 %3, %7
  %51 = add i64 %50, %2
  %52 = getelementptr { i1*, i64 }, { i1*, i64 }* %1, i32 0, i32 0
  %53 = load i1*, i1** %52
  %54 = getelementptr i1, i1* %53, i64 %51
  %55 = load i1, i1* %54
  store i1 %55, i1* %49
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %56 = load i1, i1* %49
  ret i1 %56
}

define {}* @update_cells17(i8*, { i1*, i64 }*) {
  %3 = bitcast i8* %0 to { i64, i64 }*
  %4 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 1
  %7 = load i64, i64* %6
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %10 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %10
  %11 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*
  %14 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %13, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %14
  %15 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %13, i32 0, i32 1
  store i8* %0, i8** %15
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { {}* (i8*, { i1*, i64 }*, i64)*, i8* }*
  %18 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %17, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %18
  %19 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %17, i32 0, i32 1
  store i8* %0, i8** %19
  %20 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %21 = bitcast i8* %20 to { i8 (i8*, i1)*, i8* }*
  %22 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %21, i32 0, i32 0
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %22
  %23 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %21, i32 0, i32 1
  store i8* %0, i8** %23
  %24 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %25 = bitcast i8* %24 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %26 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %25, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %26
  %27 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %25, i32 0, i32 1
  store i8* %0, i8** %27
  %28 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %29 = bitcast i8* %28 to { i64 (i8*, i1)*, i8* }*
  %30 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %29, i32 0, i32 0
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %30
  %31 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %29, i32 0, i32 1
  store i8* %0, i8** %31
  %32 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %33 = bitcast i8* %32 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %34 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %33, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %34
  %35 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %33, i32 0, i32 1
  store i8* %0, i8** %35
  %36 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %37 = bitcast i8* %36 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %38 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %37, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %38
  %39 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %37, i32 0, i32 1
  store i8* %0, i8** %39
  %40 = call { i1*, i64 }* @copy_bool_array5({ i1*, i64 }* %1)
  %41 = call i8* @GC_malloc(i64 ptrtoint ({ { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* getelementptr inbounds ({ { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %42 = bitcast i8* %41 to { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %43 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %42, i32 0, i32 0
  store { i1*, i64 }* %1, { i1*, i64 }** %43
  %44 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %42, i32 0, i32 1
  store { i1*, i64 }* %40, { i1*, i64 }** %44
  %45 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %42, i32 0, i32 2
  store { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %33, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %45
  %46 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %42, i32 0, i32 3
  store { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %13, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }** %46
  %47 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %42, i32 0, i32 4
  store i64 %5, i64* %47
  %48 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %42, i32 0, i32 5
  store i64 %7, i64* %48
  %49 = bitcast { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %42 to i8*
  %50 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %51 = bitcast i8* %50 to { {}* (i8*, i64)*, i8* }*
  %52 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %51, i32 0, i32 0
  store {}* (i8*, i64)* @go_y40, {}* (i8*, i64)** %52
  %53 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %51, i32 0, i32 1
  store i8* %49, i8** %53
  %54 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %51, i32 0, i32 0
  %55 = load {}* (i8*, i64)*, {}* (i8*, i64)** %54
  %56 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %51, i32 0, i32 1
  %57 = load i8*, i8** %56
  %58 = call {}* %55(i8* %57, i64 0)
  ret {}* %58
}

define i64 @to_int15(i8*, i1) {
  %3 = bitcast i8* %0 to { i64, i64 }*
  %4 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 1
  %7 = load i64, i64* %6
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %10 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %10
  %11 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*
  %14 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %13, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %14
  %15 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %13, i32 0, i32 1
  store i8* %0, i8** %15
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { {}* (i8*, { i1*, i64 }*, i64)*, i8* }*
  %18 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %17, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %18
  %19 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %17, i32 0, i32 1
  store i8* %0, i8** %19
  %20 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %21 = bitcast i8* %20 to { i8 (i8*, i1)*, i8* }*
  %22 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %21, i32 0, i32 0
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %22
  %23 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %21, i32 0, i32 1
  store i8* %0, i8** %23
  %24 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %25 = bitcast i8* %24 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %26 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %25, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %26
  %27 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %25, i32 0, i32 1
  store i8* %0, i8** %27
  %28 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %29 = bitcast i8* %28 to { i64 (i8*, i1)*, i8* }*
  %30 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %29, i32 0, i32 0
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %30
  %31 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %29, i32 0, i32 1
  store i8* %0, i8** %31
  %32 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %33 = bitcast i8* %32 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %34 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %33, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %34
  %35 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %33, i32 0, i32 1
  store i8* %0, i8** %35
  %36 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %37 = bitcast i8* %36 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %38 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %37, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %38
  %39 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %37, i32 0, i32 1
  store i8* %0, i8** %39
  %40 = alloca i64
  br i1 %1, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store i64 1, i64* %40
  br label %end_0

else_0:                                           ; preds = %2
  store i64 0, i64* %40
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %41 = load i64, i64* %40
  ret i64 %41
}

define i8 @to_char13(i8*, i1) {
  %3 = bitcast i8* %0 to { i64, i64 }*
  %4 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 1
  %7 = load i64, i64* %6
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %10 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %10
  %11 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*
  %14 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %13, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %14
  %15 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %13, i32 0, i32 1
  store i8* %0, i8** %15
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { {}* (i8*, { i1*, i64 }*, i64)*, i8* }*
  %18 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %17, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %18
  %19 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %17, i32 0, i32 1
  store i8* %0, i8** %19
  %20 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %21 = bitcast i8* %20 to { i8 (i8*, i1)*, i8* }*
  %22 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %21, i32 0, i32 0
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %22
  %23 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %21, i32 0, i32 1
  store i8* %0, i8** %23
  %24 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %25 = bitcast i8* %24 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %26 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %25, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %26
  %27 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %25, i32 0, i32 1
  store i8* %0, i8** %27
  %28 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %29 = bitcast i8* %28 to { i64 (i8*, i1)*, i8* }*
  %30 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %29, i32 0, i32 0
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %30
  %31 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %29, i32 0, i32 1
  store i8* %0, i8** %31
  %32 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %33 = bitcast i8* %32 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %34 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %33, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %34
  %35 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %33, i32 0, i32 1
  store i8* %0, i8** %35
  %36 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %37 = bitcast i8* %36 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %38 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %37, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %38
  %39 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %37, i32 0, i32 1
  store i8* %0, i8** %39
  %40 = alloca i8
  br i1 %1, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store i8 35, i8* %40
  br label %end_0

else_0:                                           ; preds = %2
  store i8 95, i8* %40
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %41 = load i8, i8* %40
  ret i8 %41
}

declare {}* @malgo_sleep(i64)

define {}* @sleep4(i64) {
  %2 = call {}* @malgo_sleep(i64 %0)
  ret {}* %2
}

define {}* @set11(i8*, { i1*, i64 }*, i64, i64, i1) {
  %6 = bitcast i8* %0 to { i64, i64 }*
  %7 = getelementptr { i64, i64 }, { i64, i64 }* %6, i32 0, i32 0
  %8 = load i64, i64* %7
  %9 = getelementptr { i64, i64 }, { i64, i64 }* %6, i32 0, i32 1
  %10 = load i64, i64* %9
  %11 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %12 = bitcast i8* %11 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %13 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %12, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %13
  %14 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %12, i32 0, i32 1
  store i8* %0, i8** %14
  %15 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %16 = bitcast i8* %15 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*
  %17 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %16, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %17
  %18 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %16, i32 0, i32 1
  store i8* %0, i8** %18
  %19 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %20 = bitcast i8* %19 to { {}* (i8*, { i1*, i64 }*, i64)*, i8* }*
  %21 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %20, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %21
  %22 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %20, i32 0, i32 1
  store i8* %0, i8** %22
  %23 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %24 = bitcast i8* %23 to { i8 (i8*, i1)*, i8* }*
  %25 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %24, i32 0, i32 0
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %25
  %26 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %24, i32 0, i32 1
  store i8* %0, i8** %26
  %27 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %28 = bitcast i8* %27 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %29 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %28, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %29
  %30 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %28, i32 0, i32 1
  store i8* %0, i8** %30
  %31 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %32 = bitcast i8* %31 to { i64 (i8*, i1)*, i8* }*
  %33 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %32, i32 0, i32 0
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %33
  %34 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %32, i32 0, i32 1
  store i8* %0, i8** %34
  %35 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %36 = bitcast i8* %35 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %37 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %36, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %37
  %38 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %36, i32 0, i32 1
  store i8* %0, i8** %38
  %39 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %40 = bitcast i8* %39 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %41 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %40, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %41
  %42 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %40, i32 0, i32 1
  store i8* %0, i8** %42
  %43 = mul i64 %3, %8
  %44 = add i64 %43, %2
  %45 = getelementptr { i1*, i64 }, { i1*, i64 }* %1, i32 0, i32 0
  %46 = load i1*, i1** %45
  %47 = getelementptr i1, i1* %46, i64 %44
  store i1 %4, i1* %47
  ret {}* undef
}

declare i1 @rand_bool()

define i1 @rand_bool3() {
  %1 = call i1 @rand_bool()
  ret i1 %1
}

declare {}* @pulsar({ i1*, i64 }*)

define {}* @pulsar6({ i1*, i64 }*) {
  %2 = call {}* @pulsar({ i1*, i64 }* %0)
  ret {}* %2
}

declare {}* @print_char(i8)

define {}* @print_char0(i8) {
  %2 = call {}* @print_char(i8 %0)
  ret {}* %2
}

define {}* @print_cells14(i8*, { i1*, i64 }*) {
  %3 = bitcast i8* %0 to { i64, i64 }*
  %4 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 1
  %7 = load i64, i64* %6
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %10 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %10
  %11 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*
  %14 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %13, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %14
  %15 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %13, i32 0, i32 1
  store i8* %0, i8** %15
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { {}* (i8*, { i1*, i64 }*, i64)*, i8* }*
  %18 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %17, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %18
  %19 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %17, i32 0, i32 1
  store i8* %0, i8** %19
  %20 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %21 = bitcast i8* %20 to { i8 (i8*, i1)*, i8* }*
  %22 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %21, i32 0, i32 0
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %22
  %23 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %21, i32 0, i32 1
  store i8* %0, i8** %23
  %24 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %25 = bitcast i8* %24 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %26 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %25, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %26
  %27 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %25, i32 0, i32 1
  store i8* %0, i8** %27
  %28 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %29 = bitcast i8* %28 to { i64 (i8*, i1)*, i8* }*
  %30 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %29, i32 0, i32 0
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %30
  %31 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %29, i32 0, i32 1
  store i8* %0, i8** %31
  %32 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %33 = bitcast i8* %32 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %34 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %33, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %34
  %35 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %33, i32 0, i32 1
  store i8* %0, i8** %35
  %36 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %37 = bitcast i8* %36 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %38 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %37, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %38
  %39 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %37, i32 0, i32 1
  store i8* %0, i8** %39
  %40 = call i8* @GC_malloc(i64 ptrtoint ({ { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* getelementptr inbounds ({ { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %41 = bitcast i8* %40 to { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }*
  %42 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %41, i32 0, i32 0
  store { i1*, i64 }* %1, { i1*, i64 }** %42
  %43 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %41, i32 0, i32 1
  store { i8 (i8*, i1)*, i8* }* %21, { i8 (i8*, i1)*, i8* }** %43
  %44 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %41, i32 0, i32 2
  store { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %44
  %45 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %41, i32 0, i32 3
  store i64 %5, i64* %45
  %46 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %41, i32 0, i32 4
  store i64 %7, i64* %46
  %47 = bitcast { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %41 to i8*
  %48 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %49 = bitcast i8* %48 to { {}* (i8*, i64)*, i8* }*
  %50 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %49, i32 0, i32 0
  store {}* (i8*, i64)* @go_y29, {}* (i8*, i64)** %50
  %51 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %49, i32 0, i32 1
  store i8* %47, i8** %51
  %52 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %49, i32 0, i32 0
  %53 = load {}* (i8*, i64)*, {}* (i8*, i64)** %52
  %54 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %49, i32 0, i32 1
  %55 = load i8*, i8** %54
  %56 = call {}* %53(i8* %55, i64 0)
  ret {}* %56
}

define i1 @next_state16(i8*, { i1*, i64 }*, i64, i64) {
  %5 = bitcast i8* %0 to { i64, i64 }*
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %5, i32 0, i32 0
  %7 = load i64, i64* %6
  %8 = getelementptr { i64, i64 }, { i64, i64 }* %5, i32 0, i32 1
  %9 = load i64, i64* %8
  %10 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %11 = bitcast i8* %10 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %12 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %11, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %12
  %13 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %0, i8** %13
  %14 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %15 = bitcast i8* %14 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*
  %16 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %15, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %16
  %17 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %15, i32 0, i32 1
  store i8* %0, i8** %17
  %18 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %19 = bitcast i8* %18 to { {}* (i8*, { i1*, i64 }*, i64)*, i8* }*
  %20 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %19, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %20
  %21 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %19, i32 0, i32 1
  store i8* %0, i8** %21
  %22 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %23 = bitcast i8* %22 to { i8 (i8*, i1)*, i8* }*
  %24 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %23, i32 0, i32 0
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %24
  %25 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %23, i32 0, i32 1
  store i8* %0, i8** %25
  %26 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %27 = bitcast i8* %26 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %28 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %27, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %28
  %29 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %27, i32 0, i32 1
  store i8* %0, i8** %29
  %30 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %31 = bitcast i8* %30 to { i64 (i8*, i1)*, i8* }*
  %32 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %31, i32 0, i32 0
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %32
  %33 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %31, i32 0, i32 1
  store i8* %0, i8** %33
  %34 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %35 = bitcast i8* %34 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %36 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %35, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %36
  %37 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %35, i32 0, i32 1
  store i8* %0, i8** %37
  %38 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %39 = bitcast i8* %38 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %40 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %39, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %40
  %41 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %39, i32 0, i32 1
  store i8* %0, i8** %41
  %42 = mul i64 ptrtoint (i64* getelementptr inbounds (i64, i64* null, i32 1) to i64), 1
  %43 = call i8* @GC_malloc(i64 %42)
  %44 = bitcast i8* %43 to i64*
  %45 = call i8* @GC_malloc(i64 ptrtoint ({ i64*, i64 }* getelementptr inbounds ({ i64*, i64 }, { i64*, i64 }* null, i32 1) to i64))
  %46 = bitcast i8* %45 to { i64*, i64 }*
  %47 = getelementptr { i64*, i64 }, { i64*, i64 }* %46, i32 0, i32 0
  store i64* %44, i64** %47
  %48 = getelementptr { i64*, i64 }, { i64*, i64 }* %46, i32 0, i32 1
  store i64 1, i64* %48
  %49 = alloca i64
  store i64 0, i64* %49
  br label %cond_0

cond_0:                                           ; preds = %body_0, %4
  %50 = load i64, i64* %49
  %51 = icmp slt i64 %50, 1
  br i1 %51, label %body_0, label %end_0

body_0:                                           ; preds = %cond_0
  %52 = getelementptr { i64*, i64 }, { i64*, i64 }* %46, i32 0, i32 0
  %53 = load i64*, i64** %52
  %54 = getelementptr i64, i64* %53, i64 %50
  store i64 0, i64* %54
  %55 = add i64 %50, 1
  store i64 %55, i64* %49
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %56 = sub i64 %2, 1
  %57 = add i64 %3, 1
  %58 = call i1 @view10(i8* %0, { i1*, i64 }* %1, i64 %56, i64 %57)
  %59 = call i64 @to_int15(i8* %0, i1 %58)
  %60 = add i64 %3, 1
  %61 = call i1 @view10(i8* %0, { i1*, i64 }* %1, i64 %2, i64 %60)
  %62 = call i64 @to_int15(i8* %0, i1 %61)
  %63 = add i64 %59, %62
  %64 = add i64 %2, 1
  %65 = add i64 %3, 1
  %66 = call i1 @view10(i8* %0, { i1*, i64 }* %1, i64 %64, i64 %65)
  %67 = call i64 @to_int15(i8* %0, i1 %66)
  %68 = add i64 %63, %67
  %69 = getelementptr { i64*, i64 }, { i64*, i64 }* %46, i32 0, i32 0
  %70 = load i64*, i64** %69
  %71 = getelementptr i64, i64* %70, i64 0
  store i64 %68, i64* %71
  %72 = sub i64 %2, 1
  %73 = call i1 @view10(i8* %0, { i1*, i64 }* %1, i64 %72, i64 %3)
  %74 = call i64 @to_int15(i8* %0, i1 %73)
  %75 = add i64 %2, 1
  %76 = call i1 @view10(i8* %0, { i1*, i64 }* %1, i64 %75, i64 %3)
  %77 = call i64 @to_int15(i8* %0, i1 %76)
  %78 = add i64 %74, %77
  %79 = getelementptr { i64*, i64 }, { i64*, i64 }* %46, i32 0, i32 0
  %80 = load i64*, i64** %79
  %81 = getelementptr i64, i64* %80, i64 0
  %82 = load i64, i64* %81
  %83 = add i64 %78, %82
  %84 = getelementptr { i64*, i64 }, { i64*, i64 }* %46, i32 0, i32 0
  %85 = load i64*, i64** %84
  %86 = getelementptr i64, i64* %85, i64 0
  store i64 %83, i64* %86
  %87 = sub i64 %2, 1
  %88 = sub i64 %3, 1
  %89 = call i1 @view10(i8* %0, { i1*, i64 }* %1, i64 %87, i64 %88)
  %90 = call i64 @to_int15(i8* %0, i1 %89)
  %91 = sub i64 %3, 1
  %92 = call i1 @view10(i8* %0, { i1*, i64 }* %1, i64 %2, i64 %91)
  %93 = call i64 @to_int15(i8* %0, i1 %92)
  %94 = add i64 %90, %93
  %95 = add i64 %2, 1
  %96 = sub i64 %3, 1
  %97 = call i1 @view10(i8* %0, { i1*, i64 }* %1, i64 %95, i64 %96)
  %98 = call i64 @to_int15(i8* %0, i1 %97)
  %99 = add i64 %94, %98
  %100 = getelementptr { i64*, i64 }, { i64*, i64 }* %46, i32 0, i32 0
  %101 = load i64*, i64** %100
  %102 = getelementptr i64, i64* %101, i64 0
  %103 = load i64, i64* %102
  %104 = add i64 %99, %103
  %105 = getelementptr { i64*, i64 }, { i64*, i64 }* %46, i32 0, i32 0
  %106 = load i64*, i64** %105
  %107 = getelementptr i64, i64* %106, i64 0
  store i64 %104, i64* %107
  %108 = call i1 @view10(i8* %0, { i1*, i64 }* %1, i64 %2, i64 %3)
  %109 = alloca i1
  br i1 %108, label %then_0, label %else_0

then_0:                                           ; preds = %end_0
  %110 = getelementptr { i64*, i64 }, { i64*, i64 }* %46, i32 0, i32 0
  %111 = load i64*, i64** %110
  %112 = getelementptr i64, i64* %111, i64 0
  %113 = load i64, i64* %112
  %114 = icmp eq i64 %113, 2
  %115 = getelementptr { i64*, i64 }, { i64*, i64 }* %46, i32 0, i32 0
  %116 = load i64*, i64** %115
  %117 = getelementptr i64, i64* %116, i64 0
  %118 = load i64, i64* %117
  %119 = icmp eq i64 %118, 3
  %120 = or i1 %114, %119
  store i1 %120, i1* %109
  br label %end_1

else_0:                                           ; preds = %end_0
  %121 = getelementptr { i64*, i64 }, { i64*, i64 }* %46, i32 0, i32 0
  %122 = load i64*, i64** %121
  %123 = getelementptr i64, i64* %122, i64 0
  %124 = load i64, i64* %123
  %125 = icmp eq i64 %124, 3
  store i1 %125, i1* %109
  br label %end_1

end_1:                                            ; preds = %else_0, %then_0
  %126 = load i1, i1* %109
  ret i1 %126
}

declare {}* @newline()

define {}* @newline1() {
  %1 = call {}* @newline()
  ret {}* %1
}

define {}* @loop44(i8*, i64) {
  %3 = bitcast i8* %0 to { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }*
  %4 = getelementptr { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }, { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }* %3, i32 0, i32 0
  %5 = load { i1*, i64 }*, { i1*, i64 }** %4
  %6 = getelementptr { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }, { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }* %3, i32 0, i32 1
  %7 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %6
  %8 = getelementptr { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }, { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }* %3, i32 0, i32 2
  %9 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %8
  %10 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %11 = bitcast i8* %10 to { {}* (i8*, i64)*, i8* }*
  %12 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %11, i32 0, i32 0
  store {}* (i8*, i64)* @loop44, {}* (i8*, i64)** %12
  %13 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %0, i8** %13
  %14 = icmp sgt i64 %1, 0
  %15 = alloca {}*
  br i1 %14, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %16 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %7, i32 0, i32 0
  %17 = load {}* (i8*, { i1*, i64 }*)*, {}* (i8*, { i1*, i64 }*)** %16
  %18 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %7, i32 0, i32 1
  %19 = load i8*, i8** %18
  %20 = call {}* %17(i8* %19, { i1*, i64 }* %5)
  %21 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %9, i32 0, i32 0
  %22 = load {}* (i8*, { i1*, i64 }*)*, {}* (i8*, { i1*, i64 }*)** %21
  %23 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %9, i32 0, i32 1
  %24 = load i8*, i8** %23
  %25 = call {}* %22(i8* %24, { i1*, i64 }* %5)
  %26 = call {}* @newline1()
  %27 = sub i64 %1, 1
  %28 = call {}* @loop44(i8* %0, i64 %27)
  store {}* %28, {}** %15
  br label %end_0

else_0:                                           ; preds = %2
  %29 = call i8* @GC_malloc(i64 0)
  %30 = bitcast i8* %29 to {}*
  store {}* %30, {}** %15
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %31 = load {}*, {}** %15
  ret {}* %31
}

define {}* @init_cells12(i8*, { i1*, i64 }*, i64) {
  %4 = bitcast i8* %0 to { i64, i64 }*
  %5 = getelementptr { i64, i64 }, { i64, i64 }* %4, i32 0, i32 0
  %6 = load i64, i64* %5
  %7 = getelementptr { i64, i64 }, { i64, i64 }* %4, i32 0, i32 1
  %8 = load i64, i64* %7
  %9 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %10 = bitcast i8* %9 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %11 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %10, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %11
  %12 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %10, i32 0, i32 1
  store i8* %0, i8** %12
  %13 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %14 = bitcast i8* %13 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*
  %15 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %14, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %15
  %16 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %14, i32 0, i32 1
  store i8* %0, i8** %16
  %17 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %18 = bitcast i8* %17 to { {}* (i8*, { i1*, i64 }*, i64)*, i8* }*
  %19 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %18, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %19
  %20 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %18, i32 0, i32 1
  store i8* %0, i8** %20
  %21 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %22 = bitcast i8* %21 to { i8 (i8*, i1)*, i8* }*
  %23 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %22, i32 0, i32 0
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %23
  %24 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %22, i32 0, i32 1
  store i8* %0, i8** %24
  %25 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %26 = bitcast i8* %25 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %27 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %26, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %27
  %28 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %26, i32 0, i32 1
  store i8* %0, i8** %28
  %29 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %30 = bitcast i8* %29 to { i64 (i8*, i1)*, i8* }*
  %31 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %30, i32 0, i32 0
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %31
  %32 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %30, i32 0, i32 1
  store i8* %0, i8** %32
  %33 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %34 = bitcast i8* %33 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %35 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %34, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %35
  %36 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %34, i32 0, i32 1
  store i8* %0, i8** %36
  %37 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %38 = bitcast i8* %37 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %39 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %38, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %39
  %40 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %38, i32 0, i32 1
  store i8* %0, i8** %40
  %41 = mul i64 %6, %8
  %42 = icmp sge i64 %2, %41
  %43 = alloca {}*
  br i1 %42, label %then_0, label %else_0

then_0:                                           ; preds = %3
  %44 = call i8* @GC_malloc(i64 0)
  %45 = bitcast i8* %44 to {}*
  store {}* %45, {}** %43
  br label %end_0

else_0:                                           ; preds = %3
  %46 = call i1 @rand_bool3()
  %47 = getelementptr { i1*, i64 }, { i1*, i64 }* %1, i32 0, i32 0
  %48 = load i1*, i1** %47
  %49 = getelementptr i1, i1* %48, i64 %2
  store i1 %46, i1* %49
  %50 = add i64 %2, 1
  %51 = call {}* @init_cells12(i8* %0, { i1*, i64 }* %1, i64 %50)
  store {}* %51, {}** %43
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %52 = load {}*, {}** %43
  ret {}* %52
}

define {}* @go_y40(i8*, i64) {
  %3 = bitcast i8* %0 to { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %4 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 0
  %5 = load { i1*, i64 }*, { i1*, i64 }** %4
  %6 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 1
  %7 = load { i1*, i64 }*, { i1*, i64 }** %6
  %8 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 2
  %9 = load { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %8
  %10 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 3
  %11 = load { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }** %10
  %12 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 4
  %13 = load i64, i64* %12
  %14 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 5
  %15 = load i64, i64* %14
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { {}* (i8*, i64)*, i8* }*
  %18 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %17, i32 0, i32 0
  store {}* (i8*, i64)* @go_y40, {}* (i8*, i64)** %18
  %19 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %17, i32 0, i32 1
  store i8* %0, i8** %19
  %20 = call i8* @GC_malloc(i64 ptrtoint ({ { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* getelementptr inbounds ({ { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %21 = bitcast i8* %20 to { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %22 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %21, i32 0, i32 0
  store { i1*, i64 }* %5, { i1*, i64 }** %22
  %23 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %21, i32 0, i32 1
  store { i1*, i64 }* %7, { i1*, i64 }** %23
  %24 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %21, i32 0, i32 2
  store { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %24
  %25 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %21, i32 0, i32 3
  store { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %11, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }** %25
  %26 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %21, i32 0, i32 4
  store i64 %13, i64* %26
  %27 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %21, i32 0, i32 5
  store i64 %1, i64* %27
  %28 = bitcast { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %21 to i8*
  %29 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %30 = bitcast i8* %29 to { {}* (i8*, i64)*, i8* }*
  %31 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %30, i32 0, i32 0
  store {}* (i8*, i64)* @go_x42, {}* (i8*, i64)** %31
  %32 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %30, i32 0, i32 1
  store i8* %28, i8** %32
  %33 = icmp sge i64 %1, %15
  %34 = alloca {}*
  br i1 %33, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %35 = call i8* @GC_malloc(i64 0)
  %36 = bitcast i8* %35 to {}*
  store {}* %36, {}** %34
  br label %end_0

else_0:                                           ; preds = %2
  %37 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %30, i32 0, i32 0
  %38 = load {}* (i8*, i64)*, {}* (i8*, i64)** %37
  %39 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %30, i32 0, i32 1
  %40 = load i8*, i8** %39
  %41 = call {}* %38(i8* %40, i64 0)
  %42 = add i64 %1, 1
  %43 = call {}* @go_y40(i8* %0, i64 %42)
  store {}* %43, {}** %34
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %44 = load {}*, {}** %34
  ret {}* %44
}

define {}* @go_y29(i8*, i64) {
  %3 = bitcast i8* %0 to { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }*
  %4 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 0
  %5 = load { i1*, i64 }*, { i1*, i64 }** %4
  %6 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 1
  %7 = load { i8 (i8*, i1)*, i8* }*, { i8 (i8*, i1)*, i8* }** %6
  %8 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 2
  %9 = load { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %8
  %10 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 3
  %11 = load i64, i64* %10
  %12 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 4
  %13 = load i64, i64* %12
  %14 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %15 = bitcast i8* %14 to { {}* (i8*, i64)*, i8* }*
  %16 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %15, i32 0, i32 0
  store {}* (i8*, i64)* @go_y29, {}* (i8*, i64)** %16
  %17 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %15, i32 0, i32 1
  store i8* %0, i8** %17
  %18 = call i8* @GC_malloc(i64 ptrtoint ({ { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* getelementptr inbounds ({ { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %19 = bitcast i8* %18 to { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }*
  %20 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %19, i32 0, i32 0
  store { i1*, i64 }* %5, { i1*, i64 }** %20
  %21 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %19, i32 0, i32 1
  store { i8 (i8*, i1)*, i8* }* %7, { i8 (i8*, i1)*, i8* }** %21
  %22 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %19, i32 0, i32 2
  store { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %22
  %23 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %19, i32 0, i32 3
  store i64 %11, i64* %23
  %24 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %19, i32 0, i32 4
  store i64 %1, i64* %24
  %25 = bitcast { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %19 to i8*
  %26 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %27 = bitcast i8* %26 to { {}* (i8*, i64)*, i8* }*
  %28 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %27, i32 0, i32 0
  store {}* (i8*, i64)* @go_x31, {}* (i8*, i64)** %28
  %29 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %27, i32 0, i32 1
  store i8* %25, i8** %29
  %30 = icmp sge i64 %1, %13
  %31 = alloca {}*
  br i1 %30, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %32 = call i8* @GC_malloc(i64 0)
  %33 = bitcast i8* %32 to {}*
  store {}* %33, {}** %31
  br label %end_0

else_0:                                           ; preds = %2
  %34 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %27, i32 0, i32 0
  %35 = load {}* (i8*, i64)*, {}* (i8*, i64)** %34
  %36 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %27, i32 0, i32 1
  %37 = load i8*, i8** %36
  %38 = call {}* %35(i8* %37, i64 0)
  %39 = call {}* @newline1()
  %40 = add i64 %1, 1
  %41 = call {}* @go_y29(i8* %0, i64 %40)
  store {}* %41, {}** %31
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %42 = load {}*, {}** %31
  ret {}* %42
}

define {}* @go_x42(i8*, i64) {
  %3 = bitcast i8* %0 to { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %4 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 0
  %5 = load { i1*, i64 }*, { i1*, i64 }** %4
  %6 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 1
  %7 = load { i1*, i64 }*, { i1*, i64 }** %6
  %8 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 2
  %9 = load { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %8
  %10 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 3
  %11 = load { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }** %10
  %12 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 4
  %13 = load i64, i64* %12
  %14 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %3, i32 0, i32 5
  %15 = load i64, i64* %14
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { {}* (i8*, i64)*, i8* }*
  %18 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %17, i32 0, i32 0
  store {}* (i8*, i64)* @go_x42, {}* (i8*, i64)** %18
  %19 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %17, i32 0, i32 1
  store i8* %0, i8** %19
  %20 = icmp sge i64 %1, %13
  %21 = alloca {}*
  br i1 %20, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  store {}* %23, {}** %21
  br label %end_0

else_0:                                           ; preds = %2
  %24 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 0
  %25 = load i1 (i8*, { i1*, i64 }*, i64, i64)*, i1 (i8*, { i1*, i64 }*, i64, i64)** %24
  %26 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 1
  %27 = load i8*, i8** %26
  %28 = call i1 %25(i8* %27, { i1*, i64 }* %7, i64 %1, i64 %15)
  %29 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %11, i32 0, i32 0
  %30 = load {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %29
  %31 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %11, i32 0, i32 1
  %32 = load i8*, i8** %31
  %33 = call {}* %30(i8* %32, { i1*, i64 }* %5, i64 %1, i64 %15, i1 %28)
  %34 = add i64 %1, 1
  %35 = call {}* @go_x42(i8* %0, i64 %34)
  store {}* %35, {}** %21
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %36 = load {}*, {}** %21
  ret {}* %36
}

define {}* @go_x31(i8*, i64) {
  %3 = bitcast i8* %0 to { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }*
  %4 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 0
  %5 = load { i1*, i64 }*, { i1*, i64 }** %4
  %6 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 1
  %7 = load { i8 (i8*, i1)*, i8* }*, { i8 (i8*, i1)*, i8* }** %6
  %8 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 2
  %9 = load { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %8
  %10 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 3
  %11 = load i64, i64* %10
  %12 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 4
  %13 = load i64, i64* %12
  %14 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %15 = bitcast i8* %14 to { {}* (i8*, i64)*, i8* }*
  %16 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %15, i32 0, i32 0
  store {}* (i8*, i64)* @go_x31, {}* (i8*, i64)** %16
  %17 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %15, i32 0, i32 1
  store i8* %0, i8** %17
  %18 = icmp sge i64 %1, %11
  %19 = alloca {}*
  br i1 %18, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %20 = call i8* @GC_malloc(i64 0)
  %21 = bitcast i8* %20 to {}*
  store {}* %21, {}** %19
  br label %end_0

else_0:                                           ; preds = %2
  %22 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 0
  %23 = load i1 (i8*, { i1*, i64 }*, i64, i64)*, i1 (i8*, { i1*, i64 }*, i64, i64)** %22
  %24 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 1
  %25 = load i8*, i8** %24
  %26 = call i1 %23(i8* %25, { i1*, i64 }* %5, i64 %1, i64 %13)
  %27 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %7, i32 0, i32 0
  %28 = load i8 (i8*, i1)*, i8 (i8*, i1)** %27
  %29 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %7, i32 0, i32 1
  %30 = load i8*, i8** %29
  %31 = call i8 %28(i8* %30, i1 %26)
  %32 = call {}* @print_char0(i8 %31)
  %33 = add i64 %1, 1
  %34 = call {}* @go_x31(i8* %0, i64 %33)
  store {}* %34, {}** %19
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %35 = load {}*, {}** %19
  ret {}* %35
}

declare {}* @gen_seed()

define {}* @gen_seed2() {
  %1 = call {}* @gen_seed()
  ret {}* %1
}

declare { i1*, i64 }* @copy_bool_array({ i1*, i64 }*)

define { i1*, i64 }* @copy_bool_array5({ i1*, i64 }*) {
  %2 = call { i1*, i64 }* @copy_bool_array({ i1*, i64 }* %0)
  ret { i1*, i64 }* %2
}

define i32 @main() {
  %1 = mul i64 50, 20
  %2 = mul i64 ptrtoint (i1* getelementptr inbounds (i1, i1* null, i32 1) to i64), %1
  %3 = call i8* @GC_malloc(i64 %2)
  %4 = bitcast i8* %3 to i1*
  %5 = call i8* @GC_malloc(i64 ptrtoint ({ i1*, i64 }* getelementptr inbounds ({ i1*, i64 }, { i1*, i64 }* null, i32 1) to i64))
  %6 = bitcast i8* %5 to { i1*, i64 }*
  %7 = getelementptr { i1*, i64 }, { i1*, i64 }* %6, i32 0, i32 0
  store i1* %4, i1** %7
  %8 = getelementptr { i1*, i64 }, { i1*, i64 }* %6, i32 0, i32 1
  store i64 %1, i64* %8
  %9 = alloca i64
  store i64 0, i64* %9
  br label %cond_0

cond_0:                                           ; preds = %body_0, %0
  %10 = load i64, i64* %9
  %11 = icmp slt i64 %10, %1
  br i1 %11, label %body_0, label %end_0

body_0:                                           ; preds = %cond_0
  %12 = getelementptr { i1*, i64 }, { i1*, i64 }* %6, i32 0, i32 0
  %13 = load i1*, i1** %12
  %14 = getelementptr i1, i1* %13, i64 %10
  store i1 false, i1* %14
  %15 = add i64 %10, 1
  store i64 %15, i64* %9
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { i64, i64 }*
  %18 = getelementptr { i64, i64 }, { i64, i64 }* %17, i32 0, i32 0
  store i64 50, i64* %18
  %19 = getelementptr { i64, i64 }, { i64, i64 }* %17, i32 0, i32 1
  store i64 20, i64* %19
  %20 = bitcast { i64, i64 }* %17 to i8*
  %21 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %22 = bitcast i8* %21 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %23 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %22, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %23
  %24 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %22, i32 0, i32 1
  store i8* %20, i8** %24
  %25 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %26 = bitcast i8* %25 to { i64, i64 }*
  %27 = getelementptr { i64, i64 }, { i64, i64 }* %26, i32 0, i32 0
  store i64 50, i64* %27
  %28 = getelementptr { i64, i64 }, { i64, i64 }* %26, i32 0, i32 1
  store i64 20, i64* %28
  %29 = bitcast { i64, i64 }* %26 to i8*
  %30 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %31 = bitcast i8* %30 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*
  %32 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %31, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %32
  %33 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %31, i32 0, i32 1
  store i8* %29, i8** %33
  %34 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %35 = bitcast i8* %34 to { i64, i64 }*
  %36 = getelementptr { i64, i64 }, { i64, i64 }* %35, i32 0, i32 0
  store i64 50, i64* %36
  %37 = getelementptr { i64, i64 }, { i64, i64 }* %35, i32 0, i32 1
  store i64 20, i64* %37
  %38 = bitcast { i64, i64 }* %35 to i8*
  %39 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %40 = bitcast i8* %39 to { {}* (i8*, { i1*, i64 }*, i64)*, i8* }*
  %41 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %40, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %41
  %42 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %40, i32 0, i32 1
  store i8* %38, i8** %42
  %43 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %44 = bitcast i8* %43 to { i64, i64 }*
  %45 = getelementptr { i64, i64 }, { i64, i64 }* %44, i32 0, i32 0
  store i64 50, i64* %45
  %46 = getelementptr { i64, i64 }, { i64, i64 }* %44, i32 0, i32 1
  store i64 20, i64* %46
  %47 = bitcast { i64, i64 }* %44 to i8*
  %48 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %49 = bitcast i8* %48 to { i8 (i8*, i1)*, i8* }*
  %50 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %49, i32 0, i32 0
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %50
  %51 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %49, i32 0, i32 1
  store i8* %47, i8** %51
  %52 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %53 = bitcast i8* %52 to { i64, i64 }*
  %54 = getelementptr { i64, i64 }, { i64, i64 }* %53, i32 0, i32 0
  store i64 50, i64* %54
  %55 = getelementptr { i64, i64 }, { i64, i64 }* %53, i32 0, i32 1
  store i64 20, i64* %55
  %56 = bitcast { i64, i64 }* %53 to i8*
  %57 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %58 = bitcast i8* %57 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %59 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %58, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %59
  %60 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %58, i32 0, i32 1
  store i8* %56, i8** %60
  %61 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %62 = bitcast i8* %61 to { i64, i64 }*
  %63 = getelementptr { i64, i64 }, { i64, i64 }* %62, i32 0, i32 0
  store i64 50, i64* %63
  %64 = getelementptr { i64, i64 }, { i64, i64 }* %62, i32 0, i32 1
  store i64 20, i64* %64
  %65 = bitcast { i64, i64 }* %62 to i8*
  %66 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %67 = bitcast i8* %66 to { i64 (i8*, i1)*, i8* }*
  %68 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %67, i32 0, i32 0
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %68
  %69 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %67, i32 0, i32 1
  store i8* %65, i8** %69
  %70 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %71 = bitcast i8* %70 to { i64, i64 }*
  %72 = getelementptr { i64, i64 }, { i64, i64 }* %71, i32 0, i32 0
  store i64 50, i64* %72
  %73 = getelementptr { i64, i64 }, { i64, i64 }* %71, i32 0, i32 1
  store i64 20, i64* %73
  %74 = bitcast { i64, i64 }* %71 to i8*
  %75 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %76 = bitcast i8* %75 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %77 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %76, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %77
  %78 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %76, i32 0, i32 1
  store i8* %74, i8** %78
  %79 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %80 = bitcast i8* %79 to { i64, i64 }*
  %81 = getelementptr { i64, i64 }, { i64, i64 }* %80, i32 0, i32 0
  store i64 50, i64* %81
  %82 = getelementptr { i64, i64 }, { i64, i64 }* %80, i32 0, i32 1
  store i64 20, i64* %82
  %83 = bitcast { i64, i64 }* %80 to i8*
  %84 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %85 = bitcast i8* %84 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %86 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %85, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %86
  %87 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %85, i32 0, i32 1
  store i8* %83, i8** %87
  %88 = call {}* @gen_seed2()
  %89 = call {}* @pulsar6({ i1*, i64 }* %6)
  %90 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 3))
  %91 = bitcast i8* %90 to { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }*
  %92 = getelementptr { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }, { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }* %91, i32 0, i32 0
  store { i1*, i64 }* %6, { i1*, i64 }** %92
  %93 = getelementptr { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }, { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }* %91, i32 0, i32 1
  store { {}* (i8*, { i1*, i64 }*)*, i8* }* %58, { {}* (i8*, { i1*, i64 }*)*, i8* }** %93
  %94 = getelementptr { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }, { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }* %91, i32 0, i32 2
  store { {}* (i8*, { i1*, i64 }*)*, i8* }* %85, { {}* (i8*, { i1*, i64 }*)*, i8* }** %94
  %95 = bitcast { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }* %91 to i8*
  %96 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %97 = bitcast i8* %96 to { {}* (i8*, i64)*, i8* }*
  %98 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %97, i32 0, i32 0
  store {}* (i8*, i64)* @loop44, {}* (i8*, i64)** %98
  %99 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %97, i32 0, i32 1
  store i8* %95, i8** %99
  %100 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %97, i32 0, i32 0
  %101 = load {}* (i8*, i64)*, {}* (i8*, i64)** %100
  %102 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %97, i32 0, i32 1
  %103 = load i8*, i8** %102
  %104 = call {}* %101(i8* %103, i64 10)
  ret i32 0
}
