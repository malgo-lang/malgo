; ModuleID = './examples/life.mlg'
source_filename = "./examples/life.mlg"

declare {}* @print_char(i8)

define {}* @print_char373(i8) {
  %2 = call {}* @print_char(i8 %0)
  ret {}* %2
}

declare {}* @newline()

define {}* @newline372() {
  %1 = call {}* @newline()
  ret {}* %1
}

declare {}* @gen_seed()

define {}* @gen_seed371() {
  %1 = call {}* @gen_seed()
  ret {}* %1
}

declare i1 @rand_bool()

define i1 @rand_bool370() {
  %1 = call i1 @rand_bool()
  ret i1 %1
}

declare {}* @malgo_sleep(i64)

define {}* @sleep369(i64) {
  %2 = call {}* @malgo_sleep(i64 %0)
  ret {}* %2
}

declare { i1*, i64 }* @copy_bool_array({ i1*, i64 }*)

define { i1*, i64 }* @copy_bool_array368({ i1*, i64 }*) {
  %2 = call { i1*, i64 }* @copy_bool_array({ i1*, i64 }* %0)
  ret { i1*, i64 }* %2
}

declare {}* @pulsar({ i1*, i64 }*)

define {}* @pulsar367({ i1*, i64 }*) {
  %2 = call {}* @pulsar({ i1*, i64 }* %0)
  ret {}* %2
}

declare i8* @GC_malloc(i64)

define i1 @view366(i8*, { i1*, i64 }*, i64, i64) {
  %5 = bitcast i8* %0 to { i64, i64 }*
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %5, i32 0, i32 0
  %7 = load i64, i64* %6
  %8 = getelementptr { i64, i64 }, { i64, i64 }* %5, i32 0, i32 1
  %9 = load i64, i64* %8
  %10 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %11 = bitcast i8* %10 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %12 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %11, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view366, i1 (i8*, { i1*, i64 }*, i64, i64)** %12
  %13 = call i8* @GC_malloc(i64 0)
  %14 = bitcast i8* %13 to {}*
  %15 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %0, i8** %15
  %16 = call i8* @GC_malloc(i64 0)
  %17 = bitcast i8* %16 to {}*
  %18 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %19 = bitcast i8* %18 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*
  %20 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %19, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set365, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %20
  %21 = call i8* @GC_malloc(i64 0)
  %22 = bitcast i8* %21 to {}*
  %23 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %19, i32 0, i32 1
  store i8* %0, i8** %23
  %24 = call i8* @GC_malloc(i64 0)
  %25 = bitcast i8* %24 to {}*
  %26 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %27 = bitcast i8* %26 to { {}* (i8*, { i1*, i64 }*, i64)*, i8* }*
  %28 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %27, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells364, {}* (i8*, { i1*, i64 }*, i64)** %28
  %29 = call i8* @GC_malloc(i64 0)
  %30 = bitcast i8* %29 to {}*
  %31 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %27, i32 0, i32 1
  store i8* %0, i8** %31
  %32 = call i8* @GC_malloc(i64 0)
  %33 = bitcast i8* %32 to {}*
  %34 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %35 = bitcast i8* %34 to { i8 (i8*, i1)*, i8* }*
  %36 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %35, i32 0, i32 0
  store i8 (i8*, i1)* @to_char363, i8 (i8*, i1)** %36
  %37 = call i8* @GC_malloc(i64 0)
  %38 = bitcast i8* %37 to {}*
  %39 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %35, i32 0, i32 1
  store i8* %0, i8** %39
  %40 = call i8* @GC_malloc(i64 0)
  %41 = bitcast i8* %40 to {}*
  %42 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %43 = bitcast i8* %42 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %44 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %43, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @print_cells360, {}* (i8*, { i1*, i64 }*)** %44
  %45 = call i8* @GC_malloc(i64 0)
  %46 = bitcast i8* %45 to {}*
  %47 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %43, i32 0, i32 1
  store i8* %0, i8** %47
  %48 = call i8* @GC_malloc(i64 0)
  %49 = bitcast i8* %48 to {}*
  %50 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %51 = bitcast i8* %50 to { i64 (i8*, i1)*, i8* }*
  %52 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %51, i32 0, i32 0
  store i64 (i8*, i1)* @to_int359, i64 (i8*, i1)** %52
  %53 = call i8* @GC_malloc(i64 0)
  %54 = bitcast i8* %53 to {}*
  %55 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %51, i32 0, i32 1
  store i8* %0, i8** %55
  %56 = call i8* @GC_malloc(i64 0)
  %57 = bitcast i8* %56 to {}*
  %58 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %59 = bitcast i8* %58 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %60 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %59, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state358, i1 (i8*, { i1*, i64 }*, i64, i64)** %60
  %61 = call i8* @GC_malloc(i64 0)
  %62 = bitcast i8* %61 to {}*
  %63 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %59, i32 0, i32 1
  store i8* %0, i8** %63
  %64 = call i8* @GC_malloc(i64 0)
  %65 = bitcast i8* %64 to {}*
  %66 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %67 = bitcast i8* %66 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %68 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %67, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @update_cells355, {}* (i8*, { i1*, i64 }*)** %68
  %69 = call i8* @GC_malloc(i64 0)
  %70 = bitcast i8* %69 to {}*
  %71 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %67, i32 0, i32 1
  store i8* %0, i8** %71
  %72 = call i8* @GC_malloc(i64 0)
  %73 = bitcast i8* %72 to {}*
  %74 = icmp sge i64 %2, %7
  %75 = icmp sge i64 %3, %9
  %76 = or i1 %74, %75
  %77 = icmp slt i64 %2, 0
  %78 = or i1 %76, %77
  %79 = icmp slt i64 %3, 0
  %80 = or i1 %78, %79
  %81 = alloca i1
  br i1 %80, label %then_0, label %else_0

then_0:                                           ; preds = %4
  store i1 false, i1* %81
  br label %end_0

else_0:                                           ; preds = %4
  %82 = mul i64 %3, %7
  %83 = add i64 %82, %2
  %84 = getelementptr { i1*, i64 }, { i1*, i64 }* %1, i32 0, i32 0
  %85 = load i1*, i1** %84
  %86 = getelementptr i1, i1* %85, i64 %83
  %87 = load i1, i1* %86
  store i1 %87, i1* %81
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %88 = load i1, i1* %81
  ret i1 %88
}

define {}* @set365(i8*, { i1*, i64 }*, i64, i64, i1) {
  %6 = bitcast i8* %0 to { i64, i64 }*
  %7 = getelementptr { i64, i64 }, { i64, i64 }* %6, i32 0, i32 0
  %8 = load i64, i64* %7
  %9 = getelementptr { i64, i64 }, { i64, i64 }* %6, i32 0, i32 1
  %10 = load i64, i64* %9
  %11 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %12 = bitcast i8* %11 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %13 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %12, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view366, i1 (i8*, { i1*, i64 }*, i64, i64)** %13
  %14 = call i8* @GC_malloc(i64 0)
  %15 = bitcast i8* %14 to {}*
  %16 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %12, i32 0, i32 1
  store i8* %0, i8** %16
  %17 = call i8* @GC_malloc(i64 0)
  %18 = bitcast i8* %17 to {}*
  %19 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %20 = bitcast i8* %19 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*
  %21 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %20, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set365, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %21
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  %24 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %20, i32 0, i32 1
  store i8* %0, i8** %24
  %25 = call i8* @GC_malloc(i64 0)
  %26 = bitcast i8* %25 to {}*
  %27 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %28 = bitcast i8* %27 to { {}* (i8*, { i1*, i64 }*, i64)*, i8* }*
  %29 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %28, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells364, {}* (i8*, { i1*, i64 }*, i64)** %29
  %30 = call i8* @GC_malloc(i64 0)
  %31 = bitcast i8* %30 to {}*
  %32 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %28, i32 0, i32 1
  store i8* %0, i8** %32
  %33 = call i8* @GC_malloc(i64 0)
  %34 = bitcast i8* %33 to {}*
  %35 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %36 = bitcast i8* %35 to { i8 (i8*, i1)*, i8* }*
  %37 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %36, i32 0, i32 0
  store i8 (i8*, i1)* @to_char363, i8 (i8*, i1)** %37
  %38 = call i8* @GC_malloc(i64 0)
  %39 = bitcast i8* %38 to {}*
  %40 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %36, i32 0, i32 1
  store i8* %0, i8** %40
  %41 = call i8* @GC_malloc(i64 0)
  %42 = bitcast i8* %41 to {}*
  %43 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %44 = bitcast i8* %43 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %45 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %44, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @print_cells360, {}* (i8*, { i1*, i64 }*)** %45
  %46 = call i8* @GC_malloc(i64 0)
  %47 = bitcast i8* %46 to {}*
  %48 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %44, i32 0, i32 1
  store i8* %0, i8** %48
  %49 = call i8* @GC_malloc(i64 0)
  %50 = bitcast i8* %49 to {}*
  %51 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %52 = bitcast i8* %51 to { i64 (i8*, i1)*, i8* }*
  %53 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %52, i32 0, i32 0
  store i64 (i8*, i1)* @to_int359, i64 (i8*, i1)** %53
  %54 = call i8* @GC_malloc(i64 0)
  %55 = bitcast i8* %54 to {}*
  %56 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %52, i32 0, i32 1
  store i8* %0, i8** %56
  %57 = call i8* @GC_malloc(i64 0)
  %58 = bitcast i8* %57 to {}*
  %59 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %60 = bitcast i8* %59 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %61 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %60, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state358, i1 (i8*, { i1*, i64 }*, i64, i64)** %61
  %62 = call i8* @GC_malloc(i64 0)
  %63 = bitcast i8* %62 to {}*
  %64 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %60, i32 0, i32 1
  store i8* %0, i8** %64
  %65 = call i8* @GC_malloc(i64 0)
  %66 = bitcast i8* %65 to {}*
  %67 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %68 = bitcast i8* %67 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %69 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %68, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @update_cells355, {}* (i8*, { i1*, i64 }*)** %69
  %70 = call i8* @GC_malloc(i64 0)
  %71 = bitcast i8* %70 to {}*
  %72 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %68, i32 0, i32 1
  store i8* %0, i8** %72
  %73 = call i8* @GC_malloc(i64 0)
  %74 = bitcast i8* %73 to {}*
  %75 = mul i64 %3, %8
  %76 = add i64 %75, %2
  %77 = getelementptr { i1*, i64 }, { i1*, i64 }* %1, i32 0, i32 0
  %78 = load i1*, i1** %77
  %79 = getelementptr i1, i1* %78, i64 %76
  store i1 %4, i1* %79
  %80 = call i8* @GC_malloc(i64 0)
  %81 = bitcast i8* %80 to {}*
  ret {}* %81
}

define {}* @init_cells364(i8*, { i1*, i64 }*, i64) {
  %4 = bitcast i8* %0 to { i64, i64 }*
  %5 = getelementptr { i64, i64 }, { i64, i64 }* %4, i32 0, i32 0
  %6 = load i64, i64* %5
  %7 = getelementptr { i64, i64 }, { i64, i64 }* %4, i32 0, i32 1
  %8 = load i64, i64* %7
  %9 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %10 = bitcast i8* %9 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %11 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %10, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view366, i1 (i8*, { i1*, i64 }*, i64, i64)** %11
  %12 = call i8* @GC_malloc(i64 0)
  %13 = bitcast i8* %12 to {}*
  %14 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %10, i32 0, i32 1
  store i8* %0, i8** %14
  %15 = call i8* @GC_malloc(i64 0)
  %16 = bitcast i8* %15 to {}*
  %17 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %18 = bitcast i8* %17 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*
  %19 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %18, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set365, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %19
  %20 = call i8* @GC_malloc(i64 0)
  %21 = bitcast i8* %20 to {}*
  %22 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %18, i32 0, i32 1
  store i8* %0, i8** %22
  %23 = call i8* @GC_malloc(i64 0)
  %24 = bitcast i8* %23 to {}*
  %25 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %26 = bitcast i8* %25 to { {}* (i8*, { i1*, i64 }*, i64)*, i8* }*
  %27 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %26, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells364, {}* (i8*, { i1*, i64 }*, i64)** %27
  %28 = call i8* @GC_malloc(i64 0)
  %29 = bitcast i8* %28 to {}*
  %30 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %26, i32 0, i32 1
  store i8* %0, i8** %30
  %31 = call i8* @GC_malloc(i64 0)
  %32 = bitcast i8* %31 to {}*
  %33 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %34 = bitcast i8* %33 to { i8 (i8*, i1)*, i8* }*
  %35 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %34, i32 0, i32 0
  store i8 (i8*, i1)* @to_char363, i8 (i8*, i1)** %35
  %36 = call i8* @GC_malloc(i64 0)
  %37 = bitcast i8* %36 to {}*
  %38 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %34, i32 0, i32 1
  store i8* %0, i8** %38
  %39 = call i8* @GC_malloc(i64 0)
  %40 = bitcast i8* %39 to {}*
  %41 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %42 = bitcast i8* %41 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %43 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %42, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @print_cells360, {}* (i8*, { i1*, i64 }*)** %43
  %44 = call i8* @GC_malloc(i64 0)
  %45 = bitcast i8* %44 to {}*
  %46 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %42, i32 0, i32 1
  store i8* %0, i8** %46
  %47 = call i8* @GC_malloc(i64 0)
  %48 = bitcast i8* %47 to {}*
  %49 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %50 = bitcast i8* %49 to { i64 (i8*, i1)*, i8* }*
  %51 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %50, i32 0, i32 0
  store i64 (i8*, i1)* @to_int359, i64 (i8*, i1)** %51
  %52 = call i8* @GC_malloc(i64 0)
  %53 = bitcast i8* %52 to {}*
  %54 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %50, i32 0, i32 1
  store i8* %0, i8** %54
  %55 = call i8* @GC_malloc(i64 0)
  %56 = bitcast i8* %55 to {}*
  %57 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %58 = bitcast i8* %57 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %59 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %58, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state358, i1 (i8*, { i1*, i64 }*, i64, i64)** %59
  %60 = call i8* @GC_malloc(i64 0)
  %61 = bitcast i8* %60 to {}*
  %62 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %58, i32 0, i32 1
  store i8* %0, i8** %62
  %63 = call i8* @GC_malloc(i64 0)
  %64 = bitcast i8* %63 to {}*
  %65 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %66 = bitcast i8* %65 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %67 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %66, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @update_cells355, {}* (i8*, { i1*, i64 }*)** %67
  %68 = call i8* @GC_malloc(i64 0)
  %69 = bitcast i8* %68 to {}*
  %70 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %66, i32 0, i32 1
  store i8* %0, i8** %70
  %71 = call i8* @GC_malloc(i64 0)
  %72 = bitcast i8* %71 to {}*
  %73 = mul i64 %6, %8
  %74 = icmp sge i64 %2, %73
  %75 = alloca {}*
  br i1 %74, label %then_0, label %else_0

then_0:                                           ; preds = %3
  %76 = call i8* @GC_malloc(i64 0)
  %77 = bitcast i8* %76 to {}*
  store {}* %77, {}** %75
  br label %end_0

else_0:                                           ; preds = %3
  %78 = call i1 @rand_bool370()
  %79 = getelementptr { i1*, i64 }, { i1*, i64 }* %1, i32 0, i32 0
  %80 = load i1*, i1** %79
  %81 = getelementptr i1, i1* %80, i64 %2
  store i1 %78, i1* %81
  %82 = call i8* @GC_malloc(i64 0)
  %83 = bitcast i8* %82 to {}*
  %84 = add i64 %2, 1
  %85 = call {}* @init_cells364(i8* %0, { i1*, i64 }* %1, i64 %84)
  store {}* %85, {}** %75
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %86 = load {}*, {}** %75
  ret {}* %86
}

define i8 @to_char363(i8*, i1) {
  %3 = bitcast i8* %0 to { i64, i64 }*
  %4 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 1
  %7 = load i64, i64* %6
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %10 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view366, i1 (i8*, { i1*, i64 }*, i64, i64)** %10
  %11 = call i8* @GC_malloc(i64 0)
  %12 = bitcast i8* %11 to {}*
  %13 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %13
  %14 = call i8* @GC_malloc(i64 0)
  %15 = bitcast i8* %14 to {}*
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*
  %18 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %17, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set365, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %18
  %19 = call i8* @GC_malloc(i64 0)
  %20 = bitcast i8* %19 to {}*
  %21 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %17, i32 0, i32 1
  store i8* %0, i8** %21
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  %24 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %25 = bitcast i8* %24 to { {}* (i8*, { i1*, i64 }*, i64)*, i8* }*
  %26 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %25, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells364, {}* (i8*, { i1*, i64 }*, i64)** %26
  %27 = call i8* @GC_malloc(i64 0)
  %28 = bitcast i8* %27 to {}*
  %29 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %25, i32 0, i32 1
  store i8* %0, i8** %29
  %30 = call i8* @GC_malloc(i64 0)
  %31 = bitcast i8* %30 to {}*
  %32 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %33 = bitcast i8* %32 to { i8 (i8*, i1)*, i8* }*
  %34 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %33, i32 0, i32 0
  store i8 (i8*, i1)* @to_char363, i8 (i8*, i1)** %34
  %35 = call i8* @GC_malloc(i64 0)
  %36 = bitcast i8* %35 to {}*
  %37 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %33, i32 0, i32 1
  store i8* %0, i8** %37
  %38 = call i8* @GC_malloc(i64 0)
  %39 = bitcast i8* %38 to {}*
  %40 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %41 = bitcast i8* %40 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %42 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %41, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @print_cells360, {}* (i8*, { i1*, i64 }*)** %42
  %43 = call i8* @GC_malloc(i64 0)
  %44 = bitcast i8* %43 to {}*
  %45 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %41, i32 0, i32 1
  store i8* %0, i8** %45
  %46 = call i8* @GC_malloc(i64 0)
  %47 = bitcast i8* %46 to {}*
  %48 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %49 = bitcast i8* %48 to { i64 (i8*, i1)*, i8* }*
  %50 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %49, i32 0, i32 0
  store i64 (i8*, i1)* @to_int359, i64 (i8*, i1)** %50
  %51 = call i8* @GC_malloc(i64 0)
  %52 = bitcast i8* %51 to {}*
  %53 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %49, i32 0, i32 1
  store i8* %0, i8** %53
  %54 = call i8* @GC_malloc(i64 0)
  %55 = bitcast i8* %54 to {}*
  %56 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %57 = bitcast i8* %56 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %58 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %57, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state358, i1 (i8*, { i1*, i64 }*, i64, i64)** %58
  %59 = call i8* @GC_malloc(i64 0)
  %60 = bitcast i8* %59 to {}*
  %61 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %57, i32 0, i32 1
  store i8* %0, i8** %61
  %62 = call i8* @GC_malloc(i64 0)
  %63 = bitcast i8* %62 to {}*
  %64 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %65 = bitcast i8* %64 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %66 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %65, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @update_cells355, {}* (i8*, { i1*, i64 }*)** %66
  %67 = call i8* @GC_malloc(i64 0)
  %68 = bitcast i8* %67 to {}*
  %69 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %65, i32 0, i32 1
  store i8* %0, i8** %69
  %70 = call i8* @GC_malloc(i64 0)
  %71 = bitcast i8* %70 to {}*
  %72 = alloca i8
  br i1 %1, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store i8 35, i8* %72
  br label %end_0

else_0:                                           ; preds = %2
  store i8 95, i8* %72
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %73 = load i8, i8* %72
  ret i8 %73
}

define {}* @go_x362(i8*, i64) {
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
  store {}* (i8*, i64)* @go_x362, {}* (i8*, i64)** %16
  %17 = call i8* @GC_malloc(i64 0)
  %18 = bitcast i8* %17 to {}*
  %19 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %15, i32 0, i32 1
  store i8* %0, i8** %19
  %20 = call i8* @GC_malloc(i64 0)
  %21 = bitcast i8* %20 to {}*
  %22 = icmp sge i64 %1, %11
  %23 = alloca {}*
  br i1 %22, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %24 = call i8* @GC_malloc(i64 0)
  %25 = bitcast i8* %24 to {}*
  store {}* %25, {}** %23
  br label %end_0

else_0:                                           ; preds = %2
  %26 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 0
  %27 = load i1 (i8*, { i1*, i64 }*, i64, i64)*, i1 (i8*, { i1*, i64 }*, i64, i64)** %26
  %28 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 1
  %29 = load i8*, i8** %28
  %30 = call i1 %27(i8* %29, { i1*, i64 }* %5, i64 %1, i64 %13)
  %31 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %7, i32 0, i32 0
  %32 = load i8 (i8*, i1)*, i8 (i8*, i1)** %31
  %33 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %7, i32 0, i32 1
  %34 = load i8*, i8** %33
  %35 = call i8 %32(i8* %34, i1 %30)
  %36 = call {}* @print_char373(i8 %35)
  %37 = add i64 %1, 1
  %38 = call {}* @go_x362(i8* %0, i64 %37)
  store {}* %38, {}** %23
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %39 = load {}*, {}** %23
  ret {}* %39
}

define {}* @go_y361(i8*, i64) {
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
  store {}* (i8*, i64)* @go_y361, {}* (i8*, i64)** %16
  %17 = call i8* @GC_malloc(i64 0)
  %18 = bitcast i8* %17 to {}*
  %19 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %15, i32 0, i32 1
  store i8* %0, i8** %19
  %20 = call i8* @GC_malloc(i64 0)
  %21 = bitcast i8* %20 to {}*
  %22 = call i8* @GC_malloc(i64 ptrtoint ({ { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* getelementptr inbounds ({ { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %23 = bitcast i8* %22 to { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }*
  %24 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %23, i32 0, i32 0
  store { i1*, i64 }* %5, { i1*, i64 }** %24
  %25 = call i8* @GC_malloc(i64 0)
  %26 = bitcast i8* %25 to {}*
  %27 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %23, i32 0, i32 1
  store { i8 (i8*, i1)*, i8* }* %7, { i8 (i8*, i1)*, i8* }** %27
  %28 = call i8* @GC_malloc(i64 0)
  %29 = bitcast i8* %28 to {}*
  %30 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %23, i32 0, i32 2
  store { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %30
  %31 = call i8* @GC_malloc(i64 0)
  %32 = bitcast i8* %31 to {}*
  %33 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %23, i32 0, i32 3
  store i64 %11, i64* %33
  %34 = call i8* @GC_malloc(i64 0)
  %35 = bitcast i8* %34 to {}*
  %36 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %23, i32 0, i32 4
  store i64 %1, i64* %36
  %37 = call i8* @GC_malloc(i64 0)
  %38 = bitcast i8* %37 to {}*
  %39 = bitcast { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %23 to i8*
  %40 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %41 = bitcast i8* %40 to { {}* (i8*, i64)*, i8* }*
  %42 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %41, i32 0, i32 0
  store {}* (i8*, i64)* @go_x362, {}* (i8*, i64)** %42
  %43 = call i8* @GC_malloc(i64 0)
  %44 = bitcast i8* %43 to {}*
  %45 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %41, i32 0, i32 1
  store i8* %39, i8** %45
  %46 = call i8* @GC_malloc(i64 0)
  %47 = bitcast i8* %46 to {}*
  %48 = icmp sge i64 %1, %13
  %49 = alloca {}*
  br i1 %48, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %50 = call i8* @GC_malloc(i64 0)
  %51 = bitcast i8* %50 to {}*
  store {}* %51, {}** %49
  br label %end_0

else_0:                                           ; preds = %2
  %52 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %41, i32 0, i32 0
  %53 = load {}* (i8*, i64)*, {}* (i8*, i64)** %52
  %54 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %41, i32 0, i32 1
  %55 = load i8*, i8** %54
  %56 = call {}* %53(i8* %55, i64 0)
  %57 = call {}* @newline372()
  %58 = add i64 %1, 1
  %59 = call {}* @go_y361(i8* %0, i64 %58)
  store {}* %59, {}** %49
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %60 = load {}*, {}** %49
  ret {}* %60
}

define {}* @print_cells360(i8*, { i1*, i64 }*) {
  %3 = bitcast i8* %0 to { i64, i64 }*
  %4 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 1
  %7 = load i64, i64* %6
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %10 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view366, i1 (i8*, { i1*, i64 }*, i64, i64)** %10
  %11 = call i8* @GC_malloc(i64 0)
  %12 = bitcast i8* %11 to {}*
  %13 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %13
  %14 = call i8* @GC_malloc(i64 0)
  %15 = bitcast i8* %14 to {}*
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*
  %18 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %17, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set365, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %18
  %19 = call i8* @GC_malloc(i64 0)
  %20 = bitcast i8* %19 to {}*
  %21 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %17, i32 0, i32 1
  store i8* %0, i8** %21
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  %24 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %25 = bitcast i8* %24 to { {}* (i8*, { i1*, i64 }*, i64)*, i8* }*
  %26 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %25, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells364, {}* (i8*, { i1*, i64 }*, i64)** %26
  %27 = call i8* @GC_malloc(i64 0)
  %28 = bitcast i8* %27 to {}*
  %29 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %25, i32 0, i32 1
  store i8* %0, i8** %29
  %30 = call i8* @GC_malloc(i64 0)
  %31 = bitcast i8* %30 to {}*
  %32 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %33 = bitcast i8* %32 to { i8 (i8*, i1)*, i8* }*
  %34 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %33, i32 0, i32 0
  store i8 (i8*, i1)* @to_char363, i8 (i8*, i1)** %34
  %35 = call i8* @GC_malloc(i64 0)
  %36 = bitcast i8* %35 to {}*
  %37 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %33, i32 0, i32 1
  store i8* %0, i8** %37
  %38 = call i8* @GC_malloc(i64 0)
  %39 = bitcast i8* %38 to {}*
  %40 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %41 = bitcast i8* %40 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %42 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %41, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @print_cells360, {}* (i8*, { i1*, i64 }*)** %42
  %43 = call i8* @GC_malloc(i64 0)
  %44 = bitcast i8* %43 to {}*
  %45 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %41, i32 0, i32 1
  store i8* %0, i8** %45
  %46 = call i8* @GC_malloc(i64 0)
  %47 = bitcast i8* %46 to {}*
  %48 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %49 = bitcast i8* %48 to { i64 (i8*, i1)*, i8* }*
  %50 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %49, i32 0, i32 0
  store i64 (i8*, i1)* @to_int359, i64 (i8*, i1)** %50
  %51 = call i8* @GC_malloc(i64 0)
  %52 = bitcast i8* %51 to {}*
  %53 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %49, i32 0, i32 1
  store i8* %0, i8** %53
  %54 = call i8* @GC_malloc(i64 0)
  %55 = bitcast i8* %54 to {}*
  %56 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %57 = bitcast i8* %56 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %58 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %57, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state358, i1 (i8*, { i1*, i64 }*, i64, i64)** %58
  %59 = call i8* @GC_malloc(i64 0)
  %60 = bitcast i8* %59 to {}*
  %61 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %57, i32 0, i32 1
  store i8* %0, i8** %61
  %62 = call i8* @GC_malloc(i64 0)
  %63 = bitcast i8* %62 to {}*
  %64 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %65 = bitcast i8* %64 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %66 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %65, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @update_cells355, {}* (i8*, { i1*, i64 }*)** %66
  %67 = call i8* @GC_malloc(i64 0)
  %68 = bitcast i8* %67 to {}*
  %69 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %65, i32 0, i32 1
  store i8* %0, i8** %69
  %70 = call i8* @GC_malloc(i64 0)
  %71 = bitcast i8* %70 to {}*
  %72 = call i8* @GC_malloc(i64 ptrtoint ({ { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* getelementptr inbounds ({ { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %73 = bitcast i8* %72 to { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }*
  %74 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %73, i32 0, i32 0
  store { i1*, i64 }* %1, { i1*, i64 }** %74
  %75 = call i8* @GC_malloc(i64 0)
  %76 = bitcast i8* %75 to {}*
  %77 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %73, i32 0, i32 1
  store { i8 (i8*, i1)*, i8* }* %33, { i8 (i8*, i1)*, i8* }** %77
  %78 = call i8* @GC_malloc(i64 0)
  %79 = bitcast i8* %78 to {}*
  %80 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %73, i32 0, i32 2
  store { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %80
  %81 = call i8* @GC_malloc(i64 0)
  %82 = bitcast i8* %81 to {}*
  %83 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %73, i32 0, i32 3
  store i64 %5, i64* %83
  %84 = call i8* @GC_malloc(i64 0)
  %85 = bitcast i8* %84 to {}*
  %86 = getelementptr { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %73, i32 0, i32 4
  store i64 %7, i64* %86
  %87 = call i8* @GC_malloc(i64 0)
  %88 = bitcast i8* %87 to {}*
  %89 = bitcast { { i1*, i64 }*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, i64, i64 }* %73 to i8*
  %90 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %91 = bitcast i8* %90 to { {}* (i8*, i64)*, i8* }*
  %92 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %91, i32 0, i32 0
  store {}* (i8*, i64)* @go_y361, {}* (i8*, i64)** %92
  %93 = call i8* @GC_malloc(i64 0)
  %94 = bitcast i8* %93 to {}*
  %95 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %91, i32 0, i32 1
  store i8* %89, i8** %95
  %96 = call i8* @GC_malloc(i64 0)
  %97 = bitcast i8* %96 to {}*
  %98 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %91, i32 0, i32 0
  %99 = load {}* (i8*, i64)*, {}* (i8*, i64)** %98
  %100 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %91, i32 0, i32 1
  %101 = load i8*, i8** %100
  %102 = call {}* %99(i8* %101, i64 0)
  ret {}* %102
}

define i64 @to_int359(i8*, i1) {
  %3 = bitcast i8* %0 to { i64, i64 }*
  %4 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 1
  %7 = load i64, i64* %6
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %10 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view366, i1 (i8*, { i1*, i64 }*, i64, i64)** %10
  %11 = call i8* @GC_malloc(i64 0)
  %12 = bitcast i8* %11 to {}*
  %13 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %13
  %14 = call i8* @GC_malloc(i64 0)
  %15 = bitcast i8* %14 to {}*
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*
  %18 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %17, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set365, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %18
  %19 = call i8* @GC_malloc(i64 0)
  %20 = bitcast i8* %19 to {}*
  %21 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %17, i32 0, i32 1
  store i8* %0, i8** %21
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  %24 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %25 = bitcast i8* %24 to { {}* (i8*, { i1*, i64 }*, i64)*, i8* }*
  %26 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %25, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells364, {}* (i8*, { i1*, i64 }*, i64)** %26
  %27 = call i8* @GC_malloc(i64 0)
  %28 = bitcast i8* %27 to {}*
  %29 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %25, i32 0, i32 1
  store i8* %0, i8** %29
  %30 = call i8* @GC_malloc(i64 0)
  %31 = bitcast i8* %30 to {}*
  %32 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %33 = bitcast i8* %32 to { i8 (i8*, i1)*, i8* }*
  %34 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %33, i32 0, i32 0
  store i8 (i8*, i1)* @to_char363, i8 (i8*, i1)** %34
  %35 = call i8* @GC_malloc(i64 0)
  %36 = bitcast i8* %35 to {}*
  %37 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %33, i32 0, i32 1
  store i8* %0, i8** %37
  %38 = call i8* @GC_malloc(i64 0)
  %39 = bitcast i8* %38 to {}*
  %40 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %41 = bitcast i8* %40 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %42 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %41, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @print_cells360, {}* (i8*, { i1*, i64 }*)** %42
  %43 = call i8* @GC_malloc(i64 0)
  %44 = bitcast i8* %43 to {}*
  %45 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %41, i32 0, i32 1
  store i8* %0, i8** %45
  %46 = call i8* @GC_malloc(i64 0)
  %47 = bitcast i8* %46 to {}*
  %48 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %49 = bitcast i8* %48 to { i64 (i8*, i1)*, i8* }*
  %50 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %49, i32 0, i32 0
  store i64 (i8*, i1)* @to_int359, i64 (i8*, i1)** %50
  %51 = call i8* @GC_malloc(i64 0)
  %52 = bitcast i8* %51 to {}*
  %53 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %49, i32 0, i32 1
  store i8* %0, i8** %53
  %54 = call i8* @GC_malloc(i64 0)
  %55 = bitcast i8* %54 to {}*
  %56 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %57 = bitcast i8* %56 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %58 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %57, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state358, i1 (i8*, { i1*, i64 }*, i64, i64)** %58
  %59 = call i8* @GC_malloc(i64 0)
  %60 = bitcast i8* %59 to {}*
  %61 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %57, i32 0, i32 1
  store i8* %0, i8** %61
  %62 = call i8* @GC_malloc(i64 0)
  %63 = bitcast i8* %62 to {}*
  %64 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %65 = bitcast i8* %64 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %66 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %65, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @update_cells355, {}* (i8*, { i1*, i64 }*)** %66
  %67 = call i8* @GC_malloc(i64 0)
  %68 = bitcast i8* %67 to {}*
  %69 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %65, i32 0, i32 1
  store i8* %0, i8** %69
  %70 = call i8* @GC_malloc(i64 0)
  %71 = bitcast i8* %70 to {}*
  %72 = alloca i64
  br i1 %1, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store i64 1, i64* %72
  br label %end_0

else_0:                                           ; preds = %2
  store i64 0, i64* %72
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %73 = load i64, i64* %72
  ret i64 %73
}

define i1 @next_state358(i8*, { i1*, i64 }*, i64, i64) {
  %5 = bitcast i8* %0 to { i64, i64 }*
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %5, i32 0, i32 0
  %7 = load i64, i64* %6
  %8 = getelementptr { i64, i64 }, { i64, i64 }* %5, i32 0, i32 1
  %9 = load i64, i64* %8
  %10 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %11 = bitcast i8* %10 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %12 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %11, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view366, i1 (i8*, { i1*, i64 }*, i64, i64)** %12
  %13 = call i8* @GC_malloc(i64 0)
  %14 = bitcast i8* %13 to {}*
  %15 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %0, i8** %15
  %16 = call i8* @GC_malloc(i64 0)
  %17 = bitcast i8* %16 to {}*
  %18 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %19 = bitcast i8* %18 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*
  %20 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %19, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set365, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %20
  %21 = call i8* @GC_malloc(i64 0)
  %22 = bitcast i8* %21 to {}*
  %23 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %19, i32 0, i32 1
  store i8* %0, i8** %23
  %24 = call i8* @GC_malloc(i64 0)
  %25 = bitcast i8* %24 to {}*
  %26 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %27 = bitcast i8* %26 to { {}* (i8*, { i1*, i64 }*, i64)*, i8* }*
  %28 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %27, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells364, {}* (i8*, { i1*, i64 }*, i64)** %28
  %29 = call i8* @GC_malloc(i64 0)
  %30 = bitcast i8* %29 to {}*
  %31 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %27, i32 0, i32 1
  store i8* %0, i8** %31
  %32 = call i8* @GC_malloc(i64 0)
  %33 = bitcast i8* %32 to {}*
  %34 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %35 = bitcast i8* %34 to { i8 (i8*, i1)*, i8* }*
  %36 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %35, i32 0, i32 0
  store i8 (i8*, i1)* @to_char363, i8 (i8*, i1)** %36
  %37 = call i8* @GC_malloc(i64 0)
  %38 = bitcast i8* %37 to {}*
  %39 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %35, i32 0, i32 1
  store i8* %0, i8** %39
  %40 = call i8* @GC_malloc(i64 0)
  %41 = bitcast i8* %40 to {}*
  %42 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %43 = bitcast i8* %42 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %44 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %43, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @print_cells360, {}* (i8*, { i1*, i64 }*)** %44
  %45 = call i8* @GC_malloc(i64 0)
  %46 = bitcast i8* %45 to {}*
  %47 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %43, i32 0, i32 1
  store i8* %0, i8** %47
  %48 = call i8* @GC_malloc(i64 0)
  %49 = bitcast i8* %48 to {}*
  %50 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %51 = bitcast i8* %50 to { i64 (i8*, i1)*, i8* }*
  %52 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %51, i32 0, i32 0
  store i64 (i8*, i1)* @to_int359, i64 (i8*, i1)** %52
  %53 = call i8* @GC_malloc(i64 0)
  %54 = bitcast i8* %53 to {}*
  %55 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %51, i32 0, i32 1
  store i8* %0, i8** %55
  %56 = call i8* @GC_malloc(i64 0)
  %57 = bitcast i8* %56 to {}*
  %58 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %59 = bitcast i8* %58 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %60 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %59, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state358, i1 (i8*, { i1*, i64 }*, i64, i64)** %60
  %61 = call i8* @GC_malloc(i64 0)
  %62 = bitcast i8* %61 to {}*
  %63 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %59, i32 0, i32 1
  store i8* %0, i8** %63
  %64 = call i8* @GC_malloc(i64 0)
  %65 = bitcast i8* %64 to {}*
  %66 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %67 = bitcast i8* %66 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %68 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %67, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @update_cells355, {}* (i8*, { i1*, i64 }*)** %68
  %69 = call i8* @GC_malloc(i64 0)
  %70 = bitcast i8* %69 to {}*
  %71 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %67, i32 0, i32 1
  store i8* %0, i8** %71
  %72 = call i8* @GC_malloc(i64 0)
  %73 = bitcast i8* %72 to {}*
  %74 = mul i64 ptrtoint (i64* getelementptr inbounds (i64, i64* null, i32 1) to i64), 1
  %75 = call i8* @GC_malloc(i64 %74)
  %76 = bitcast i8* %75 to i64*
  %77 = call i8* @GC_malloc(i64 ptrtoint ({ i64*, i64 }* getelementptr inbounds ({ i64*, i64 }, { i64*, i64 }* null, i32 1) to i64))
  %78 = bitcast i8* %77 to { i64*, i64 }*
  %79 = getelementptr { i64*, i64 }, { i64*, i64 }* %78, i32 0, i32 0
  store i64* %76, i64** %79
  %80 = getelementptr { i64*, i64 }, { i64*, i64 }* %78, i32 0, i32 1
  store i64 1, i64* %80
  %81 = alloca i64
  store i64 0, i64* %81
  br label %cond_0

cond_0:                                           ; preds = %body_0, %4
  %82 = load i64, i64* %81
  %83 = icmp slt i64 %82, 1
  br i1 %83, label %body_0, label %end_0

body_0:                                           ; preds = %cond_0
  %84 = getelementptr { i64*, i64 }, { i64*, i64 }* %78, i32 0, i32 0
  %85 = load i64*, i64** %84
  %86 = getelementptr i64, i64* %85, i64 %82
  store i64 0, i64* %86
  %87 = call i8* @GC_malloc(i64 0)
  %88 = bitcast i8* %87 to {}*
  %89 = add i64 %82, 1
  store i64 %89, i64* %81
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %90 = call i8* @GC_malloc(i64 0)
  %91 = bitcast i8* %90 to {}*
  %92 = sub i64 %2, 1
  %93 = add i64 %3, 1
  %94 = call i1 @view366(i8* %0, { i1*, i64 }* %1, i64 %92, i64 %93)
  %95 = call i64 @to_int359(i8* %0, i1 %94)
  %96 = add i64 %3, 1
  %97 = call i1 @view366(i8* %0, { i1*, i64 }* %1, i64 %2, i64 %96)
  %98 = call i64 @to_int359(i8* %0, i1 %97)
  %99 = add i64 %95, %98
  %100 = add i64 %2, 1
  %101 = add i64 %3, 1
  %102 = call i1 @view366(i8* %0, { i1*, i64 }* %1, i64 %100, i64 %101)
  %103 = call i64 @to_int359(i8* %0, i1 %102)
  %104 = add i64 %99, %103
  %105 = getelementptr { i64*, i64 }, { i64*, i64 }* %78, i32 0, i32 0
  %106 = load i64*, i64** %105
  %107 = getelementptr i64, i64* %106, i64 0
  store i64 %104, i64* %107
  %108 = call i8* @GC_malloc(i64 0)
  %109 = bitcast i8* %108 to {}*
  %110 = sub i64 %2, 1
  %111 = call i1 @view366(i8* %0, { i1*, i64 }* %1, i64 %110, i64 %3)
  %112 = call i64 @to_int359(i8* %0, i1 %111)
  %113 = add i64 %2, 1
  %114 = call i1 @view366(i8* %0, { i1*, i64 }* %1, i64 %113, i64 %3)
  %115 = call i64 @to_int359(i8* %0, i1 %114)
  %116 = add i64 %112, %115
  %117 = getelementptr { i64*, i64 }, { i64*, i64 }* %78, i32 0, i32 0
  %118 = load i64*, i64** %117
  %119 = getelementptr i64, i64* %118, i64 0
  %120 = load i64, i64* %119
  %121 = add i64 %116, %120
  %122 = getelementptr { i64*, i64 }, { i64*, i64 }* %78, i32 0, i32 0
  %123 = load i64*, i64** %122
  %124 = getelementptr i64, i64* %123, i64 0
  store i64 %121, i64* %124
  %125 = call i8* @GC_malloc(i64 0)
  %126 = bitcast i8* %125 to {}*
  %127 = sub i64 %2, 1
  %128 = sub i64 %3, 1
  %129 = call i1 @view366(i8* %0, { i1*, i64 }* %1, i64 %127, i64 %128)
  %130 = call i64 @to_int359(i8* %0, i1 %129)
  %131 = sub i64 %3, 1
  %132 = call i1 @view366(i8* %0, { i1*, i64 }* %1, i64 %2, i64 %131)
  %133 = call i64 @to_int359(i8* %0, i1 %132)
  %134 = add i64 %130, %133
  %135 = add i64 %2, 1
  %136 = sub i64 %3, 1
  %137 = call i1 @view366(i8* %0, { i1*, i64 }* %1, i64 %135, i64 %136)
  %138 = call i64 @to_int359(i8* %0, i1 %137)
  %139 = add i64 %134, %138
  %140 = getelementptr { i64*, i64 }, { i64*, i64 }* %78, i32 0, i32 0
  %141 = load i64*, i64** %140
  %142 = getelementptr i64, i64* %141, i64 0
  %143 = load i64, i64* %142
  %144 = add i64 %139, %143
  %145 = getelementptr { i64*, i64 }, { i64*, i64 }* %78, i32 0, i32 0
  %146 = load i64*, i64** %145
  %147 = getelementptr i64, i64* %146, i64 0
  store i64 %144, i64* %147
  %148 = call i8* @GC_malloc(i64 0)
  %149 = bitcast i8* %148 to {}*
  %150 = call i1 @view366(i8* %0, { i1*, i64 }* %1, i64 %2, i64 %3)
  %151 = alloca i1
  br i1 %150, label %then_0, label %else_0

then_0:                                           ; preds = %end_0
  %152 = getelementptr { i64*, i64 }, { i64*, i64 }* %78, i32 0, i32 0
  %153 = load i64*, i64** %152
  %154 = getelementptr i64, i64* %153, i64 0
  %155 = load i64, i64* %154
  %156 = icmp eq i64 %155, 2
  %157 = getelementptr { i64*, i64 }, { i64*, i64 }* %78, i32 0, i32 0
  %158 = load i64*, i64** %157
  %159 = getelementptr i64, i64* %158, i64 0
  %160 = load i64, i64* %159
  %161 = icmp eq i64 %160, 3
  %162 = or i1 %156, %161
  store i1 %162, i1* %151
  br label %end_1

else_0:                                           ; preds = %end_0
  %163 = getelementptr { i64*, i64 }, { i64*, i64 }* %78, i32 0, i32 0
  %164 = load i64*, i64** %163
  %165 = getelementptr i64, i64* %164, i64 0
  %166 = load i64, i64* %165
  %167 = icmp eq i64 %166, 3
  store i1 %167, i1* %151
  br label %end_1

end_1:                                            ; preds = %else_0, %then_0
  %168 = load i1, i1* %151
  ret i1 %168
}

define {}* @go_x357(i8*, i64) {
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
  store {}* (i8*, i64)* @go_x357, {}* (i8*, i64)** %18
  %19 = call i8* @GC_malloc(i64 0)
  %20 = bitcast i8* %19 to {}*
  %21 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %17, i32 0, i32 1
  store i8* %0, i8** %21
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  %24 = icmp sge i64 %1, %13
  %25 = alloca {}*
  br i1 %24, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %26 = call i8* @GC_malloc(i64 0)
  %27 = bitcast i8* %26 to {}*
  store {}* %27, {}** %25
  br label %end_0

else_0:                                           ; preds = %2
  %28 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 0
  %29 = load i1 (i8*, { i1*, i64 }*, i64, i64)*, i1 (i8*, { i1*, i64 }*, i64, i64)** %28
  %30 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 1
  %31 = load i8*, i8** %30
  %32 = call i1 %29(i8* %31, { i1*, i64 }* %7, i64 %1, i64 %15)
  %33 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %11, i32 0, i32 0
  %34 = load {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %33
  %35 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %11, i32 0, i32 1
  %36 = load i8*, i8** %35
  %37 = call {}* %34(i8* %36, { i1*, i64 }* %5, i64 %1, i64 %15, i1 %32)
  %38 = add i64 %1, 1
  %39 = call {}* @go_x357(i8* %0, i64 %38)
  store {}* %39, {}** %25
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %40 = load {}*, {}** %25
  ret {}* %40
}

define {}* @go_y356(i8*, i64) {
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
  store {}* (i8*, i64)* @go_y356, {}* (i8*, i64)** %18
  %19 = call i8* @GC_malloc(i64 0)
  %20 = bitcast i8* %19 to {}*
  %21 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %17, i32 0, i32 1
  store i8* %0, i8** %21
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  %24 = call i8* @GC_malloc(i64 ptrtoint ({ { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* getelementptr inbounds ({ { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %25 = bitcast i8* %24 to { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %26 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %25, i32 0, i32 0
  store { i1*, i64 }* %5, { i1*, i64 }** %26
  %27 = call i8* @GC_malloc(i64 0)
  %28 = bitcast i8* %27 to {}*
  %29 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %25, i32 0, i32 1
  store { i1*, i64 }* %7, { i1*, i64 }** %29
  %30 = call i8* @GC_malloc(i64 0)
  %31 = bitcast i8* %30 to {}*
  %32 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %25, i32 0, i32 2
  store { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %32
  %33 = call i8* @GC_malloc(i64 0)
  %34 = bitcast i8* %33 to {}*
  %35 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %25, i32 0, i32 3
  store { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %11, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }** %35
  %36 = call i8* @GC_malloc(i64 0)
  %37 = bitcast i8* %36 to {}*
  %38 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %25, i32 0, i32 4
  store i64 %13, i64* %38
  %39 = call i8* @GC_malloc(i64 0)
  %40 = bitcast i8* %39 to {}*
  %41 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %25, i32 0, i32 5
  store i64 %1, i64* %41
  %42 = call i8* @GC_malloc(i64 0)
  %43 = bitcast i8* %42 to {}*
  %44 = bitcast { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %25 to i8*
  %45 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %46 = bitcast i8* %45 to { {}* (i8*, i64)*, i8* }*
  %47 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %46, i32 0, i32 0
  store {}* (i8*, i64)* @go_x357, {}* (i8*, i64)** %47
  %48 = call i8* @GC_malloc(i64 0)
  %49 = bitcast i8* %48 to {}*
  %50 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %46, i32 0, i32 1
  store i8* %44, i8** %50
  %51 = call i8* @GC_malloc(i64 0)
  %52 = bitcast i8* %51 to {}*
  %53 = icmp sge i64 %1, %15
  %54 = alloca {}*
  br i1 %53, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %55 = call i8* @GC_malloc(i64 0)
  %56 = bitcast i8* %55 to {}*
  store {}* %56, {}** %54
  br label %end_0

else_0:                                           ; preds = %2
  %57 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %46, i32 0, i32 0
  %58 = load {}* (i8*, i64)*, {}* (i8*, i64)** %57
  %59 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %46, i32 0, i32 1
  %60 = load i8*, i8** %59
  %61 = call {}* %58(i8* %60, i64 0)
  %62 = add i64 %1, 1
  %63 = call {}* @go_y356(i8* %0, i64 %62)
  store {}* %63, {}** %54
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %64 = load {}*, {}** %54
  ret {}* %64
}

define {}* @update_cells355(i8*, { i1*, i64 }*) {
  %3 = bitcast i8* %0 to { i64, i64 }*
  %4 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 1
  %7 = load i64, i64* %6
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %10 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view366, i1 (i8*, { i1*, i64 }*, i64, i64)** %10
  %11 = call i8* @GC_malloc(i64 0)
  %12 = bitcast i8* %11 to {}*
  %13 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %13
  %14 = call i8* @GC_malloc(i64 0)
  %15 = bitcast i8* %14 to {}*
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*
  %18 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %17, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set365, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %18
  %19 = call i8* @GC_malloc(i64 0)
  %20 = bitcast i8* %19 to {}*
  %21 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %17, i32 0, i32 1
  store i8* %0, i8** %21
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  %24 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %25 = bitcast i8* %24 to { {}* (i8*, { i1*, i64 }*, i64)*, i8* }*
  %26 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %25, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells364, {}* (i8*, { i1*, i64 }*, i64)** %26
  %27 = call i8* @GC_malloc(i64 0)
  %28 = bitcast i8* %27 to {}*
  %29 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %25, i32 0, i32 1
  store i8* %0, i8** %29
  %30 = call i8* @GC_malloc(i64 0)
  %31 = bitcast i8* %30 to {}*
  %32 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %33 = bitcast i8* %32 to { i8 (i8*, i1)*, i8* }*
  %34 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %33, i32 0, i32 0
  store i8 (i8*, i1)* @to_char363, i8 (i8*, i1)** %34
  %35 = call i8* @GC_malloc(i64 0)
  %36 = bitcast i8* %35 to {}*
  %37 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %33, i32 0, i32 1
  store i8* %0, i8** %37
  %38 = call i8* @GC_malloc(i64 0)
  %39 = bitcast i8* %38 to {}*
  %40 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %41 = bitcast i8* %40 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %42 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %41, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @print_cells360, {}* (i8*, { i1*, i64 }*)** %42
  %43 = call i8* @GC_malloc(i64 0)
  %44 = bitcast i8* %43 to {}*
  %45 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %41, i32 0, i32 1
  store i8* %0, i8** %45
  %46 = call i8* @GC_malloc(i64 0)
  %47 = bitcast i8* %46 to {}*
  %48 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %49 = bitcast i8* %48 to { i64 (i8*, i1)*, i8* }*
  %50 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %49, i32 0, i32 0
  store i64 (i8*, i1)* @to_int359, i64 (i8*, i1)** %50
  %51 = call i8* @GC_malloc(i64 0)
  %52 = bitcast i8* %51 to {}*
  %53 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %49, i32 0, i32 1
  store i8* %0, i8** %53
  %54 = call i8* @GC_malloc(i64 0)
  %55 = bitcast i8* %54 to {}*
  %56 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %57 = bitcast i8* %56 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %58 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %57, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state358, i1 (i8*, { i1*, i64 }*, i64, i64)** %58
  %59 = call i8* @GC_malloc(i64 0)
  %60 = bitcast i8* %59 to {}*
  %61 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %57, i32 0, i32 1
  store i8* %0, i8** %61
  %62 = call i8* @GC_malloc(i64 0)
  %63 = bitcast i8* %62 to {}*
  %64 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %65 = bitcast i8* %64 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %66 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %65, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @update_cells355, {}* (i8*, { i1*, i64 }*)** %66
  %67 = call i8* @GC_malloc(i64 0)
  %68 = bitcast i8* %67 to {}*
  %69 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %65, i32 0, i32 1
  store i8* %0, i8** %69
  %70 = call i8* @GC_malloc(i64 0)
  %71 = bitcast i8* %70 to {}*
  %72 = call { i1*, i64 }* @copy_bool_array368({ i1*, i64 }* %1)
  %73 = call i8* @GC_malloc(i64 ptrtoint ({ { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* getelementptr inbounds ({ { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %74 = bitcast i8* %73 to { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %75 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %74, i32 0, i32 0
  store { i1*, i64 }* %1, { i1*, i64 }** %75
  %76 = call i8* @GC_malloc(i64 0)
  %77 = bitcast i8* %76 to {}*
  %78 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %74, i32 0, i32 1
  store { i1*, i64 }* %72, { i1*, i64 }** %78
  %79 = call i8* @GC_malloc(i64 0)
  %80 = bitcast i8* %79 to {}*
  %81 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %74, i32 0, i32 2
  store { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %57, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %81
  %82 = call i8* @GC_malloc(i64 0)
  %83 = bitcast i8* %82 to {}*
  %84 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %74, i32 0, i32 3
  store { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %17, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }** %84
  %85 = call i8* @GC_malloc(i64 0)
  %86 = bitcast i8* %85 to {}*
  %87 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %74, i32 0, i32 4
  store i64 %5, i64* %87
  %88 = call i8* @GC_malloc(i64 0)
  %89 = bitcast i8* %88 to {}*
  %90 = getelementptr { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %74, i32 0, i32 5
  store i64 %7, i64* %90
  %91 = call i8* @GC_malloc(i64 0)
  %92 = bitcast i8* %91 to {}*
  %93 = bitcast { { i1*, i64 }*, { i1*, i64 }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, i64, i64 }* %74 to i8*
  %94 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %95 = bitcast i8* %94 to { {}* (i8*, i64)*, i8* }*
  %96 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %95, i32 0, i32 0
  store {}* (i8*, i64)* @go_y356, {}* (i8*, i64)** %96
  %97 = call i8* @GC_malloc(i64 0)
  %98 = bitcast i8* %97 to {}*
  %99 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %95, i32 0, i32 1
  store i8* %93, i8** %99
  %100 = call i8* @GC_malloc(i64 0)
  %101 = bitcast i8* %100 to {}*
  %102 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %95, i32 0, i32 0
  %103 = load {}* (i8*, i64)*, {}* (i8*, i64)** %102
  %104 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %95, i32 0, i32 1
  %105 = load i8*, i8** %104
  %106 = call {}* %103(i8* %105, i64 0)
  ret {}* %106
}

define {}* @loop354(i8*, i64) {
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
  store {}* (i8*, i64)* @loop354, {}* (i8*, i64)** %12
  %13 = call i8* @GC_malloc(i64 0)
  %14 = bitcast i8* %13 to {}*
  %15 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %0, i8** %15
  %16 = call i8* @GC_malloc(i64 0)
  %17 = bitcast i8* %16 to {}*
  %18 = icmp sgt i64 %1, 0
  %19 = alloca {}*
  br i1 %18, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %20 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %7, i32 0, i32 0
  %21 = load {}* (i8*, { i1*, i64 }*)*, {}* (i8*, { i1*, i64 }*)** %20
  %22 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %7, i32 0, i32 1
  %23 = load i8*, i8** %22
  %24 = call {}* %21(i8* %23, { i1*, i64 }* %5)
  %25 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %9, i32 0, i32 0
  %26 = load {}* (i8*, { i1*, i64 }*)*, {}* (i8*, { i1*, i64 }*)** %25
  %27 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %9, i32 0, i32 1
  %28 = load i8*, i8** %27
  %29 = call {}* %26(i8* %28, { i1*, i64 }* %5)
  %30 = call {}* @newline372()
  %31 = sub i64 %1, 1
  %32 = call {}* @loop354(i8* %0, i64 %31)
  store {}* %32, {}** %19
  br label %end_0

else_0:                                           ; preds = %2
  %33 = call i8* @GC_malloc(i64 0)
  %34 = bitcast i8* %33 to {}*
  store {}* %34, {}** %19
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %35 = load {}*, {}** %19
  ret {}* %35
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
  %15 = call i8* @GC_malloc(i64 0)
  %16 = bitcast i8* %15 to {}*
  %17 = add i64 %10, 1
  store i64 %17, i64* %9
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %18 = call i8* @GC_malloc(i64 0)
  %19 = bitcast i8* %18 to {}*
  %20 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %21 = bitcast i8* %20 to { i64, i64 }*
  %22 = getelementptr { i64, i64 }, { i64, i64 }* %21, i32 0, i32 0
  store i64 50, i64* %22
  %23 = call i8* @GC_malloc(i64 0)
  %24 = bitcast i8* %23 to {}*
  %25 = getelementptr { i64, i64 }, { i64, i64 }* %21, i32 0, i32 1
  store i64 20, i64* %25
  %26 = call i8* @GC_malloc(i64 0)
  %27 = bitcast i8* %26 to {}*
  %28 = bitcast { i64, i64 }* %21 to i8*
  %29 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %30 = bitcast i8* %29 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %31 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %30, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view366, i1 (i8*, { i1*, i64 }*, i64, i64)** %31
  %32 = call i8* @GC_malloc(i64 0)
  %33 = bitcast i8* %32 to {}*
  %34 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %30, i32 0, i32 1
  store i8* %28, i8** %34
  %35 = call i8* @GC_malloc(i64 0)
  %36 = bitcast i8* %35 to {}*
  %37 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %38 = bitcast i8* %37 to { i64, i64 }*
  %39 = getelementptr { i64, i64 }, { i64, i64 }* %38, i32 0, i32 0
  store i64 50, i64* %39
  %40 = call i8* @GC_malloc(i64 0)
  %41 = bitcast i8* %40 to {}*
  %42 = getelementptr { i64, i64 }, { i64, i64 }* %38, i32 0, i32 1
  store i64 20, i64* %42
  %43 = call i8* @GC_malloc(i64 0)
  %44 = bitcast i8* %43 to {}*
  %45 = bitcast { i64, i64 }* %38 to i8*
  %46 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %47 = bitcast i8* %46 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*
  %48 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %47, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set365, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %48
  %49 = call i8* @GC_malloc(i64 0)
  %50 = bitcast i8* %49 to {}*
  %51 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %47, i32 0, i32 1
  store i8* %45, i8** %51
  %52 = call i8* @GC_malloc(i64 0)
  %53 = bitcast i8* %52 to {}*
  %54 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %55 = bitcast i8* %54 to { i64, i64 }*
  %56 = getelementptr { i64, i64 }, { i64, i64 }* %55, i32 0, i32 0
  store i64 50, i64* %56
  %57 = call i8* @GC_malloc(i64 0)
  %58 = bitcast i8* %57 to {}*
  %59 = getelementptr { i64, i64 }, { i64, i64 }* %55, i32 0, i32 1
  store i64 20, i64* %59
  %60 = call i8* @GC_malloc(i64 0)
  %61 = bitcast i8* %60 to {}*
  %62 = bitcast { i64, i64 }* %55 to i8*
  %63 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %64 = bitcast i8* %63 to { {}* (i8*, { i1*, i64 }*, i64)*, i8* }*
  %65 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %64, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells364, {}* (i8*, { i1*, i64 }*, i64)** %65
  %66 = call i8* @GC_malloc(i64 0)
  %67 = bitcast i8* %66 to {}*
  %68 = getelementptr { {}* (i8*, { i1*, i64 }*, i64)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64)*, i8* }* %64, i32 0, i32 1
  store i8* %62, i8** %68
  %69 = call i8* @GC_malloc(i64 0)
  %70 = bitcast i8* %69 to {}*
  %71 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %72 = bitcast i8* %71 to { i64, i64 }*
  %73 = getelementptr { i64, i64 }, { i64, i64 }* %72, i32 0, i32 0
  store i64 50, i64* %73
  %74 = call i8* @GC_malloc(i64 0)
  %75 = bitcast i8* %74 to {}*
  %76 = getelementptr { i64, i64 }, { i64, i64 }* %72, i32 0, i32 1
  store i64 20, i64* %76
  %77 = call i8* @GC_malloc(i64 0)
  %78 = bitcast i8* %77 to {}*
  %79 = bitcast { i64, i64 }* %72 to i8*
  %80 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %81 = bitcast i8* %80 to { i8 (i8*, i1)*, i8* }*
  %82 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %81, i32 0, i32 0
  store i8 (i8*, i1)* @to_char363, i8 (i8*, i1)** %82
  %83 = call i8* @GC_malloc(i64 0)
  %84 = bitcast i8* %83 to {}*
  %85 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %81, i32 0, i32 1
  store i8* %79, i8** %85
  %86 = call i8* @GC_malloc(i64 0)
  %87 = bitcast i8* %86 to {}*
  %88 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %89 = bitcast i8* %88 to { i64, i64 }*
  %90 = getelementptr { i64, i64 }, { i64, i64 }* %89, i32 0, i32 0
  store i64 50, i64* %90
  %91 = call i8* @GC_malloc(i64 0)
  %92 = bitcast i8* %91 to {}*
  %93 = getelementptr { i64, i64 }, { i64, i64 }* %89, i32 0, i32 1
  store i64 20, i64* %93
  %94 = call i8* @GC_malloc(i64 0)
  %95 = bitcast i8* %94 to {}*
  %96 = bitcast { i64, i64 }* %89 to i8*
  %97 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %98 = bitcast i8* %97 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %99 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %98, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @print_cells360, {}* (i8*, { i1*, i64 }*)** %99
  %100 = call i8* @GC_malloc(i64 0)
  %101 = bitcast i8* %100 to {}*
  %102 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %98, i32 0, i32 1
  store i8* %96, i8** %102
  %103 = call i8* @GC_malloc(i64 0)
  %104 = bitcast i8* %103 to {}*
  %105 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %106 = bitcast i8* %105 to { i64, i64 }*
  %107 = getelementptr { i64, i64 }, { i64, i64 }* %106, i32 0, i32 0
  store i64 50, i64* %107
  %108 = call i8* @GC_malloc(i64 0)
  %109 = bitcast i8* %108 to {}*
  %110 = getelementptr { i64, i64 }, { i64, i64 }* %106, i32 0, i32 1
  store i64 20, i64* %110
  %111 = call i8* @GC_malloc(i64 0)
  %112 = bitcast i8* %111 to {}*
  %113 = bitcast { i64, i64 }* %106 to i8*
  %114 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %115 = bitcast i8* %114 to { i64 (i8*, i1)*, i8* }*
  %116 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %115, i32 0, i32 0
  store i64 (i8*, i1)* @to_int359, i64 (i8*, i1)** %116
  %117 = call i8* @GC_malloc(i64 0)
  %118 = bitcast i8* %117 to {}*
  %119 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %115, i32 0, i32 1
  store i8* %113, i8** %119
  %120 = call i8* @GC_malloc(i64 0)
  %121 = bitcast i8* %120 to {}*
  %122 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %123 = bitcast i8* %122 to { i64, i64 }*
  %124 = getelementptr { i64, i64 }, { i64, i64 }* %123, i32 0, i32 0
  store i64 50, i64* %124
  %125 = call i8* @GC_malloc(i64 0)
  %126 = bitcast i8* %125 to {}*
  %127 = getelementptr { i64, i64 }, { i64, i64 }* %123, i32 0, i32 1
  store i64 20, i64* %127
  %128 = call i8* @GC_malloc(i64 0)
  %129 = bitcast i8* %128 to {}*
  %130 = bitcast { i64, i64 }* %123 to i8*
  %131 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %132 = bitcast i8* %131 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*
  %133 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %132, i32 0, i32 0
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state358, i1 (i8*, { i1*, i64 }*, i64, i64)** %133
  %134 = call i8* @GC_malloc(i64 0)
  %135 = bitcast i8* %134 to {}*
  %136 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %132, i32 0, i32 1
  store i8* %130, i8** %136
  %137 = call i8* @GC_malloc(i64 0)
  %138 = bitcast i8* %137 to {}*
  %139 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %140 = bitcast i8* %139 to { i64, i64 }*
  %141 = getelementptr { i64, i64 }, { i64, i64 }* %140, i32 0, i32 0
  store i64 50, i64* %141
  %142 = call i8* @GC_malloc(i64 0)
  %143 = bitcast i8* %142 to {}*
  %144 = getelementptr { i64, i64 }, { i64, i64 }* %140, i32 0, i32 1
  store i64 20, i64* %144
  %145 = call i8* @GC_malloc(i64 0)
  %146 = bitcast i8* %145 to {}*
  %147 = bitcast { i64, i64 }* %140 to i8*
  %148 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %149 = bitcast i8* %148 to { {}* (i8*, { i1*, i64 }*)*, i8* }*
  %150 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %149, i32 0, i32 0
  store {}* (i8*, { i1*, i64 }*)* @update_cells355, {}* (i8*, { i1*, i64 }*)** %150
  %151 = call i8* @GC_malloc(i64 0)
  %152 = bitcast i8* %151 to {}*
  %153 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %149, i32 0, i32 1
  store i8* %147, i8** %153
  %154 = call i8* @GC_malloc(i64 0)
  %155 = bitcast i8* %154 to {}*
  %156 = call {}* @gen_seed371()
  %157 = call {}* @pulsar367({ i1*, i64 }* %6)
  %158 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 3))
  %159 = bitcast i8* %158 to { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }*
  %160 = getelementptr { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }, { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }* %159, i32 0, i32 0
  store { i1*, i64 }* %6, { i1*, i64 }** %160
  %161 = call i8* @GC_malloc(i64 0)
  %162 = bitcast i8* %161 to {}*
  %163 = getelementptr { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }, { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }* %159, i32 0, i32 1
  store { {}* (i8*, { i1*, i64 }*)*, i8* }* %98, { {}* (i8*, { i1*, i64 }*)*, i8* }** %163
  %164 = call i8* @GC_malloc(i64 0)
  %165 = bitcast i8* %164 to {}*
  %166 = getelementptr { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }, { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }* %159, i32 0, i32 2
  store { {}* (i8*, { i1*, i64 }*)*, i8* }* %149, { {}* (i8*, { i1*, i64 }*)*, i8* }** %166
  %167 = call i8* @GC_malloc(i64 0)
  %168 = bitcast i8* %167 to {}*
  %169 = bitcast { { i1*, i64 }*, { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }* }* %159 to i8*
  %170 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %171 = bitcast i8* %170 to { {}* (i8*, i64)*, i8* }*
  %172 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %171, i32 0, i32 0
  store {}* (i8*, i64)* @loop354, {}* (i8*, i64)** %172
  %173 = call i8* @GC_malloc(i64 0)
  %174 = bitcast i8* %173 to {}*
  %175 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %171, i32 0, i32 1
  store i8* %169, i8** %175
  %176 = call i8* @GC_malloc(i64 0)
  %177 = bitcast i8* %176 to {}*
  %178 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %171, i32 0, i32 0
  %179 = load {}* (i8*, i64)*, {}* (i8*, i64)** %178
  %180 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %171, i32 0, i32 1
  %181 = load i8*, i8** %180
  %182 = call {}* %179(i8* %181, i64 10)
  ret i32 0
}
