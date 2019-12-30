; ModuleID = './examples/life.mlg'
source_filename = "./examples/life.mlg"

declare {}* @print_char(i8)

define {}* @print_char376(i8) {
  %2 = call {}* @print_char(i8 %0)
  ret {}* %2
}

declare {}* @newline()

define {}* @newline375() {
  %1 = call {}* @newline()
  ret {}* %1
}

declare {}* @gen_seed()

define {}* @gen_seed374() {
  %1 = call {}* @gen_seed()
  ret {}* %1
}

declare i1 @rand_bool()

define i1 @rand_bool373() {
  %1 = call i1 @rand_bool()
  ret i1 %1
}

declare {}* @malgo_sleep(i64)

define {}* @sleep372(i64) {
  %2 = call {}* @malgo_sleep(i64 %0)
  ret {}* %2
}

declare i1* @copy_bool_array(i1*, i64)

define i1* @copy_bool_array371(i1*, i64) {
  %3 = call i1* @copy_bool_array(i1* %0, i64 %1)
  ret i1* %3
}

declare {}* @pulsar(i1*)

define {}* @pulsar370(i1*) {
  %2 = call {}* @pulsar(i1* %0)
  ret {}* %2
}

declare i8* @GC_malloc(i64)

define i1 @view369(i8*, i1*, i64, i64) {
  %5 = bitcast i8* %0 to { i64, i64 }*
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %5, i32 0, i32 0
  %7 = load i64, i64* %6
  %8 = getelementptr { i64, i64 }, { i64, i64 }* %5, i32 0, i32 1
  %9 = load i64, i64* %8
  %10 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %11 = bitcast i8* %10 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %12 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %11, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @view369, i1 (i8*, i1*, i64, i64)** %12
  %13 = call i8* @GC_malloc(i64 0)
  %14 = bitcast i8* %13 to {}*
  %15 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %0, i8** %15
  %16 = call i8* @GC_malloc(i64 0)
  %17 = bitcast i8* %16 to {}*
  %18 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %19 = bitcast i8* %18 to { {}* (i8*, i1*, i64, i64, i1)*, i8* }*
  %20 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %19, i32 0, i32 0
  store {}* (i8*, i1*, i64, i64, i1)* @set368, {}* (i8*, i1*, i64, i64, i1)** %20
  %21 = call i8* @GC_malloc(i64 0)
  %22 = bitcast i8* %21 to {}*
  %23 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %19, i32 0, i32 1
  store i8* %0, i8** %23
  %24 = call i8* @GC_malloc(i64 0)
  %25 = bitcast i8* %24 to {}*
  %26 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %27 = bitcast i8* %26 to { {}* (i8*, i1*, i64)*, i8* }*
  %28 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %27, i32 0, i32 0
  store {}* (i8*, i1*, i64)* @init_cells367, {}* (i8*, i1*, i64)** %28
  %29 = call i8* @GC_malloc(i64 0)
  %30 = bitcast i8* %29 to {}*
  %31 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %27, i32 0, i32 1
  store i8* %0, i8** %31
  %32 = call i8* @GC_malloc(i64 0)
  %33 = bitcast i8* %32 to {}*
  %34 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %35 = bitcast i8* %34 to { i8 (i8*, i1)*, i8* }*
  %36 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %35, i32 0, i32 0
  store i8 (i8*, i1)* @to_char366, i8 (i8*, i1)** %36
  %37 = call i8* @GC_malloc(i64 0)
  %38 = bitcast i8* %37 to {}*
  %39 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %35, i32 0, i32 1
  store i8* %0, i8** %39
  %40 = call i8* @GC_malloc(i64 0)
  %41 = bitcast i8* %40 to {}*
  %42 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %43 = bitcast i8* %42 to { {}* (i8*, i1*)*, i8* }*
  %44 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %43, i32 0, i32 0
  store {}* (i8*, i1*)* @print_cells363, {}* (i8*, i1*)** %44
  %45 = call i8* @GC_malloc(i64 0)
  %46 = bitcast i8* %45 to {}*
  %47 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %43, i32 0, i32 1
  store i8* %0, i8** %47
  %48 = call i8* @GC_malloc(i64 0)
  %49 = bitcast i8* %48 to {}*
  %50 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %51 = bitcast i8* %50 to { i64 (i8*, i1)*, i8* }*
  %52 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %51, i32 0, i32 0
  store i64 (i8*, i1)* @to_int362, i64 (i8*, i1)** %52
  %53 = call i8* @GC_malloc(i64 0)
  %54 = bitcast i8* %53 to {}*
  %55 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %51, i32 0, i32 1
  store i8* %0, i8** %55
  %56 = call i8* @GC_malloc(i64 0)
  %57 = bitcast i8* %56 to {}*
  %58 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %59 = bitcast i8* %58 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %60 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %59, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @next_state361, i1 (i8*, i1*, i64, i64)** %60
  %61 = call i8* @GC_malloc(i64 0)
  %62 = bitcast i8* %61 to {}*
  %63 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %59, i32 0, i32 1
  store i8* %0, i8** %63
  %64 = call i8* @GC_malloc(i64 0)
  %65 = bitcast i8* %64 to {}*
  %66 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %67 = bitcast i8* %66 to { {}* (i8*, i1*)*, i8* }*
  %68 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %67, i32 0, i32 0
  store {}* (i8*, i1*)* @update_cells358, {}* (i8*, i1*)** %68
  %69 = call i8* @GC_malloc(i64 0)
  %70 = bitcast i8* %69 to {}*
  %71 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %67, i32 0, i32 1
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
  %84 = getelementptr i1, i1* %1, i64 %83
  %85 = load i1, i1* %84
  store i1 %85, i1* %81
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %86 = load i1, i1* %81
  ret i1 %86
}

define {}* @set368(i8*, i1*, i64, i64, i1) {
  %6 = bitcast i8* %0 to { i64, i64 }*
  %7 = getelementptr { i64, i64 }, { i64, i64 }* %6, i32 0, i32 0
  %8 = load i64, i64* %7
  %9 = getelementptr { i64, i64 }, { i64, i64 }* %6, i32 0, i32 1
  %10 = load i64, i64* %9
  %11 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %12 = bitcast i8* %11 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %13 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %12, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @view369, i1 (i8*, i1*, i64, i64)** %13
  %14 = call i8* @GC_malloc(i64 0)
  %15 = bitcast i8* %14 to {}*
  %16 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %12, i32 0, i32 1
  store i8* %0, i8** %16
  %17 = call i8* @GC_malloc(i64 0)
  %18 = bitcast i8* %17 to {}*
  %19 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %20 = bitcast i8* %19 to { {}* (i8*, i1*, i64, i64, i1)*, i8* }*
  %21 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %20, i32 0, i32 0
  store {}* (i8*, i1*, i64, i64, i1)* @set368, {}* (i8*, i1*, i64, i64, i1)** %21
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  %24 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %20, i32 0, i32 1
  store i8* %0, i8** %24
  %25 = call i8* @GC_malloc(i64 0)
  %26 = bitcast i8* %25 to {}*
  %27 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %28 = bitcast i8* %27 to { {}* (i8*, i1*, i64)*, i8* }*
  %29 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %28, i32 0, i32 0
  store {}* (i8*, i1*, i64)* @init_cells367, {}* (i8*, i1*, i64)** %29
  %30 = call i8* @GC_malloc(i64 0)
  %31 = bitcast i8* %30 to {}*
  %32 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %28, i32 0, i32 1
  store i8* %0, i8** %32
  %33 = call i8* @GC_malloc(i64 0)
  %34 = bitcast i8* %33 to {}*
  %35 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %36 = bitcast i8* %35 to { i8 (i8*, i1)*, i8* }*
  %37 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %36, i32 0, i32 0
  store i8 (i8*, i1)* @to_char366, i8 (i8*, i1)** %37
  %38 = call i8* @GC_malloc(i64 0)
  %39 = bitcast i8* %38 to {}*
  %40 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %36, i32 0, i32 1
  store i8* %0, i8** %40
  %41 = call i8* @GC_malloc(i64 0)
  %42 = bitcast i8* %41 to {}*
  %43 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %44 = bitcast i8* %43 to { {}* (i8*, i1*)*, i8* }*
  %45 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %44, i32 0, i32 0
  store {}* (i8*, i1*)* @print_cells363, {}* (i8*, i1*)** %45
  %46 = call i8* @GC_malloc(i64 0)
  %47 = bitcast i8* %46 to {}*
  %48 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %44, i32 0, i32 1
  store i8* %0, i8** %48
  %49 = call i8* @GC_malloc(i64 0)
  %50 = bitcast i8* %49 to {}*
  %51 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %52 = bitcast i8* %51 to { i64 (i8*, i1)*, i8* }*
  %53 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %52, i32 0, i32 0
  store i64 (i8*, i1)* @to_int362, i64 (i8*, i1)** %53
  %54 = call i8* @GC_malloc(i64 0)
  %55 = bitcast i8* %54 to {}*
  %56 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %52, i32 0, i32 1
  store i8* %0, i8** %56
  %57 = call i8* @GC_malloc(i64 0)
  %58 = bitcast i8* %57 to {}*
  %59 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %60 = bitcast i8* %59 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %61 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %60, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @next_state361, i1 (i8*, i1*, i64, i64)** %61
  %62 = call i8* @GC_malloc(i64 0)
  %63 = bitcast i8* %62 to {}*
  %64 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %60, i32 0, i32 1
  store i8* %0, i8** %64
  %65 = call i8* @GC_malloc(i64 0)
  %66 = bitcast i8* %65 to {}*
  %67 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %68 = bitcast i8* %67 to { {}* (i8*, i1*)*, i8* }*
  %69 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %68, i32 0, i32 0
  store {}* (i8*, i1*)* @update_cells358, {}* (i8*, i1*)** %69
  %70 = call i8* @GC_malloc(i64 0)
  %71 = bitcast i8* %70 to {}*
  %72 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %68, i32 0, i32 1
  store i8* %0, i8** %72
  %73 = call i8* @GC_malloc(i64 0)
  %74 = bitcast i8* %73 to {}*
  %75 = mul i64 %3, %8
  %76 = add i64 %75, %2
  %77 = getelementptr i1, i1* %1, i64 %76
  store i1 %4, i1* %77
  %78 = call i8* @GC_malloc(i64 0)
  %79 = bitcast i8* %78 to {}*
  ret {}* undef
}

define {}* @init_cells367(i8*, i1*, i64) {
  %4 = bitcast i8* %0 to { i64, i64 }*
  %5 = getelementptr { i64, i64 }, { i64, i64 }* %4, i32 0, i32 0
  %6 = load i64, i64* %5
  %7 = getelementptr { i64, i64 }, { i64, i64 }* %4, i32 0, i32 1
  %8 = load i64, i64* %7
  %9 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %10 = bitcast i8* %9 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %11 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %10, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @view369, i1 (i8*, i1*, i64, i64)** %11
  %12 = call i8* @GC_malloc(i64 0)
  %13 = bitcast i8* %12 to {}*
  %14 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %10, i32 0, i32 1
  store i8* %0, i8** %14
  %15 = call i8* @GC_malloc(i64 0)
  %16 = bitcast i8* %15 to {}*
  %17 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %18 = bitcast i8* %17 to { {}* (i8*, i1*, i64, i64, i1)*, i8* }*
  %19 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %18, i32 0, i32 0
  store {}* (i8*, i1*, i64, i64, i1)* @set368, {}* (i8*, i1*, i64, i64, i1)** %19
  %20 = call i8* @GC_malloc(i64 0)
  %21 = bitcast i8* %20 to {}*
  %22 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %18, i32 0, i32 1
  store i8* %0, i8** %22
  %23 = call i8* @GC_malloc(i64 0)
  %24 = bitcast i8* %23 to {}*
  %25 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %26 = bitcast i8* %25 to { {}* (i8*, i1*, i64)*, i8* }*
  %27 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %26, i32 0, i32 0
  store {}* (i8*, i1*, i64)* @init_cells367, {}* (i8*, i1*, i64)** %27
  %28 = call i8* @GC_malloc(i64 0)
  %29 = bitcast i8* %28 to {}*
  %30 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %26, i32 0, i32 1
  store i8* %0, i8** %30
  %31 = call i8* @GC_malloc(i64 0)
  %32 = bitcast i8* %31 to {}*
  %33 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %34 = bitcast i8* %33 to { i8 (i8*, i1)*, i8* }*
  %35 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %34, i32 0, i32 0
  store i8 (i8*, i1)* @to_char366, i8 (i8*, i1)** %35
  %36 = call i8* @GC_malloc(i64 0)
  %37 = bitcast i8* %36 to {}*
  %38 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %34, i32 0, i32 1
  store i8* %0, i8** %38
  %39 = call i8* @GC_malloc(i64 0)
  %40 = bitcast i8* %39 to {}*
  %41 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %42 = bitcast i8* %41 to { {}* (i8*, i1*)*, i8* }*
  %43 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %42, i32 0, i32 0
  store {}* (i8*, i1*)* @print_cells363, {}* (i8*, i1*)** %43
  %44 = call i8* @GC_malloc(i64 0)
  %45 = bitcast i8* %44 to {}*
  %46 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %42, i32 0, i32 1
  store i8* %0, i8** %46
  %47 = call i8* @GC_malloc(i64 0)
  %48 = bitcast i8* %47 to {}*
  %49 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %50 = bitcast i8* %49 to { i64 (i8*, i1)*, i8* }*
  %51 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %50, i32 0, i32 0
  store i64 (i8*, i1)* @to_int362, i64 (i8*, i1)** %51
  %52 = call i8* @GC_malloc(i64 0)
  %53 = bitcast i8* %52 to {}*
  %54 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %50, i32 0, i32 1
  store i8* %0, i8** %54
  %55 = call i8* @GC_malloc(i64 0)
  %56 = bitcast i8* %55 to {}*
  %57 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %58 = bitcast i8* %57 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %59 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %58, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @next_state361, i1 (i8*, i1*, i64, i64)** %59
  %60 = call i8* @GC_malloc(i64 0)
  %61 = bitcast i8* %60 to {}*
  %62 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %58, i32 0, i32 1
  store i8* %0, i8** %62
  %63 = call i8* @GC_malloc(i64 0)
  %64 = bitcast i8* %63 to {}*
  %65 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %66 = bitcast i8* %65 to { {}* (i8*, i1*)*, i8* }*
  %67 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %66, i32 0, i32 0
  store {}* (i8*, i1*)* @update_cells358, {}* (i8*, i1*)** %67
  %68 = call i8* @GC_malloc(i64 0)
  %69 = bitcast i8* %68 to {}*
  %70 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %66, i32 0, i32 1
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
  %78 = call i1 @rand_bool373()
  %79 = getelementptr i1, i1* %1, i64 %2
  store i1 %78, i1* %79
  %80 = call i8* @GC_malloc(i64 0)
  %81 = bitcast i8* %80 to {}*
  %82 = add i64 %2, 1
  %83 = call {}* @init_cells367(i8* %0, i1* %1, i64 %82)
  store {}* %83, {}** %75
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %84 = load {}*, {}** %75
  ret {}* %84
}

define i8 @to_char366(i8*, i1) {
  %3 = bitcast i8* %0 to { i64, i64 }*
  %4 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 1
  %7 = load i64, i64* %6
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %10 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %9, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @view369, i1 (i8*, i1*, i64, i64)** %10
  %11 = call i8* @GC_malloc(i64 0)
  %12 = bitcast i8* %11 to {}*
  %13 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %13
  %14 = call i8* @GC_malloc(i64 0)
  %15 = bitcast i8* %14 to {}*
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { {}* (i8*, i1*, i64, i64, i1)*, i8* }*
  %18 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %17, i32 0, i32 0
  store {}* (i8*, i1*, i64, i64, i1)* @set368, {}* (i8*, i1*, i64, i64, i1)** %18
  %19 = call i8* @GC_malloc(i64 0)
  %20 = bitcast i8* %19 to {}*
  %21 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %17, i32 0, i32 1
  store i8* %0, i8** %21
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  %24 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %25 = bitcast i8* %24 to { {}* (i8*, i1*, i64)*, i8* }*
  %26 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %25, i32 0, i32 0
  store {}* (i8*, i1*, i64)* @init_cells367, {}* (i8*, i1*, i64)** %26
  %27 = call i8* @GC_malloc(i64 0)
  %28 = bitcast i8* %27 to {}*
  %29 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %25, i32 0, i32 1
  store i8* %0, i8** %29
  %30 = call i8* @GC_malloc(i64 0)
  %31 = bitcast i8* %30 to {}*
  %32 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %33 = bitcast i8* %32 to { i8 (i8*, i1)*, i8* }*
  %34 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %33, i32 0, i32 0
  store i8 (i8*, i1)* @to_char366, i8 (i8*, i1)** %34
  %35 = call i8* @GC_malloc(i64 0)
  %36 = bitcast i8* %35 to {}*
  %37 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %33, i32 0, i32 1
  store i8* %0, i8** %37
  %38 = call i8* @GC_malloc(i64 0)
  %39 = bitcast i8* %38 to {}*
  %40 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %41 = bitcast i8* %40 to { {}* (i8*, i1*)*, i8* }*
  %42 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %41, i32 0, i32 0
  store {}* (i8*, i1*)* @print_cells363, {}* (i8*, i1*)** %42
  %43 = call i8* @GC_malloc(i64 0)
  %44 = bitcast i8* %43 to {}*
  %45 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %41, i32 0, i32 1
  store i8* %0, i8** %45
  %46 = call i8* @GC_malloc(i64 0)
  %47 = bitcast i8* %46 to {}*
  %48 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %49 = bitcast i8* %48 to { i64 (i8*, i1)*, i8* }*
  %50 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %49, i32 0, i32 0
  store i64 (i8*, i1)* @to_int362, i64 (i8*, i1)** %50
  %51 = call i8* @GC_malloc(i64 0)
  %52 = bitcast i8* %51 to {}*
  %53 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %49, i32 0, i32 1
  store i8* %0, i8** %53
  %54 = call i8* @GC_malloc(i64 0)
  %55 = bitcast i8* %54 to {}*
  %56 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %57 = bitcast i8* %56 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %58 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %57, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @next_state361, i1 (i8*, i1*, i64, i64)** %58
  %59 = call i8* @GC_malloc(i64 0)
  %60 = bitcast i8* %59 to {}*
  %61 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %57, i32 0, i32 1
  store i8* %0, i8** %61
  %62 = call i8* @GC_malloc(i64 0)
  %63 = bitcast i8* %62 to {}*
  %64 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %65 = bitcast i8* %64 to { {}* (i8*, i1*)*, i8* }*
  %66 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %65, i32 0, i32 0
  store {}* (i8*, i1*)* @update_cells358, {}* (i8*, i1*)** %66
  %67 = call i8* @GC_malloc(i64 0)
  %68 = bitcast i8* %67 to {}*
  %69 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %65, i32 0, i32 1
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

define {}* @go_x365(i8*, i64) {
  %3 = bitcast i8* %0 to { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }*
  %4 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 0
  %5 = load i1*, i1** %4
  %6 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 1
  %7 = load { i8 (i8*, i1)*, i8* }*, { i8 (i8*, i1)*, i8* }** %6
  %8 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 2
  %9 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %8
  %10 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 3
  %11 = load i64, i64* %10
  %12 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 4
  %13 = load i64, i64* %12
  %14 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %15 = bitcast i8* %14 to { {}* (i8*, i64)*, i8* }*
  %16 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %15, i32 0, i32 0
  store {}* (i8*, i64)* @go_x365, {}* (i8*, i64)** %16
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
  %26 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %9, i32 0, i32 0
  %27 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %26
  %28 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %9, i32 0, i32 1
  %29 = load i8*, i8** %28
  %30 = call i1 %27(i8* %29, i1* %5, i64 %1, i64 %13)
  %31 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %7, i32 0, i32 0
  %32 = load i8 (i8*, i1)*, i8 (i8*, i1)** %31
  %33 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %7, i32 0, i32 1
  %34 = load i8*, i8** %33
  %35 = call i8 %32(i8* %34, i1 %30)
  %36 = call {}* @print_char376(i8 %35)
  %37 = add i64 %1, 1
  %38 = call {}* @go_x365(i8* %0, i64 %37)
  store {}* %38, {}** %23
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %39 = load {}*, {}** %23
  ret {}* %39
}

define {}* @go_y364(i8*, i64) {
  %3 = bitcast i8* %0 to { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }*
  %4 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 0
  %5 = load i1*, i1** %4
  %6 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 1
  %7 = load { i8 (i8*, i1)*, i8* }*, { i8 (i8*, i1)*, i8* }** %6
  %8 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 2
  %9 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %8
  %10 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 3
  %11 = load i64, i64* %10
  %12 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %3, i32 0, i32 4
  %13 = load i64, i64* %12
  %14 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %15 = bitcast i8* %14 to { {}* (i8*, i64)*, i8* }*
  %16 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %15, i32 0, i32 0
  store {}* (i8*, i64)* @go_y364, {}* (i8*, i64)** %16
  %17 = call i8* @GC_malloc(i64 0)
  %18 = bitcast i8* %17 to {}*
  %19 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %15, i32 0, i32 1
  store i8* %0, i8** %19
  %20 = call i8* @GC_malloc(i64 0)
  %21 = bitcast i8* %20 to {}*
  %22 = call i8* @GC_malloc(i64 ptrtoint ({ i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* getelementptr inbounds ({ i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %23 = bitcast i8* %22 to { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }*
  %24 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %23, i32 0, i32 0
  store i1* %5, i1** %24
  %25 = call i8* @GC_malloc(i64 0)
  %26 = bitcast i8* %25 to {}*
  %27 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %23, i32 0, i32 1
  store { i8 (i8*, i1)*, i8* }* %7, { i8 (i8*, i1)*, i8* }** %27
  %28 = call i8* @GC_malloc(i64 0)
  %29 = bitcast i8* %28 to {}*
  %30 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %23, i32 0, i32 2
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %9, { i1 (i8*, i1*, i64, i64)*, i8* }** %30
  %31 = call i8* @GC_malloc(i64 0)
  %32 = bitcast i8* %31 to {}*
  %33 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %23, i32 0, i32 3
  store i64 %11, i64* %33
  %34 = call i8* @GC_malloc(i64 0)
  %35 = bitcast i8* %34 to {}*
  %36 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %23, i32 0, i32 4
  store i64 %1, i64* %36
  %37 = call i8* @GC_malloc(i64 0)
  %38 = bitcast i8* %37 to {}*
  %39 = bitcast { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %23 to i8*
  %40 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %41 = bitcast i8* %40 to { {}* (i8*, i64)*, i8* }*
  %42 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %41, i32 0, i32 0
  store {}* (i8*, i64)* @go_x365, {}* (i8*, i64)** %42
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
  %57 = call {}* @newline375()
  %58 = add i64 %1, 1
  %59 = call {}* @go_y364(i8* %0, i64 %58)
  store {}* %59, {}** %49
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %60 = load {}*, {}** %49
  ret {}* %60
}

define {}* @print_cells363(i8*, i1*) {
  %3 = bitcast i8* %0 to { i64, i64 }*
  %4 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 1
  %7 = load i64, i64* %6
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %10 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %9, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @view369, i1 (i8*, i1*, i64, i64)** %10
  %11 = call i8* @GC_malloc(i64 0)
  %12 = bitcast i8* %11 to {}*
  %13 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %13
  %14 = call i8* @GC_malloc(i64 0)
  %15 = bitcast i8* %14 to {}*
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { {}* (i8*, i1*, i64, i64, i1)*, i8* }*
  %18 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %17, i32 0, i32 0
  store {}* (i8*, i1*, i64, i64, i1)* @set368, {}* (i8*, i1*, i64, i64, i1)** %18
  %19 = call i8* @GC_malloc(i64 0)
  %20 = bitcast i8* %19 to {}*
  %21 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %17, i32 0, i32 1
  store i8* %0, i8** %21
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  %24 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %25 = bitcast i8* %24 to { {}* (i8*, i1*, i64)*, i8* }*
  %26 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %25, i32 0, i32 0
  store {}* (i8*, i1*, i64)* @init_cells367, {}* (i8*, i1*, i64)** %26
  %27 = call i8* @GC_malloc(i64 0)
  %28 = bitcast i8* %27 to {}*
  %29 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %25, i32 0, i32 1
  store i8* %0, i8** %29
  %30 = call i8* @GC_malloc(i64 0)
  %31 = bitcast i8* %30 to {}*
  %32 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %33 = bitcast i8* %32 to { i8 (i8*, i1)*, i8* }*
  %34 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %33, i32 0, i32 0
  store i8 (i8*, i1)* @to_char366, i8 (i8*, i1)** %34
  %35 = call i8* @GC_malloc(i64 0)
  %36 = bitcast i8* %35 to {}*
  %37 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %33, i32 0, i32 1
  store i8* %0, i8** %37
  %38 = call i8* @GC_malloc(i64 0)
  %39 = bitcast i8* %38 to {}*
  %40 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %41 = bitcast i8* %40 to { {}* (i8*, i1*)*, i8* }*
  %42 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %41, i32 0, i32 0
  store {}* (i8*, i1*)* @print_cells363, {}* (i8*, i1*)** %42
  %43 = call i8* @GC_malloc(i64 0)
  %44 = bitcast i8* %43 to {}*
  %45 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %41, i32 0, i32 1
  store i8* %0, i8** %45
  %46 = call i8* @GC_malloc(i64 0)
  %47 = bitcast i8* %46 to {}*
  %48 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %49 = bitcast i8* %48 to { i64 (i8*, i1)*, i8* }*
  %50 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %49, i32 0, i32 0
  store i64 (i8*, i1)* @to_int362, i64 (i8*, i1)** %50
  %51 = call i8* @GC_malloc(i64 0)
  %52 = bitcast i8* %51 to {}*
  %53 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %49, i32 0, i32 1
  store i8* %0, i8** %53
  %54 = call i8* @GC_malloc(i64 0)
  %55 = bitcast i8* %54 to {}*
  %56 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %57 = bitcast i8* %56 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %58 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %57, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @next_state361, i1 (i8*, i1*, i64, i64)** %58
  %59 = call i8* @GC_malloc(i64 0)
  %60 = bitcast i8* %59 to {}*
  %61 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %57, i32 0, i32 1
  store i8* %0, i8** %61
  %62 = call i8* @GC_malloc(i64 0)
  %63 = bitcast i8* %62 to {}*
  %64 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %65 = bitcast i8* %64 to { {}* (i8*, i1*)*, i8* }*
  %66 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %65, i32 0, i32 0
  store {}* (i8*, i1*)* @update_cells358, {}* (i8*, i1*)** %66
  %67 = call i8* @GC_malloc(i64 0)
  %68 = bitcast i8* %67 to {}*
  %69 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %65, i32 0, i32 1
  store i8* %0, i8** %69
  %70 = call i8* @GC_malloc(i64 0)
  %71 = bitcast i8* %70 to {}*
  %72 = call i8* @GC_malloc(i64 ptrtoint ({ i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* getelementptr inbounds ({ i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %73 = bitcast i8* %72 to { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }*
  %74 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %73, i32 0, i32 0
  store i1* %1, i1** %74
  %75 = call i8* @GC_malloc(i64 0)
  %76 = bitcast i8* %75 to {}*
  %77 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %73, i32 0, i32 1
  store { i8 (i8*, i1)*, i8* }* %33, { i8 (i8*, i1)*, i8* }** %77
  %78 = call i8* @GC_malloc(i64 0)
  %79 = bitcast i8* %78 to {}*
  %80 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %73, i32 0, i32 2
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %9, { i1 (i8*, i1*, i64, i64)*, i8* }** %80
  %81 = call i8* @GC_malloc(i64 0)
  %82 = bitcast i8* %81 to {}*
  %83 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %73, i32 0, i32 3
  store i64 %5, i64* %83
  %84 = call i8* @GC_malloc(i64 0)
  %85 = bitcast i8* %84 to {}*
  %86 = getelementptr { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %73, i32 0, i32 4
  store i64 %7, i64* %86
  %87 = call i8* @GC_malloc(i64 0)
  %88 = bitcast i8* %87 to {}*
  %89 = bitcast { i1*, { i8 (i8*, i1)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %73 to i8*
  %90 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %91 = bitcast i8* %90 to { {}* (i8*, i64)*, i8* }*
  %92 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %91, i32 0, i32 0
  store {}* (i8*, i64)* @go_y364, {}* (i8*, i64)** %92
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

define i64 @to_int362(i8*, i1) {
  %3 = bitcast i8* %0 to { i64, i64 }*
  %4 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 1
  %7 = load i64, i64* %6
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %10 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %9, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @view369, i1 (i8*, i1*, i64, i64)** %10
  %11 = call i8* @GC_malloc(i64 0)
  %12 = bitcast i8* %11 to {}*
  %13 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %13
  %14 = call i8* @GC_malloc(i64 0)
  %15 = bitcast i8* %14 to {}*
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { {}* (i8*, i1*, i64, i64, i1)*, i8* }*
  %18 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %17, i32 0, i32 0
  store {}* (i8*, i1*, i64, i64, i1)* @set368, {}* (i8*, i1*, i64, i64, i1)** %18
  %19 = call i8* @GC_malloc(i64 0)
  %20 = bitcast i8* %19 to {}*
  %21 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %17, i32 0, i32 1
  store i8* %0, i8** %21
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  %24 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %25 = bitcast i8* %24 to { {}* (i8*, i1*, i64)*, i8* }*
  %26 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %25, i32 0, i32 0
  store {}* (i8*, i1*, i64)* @init_cells367, {}* (i8*, i1*, i64)** %26
  %27 = call i8* @GC_malloc(i64 0)
  %28 = bitcast i8* %27 to {}*
  %29 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %25, i32 0, i32 1
  store i8* %0, i8** %29
  %30 = call i8* @GC_malloc(i64 0)
  %31 = bitcast i8* %30 to {}*
  %32 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %33 = bitcast i8* %32 to { i8 (i8*, i1)*, i8* }*
  %34 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %33, i32 0, i32 0
  store i8 (i8*, i1)* @to_char366, i8 (i8*, i1)** %34
  %35 = call i8* @GC_malloc(i64 0)
  %36 = bitcast i8* %35 to {}*
  %37 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %33, i32 0, i32 1
  store i8* %0, i8** %37
  %38 = call i8* @GC_malloc(i64 0)
  %39 = bitcast i8* %38 to {}*
  %40 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %41 = bitcast i8* %40 to { {}* (i8*, i1*)*, i8* }*
  %42 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %41, i32 0, i32 0
  store {}* (i8*, i1*)* @print_cells363, {}* (i8*, i1*)** %42
  %43 = call i8* @GC_malloc(i64 0)
  %44 = bitcast i8* %43 to {}*
  %45 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %41, i32 0, i32 1
  store i8* %0, i8** %45
  %46 = call i8* @GC_malloc(i64 0)
  %47 = bitcast i8* %46 to {}*
  %48 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %49 = bitcast i8* %48 to { i64 (i8*, i1)*, i8* }*
  %50 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %49, i32 0, i32 0
  store i64 (i8*, i1)* @to_int362, i64 (i8*, i1)** %50
  %51 = call i8* @GC_malloc(i64 0)
  %52 = bitcast i8* %51 to {}*
  %53 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %49, i32 0, i32 1
  store i8* %0, i8** %53
  %54 = call i8* @GC_malloc(i64 0)
  %55 = bitcast i8* %54 to {}*
  %56 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %57 = bitcast i8* %56 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %58 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %57, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @next_state361, i1 (i8*, i1*, i64, i64)** %58
  %59 = call i8* @GC_malloc(i64 0)
  %60 = bitcast i8* %59 to {}*
  %61 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %57, i32 0, i32 1
  store i8* %0, i8** %61
  %62 = call i8* @GC_malloc(i64 0)
  %63 = bitcast i8* %62 to {}*
  %64 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %65 = bitcast i8* %64 to { {}* (i8*, i1*)*, i8* }*
  %66 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %65, i32 0, i32 0
  store {}* (i8*, i1*)* @update_cells358, {}* (i8*, i1*)** %66
  %67 = call i8* @GC_malloc(i64 0)
  %68 = bitcast i8* %67 to {}*
  %69 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %65, i32 0, i32 1
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

define i1 @next_state361(i8*, i1*, i64, i64) {
  %5 = bitcast i8* %0 to { i64, i64 }*
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %5, i32 0, i32 0
  %7 = load i64, i64* %6
  %8 = getelementptr { i64, i64 }, { i64, i64 }* %5, i32 0, i32 1
  %9 = load i64, i64* %8
  %10 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %11 = bitcast i8* %10 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %12 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %11, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @view369, i1 (i8*, i1*, i64, i64)** %12
  %13 = call i8* @GC_malloc(i64 0)
  %14 = bitcast i8* %13 to {}*
  %15 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %0, i8** %15
  %16 = call i8* @GC_malloc(i64 0)
  %17 = bitcast i8* %16 to {}*
  %18 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %19 = bitcast i8* %18 to { {}* (i8*, i1*, i64, i64, i1)*, i8* }*
  %20 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %19, i32 0, i32 0
  store {}* (i8*, i1*, i64, i64, i1)* @set368, {}* (i8*, i1*, i64, i64, i1)** %20
  %21 = call i8* @GC_malloc(i64 0)
  %22 = bitcast i8* %21 to {}*
  %23 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %19, i32 0, i32 1
  store i8* %0, i8** %23
  %24 = call i8* @GC_malloc(i64 0)
  %25 = bitcast i8* %24 to {}*
  %26 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %27 = bitcast i8* %26 to { {}* (i8*, i1*, i64)*, i8* }*
  %28 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %27, i32 0, i32 0
  store {}* (i8*, i1*, i64)* @init_cells367, {}* (i8*, i1*, i64)** %28
  %29 = call i8* @GC_malloc(i64 0)
  %30 = bitcast i8* %29 to {}*
  %31 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %27, i32 0, i32 1
  store i8* %0, i8** %31
  %32 = call i8* @GC_malloc(i64 0)
  %33 = bitcast i8* %32 to {}*
  %34 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %35 = bitcast i8* %34 to { i8 (i8*, i1)*, i8* }*
  %36 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %35, i32 0, i32 0
  store i8 (i8*, i1)* @to_char366, i8 (i8*, i1)** %36
  %37 = call i8* @GC_malloc(i64 0)
  %38 = bitcast i8* %37 to {}*
  %39 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %35, i32 0, i32 1
  store i8* %0, i8** %39
  %40 = call i8* @GC_malloc(i64 0)
  %41 = bitcast i8* %40 to {}*
  %42 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %43 = bitcast i8* %42 to { {}* (i8*, i1*)*, i8* }*
  %44 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %43, i32 0, i32 0
  store {}* (i8*, i1*)* @print_cells363, {}* (i8*, i1*)** %44
  %45 = call i8* @GC_malloc(i64 0)
  %46 = bitcast i8* %45 to {}*
  %47 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %43, i32 0, i32 1
  store i8* %0, i8** %47
  %48 = call i8* @GC_malloc(i64 0)
  %49 = bitcast i8* %48 to {}*
  %50 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %51 = bitcast i8* %50 to { i64 (i8*, i1)*, i8* }*
  %52 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %51, i32 0, i32 0
  store i64 (i8*, i1)* @to_int362, i64 (i8*, i1)** %52
  %53 = call i8* @GC_malloc(i64 0)
  %54 = bitcast i8* %53 to {}*
  %55 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %51, i32 0, i32 1
  store i8* %0, i8** %55
  %56 = call i8* @GC_malloc(i64 0)
  %57 = bitcast i8* %56 to {}*
  %58 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %59 = bitcast i8* %58 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %60 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %59, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @next_state361, i1 (i8*, i1*, i64, i64)** %60
  %61 = call i8* @GC_malloc(i64 0)
  %62 = bitcast i8* %61 to {}*
  %63 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %59, i32 0, i32 1
  store i8* %0, i8** %63
  %64 = call i8* @GC_malloc(i64 0)
  %65 = bitcast i8* %64 to {}*
  %66 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %67 = bitcast i8* %66 to { {}* (i8*, i1*)*, i8* }*
  %68 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %67, i32 0, i32 0
  store {}* (i8*, i1*)* @update_cells358, {}* (i8*, i1*)** %68
  %69 = call i8* @GC_malloc(i64 0)
  %70 = bitcast i8* %69 to {}*
  %71 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %67, i32 0, i32 1
  store i8* %0, i8** %71
  %72 = call i8* @GC_malloc(i64 0)
  %73 = bitcast i8* %72 to {}*
  %74 = mul i64 ptrtoint (i64* getelementptr inbounds (i64, i64* null, i32 1) to i64), 1
  %75 = call i8* @GC_malloc(i64 %74)
  %76 = bitcast i8* %75 to i64*
  %77 = alloca i64
  store i64 0, i64* %77
  br label %cond_0

cond_0:                                           ; preds = %copyelem_0, %4
  %78 = load i64, i64* %77
  %79 = icmp slt i64 %78, 1
  br i1 %79, label %copyelem_0, label %end_0

copyelem_0:                                       ; preds = %cond_0
  %80 = getelementptr i64, i64* %76, i64 %78
  store i64 0, i64* %80
  %81 = add i64 %78, 1
  store i64 %81, i64* %77
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %82 = sub i64 %2, 1
  %83 = add i64 %3, 1
  %84 = call i1 @view369(i8* %0, i1* %1, i64 %82, i64 %83)
  %85 = call i64 @to_int362(i8* %0, i1 %84)
  %86 = add i64 %3, 1
  %87 = call i1 @view369(i8* %0, i1* %1, i64 %2, i64 %86)
  %88 = call i64 @to_int362(i8* %0, i1 %87)
  %89 = add i64 %85, %88
  %90 = add i64 %2, 1
  %91 = add i64 %3, 1
  %92 = call i1 @view369(i8* %0, i1* %1, i64 %90, i64 %91)
  %93 = call i64 @to_int362(i8* %0, i1 %92)
  %94 = add i64 %89, %93
  %95 = getelementptr i64, i64* %76, i64 0
  store i64 %94, i64* %95
  %96 = call i8* @GC_malloc(i64 0)
  %97 = bitcast i8* %96 to {}*
  %98 = sub i64 %2, 1
  %99 = call i1 @view369(i8* %0, i1* %1, i64 %98, i64 %3)
  %100 = call i64 @to_int362(i8* %0, i1 %99)
  %101 = add i64 %2, 1
  %102 = call i1 @view369(i8* %0, i1* %1, i64 %101, i64 %3)
  %103 = call i64 @to_int362(i8* %0, i1 %102)
  %104 = add i64 %100, %103
  %105 = getelementptr i64, i64* %76, i64 0
  %106 = load i64, i64* %105
  %107 = add i64 %104, %106
  %108 = getelementptr i64, i64* %76, i64 0
  store i64 %107, i64* %108
  %109 = call i8* @GC_malloc(i64 0)
  %110 = bitcast i8* %109 to {}*
  %111 = sub i64 %2, 1
  %112 = sub i64 %3, 1
  %113 = call i1 @view369(i8* %0, i1* %1, i64 %111, i64 %112)
  %114 = call i64 @to_int362(i8* %0, i1 %113)
  %115 = sub i64 %3, 1
  %116 = call i1 @view369(i8* %0, i1* %1, i64 %2, i64 %115)
  %117 = call i64 @to_int362(i8* %0, i1 %116)
  %118 = add i64 %114, %117
  %119 = add i64 %2, 1
  %120 = sub i64 %3, 1
  %121 = call i1 @view369(i8* %0, i1* %1, i64 %119, i64 %120)
  %122 = call i64 @to_int362(i8* %0, i1 %121)
  %123 = add i64 %118, %122
  %124 = getelementptr i64, i64* %76, i64 0
  %125 = load i64, i64* %124
  %126 = add i64 %123, %125
  %127 = getelementptr i64, i64* %76, i64 0
  store i64 %126, i64* %127
  %128 = call i8* @GC_malloc(i64 0)
  %129 = bitcast i8* %128 to {}*
  %130 = call i1 @view369(i8* %0, i1* %1, i64 %2, i64 %3)
  %131 = alloca i1
  br i1 %130, label %then_0, label %else_0

then_0:                                           ; preds = %end_0
  %132 = getelementptr i64, i64* %76, i64 0
  %133 = load i64, i64* %132
  %134 = icmp eq i64 %133, 2
  %135 = getelementptr i64, i64* %76, i64 0
  %136 = load i64, i64* %135
  %137 = icmp eq i64 %136, 3
  %138 = or i1 %134, %137
  store i1 %138, i1* %131
  br label %end_1

else_0:                                           ; preds = %end_0
  %139 = getelementptr i64, i64* %76, i64 0
  %140 = load i64, i64* %139
  %141 = icmp eq i64 %140, 3
  store i1 %141, i1* %131
  br label %end_1

end_1:                                            ; preds = %else_0, %then_0
  %142 = load i1, i1* %131
  ret i1 %142
}

define {}* @go_x360(i8*, i64) {
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
  store {}* (i8*, i64)* @go_x360, {}* (i8*, i64)** %18
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
  %28 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %9, i32 0, i32 0
  %29 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %28
  %30 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %9, i32 0, i32 1
  %31 = load i8*, i8** %30
  %32 = call i1 %29(i8* %31, i1* %7, i64 %1, i64 %15)
  %33 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %11, i32 0, i32 0
  %34 = load {}* (i8*, i1*, i64, i64, i1)*, {}* (i8*, i1*, i64, i64, i1)** %33
  %35 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %11, i32 0, i32 1
  %36 = load i8*, i8** %35
  %37 = call {}* %34(i8* %36, i1* %5, i64 %1, i64 %15, i1 %32)
  %38 = add i64 %1, 1
  %39 = call {}* @go_x360(i8* %0, i64 %38)
  store {}* %39, {}** %25
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %40 = load {}*, {}** %25
  ret {}* %40
}

define {}* @go_y359(i8*, i64) {
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
  store {}* (i8*, i64)* @go_y359, {}* (i8*, i64)** %18
  %19 = call i8* @GC_malloc(i64 0)
  %20 = bitcast i8* %19 to {}*
  %21 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %17, i32 0, i32 1
  store i8* %0, i8** %21
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  %24 = call i8* @GC_malloc(i64 ptrtoint ({ i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* getelementptr inbounds ({ i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %25 = bitcast i8* %24 to { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %26 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %25, i32 0, i32 0
  store i1* %5, i1** %26
  %27 = call i8* @GC_malloc(i64 0)
  %28 = bitcast i8* %27 to {}*
  %29 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %25, i32 0, i32 1
  store i1* %7, i1** %29
  %30 = call i8* @GC_malloc(i64 0)
  %31 = bitcast i8* %30 to {}*
  %32 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %25, i32 0, i32 2
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %9, { i1 (i8*, i1*, i64, i64)*, i8* }** %32
  %33 = call i8* @GC_malloc(i64 0)
  %34 = bitcast i8* %33 to {}*
  %35 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %25, i32 0, i32 3
  store { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %11, { {}* (i8*, i1*, i64, i64, i1)*, i8* }** %35
  %36 = call i8* @GC_malloc(i64 0)
  %37 = bitcast i8* %36 to {}*
  %38 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %25, i32 0, i32 4
  store i64 %13, i64* %38
  %39 = call i8* @GC_malloc(i64 0)
  %40 = bitcast i8* %39 to {}*
  %41 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %25, i32 0, i32 5
  store i64 %1, i64* %41
  %42 = call i8* @GC_malloc(i64 0)
  %43 = bitcast i8* %42 to {}*
  %44 = bitcast { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %25 to i8*
  %45 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %46 = bitcast i8* %45 to { {}* (i8*, i64)*, i8* }*
  %47 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %46, i32 0, i32 0
  store {}* (i8*, i64)* @go_x360, {}* (i8*, i64)** %47
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
  %63 = call {}* @go_y359(i8* %0, i64 %62)
  store {}* %63, {}** %54
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %64 = load {}*, {}** %54
  ret {}* %64
}

define {}* @update_cells358(i8*, i1*) {
  %3 = bitcast i8* %0 to { i64, i64 }*
  %4 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %3, i32 0, i32 1
  %7 = load i64, i64* %6
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %10 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %9, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @view369, i1 (i8*, i1*, i64, i64)** %10
  %11 = call i8* @GC_malloc(i64 0)
  %12 = bitcast i8* %11 to {}*
  %13 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %13
  %14 = call i8* @GC_malloc(i64 0)
  %15 = bitcast i8* %14 to {}*
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { {}* (i8*, i1*, i64, i64, i1)*, i8* }*
  %18 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %17, i32 0, i32 0
  store {}* (i8*, i1*, i64, i64, i1)* @set368, {}* (i8*, i1*, i64, i64, i1)** %18
  %19 = call i8* @GC_malloc(i64 0)
  %20 = bitcast i8* %19 to {}*
  %21 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %17, i32 0, i32 1
  store i8* %0, i8** %21
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  %24 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %25 = bitcast i8* %24 to { {}* (i8*, i1*, i64)*, i8* }*
  %26 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %25, i32 0, i32 0
  store {}* (i8*, i1*, i64)* @init_cells367, {}* (i8*, i1*, i64)** %26
  %27 = call i8* @GC_malloc(i64 0)
  %28 = bitcast i8* %27 to {}*
  %29 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %25, i32 0, i32 1
  store i8* %0, i8** %29
  %30 = call i8* @GC_malloc(i64 0)
  %31 = bitcast i8* %30 to {}*
  %32 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %33 = bitcast i8* %32 to { i8 (i8*, i1)*, i8* }*
  %34 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %33, i32 0, i32 0
  store i8 (i8*, i1)* @to_char366, i8 (i8*, i1)** %34
  %35 = call i8* @GC_malloc(i64 0)
  %36 = bitcast i8* %35 to {}*
  %37 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %33, i32 0, i32 1
  store i8* %0, i8** %37
  %38 = call i8* @GC_malloc(i64 0)
  %39 = bitcast i8* %38 to {}*
  %40 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %41 = bitcast i8* %40 to { {}* (i8*, i1*)*, i8* }*
  %42 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %41, i32 0, i32 0
  store {}* (i8*, i1*)* @print_cells363, {}* (i8*, i1*)** %42
  %43 = call i8* @GC_malloc(i64 0)
  %44 = bitcast i8* %43 to {}*
  %45 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %41, i32 0, i32 1
  store i8* %0, i8** %45
  %46 = call i8* @GC_malloc(i64 0)
  %47 = bitcast i8* %46 to {}*
  %48 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %49 = bitcast i8* %48 to { i64 (i8*, i1)*, i8* }*
  %50 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %49, i32 0, i32 0
  store i64 (i8*, i1)* @to_int362, i64 (i8*, i1)** %50
  %51 = call i8* @GC_malloc(i64 0)
  %52 = bitcast i8* %51 to {}*
  %53 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %49, i32 0, i32 1
  store i8* %0, i8** %53
  %54 = call i8* @GC_malloc(i64 0)
  %55 = bitcast i8* %54 to {}*
  %56 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %57 = bitcast i8* %56 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %58 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %57, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @next_state361, i1 (i8*, i1*, i64, i64)** %58
  %59 = call i8* @GC_malloc(i64 0)
  %60 = bitcast i8* %59 to {}*
  %61 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %57, i32 0, i32 1
  store i8* %0, i8** %61
  %62 = call i8* @GC_malloc(i64 0)
  %63 = bitcast i8* %62 to {}*
  %64 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %65 = bitcast i8* %64 to { {}* (i8*, i1*)*, i8* }*
  %66 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %65, i32 0, i32 0
  store {}* (i8*, i1*)* @update_cells358, {}* (i8*, i1*)** %66
  %67 = call i8* @GC_malloc(i64 0)
  %68 = bitcast i8* %67 to {}*
  %69 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %65, i32 0, i32 1
  store i8* %0, i8** %69
  %70 = call i8* @GC_malloc(i64 0)
  %71 = bitcast i8* %70 to {}*
  %72 = mul i64 %5, %7
  %73 = call i1* @copy_bool_array371(i1* %1, i64 %72)
  %74 = call i8* @GC_malloc(i64 ptrtoint ({ i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* getelementptr inbounds ({ i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %75 = bitcast i8* %74 to { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %76 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %75, i32 0, i32 0
  store i1* %1, i1** %76
  %77 = call i8* @GC_malloc(i64 0)
  %78 = bitcast i8* %77 to {}*
  %79 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %75, i32 0, i32 1
  store i1* %73, i1** %79
  %80 = call i8* @GC_malloc(i64 0)
  %81 = bitcast i8* %80 to {}*
  %82 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %75, i32 0, i32 2
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %57, { i1 (i8*, i1*, i64, i64)*, i8* }** %82
  %83 = call i8* @GC_malloc(i64 0)
  %84 = bitcast i8* %83 to {}*
  %85 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %75, i32 0, i32 3
  store { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %17, { {}* (i8*, i1*, i64, i64, i1)*, i8* }** %85
  %86 = call i8* @GC_malloc(i64 0)
  %87 = bitcast i8* %86 to {}*
  %88 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %75, i32 0, i32 4
  store i64 %5, i64* %88
  %89 = call i8* @GC_malloc(i64 0)
  %90 = bitcast i8* %89 to {}*
  %91 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %75, i32 0, i32 5
  store i64 %7, i64* %91
  %92 = call i8* @GC_malloc(i64 0)
  %93 = bitcast i8* %92 to {}*
  %94 = bitcast { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %75 to i8*
  %95 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %96 = bitcast i8* %95 to { {}* (i8*, i64)*, i8* }*
  %97 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %96, i32 0, i32 0
  store {}* (i8*, i64)* @go_y359, {}* (i8*, i64)** %97
  %98 = call i8* @GC_malloc(i64 0)
  %99 = bitcast i8* %98 to {}*
  %100 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %96, i32 0, i32 1
  store i8* %94, i8** %100
  %101 = call i8* @GC_malloc(i64 0)
  %102 = bitcast i8* %101 to {}*
  %103 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %96, i32 0, i32 0
  %104 = load {}* (i8*, i64)*, {}* (i8*, i64)** %103
  %105 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %96, i32 0, i32 1
  %106 = load i8*, i8** %105
  %107 = call {}* %104(i8* %106, i64 0)
  ret {}* %107
}

define {}* @loop357(i8*, i64) {
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
  store {}* (i8*, i64)* @loop357, {}* (i8*, i64)** %12
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
  %20 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %7, i32 0, i32 0
  %21 = load {}* (i8*, i1*)*, {}* (i8*, i1*)** %20
  %22 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %7, i32 0, i32 1
  %23 = load i8*, i8** %22
  %24 = call {}* %21(i8* %23, i1* %5)
  %25 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %9, i32 0, i32 0
  %26 = load {}* (i8*, i1*)*, {}* (i8*, i1*)** %25
  %27 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %9, i32 0, i32 1
  %28 = load i8*, i8** %27
  %29 = call {}* %26(i8* %28, i1* %5)
  %30 = call {}* @newline375()
  %31 = sub i64 %1, 1
  %32 = call {}* @loop357(i8* %0, i64 %31)
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
  %5 = alloca i64
  store i64 0, i64* %5
  br label %cond_0

cond_0:                                           ; preds = %copyelem_0, %0
  %6 = load i64, i64* %5
  %7 = icmp slt i64 %6, %1
  br i1 %7, label %copyelem_0, label %end_0

copyelem_0:                                       ; preds = %cond_0
  %8 = getelementptr i1, i1* %4, i64 %6
  store i1 false, i1* %8
  %9 = add i64 %6, 1
  store i64 %9, i64* %5
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %10 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %11 = bitcast i8* %10 to { i64, i64 }*
  %12 = getelementptr { i64, i64 }, { i64, i64 }* %11, i32 0, i32 0
  store i64 50, i64* %12
  %13 = call i8* @GC_malloc(i64 0)
  %14 = bitcast i8* %13 to {}*
  %15 = getelementptr { i64, i64 }, { i64, i64 }* %11, i32 0, i32 1
  store i64 20, i64* %15
  %16 = call i8* @GC_malloc(i64 0)
  %17 = bitcast i8* %16 to {}*
  %18 = bitcast { i64, i64 }* %11 to i8*
  %19 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %20 = bitcast i8* %19 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %21 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %20, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @view369, i1 (i8*, i1*, i64, i64)** %21
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  %24 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %20, i32 0, i32 1
  store i8* %18, i8** %24
  %25 = call i8* @GC_malloc(i64 0)
  %26 = bitcast i8* %25 to {}*
  %27 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %28 = bitcast i8* %27 to { i64, i64 }*
  %29 = getelementptr { i64, i64 }, { i64, i64 }* %28, i32 0, i32 0
  store i64 50, i64* %29
  %30 = call i8* @GC_malloc(i64 0)
  %31 = bitcast i8* %30 to {}*
  %32 = getelementptr { i64, i64 }, { i64, i64 }* %28, i32 0, i32 1
  store i64 20, i64* %32
  %33 = call i8* @GC_malloc(i64 0)
  %34 = bitcast i8* %33 to {}*
  %35 = bitcast { i64, i64 }* %28 to i8*
  %36 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %37 = bitcast i8* %36 to { {}* (i8*, i1*, i64, i64, i1)*, i8* }*
  %38 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %37, i32 0, i32 0
  store {}* (i8*, i1*, i64, i64, i1)* @set368, {}* (i8*, i1*, i64, i64, i1)** %38
  %39 = call i8* @GC_malloc(i64 0)
  %40 = bitcast i8* %39 to {}*
  %41 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %37, i32 0, i32 1
  store i8* %35, i8** %41
  %42 = call i8* @GC_malloc(i64 0)
  %43 = bitcast i8* %42 to {}*
  %44 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %45 = bitcast i8* %44 to { i64, i64 }*
  %46 = getelementptr { i64, i64 }, { i64, i64 }* %45, i32 0, i32 0
  store i64 50, i64* %46
  %47 = call i8* @GC_malloc(i64 0)
  %48 = bitcast i8* %47 to {}*
  %49 = getelementptr { i64, i64 }, { i64, i64 }* %45, i32 0, i32 1
  store i64 20, i64* %49
  %50 = call i8* @GC_malloc(i64 0)
  %51 = bitcast i8* %50 to {}*
  %52 = bitcast { i64, i64 }* %45 to i8*
  %53 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %54 = bitcast i8* %53 to { {}* (i8*, i1*, i64)*, i8* }*
  %55 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %54, i32 0, i32 0
  store {}* (i8*, i1*, i64)* @init_cells367, {}* (i8*, i1*, i64)** %55
  %56 = call i8* @GC_malloc(i64 0)
  %57 = bitcast i8* %56 to {}*
  %58 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %54, i32 0, i32 1
  store i8* %52, i8** %58
  %59 = call i8* @GC_malloc(i64 0)
  %60 = bitcast i8* %59 to {}*
  %61 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %62 = bitcast i8* %61 to { i64, i64 }*
  %63 = getelementptr { i64, i64 }, { i64, i64 }* %62, i32 0, i32 0
  store i64 50, i64* %63
  %64 = call i8* @GC_malloc(i64 0)
  %65 = bitcast i8* %64 to {}*
  %66 = getelementptr { i64, i64 }, { i64, i64 }* %62, i32 0, i32 1
  store i64 20, i64* %66
  %67 = call i8* @GC_malloc(i64 0)
  %68 = bitcast i8* %67 to {}*
  %69 = bitcast { i64, i64 }* %62 to i8*
  %70 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %71 = bitcast i8* %70 to { i8 (i8*, i1)*, i8* }*
  %72 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %71, i32 0, i32 0
  store i8 (i8*, i1)* @to_char366, i8 (i8*, i1)** %72
  %73 = call i8* @GC_malloc(i64 0)
  %74 = bitcast i8* %73 to {}*
  %75 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %71, i32 0, i32 1
  store i8* %69, i8** %75
  %76 = call i8* @GC_malloc(i64 0)
  %77 = bitcast i8* %76 to {}*
  %78 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %79 = bitcast i8* %78 to { i64, i64 }*
  %80 = getelementptr { i64, i64 }, { i64, i64 }* %79, i32 0, i32 0
  store i64 50, i64* %80
  %81 = call i8* @GC_malloc(i64 0)
  %82 = bitcast i8* %81 to {}*
  %83 = getelementptr { i64, i64 }, { i64, i64 }* %79, i32 0, i32 1
  store i64 20, i64* %83
  %84 = call i8* @GC_malloc(i64 0)
  %85 = bitcast i8* %84 to {}*
  %86 = bitcast { i64, i64 }* %79 to i8*
  %87 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %88 = bitcast i8* %87 to { {}* (i8*, i1*)*, i8* }*
  %89 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %88, i32 0, i32 0
  store {}* (i8*, i1*)* @print_cells363, {}* (i8*, i1*)** %89
  %90 = call i8* @GC_malloc(i64 0)
  %91 = bitcast i8* %90 to {}*
  %92 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %88, i32 0, i32 1
  store i8* %86, i8** %92
  %93 = call i8* @GC_malloc(i64 0)
  %94 = bitcast i8* %93 to {}*
  %95 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %96 = bitcast i8* %95 to { i64, i64 }*
  %97 = getelementptr { i64, i64 }, { i64, i64 }* %96, i32 0, i32 0
  store i64 50, i64* %97
  %98 = call i8* @GC_malloc(i64 0)
  %99 = bitcast i8* %98 to {}*
  %100 = getelementptr { i64, i64 }, { i64, i64 }* %96, i32 0, i32 1
  store i64 20, i64* %100
  %101 = call i8* @GC_malloc(i64 0)
  %102 = bitcast i8* %101 to {}*
  %103 = bitcast { i64, i64 }* %96 to i8*
  %104 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %105 = bitcast i8* %104 to { i64 (i8*, i1)*, i8* }*
  %106 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %105, i32 0, i32 0
  store i64 (i8*, i1)* @to_int362, i64 (i8*, i1)** %106
  %107 = call i8* @GC_malloc(i64 0)
  %108 = bitcast i8* %107 to {}*
  %109 = getelementptr { i64 (i8*, i1)*, i8* }, { i64 (i8*, i1)*, i8* }* %105, i32 0, i32 1
  store i8* %103, i8** %109
  %110 = call i8* @GC_malloc(i64 0)
  %111 = bitcast i8* %110 to {}*
  %112 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %113 = bitcast i8* %112 to { i64, i64 }*
  %114 = getelementptr { i64, i64 }, { i64, i64 }* %113, i32 0, i32 0
  store i64 50, i64* %114
  %115 = call i8* @GC_malloc(i64 0)
  %116 = bitcast i8* %115 to {}*
  %117 = getelementptr { i64, i64 }, { i64, i64 }* %113, i32 0, i32 1
  store i64 20, i64* %117
  %118 = call i8* @GC_malloc(i64 0)
  %119 = bitcast i8* %118 to {}*
  %120 = bitcast { i64, i64 }* %113 to i8*
  %121 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %122 = bitcast i8* %121 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %123 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %122, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @next_state361, i1 (i8*, i1*, i64, i64)** %123
  %124 = call i8* @GC_malloc(i64 0)
  %125 = bitcast i8* %124 to {}*
  %126 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %122, i32 0, i32 1
  store i8* %120, i8** %126
  %127 = call i8* @GC_malloc(i64 0)
  %128 = bitcast i8* %127 to {}*
  %129 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %130 = bitcast i8* %129 to { i64, i64 }*
  %131 = getelementptr { i64, i64 }, { i64, i64 }* %130, i32 0, i32 0
  store i64 50, i64* %131
  %132 = call i8* @GC_malloc(i64 0)
  %133 = bitcast i8* %132 to {}*
  %134 = getelementptr { i64, i64 }, { i64, i64 }* %130, i32 0, i32 1
  store i64 20, i64* %134
  %135 = call i8* @GC_malloc(i64 0)
  %136 = bitcast i8* %135 to {}*
  %137 = bitcast { i64, i64 }* %130 to i8*
  %138 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %139 = bitcast i8* %138 to { {}* (i8*, i1*)*, i8* }*
  %140 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %139, i32 0, i32 0
  store {}* (i8*, i1*)* @update_cells358, {}* (i8*, i1*)** %140
  %141 = call i8* @GC_malloc(i64 0)
  %142 = bitcast i8* %141 to {}*
  %143 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %139, i32 0, i32 1
  store i8* %137, i8** %143
  %144 = call i8* @GC_malloc(i64 0)
  %145 = bitcast i8* %144 to {}*
  %146 = call {}* @gen_seed374()
  %147 = call {}* @pulsar370(i1* %4)
  %148 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 3))
  %149 = bitcast i8* %148 to { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }*
  %150 = getelementptr { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }, { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }* %149, i32 0, i32 0
  store i1* %4, i1** %150
  %151 = call i8* @GC_malloc(i64 0)
  %152 = bitcast i8* %151 to {}*
  %153 = getelementptr { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }, { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }* %149, i32 0, i32 1
  store { {}* (i8*, i1*)*, i8* }* %88, { {}* (i8*, i1*)*, i8* }** %153
  %154 = call i8* @GC_malloc(i64 0)
  %155 = bitcast i8* %154 to {}*
  %156 = getelementptr { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }, { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }* %149, i32 0, i32 2
  store { {}* (i8*, i1*)*, i8* }* %139, { {}* (i8*, i1*)*, i8* }** %156
  %157 = call i8* @GC_malloc(i64 0)
  %158 = bitcast i8* %157 to {}*
  %159 = bitcast { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }* %149 to i8*
  %160 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %161 = bitcast i8* %160 to { {}* (i8*, i64)*, i8* }*
  %162 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %161, i32 0, i32 0
  store {}* (i8*, i64)* @loop357, {}* (i8*, i64)** %162
  %163 = call i8* @GC_malloc(i64 0)
  %164 = bitcast i8* %163 to {}*
  %165 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %161, i32 0, i32 1
  store i8* %159, i8** %165
  %166 = call i8* @GC_malloc(i64 0)
  %167 = bitcast i8* %166 to {}*
  %168 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %161, i32 0, i32 0
  %169 = load {}* (i8*, i64)*, {}* (i8*, i64)** %168
  %170 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %161, i32 0, i32 1
  %171 = load i8*, i8** %170
  %172 = call {}* %169(i8* %171, i64 10)
  ret i32 0
}
