; ModuleID = './examples/life.mlg'
source_filename = "./examples/life.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

declare i64 @mul_i64(i64, i64)

define {} @main.1305() {
  %1 = call i64 @mul_i64(i64 50, i64 20)
  %2 = getelementptr i1, i1* null, i64 1
  %3 = ptrtoint i1* %2 to i64
  %4 = mul i64 %1, %3
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to i1*
  %7 = getelementptr { i64, i64 }, { i64, i64 }* null, i64 1
  %8 = ptrtoint { i64, i64 }* %7 to i64
  %9 = call i8* @malloc_gc(i64 %8)
  %10 = bitcast i8* %9 to { i64, i64 }*
  %11 = getelementptr { i64, i64 }, { i64, i64 }* %10, i32 0, i32 0
  store i64 50, i64* %11
  %12 = getelementptr { i64, i64 }, { i64, i64 }* %10, i32 0, i32 1
  store i64 20, i64* %12
  %13 = bitcast { i64, i64 }* %10 to i8*
  %14 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* null, i64 1
  %15 = ptrtoint { i1 (i8*, i1*, i64, i64)*, i8* }* %14 to i64
  %16 = call i8* @malloc_gc(i64 %15)
  %17 = bitcast i8* %16 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %18 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %17, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @view.755, i1 (i8*, i1*, i64, i64)** %18
  %19 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %17, i32 0, i32 1
  store i8* %13, i8** %19
  %20 = getelementptr { i64 }, { i64 }* null, i64 1
  %21 = ptrtoint { i64 }* %20 to i64
  %22 = call i8* @malloc_gc(i64 %21)
  %23 = bitcast i8* %22 to { i64 }*
  %24 = getelementptr { i64 }, { i64 }* %23, i32 0, i32 0
  store i64 50, i64* %24
  %25 = bitcast { i64 }* %23 to i8*
  %26 = getelementptr { {} (i8*, i1*, i64, i64, i1)*, i8* }, { {} (i8*, i1*, i64, i64, i1)*, i8* }* null, i64 1
  %27 = ptrtoint { {} (i8*, i1*, i64, i64, i1)*, i8* }* %26 to i64
  %28 = call i8* @malloc_gc(i64 %27)
  %29 = bitcast i8* %28 to { {} (i8*, i1*, i64, i64, i1)*, i8* }*
  %30 = getelementptr { {} (i8*, i1*, i64, i64, i1)*, i8* }, { {} (i8*, i1*, i64, i64, i1)*, i8* }* %29, i32 0, i32 0
  store {} (i8*, i1*, i64, i64, i1)* @set.779, {} (i8*, i1*, i64, i64, i1)** %30
  %31 = getelementptr { {} (i8*, i1*, i64, i64, i1)*, i8* }, { {} (i8*, i1*, i64, i64, i1)*, i8* }* %29, i32 0, i32 1
  store i8* %25, i8** %31
  %32 = getelementptr { i64, i64 }, { i64, i64 }* null, i64 1
  %33 = ptrtoint { i64, i64 }* %32 to i64
  %34 = call i8* @malloc_gc(i64 %33)
  %35 = bitcast i8* %34 to { i64, i64 }*
  %36 = getelementptr { i64, i64 }, { i64, i64 }* %35, i32 0, i32 0
  store i64 50, i64* %36
  %37 = getelementptr { i64, i64 }, { i64, i64 }* %35, i32 0, i32 1
  store i64 20, i64* %37
  %38 = bitcast { i64, i64 }* %35 to i8*
  %39 = getelementptr { {} (i8*, i1*, i64)*, i8* }, { {} (i8*, i1*, i64)*, i8* }* null, i64 1
  %40 = ptrtoint { {} (i8*, i1*, i64)*, i8* }* %39 to i64
  %41 = call i8* @malloc_gc(i64 %40)
  %42 = bitcast i8* %41 to { {} (i8*, i1*, i64)*, i8* }*
  %43 = getelementptr { {} (i8*, i1*, i64)*, i8* }, { {} (i8*, i1*, i64)*, i8* }* %42, i32 0, i32 0
  store {} (i8*, i1*, i64)* @init_cells.788, {} (i8*, i1*, i64)** %43
  %44 = getelementptr { {} (i8*, i1*, i64)*, i8* }, { {} (i8*, i1*, i64)*, i8* }* %42, i32 0, i32 1
  store i8* %38, i8** %44
  %45 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* null, i64 1
  %46 = ptrtoint { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %45 to i64
  %47 = call i8* @malloc_gc(i64 %46)
  %48 = bitcast i8* %47 to { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }*
  %49 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %48, i32 0, i32 0
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %17, { i1 (i8*, i1*, i64, i64)*, i8* }** %49
  %50 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %48, i32 0, i32 1
  store i64 50, i64* %50
  %51 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %48, i32 0, i32 2
  store i64 20, i64* %51
  %52 = bitcast { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %48 to i8*
  %53 = getelementptr { {} (i8*, i1*)*, i8* }, { {} (i8*, i1*)*, i8* }* null, i64 1
  %54 = ptrtoint { {} (i8*, i1*)*, i8* }* %53 to i64
  %55 = call i8* @malloc_gc(i64 %54)
  %56 = bitcast i8* %55 to { {} (i8*, i1*)*, i8* }*
  %57 = getelementptr { {} (i8*, i1*)*, i8* }, { {} (i8*, i1*)*, i8* }* %56, i32 0, i32 0
  store {} (i8*, i1*)* @print_cells.803, {} (i8*, i1*)** %57
  %58 = getelementptr { {} (i8*, i1*)*, i8* }, { {} (i8*, i1*)*, i8* }* %56, i32 0, i32 1
  store i8* %52, i8** %58
  %59 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }* }, { { i1 (i8*, i1*, i64, i64)*, i8* }* }* null, i64 1
  %60 = ptrtoint { { i1 (i8*, i1*, i64, i64)*, i8* }* }* %59 to i64
  %61 = call i8* @malloc_gc(i64 %60)
  %62 = bitcast i8* %61 to { { i1 (i8*, i1*, i64, i64)*, i8* }* }*
  %63 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }* }, { { i1 (i8*, i1*, i64, i64)*, i8* }* }* %62, i32 0, i32 0
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %17, { i1 (i8*, i1*, i64, i64)*, i8* }** %63
  %64 = bitcast { { i1 (i8*, i1*, i64, i64)*, i8* }* }* %62 to i8*
  %65 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* null, i64 1
  %66 = ptrtoint { i1 (i8*, i1*, i64, i64)*, i8* }* %65 to i64
  %67 = call i8* @malloc_gc(i64 %66)
  %68 = bitcast i8* %67 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %69 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %68, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @next_state.829, i1 (i8*, i1*, i64, i64)** %69
  %70 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %68, i32 0, i32 1
  store i8* %64, i8** %70
  %71 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* null, i64 1
  %72 = ptrtoint { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %71 to i64
  %73 = call i8* @malloc_gc(i64 %72)
  %74 = bitcast i8* %73 to { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %75 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %74, i32 0, i32 0
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %68, { i1 (i8*, i1*, i64, i64)*, i8* }** %75
  %76 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %74, i32 0, i32 1
  store { {} (i8*, i1*, i64, i64, i1)*, i8* }* %29, { {} (i8*, i1*, i64, i64, i1)*, i8* }** %76
  %77 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %74, i32 0, i32 2
  store i64 50, i64* %77
  %78 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %74, i32 0, i32 3
  store i64 20, i64* %78
  %79 = bitcast { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %74 to i8*
  %80 = getelementptr { {} (i8*, i1*)*, i8* }, { {} (i8*, i1*)*, i8* }* null, i64 1
  %81 = ptrtoint { {} (i8*, i1*)*, i8* }* %80 to i64
  %82 = call i8* @malloc_gc(i64 %81)
  %83 = bitcast i8* %82 to { {} (i8*, i1*)*, i8* }*
  %84 = getelementptr { {} (i8*, i1*)*, i8* }, { {} (i8*, i1*)*, i8* }* %83, i32 0, i32 0
  store {} (i8*, i1*)* @update_cells.927, {} (i8*, i1*)** %84
  %85 = getelementptr { {} (i8*, i1*)*, i8* }, { {} (i8*, i1*)*, i8* }* %83, i32 0, i32 1
  store i8* %79, i8** %85
  %86 = call {} @gen_seed.734({} undef)
  %87 = call {} @pulsar.747(i1* %6)
  %88 = getelementptr { i1*, { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }* }, { i1*, { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }* }* null, i64 1
  %89 = ptrtoint { i1*, { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }* }* %88 to i64
  %90 = call i8* @malloc_gc(i64 %89)
  %91 = bitcast i8* %90 to { i1*, { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }* }*
  %92 = getelementptr { i1*, { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }* }, { i1*, { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }* }* %91, i32 0, i32 0
  store i1* %6, i1** %92
  %93 = getelementptr { i1*, { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }* }, { i1*, { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }* }* %91, i32 0, i32 1
  store { {} (i8*, i1*)*, i8* }* %56, { {} (i8*, i1*)*, i8* }** %93
  %94 = getelementptr { i1*, { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }* }, { i1*, { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }* }* %91, i32 0, i32 2
  store { {} (i8*, i1*)*, i8* }* %83, { {} (i8*, i1*)*, i8* }** %94
  %95 = bitcast { i1*, { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }* }* %91 to i8*
  %96 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* null, i64 1
  %97 = ptrtoint { {} (i8*, i64)*, i8* }* %96 to i64
  %98 = call i8* @malloc_gc(i64 %97)
  %99 = bitcast i8* %98 to { {} (i8*, i64)*, i8* }*
  %100 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %99, i32 0, i32 0
  store {} (i8*, i64)* @loop.954, {} (i8*, i64)** %100
  %101 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %99, i32 0, i32 1
  store i8* %95, i8** %101
  %102 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %99, i32 0, i32 0
  %103 = load {} (i8*, i64)*, {} (i8*, i64)** %102
  %104 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %99, i32 0, i32 1
  %105 = load i8*, i8** %104
  %106 = call {} %103(i8* %105, i64 10)
  ret {} %106
}

declare i1 @gt_i64(i64, i64)

declare i64 @sub_i64(i64, i64)

define {} @loop.954(i8*, i64) {
  %3 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { {} (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { {} (i8*, i64)*, i8* }*
  %7 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store {} (i8*, i64)* @loop.954, {} (i8*, i64)** %7
  %8 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i1*, { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }* }*
  %10 = getelementptr { i1*, { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }* }, { i1*, { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }* }* %9, i32 0, i32 0
  %11 = load i1*, i1** %10
  %12 = getelementptr { i1*, { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }* }, { i1*, { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }* }* %9, i32 0, i32 1
  %13 = load { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }** %12
  %14 = getelementptr { i1*, { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }* }, { i1*, { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }* }* %9, i32 0, i32 2
  %15 = load { {} (i8*, i1*)*, i8* }*, { {} (i8*, i1*)*, i8* }** %14
  %16 = call i1 @gt_i64(i64 %1, i64 0)
  %17 = alloca {}
  br i1 %16, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %18 = getelementptr { {} (i8*, i1*)*, i8* }, { {} (i8*, i1*)*, i8* }* %13, i32 0, i32 0
  %19 = load {} (i8*, i1*)*, {} (i8*, i1*)** %18
  %20 = getelementptr { {} (i8*, i1*)*, i8* }, { {} (i8*, i1*)*, i8* }* %13, i32 0, i32 1
  %21 = load i8*, i8** %20
  %22 = call {} %19(i8* %21, i1* %11)
  %23 = getelementptr { {} (i8*, i1*)*, i8* }, { {} (i8*, i1*)*, i8* }* %15, i32 0, i32 0
  %24 = load {} (i8*, i1*)*, {} (i8*, i1*)** %23
  %25 = getelementptr { {} (i8*, i1*)*, i8* }, { {} (i8*, i1*)*, i8* }* %15, i32 0, i32 1
  %26 = load i8*, i8** %25
  %27 = call {} %24(i8* %26, i1* %11)
  %28 = call {} @newline.731({} undef)
  %29 = call {} @sleep.740(i64 1)
  %30 = call i64 @sub_i64(i64 %1, i64 1)
  %31 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 0
  %32 = load {} (i8*, i64)*, {} (i8*, i64)** %31
  %33 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 1
  %34 = load i8*, i8** %33
  %35 = call {} %32(i8* %34, i64 %30)
  store {} %35, {}* %17
  br label %endif_0

else_0:                                           ; preds = %2
  store {} undef, {}* %17
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %36 = load {}, {}* %17
  ret {} %36
}

define {} @update_cells.927(i8*, i1*) {
  %3 = getelementptr { {} (i8*, i1*)*, i8* }, { {} (i8*, i1*)*, i8* }* null, i64 1
  %4 = ptrtoint { {} (i8*, i1*)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { {} (i8*, i1*)*, i8* }*
  %7 = getelementptr { {} (i8*, i1*)*, i8* }, { {} (i8*, i1*)*, i8* }* %6, i32 0, i32 0
  store {} (i8*, i1*)* @update_cells.927, {} (i8*, i1*)** %7
  %8 = getelementptr { {} (i8*, i1*)*, i8* }, { {} (i8*, i1*)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %10 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %9, i32 0, i32 0
  %11 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %10
  %12 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %9, i32 0, i32 1
  %13 = load { {} (i8*, i1*, i64, i64, i1)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }** %12
  %14 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %9, i32 0, i32 2
  %15 = load i64, i64* %14
  %16 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %9, i32 0, i32 3
  %17 = load i64, i64* %16
  %18 = call i64 @mul_i64(i64 %15, i64 %17)
  %19 = call i1* @copy_bool_array.743(i1* %1, i64 %18)
  %20 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* null, i64 1
  %21 = ptrtoint { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %20 to i64
  %22 = call i8* @malloc_gc(i64 %21)
  %23 = bitcast i8* %22 to { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %24 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %23, i32 0, i32 0
  store i1* %1, i1** %24
  %25 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %23, i32 0, i32 1
  store i1* %19, i1** %25
  %26 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %23, i32 0, i32 2
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %11, { i1 (i8*, i1*, i64, i64)*, i8* }** %26
  %27 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %23, i32 0, i32 3
  store { {} (i8*, i1*, i64, i64, i1)*, i8* }* %13, { {} (i8*, i1*, i64, i64, i1)*, i8* }** %27
  %28 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %23, i32 0, i32 4
  store i64 %15, i64* %28
  %29 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %23, i32 0, i32 5
  store i64 %17, i64* %29
  %30 = bitcast { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %23 to i8*
  %31 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* null, i64 1
  %32 = ptrtoint { {} (i8*, i64)*, i8* }* %31 to i64
  %33 = call i8* @malloc_gc(i64 %32)
  %34 = bitcast i8* %33 to { {} (i8*, i64)*, i8* }*
  %35 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %34, i32 0, i32 0
  store {} (i8*, i64)* @go_y.932, {} (i8*, i64)** %35
  %36 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %34, i32 0, i32 1
  store i8* %30, i8** %36
  %37 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %34, i32 0, i32 0
  %38 = load {} (i8*, i64)*, {} (i8*, i64)** %37
  %39 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %34, i32 0, i32 1
  %40 = load i8*, i8** %39
  %41 = call {} %38(i8* %40, i64 0)
  ret {} %41
}

declare i1 @ge_i64(i64, i64)

declare i64 @add_i64(i64, i64)

define {} @go_y.932(i8*, i64) {
  %3 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { {} (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { {} (i8*, i64)*, i8* }*
  %7 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store {} (i8*, i64)* @go_y.932, {} (i8*, i64)** %7
  %8 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %10 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %9, i32 0, i32 0
  %11 = load i1*, i1** %10
  %12 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %9, i32 0, i32 1
  %13 = load i1*, i1** %12
  %14 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %9, i32 0, i32 2
  %15 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %14
  %16 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %9, i32 0, i32 3
  %17 = load { {} (i8*, i1*, i64, i64, i1)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }** %16
  %18 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %9, i32 0, i32 4
  %19 = load i64, i64* %18
  %20 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %9, i32 0, i32 5
  %21 = load i64, i64* %20
  %22 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* null, i64 1
  %23 = ptrtoint { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %22 to i64
  %24 = call i8* @malloc_gc(i64 %23)
  %25 = bitcast i8* %24 to { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %26 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %25, i32 0, i32 0
  store i1* %11, i1** %26
  %27 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %25, i32 0, i32 1
  store i1* %13, i1** %27
  %28 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %25, i32 0, i32 2
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %15, { i1 (i8*, i1*, i64, i64)*, i8* }** %28
  %29 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %25, i32 0, i32 3
  store { {} (i8*, i1*, i64, i64, i1)*, i8* }* %17, { {} (i8*, i1*, i64, i64, i1)*, i8* }** %29
  %30 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %25, i32 0, i32 4
  store i64 %19, i64* %30
  %31 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %25, i32 0, i32 5
  store i64 %1, i64* %31
  %32 = bitcast { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %25 to i8*
  %33 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* null, i64 1
  %34 = ptrtoint { {} (i8*, i64)*, i8* }* %33 to i64
  %35 = call i8* @malloc_gc(i64 %34)
  %36 = bitcast i8* %35 to { {} (i8*, i64)*, i8* }*
  %37 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %36, i32 0, i32 0
  store {} (i8*, i64)* @go_x.934, {} (i8*, i64)** %37
  %38 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %36, i32 0, i32 1
  store i8* %32, i8** %38
  %39 = call i1 @ge_i64(i64 %1, i64 %21)
  %40 = alloca {}
  br i1 %39, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store {} undef, {}* %40
  br label %endif_0

else_0:                                           ; preds = %2
  %41 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %36, i32 0, i32 0
  %42 = load {} (i8*, i64)*, {} (i8*, i64)** %41
  %43 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %36, i32 0, i32 1
  %44 = load i8*, i8** %43
  %45 = call {} %42(i8* %44, i64 0)
  %46 = call i64 @add_i64(i64 %1, i64 1)
  %47 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 0
  %48 = load {} (i8*, i64)*, {} (i8*, i64)** %47
  %49 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 1
  %50 = load i8*, i8** %49
  %51 = call {} %48(i8* %50, i64 %46)
  store {} %51, {}* %40
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %52 = load {}, {}* %40
  ret {} %52
}

define {} @go_x.934(i8*, i64) {
  %3 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { {} (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { {} (i8*, i64)*, i8* }*
  %7 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store {} (i8*, i64)* @go_x.934, {} (i8*, i64)** %7
  %8 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %10 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %9, i32 0, i32 0
  %11 = load i1*, i1** %10
  %12 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %9, i32 0, i32 1
  %13 = load i1*, i1** %12
  %14 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %9, i32 0, i32 2
  %15 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %14
  %16 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %9, i32 0, i32 3
  %17 = load { {} (i8*, i1*, i64, i64, i1)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }** %16
  %18 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %9, i32 0, i32 4
  %19 = load i64, i64* %18
  %20 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {} (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %9, i32 0, i32 5
  %21 = load i64, i64* %20
  %22 = call i1 @ge_i64(i64 %1, i64 %19)
  %23 = alloca {}
  br i1 %22, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store {} undef, {}* %23
  br label %endif_0

else_0:                                           ; preds = %2
  %24 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %15, i32 0, i32 0
  %25 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %24
  %26 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %15, i32 0, i32 1
  %27 = load i8*, i8** %26
  %28 = call i1 %25(i8* %27, i1* %13, i64 %1, i64 %21)
  %29 = getelementptr { {} (i8*, i1*, i64, i64, i1)*, i8* }, { {} (i8*, i1*, i64, i64, i1)*, i8* }* %17, i32 0, i32 0
  %30 = load {} (i8*, i1*, i64, i64, i1)*, {} (i8*, i1*, i64, i64, i1)** %29
  %31 = getelementptr { {} (i8*, i1*, i64, i64, i1)*, i8* }, { {} (i8*, i1*, i64, i64, i1)*, i8* }* %17, i32 0, i32 1
  %32 = load i8*, i8** %31
  %33 = call {} %30(i8* %32, i1* %11, i64 %1, i64 %21, i1 %28)
  %34 = call i64 @add_i64(i64 %1, i64 1)
  %35 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 0
  %36 = load {} (i8*, i64)*, {} (i8*, i64)** %35
  %37 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 1
  %38 = load i8*, i8** %37
  %39 = call {} %36(i8* %38, i64 %34)
  store {} %39, {}* %23
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %40 = load {}, {}* %23
  ret {} %40
}

declare i1 @eq_i64(i64, i64)

declare i1 @or(i1, i1)

define i1 @next_state.829(i8*, i1*, i64, i64) {
  %5 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* null, i64 1
  %6 = ptrtoint { i1 (i8*, i1*, i64, i64)*, i8* }* %5 to i64
  %7 = call i8* @malloc_gc(i64 %6)
  %8 = bitcast i8* %7 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %9 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %8, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @next_state.829, i1 (i8*, i1*, i64, i64)** %9
  %10 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %8, i32 0, i32 1
  store i8* %0, i8** %10
  %11 = bitcast i8* %0 to { { i1 (i8*, i1*, i64, i64)*, i8* }* }*
  %12 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }* }, { { i1 (i8*, i1*, i64, i64)*, i8* }* }* %11, i32 0, i32 0
  %13 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %12
  %14 = getelementptr i64, i64* null, i64 1
  %15 = ptrtoint i64* %14 to i64
  %16 = mul i64 1, %15
  %17 = call i8* @malloc_gc(i64 %16)
  %18 = bitcast i8* %17 to i64*
  %19 = call i64 @sub_i64(i64 %2, i64 1)
  %20 = call i64 @add_i64(i64 %3, i64 1)
  %21 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 0
  %22 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %21
  %23 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 1
  %24 = load i8*, i8** %23
  %25 = call i1 %22(i8* %24, i1* %1, i64 %19, i64 %20)
  %26 = call i64 @to_int.827(i1 %25)
  %27 = call i64 @add_i64(i64 %3, i64 1)
  %28 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 0
  %29 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %28
  %30 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 1
  %31 = load i8*, i8** %30
  %32 = call i1 %29(i8* %31, i1* %1, i64 %2, i64 %27)
  %33 = call i64 @to_int.827(i1 %32)
  %34 = call i64 @add_i64(i64 %26, i64 %33)
  %35 = call i64 @add_i64(i64 %2, i64 1)
  %36 = call i64 @add_i64(i64 %3, i64 1)
  %37 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 0
  %38 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %37
  %39 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 1
  %40 = load i8*, i8** %39
  %41 = call i1 %38(i8* %40, i1* %1, i64 %35, i64 %36)
  %42 = call i64 @to_int.827(i1 %41)
  %43 = call i64 @add_i64(i64 %34, i64 %42)
  %44 = getelementptr i64, i64* %18, i64 0
  store i64 %43, i64* %44
  %45 = call i64 @sub_i64(i64 %2, i64 1)
  %46 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 0
  %47 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %46
  %48 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 1
  %49 = load i8*, i8** %48
  %50 = call i1 %47(i8* %49, i1* %1, i64 %45, i64 %3)
  %51 = call i64 @to_int.827(i1 %50)
  %52 = call i64 @add_i64(i64 %2, i64 1)
  %53 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 0
  %54 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %53
  %55 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 1
  %56 = load i8*, i8** %55
  %57 = call i1 %54(i8* %56, i1* %1, i64 %52, i64 %3)
  %58 = call i64 @to_int.827(i1 %57)
  %59 = call i64 @add_i64(i64 %51, i64 %58)
  %60 = getelementptr i64, i64* %18, i64 0
  %61 = load i64, i64* %60
  %62 = call i64 @add_i64(i64 %59, i64 %61)
  %63 = getelementptr i64, i64* %18, i64 0
  store i64 %62, i64* %63
  %64 = call i64 @sub_i64(i64 %2, i64 1)
  %65 = call i64 @sub_i64(i64 %3, i64 1)
  %66 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 0
  %67 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %66
  %68 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 1
  %69 = load i8*, i8** %68
  %70 = call i1 %67(i8* %69, i1* %1, i64 %64, i64 %65)
  %71 = call i64 @to_int.827(i1 %70)
  %72 = call i64 @sub_i64(i64 %3, i64 1)
  %73 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 0
  %74 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %73
  %75 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 1
  %76 = load i8*, i8** %75
  %77 = call i1 %74(i8* %76, i1* %1, i64 %2, i64 %72)
  %78 = call i64 @to_int.827(i1 %77)
  %79 = call i64 @add_i64(i64 %71, i64 %78)
  %80 = call i64 @add_i64(i64 %2, i64 1)
  %81 = call i64 @sub_i64(i64 %3, i64 1)
  %82 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 0
  %83 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %82
  %84 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 1
  %85 = load i8*, i8** %84
  %86 = call i1 %83(i8* %85, i1* %1, i64 %80, i64 %81)
  %87 = call i64 @to_int.827(i1 %86)
  %88 = call i64 @add_i64(i64 %79, i64 %87)
  %89 = getelementptr i64, i64* %18, i64 0
  %90 = load i64, i64* %89
  %91 = call i64 @add_i64(i64 %88, i64 %90)
  %92 = getelementptr i64, i64* %18, i64 0
  store i64 %91, i64* %92
  %93 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 0
  %94 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %93
  %95 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 1
  %96 = load i8*, i8** %95
  %97 = call i1 %94(i8* %96, i1* %1, i64 %2, i64 %3)
  %98 = alloca i1
  br i1 %97, label %then_0, label %else_0

then_0:                                           ; preds = %4
  %99 = getelementptr i64, i64* %18, i64 0
  %100 = load i64, i64* %99
  %101 = call i1 @eq_i64(i64 %100, i64 2)
  %102 = getelementptr i64, i64* %18, i64 0
  %103 = load i64, i64* %102
  %104 = call i1 @eq_i64(i64 %103, i64 3)
  %105 = call i1 @or(i1 %101, i1 %104)
  store i1 %105, i1* %98
  br label %endif_0

else_0:                                           ; preds = %4
  %106 = getelementptr i64, i64* %18, i64 0
  %107 = load i64, i64* %106
  %108 = call i1 @eq_i64(i64 %107, i64 3)
  store i1 %108, i1* %98
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %109 = load i1, i1* %98
  ret i1 %109
}

define i64 @to_int.827(i1) {
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

define {} @print_cells.803(i8*, i1*) {
  %3 = getelementptr { {} (i8*, i1*)*, i8* }, { {} (i8*, i1*)*, i8* }* null, i64 1
  %4 = ptrtoint { {} (i8*, i1*)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { {} (i8*, i1*)*, i8* }*
  %7 = getelementptr { {} (i8*, i1*)*, i8* }, { {} (i8*, i1*)*, i8* }* %6, i32 0, i32 0
  store {} (i8*, i1*)* @print_cells.803, {} (i8*, i1*)** %7
  %8 = getelementptr { {} (i8*, i1*)*, i8* }, { {} (i8*, i1*)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }*
  %10 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %9, i32 0, i32 0
  %11 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %10
  %12 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %9, i32 0, i32 1
  %13 = load i64, i64* %12
  %14 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %9, i32 0, i32 2
  %15 = load i64, i64* %14
  %16 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* null, i64 1
  %17 = ptrtoint { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %16 to i64
  %18 = call i8* @malloc_gc(i64 %17)
  %19 = bitcast i8* %18 to { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }*
  %20 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %19, i32 0, i32 0
  store i1* %1, i1** %20
  %21 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %19, i32 0, i32 1
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %11, { i1 (i8*, i1*, i64, i64)*, i8* }** %21
  %22 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %19, i32 0, i32 2
  store i64 %13, i64* %22
  %23 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %19, i32 0, i32 3
  store i64 %15, i64* %23
  %24 = bitcast { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %19 to i8*
  %25 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* null, i64 1
  %26 = ptrtoint { {} (i8*, i64)*, i8* }* %25 to i64
  %27 = call i8* @malloc_gc(i64 %26)
  %28 = bitcast i8* %27 to { {} (i8*, i64)*, i8* }*
  %29 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %28, i32 0, i32 0
  store {} (i8*, i64)* @go_y.805, {} (i8*, i64)** %29
  %30 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %28, i32 0, i32 1
  store i8* %24, i8** %30
  %31 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %28, i32 0, i32 0
  %32 = load {} (i8*, i64)*, {} (i8*, i64)** %31
  %33 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %28, i32 0, i32 1
  %34 = load i8*, i8** %33
  %35 = call {} %32(i8* %34, i64 0)
  ret {} %35
}

define {} @go_y.805(i8*, i64) {
  %3 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { {} (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { {} (i8*, i64)*, i8* }*
  %7 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store {} (i8*, i64)* @go_y.805, {} (i8*, i64)** %7
  %8 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }*
  %10 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %9, i32 0, i32 0
  %11 = load i1*, i1** %10
  %12 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %9, i32 0, i32 1
  %13 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %12
  %14 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %9, i32 0, i32 2
  %15 = load i64, i64* %14
  %16 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %9, i32 0, i32 3
  %17 = load i64, i64* %16
  %18 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* null, i64 1
  %19 = ptrtoint { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %18 to i64
  %20 = call i8* @malloc_gc(i64 %19)
  %21 = bitcast i8* %20 to { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }*
  %22 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %21, i32 0, i32 0
  store i1* %11, i1** %22
  %23 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %21, i32 0, i32 1
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %13, { i1 (i8*, i1*, i64, i64)*, i8* }** %23
  %24 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %21, i32 0, i32 2
  store i64 %15, i64* %24
  %25 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %21, i32 0, i32 3
  store i64 %1, i64* %25
  %26 = bitcast { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %21 to i8*
  %27 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* null, i64 1
  %28 = ptrtoint { {} (i8*, i64)*, i8* }* %27 to i64
  %29 = call i8* @malloc_gc(i64 %28)
  %30 = bitcast i8* %29 to { {} (i8*, i64)*, i8* }*
  %31 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %30, i32 0, i32 0
  store {} (i8*, i64)* @go_x.807, {} (i8*, i64)** %31
  %32 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %30, i32 0, i32 1
  store i8* %26, i8** %32
  %33 = call i1 @ge_i64(i64 %1, i64 %17)
  %34 = alloca {}
  br i1 %33, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store {} undef, {}* %34
  br label %endif_0

else_0:                                           ; preds = %2
  %35 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %30, i32 0, i32 0
  %36 = load {} (i8*, i64)*, {} (i8*, i64)** %35
  %37 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %30, i32 0, i32 1
  %38 = load i8*, i8** %37
  %39 = call {} %36(i8* %38, i64 0)
  %40 = call {} @newline.731({} undef)
  %41 = call i64 @add_i64(i64 %1, i64 1)
  %42 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 0
  %43 = load {} (i8*, i64)*, {} (i8*, i64)** %42
  %44 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 1
  %45 = load i8*, i8** %44
  %46 = call {} %43(i8* %45, i64 %41)
  store {} %46, {}* %34
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %47 = load {}, {}* %34
  ret {} %47
}

define {} @go_x.807(i8*, i64) {
  %3 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { {} (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { {} (i8*, i64)*, i8* }*
  %7 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store {} (i8*, i64)* @go_x.807, {} (i8*, i64)** %7
  %8 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }*
  %10 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %9, i32 0, i32 0
  %11 = load i1*, i1** %10
  %12 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %9, i32 0, i32 1
  %13 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %12
  %14 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %9, i32 0, i32 2
  %15 = load i64, i64* %14
  %16 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %9, i32 0, i32 3
  %17 = load i64, i64* %16
  %18 = call i1 @ge_i64(i64 %1, i64 %15)
  %19 = alloca {}
  br i1 %18, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store {} undef, {}* %19
  br label %endif_0

else_0:                                           ; preds = %2
  %20 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 0
  %21 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %20
  %22 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %13, i32 0, i32 1
  %23 = load i8*, i8** %22
  %24 = call i1 %21(i8* %23, i1* %11, i64 %1, i64 %17)
  %25 = call i8 @to_char.801(i1 %24)
  %26 = call {} @print_char.728(i8 %25)
  %27 = call i64 @add_i64(i64 %1, i64 1)
  %28 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 0
  %29 = load {} (i8*, i64)*, {} (i8*, i64)** %28
  %30 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 1
  %31 = load i8*, i8** %30
  %32 = call {} %29(i8* %31, i64 %27)
  store {} %32, {}* %19
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %33 = load {}, {}* %19
  ret {} %33
}

define i8 @to_char.801(i1) {
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

define {} @init_cells.788(i8*, i1*, i64) {
  %4 = getelementptr { {} (i8*, i1*, i64)*, i8* }, { {} (i8*, i1*, i64)*, i8* }* null, i64 1
  %5 = ptrtoint { {} (i8*, i1*, i64)*, i8* }* %4 to i64
  %6 = call i8* @malloc_gc(i64 %5)
  %7 = bitcast i8* %6 to { {} (i8*, i1*, i64)*, i8* }*
  %8 = getelementptr { {} (i8*, i1*, i64)*, i8* }, { {} (i8*, i1*, i64)*, i8* }* %7, i32 0, i32 0
  store {} (i8*, i1*, i64)* @init_cells.788, {} (i8*, i1*, i64)** %8
  %9 = getelementptr { {} (i8*, i1*, i64)*, i8* }, { {} (i8*, i1*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = bitcast i8* %0 to { i64, i64 }*
  %11 = getelementptr { i64, i64 }, { i64, i64 }* %10, i32 0, i32 0
  %12 = load i64, i64* %11
  %13 = getelementptr { i64, i64 }, { i64, i64 }* %10, i32 0, i32 1
  %14 = load i64, i64* %13
  %15 = call i64 @mul_i64(i64 %12, i64 %14)
  %16 = call i1 @ge_i64(i64 %2, i64 %15)
  %17 = alloca {}
  br i1 %16, label %then_0, label %else_0

then_0:                                           ; preds = %3
  store {} undef, {}* %17
  br label %endif_0

else_0:                                           ; preds = %3
  %18 = call i1 @rand_bool.737({} undef)
  %19 = getelementptr i1, i1* %1, i64 %2
  store i1 %18, i1* %19
  %20 = call i64 @add_i64(i64 %2, i64 1)
  %21 = getelementptr { {} (i8*, i1*, i64)*, i8* }, { {} (i8*, i1*, i64)*, i8* }* %7, i32 0, i32 0
  %22 = load {} (i8*, i1*, i64)*, {} (i8*, i1*, i64)** %21
  %23 = getelementptr { {} (i8*, i1*, i64)*, i8* }, { {} (i8*, i1*, i64)*, i8* }* %7, i32 0, i32 1
  %24 = load i8*, i8** %23
  %25 = call {} %22(i8* %24, i1* %1, i64 %20)
  store {} %25, {}* %17
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %26 = load {}, {}* %17
  ret {} %26
}

define {} @set.779(i8*, i1*, i64, i64, i1) {
  %6 = getelementptr { {} (i8*, i1*, i64, i64, i1)*, i8* }, { {} (i8*, i1*, i64, i64, i1)*, i8* }* null, i64 1
  %7 = ptrtoint { {} (i8*, i1*, i64, i64, i1)*, i8* }* %6 to i64
  %8 = call i8* @malloc_gc(i64 %7)
  %9 = bitcast i8* %8 to { {} (i8*, i1*, i64, i64, i1)*, i8* }*
  %10 = getelementptr { {} (i8*, i1*, i64, i64, i1)*, i8* }, { {} (i8*, i1*, i64, i64, i1)*, i8* }* %9, i32 0, i32 0
  store {} (i8*, i1*, i64, i64, i1)* @set.779, {} (i8*, i1*, i64, i64, i1)** %10
  %11 = getelementptr { {} (i8*, i1*, i64, i64, i1)*, i8* }, { {} (i8*, i1*, i64, i64, i1)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = bitcast i8* %0 to { i64 }*
  %13 = getelementptr { i64 }, { i64 }* %12, i32 0, i32 0
  %14 = load i64, i64* %13
  %15 = call i64 @mul_i64(i64 %3, i64 %14)
  %16 = call i64 @add_i64(i64 %15, i64 %2)
  %17 = getelementptr i1, i1* %1, i64 %16
  store i1 %4, i1* %17
  ret {} undef
}

declare i1 @lt_i64(i64, i64)

define i1 @view.755(i8*, i1*, i64, i64) {
  %5 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* null, i64 1
  %6 = ptrtoint { i1 (i8*, i1*, i64, i64)*, i8* }* %5 to i64
  %7 = call i8* @malloc_gc(i64 %6)
  %8 = bitcast i8* %7 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %9 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %8, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @view.755, i1 (i8*, i1*, i64, i64)** %9
  %10 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %8, i32 0, i32 1
  store i8* %0, i8** %10
  %11 = bitcast i8* %0 to { i64, i64 }*
  %12 = getelementptr { i64, i64 }, { i64, i64 }* %11, i32 0, i32 0
  %13 = load i64, i64* %12
  %14 = getelementptr { i64, i64 }, { i64, i64 }* %11, i32 0, i32 1
  %15 = load i64, i64* %14
  %16 = call i1 @ge_i64(i64 %2, i64 %13)
  %17 = call i1 @ge_i64(i64 %3, i64 %15)
  %18 = call i1 @or(i1 %16, i1 %17)
  %19 = call i1 @lt_i64(i64 %2, i64 0)
  %20 = call i1 @or(i1 %18, i1 %19)
  %21 = call i1 @lt_i64(i64 %3, i64 0)
  %22 = call i1 @or(i1 %20, i1 %21)
  %23 = alloca i1
  br i1 %22, label %then_0, label %else_0

then_0:                                           ; preds = %4
  store i1 false, i1* %23
  br label %endif_0

else_0:                                           ; preds = %4
  %24 = call i64 @mul_i64(i64 %3, i64 %13)
  %25 = call i64 @add_i64(i64 %24, i64 %2)
  %26 = getelementptr i1, i1* %1, i64 %25
  %27 = load i1, i1* %26
  store i1 %27, i1* %23
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %28 = load i1, i1* %23
  ret i1 %28
}

declare {} @pulsar(i1*)

define {} @pulsar.747(i1*) {
  %2 = call {} @pulsar(i1* %0)
  ret {} %2
}

declare i1* @copy_bool_array(i1*, i64)

define i1* @copy_bool_array.743(i1*, i64) {
  %3 = call i1* @copy_bool_array(i1* %0, i64 %1)
  ret i1* %3
}

declare {} @malgo_sleep(i64)

define {} @sleep.740(i64) {
  %2 = call {} @malgo_sleep(i64 %0)
  ret {} %2
}

declare i1 @rand_bool({})

define i1 @rand_bool.737({}) {
  %2 = call i1 @rand_bool({} %0)
  ret i1 %2
}

declare {} @gen_seed({})

define {} @gen_seed.734({}) {
  %2 = call {} @gen_seed({} %0)
  ret {} %2
}

declare {} @newline({})

define {} @newline.731({}) {
  %2 = call {} @newline({} %0)
  ret {} %2
}

declare {} @print_char(i8)

define {} @print_char.728(i8) {
  %2 = call {} @print_char(i8 %0)
  ret {} %2
}

define i32 @main() {
  call void @init_gc()
  %1 = call {} @main.1305()
  ret i32 0
}
