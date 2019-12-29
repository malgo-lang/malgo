; ModuleID = './examples/life.mlg'
source_filename = "./examples/life.mlg"

declare i8* @GC_malloc(i64)

define i32 @main360() {
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
  store i1 (i8*, i1*, i64, i64)* @view373, i1 (i8*, i1*, i64, i64)** %21
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  %24 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %20, i32 0, i32 1
  store i8* %18, i8** %24
  %25 = call i8* @GC_malloc(i64 0)
  %26 = bitcast i8* %25 to {}*
  %27 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %28 = bitcast i8* %27 to { i64 }*
  %29 = getelementptr { i64 }, { i64 }* %28, i32 0, i32 0
  store i64 50, i64* %29
  %30 = call i8* @GC_malloc(i64 0)
  %31 = bitcast i8* %30 to {}*
  %32 = bitcast { i64 }* %28 to i8*
  %33 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %34 = bitcast i8* %33 to { {}* (i8*, i1*, i64, i64, i1)*, i8* }*
  %35 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %34, i32 0, i32 0
  store {}* (i8*, i1*, i64, i64, i1)* @set372, {}* (i8*, i1*, i64, i64, i1)** %35
  %36 = call i8* @GC_malloc(i64 0)
  %37 = bitcast i8* %36 to {}*
  %38 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %34, i32 0, i32 1
  store i8* %32, i8** %38
  %39 = call i8* @GC_malloc(i64 0)
  %40 = bitcast i8* %39 to {}*
  %41 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %42 = bitcast i8* %41 to { i64, i64 }*
  %43 = getelementptr { i64, i64 }, { i64, i64 }* %42, i32 0, i32 0
  store i64 50, i64* %43
  %44 = call i8* @GC_malloc(i64 0)
  %45 = bitcast i8* %44 to {}*
  %46 = getelementptr { i64, i64 }, { i64, i64 }* %42, i32 0, i32 1
  store i64 20, i64* %46
  %47 = call i8* @GC_malloc(i64 0)
  %48 = bitcast i8* %47 to {}*
  %49 = bitcast { i64, i64 }* %42 to i8*
  %50 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %51 = bitcast i8* %50 to { {}* (i8*, i1*, i64)*, i8* }*
  %52 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %51, i32 0, i32 0
  store {}* (i8*, i1*, i64)* @init_cells371, {}* (i8*, i1*, i64)** %52
  %53 = call i8* @GC_malloc(i64 0)
  %54 = bitcast i8* %53 to {}*
  %55 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %51, i32 0, i32 1
  store i8* %49, i8** %55
  %56 = call i8* @GC_malloc(i64 0)
  %57 = bitcast i8* %56 to {}*
  %58 = call i8* @GC_malloc(i64 ptrtoint ({ { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* getelementptr inbounds ({ { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %59 = bitcast i8* %58 to { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }*
  %60 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %59, i32 0, i32 0
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %20, { i1 (i8*, i1*, i64, i64)*, i8* }** %60
  %61 = call i8* @GC_malloc(i64 0)
  %62 = bitcast i8* %61 to {}*
  %63 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %59, i32 0, i32 1
  store i64 50, i64* %63
  %64 = call i8* @GC_malloc(i64 0)
  %65 = bitcast i8* %64 to {}*
  %66 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %59, i32 0, i32 2
  store i64 20, i64* %66
  %67 = call i8* @GC_malloc(i64 0)
  %68 = bitcast i8* %67 to {}*
  %69 = bitcast { { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %59 to i8*
  %70 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %71 = bitcast i8* %70 to { {}* (i8*, i1*)*, i8* }*
  %72 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %71, i32 0, i32 0
  store {}* (i8*, i1*)* @print_cells367, {}* (i8*, i1*)** %72
  %73 = call i8* @GC_malloc(i64 0)
  %74 = bitcast i8* %73 to {}*
  %75 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %71, i32 0, i32 1
  store i8* %69, i8** %75
  %76 = call i8* @GC_malloc(i64 0)
  %77 = bitcast i8* %76 to {}*
  %78 = call i8* @GC_malloc(i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64))
  %79 = bitcast i8* %78 to { { i1 (i8*, i1*, i64, i64)*, i8* }* }*
  %80 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }* }, { { i1 (i8*, i1*, i64, i64)*, i8* }* }* %79, i32 0, i32 0
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %20, { i1 (i8*, i1*, i64, i64)*, i8* }** %80
  %81 = call i8* @GC_malloc(i64 0)
  %82 = bitcast i8* %81 to {}*
  %83 = bitcast { { i1 (i8*, i1*, i64, i64)*, i8* }* }* %79 to i8*
  %84 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %85 = bitcast i8* %84 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %86 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %85, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @next_state365, i1 (i8*, i1*, i64, i64)** %86
  %87 = call i8* @GC_malloc(i64 0)
  %88 = bitcast i8* %87 to {}*
  %89 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %85, i32 0, i32 1
  store i8* %83, i8** %89
  %90 = call i8* @GC_malloc(i64 0)
  %91 = bitcast i8* %90 to {}*
  %92 = call i8* @GC_malloc(i64 ptrtoint ({ { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* getelementptr inbounds ({ { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %93 = bitcast i8* %92 to { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %94 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %93, i32 0, i32 0
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %85, { i1 (i8*, i1*, i64, i64)*, i8* }** %94
  %95 = call i8* @GC_malloc(i64 0)
  %96 = bitcast i8* %95 to {}*
  %97 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %93, i32 0, i32 1
  store { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %34, { {}* (i8*, i1*, i64, i64, i1)*, i8* }** %97
  %98 = call i8* @GC_malloc(i64 0)
  %99 = bitcast i8* %98 to {}*
  %100 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %93, i32 0, i32 2
  store i64 50, i64* %100
  %101 = call i8* @GC_malloc(i64 0)
  %102 = bitcast i8* %101 to {}*
  %103 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %93, i32 0, i32 3
  store i64 20, i64* %103
  %104 = call i8* @GC_malloc(i64 0)
  %105 = bitcast i8* %104 to {}*
  %106 = bitcast { { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %93 to i8*
  %107 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %108 = bitcast i8* %107 to { {}* (i8*, i1*)*, i8* }*
  %109 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %108, i32 0, i32 0
  store {}* (i8*, i1*)* @update_cells362, {}* (i8*, i1*)** %109
  %110 = call i8* @GC_malloc(i64 0)
  %111 = bitcast i8* %110 to {}*
  %112 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %108, i32 0, i32 1
  store i8* %106, i8** %112
  %113 = call i8* @GC_malloc(i64 0)
  %114 = bitcast i8* %113 to {}*
  %115 = call {}* @gen_seed378()
  %116 = call {}* @pulsar374(i1* %4)
  %117 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 3))
  %118 = bitcast i8* %117 to { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }*
  %119 = getelementptr { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }, { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }* %118, i32 0, i32 0
  store i1* %4, i1** %119
  %120 = call i8* @GC_malloc(i64 0)
  %121 = bitcast i8* %120 to {}*
  %122 = getelementptr { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }, { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }* %118, i32 0, i32 1
  store { {}* (i8*, i1*)*, i8* }* %71, { {}* (i8*, i1*)*, i8* }** %122
  %123 = call i8* @GC_malloc(i64 0)
  %124 = bitcast i8* %123 to {}*
  %125 = getelementptr { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }, { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }* %118, i32 0, i32 2
  store { {}* (i8*, i1*)*, i8* }* %108, { {}* (i8*, i1*)*, i8* }** %125
  %126 = call i8* @GC_malloc(i64 0)
  %127 = bitcast i8* %126 to {}*
  %128 = bitcast { i1*, { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }* }* %118 to i8*
  %129 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %130 = bitcast i8* %129 to { {}* (i8*, i64)*, i8* }*
  %131 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %130, i32 0, i32 0
  store {}* (i8*, i64)* @loop361, {}* (i8*, i64)** %131
  %132 = call i8* @GC_malloc(i64 0)
  %133 = bitcast i8* %132 to {}*
  %134 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %130, i32 0, i32 1
  store i8* %128, i8** %134
  %135 = call i8* @GC_malloc(i64 0)
  %136 = bitcast i8* %135 to {}*
  %137 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %130, i32 0, i32 0
  %138 = load {}* (i8*, i64)*, {}* (i8*, i64)** %137
  %139 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %130, i32 0, i32 1
  %140 = load i8*, i8** %139
  %141 = call {}* %138(i8* %140, i64 50)
  ret i32 0
}

define {}* @loop361(i8*, i64) {
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
  store {}* (i8*, i64)* @loop361, {}* (i8*, i64)** %12
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
  %30 = call {}* @newline379()
  %31 = call {}* @sleep376(i64 1)
  %32 = sub i64 %1, 1
  %33 = call {}* @loop361(i8* %0, i64 %32)
  store {}* %33, {}** %19
  br label %end_0

else_0:                                           ; preds = %2
  %34 = call i8* @GC_malloc(i64 0)
  %35 = bitcast i8* %34 to {}*
  store {}* %35, {}** %19
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %36 = load {}*, {}** %19
  ret {}* %36
}

define {}* @update_cells362(i8*, i1*) {
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
  store {}* (i8*, i1*)* @update_cells362, {}* (i8*, i1*)** %14
  %15 = call i8* @GC_malloc(i64 0)
  %16 = bitcast i8* %15 to {}*
  %17 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %13, i32 0, i32 1
  store i8* %0, i8** %17
  %18 = call i8* @GC_malloc(i64 0)
  %19 = bitcast i8* %18 to {}*
  %20 = mul i64 %9, %11
  %21 = call i1* @copy_bool_array375(i1* %1, i64 %20)
  %22 = call i8* @GC_malloc(i64 ptrtoint ({ i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* getelementptr inbounds ({ i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %23 = bitcast i8* %22 to { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }*
  %24 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %23, i32 0, i32 0
  store i1* %1, i1** %24
  %25 = call i8* @GC_malloc(i64 0)
  %26 = bitcast i8* %25 to {}*
  %27 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %23, i32 0, i32 1
  store i1* %21, i1** %27
  %28 = call i8* @GC_malloc(i64 0)
  %29 = bitcast i8* %28 to {}*
  %30 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %23, i32 0, i32 2
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %5, { i1 (i8*, i1*, i64, i64)*, i8* }** %30
  %31 = call i8* @GC_malloc(i64 0)
  %32 = bitcast i8* %31 to {}*
  %33 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %23, i32 0, i32 3
  store { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %7, { {}* (i8*, i1*, i64, i64, i1)*, i8* }** %33
  %34 = call i8* @GC_malloc(i64 0)
  %35 = bitcast i8* %34 to {}*
  %36 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %23, i32 0, i32 4
  store i64 %9, i64* %36
  %37 = call i8* @GC_malloc(i64 0)
  %38 = bitcast i8* %37 to {}*
  %39 = getelementptr { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }, { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %23, i32 0, i32 5
  store i64 %11, i64* %39
  %40 = call i8* @GC_malloc(i64 0)
  %41 = bitcast i8* %40 to {}*
  %42 = bitcast { i1*, i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, i64, i64 }* %23 to i8*
  %43 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %44 = bitcast i8* %43 to { {}* (i8*, i64)*, i8* }*
  %45 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %44, i32 0, i32 0
  store {}* (i8*, i64)* @go_y363, {}* (i8*, i64)** %45
  %46 = call i8* @GC_malloc(i64 0)
  %47 = bitcast i8* %46 to {}*
  %48 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %44, i32 0, i32 1
  store i8* %42, i8** %48
  %49 = call i8* @GC_malloc(i64 0)
  %50 = bitcast i8* %49 to {}*
  %51 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %44, i32 0, i32 0
  %52 = load {}* (i8*, i64)*, {}* (i8*, i64)** %51
  %53 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %44, i32 0, i32 1
  %54 = load i8*, i8** %53
  %55 = call {}* %52(i8* %54, i64 0)
  ret {}* %55
}

define {}* @go_y363(i8*, i64) {
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
  store {}* (i8*, i64)* @go_y363, {}* (i8*, i64)** %18
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
  store {}* (i8*, i64)* @go_x364, {}* (i8*, i64)** %47
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
  %63 = call {}* @go_y363(i8* %0, i64 %62)
  store {}* %63, {}** %54
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %64 = load {}*, {}** %54
  ret {}* %64
}

define {}* @go_x364(i8*, i64) {
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
  store {}* (i8*, i64)* @go_x364, {}* (i8*, i64)** %18
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
  %39 = call {}* @go_x364(i8* %0, i64 %38)
  store {}* %39, {}** %25
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %40 = load {}*, {}** %25
  ret {}* %40
}

define i1 @next_state365(i8*, i1*, i64, i64) {
  %5 = bitcast i8* %0 to { { i1 (i8*, i1*, i64, i64)*, i8* }* }*
  %6 = getelementptr { { i1 (i8*, i1*, i64, i64)*, i8* }* }, { { i1 (i8*, i1*, i64, i64)*, i8* }* }* %5, i32 0, i32 0
  %7 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %6
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %10 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %9, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @next_state365, i1 (i8*, i1*, i64, i64)** %10
  %11 = call i8* @GC_malloc(i64 0)
  %12 = bitcast i8* %11 to {}*
  %13 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %0, i8** %13
  %14 = call i8* @GC_malloc(i64 0)
  %15 = bitcast i8* %14 to {}*
  %16 = mul i64 ptrtoint (i64* getelementptr inbounds (i64, i64* null, i32 1) to i64), 1
  %17 = call i8* @GC_malloc(i64 %16)
  %18 = bitcast i8* %17 to i64*
  %19 = alloca i64
  store i64 0, i64* %19
  br label %cond_0

cond_0:                                           ; preds = %copyelem_0, %4
  %20 = load i64, i64* %19
  %21 = icmp slt i64 %20, 1
  br i1 %21, label %copyelem_0, label %end_0

copyelem_0:                                       ; preds = %cond_0
  %22 = getelementptr i64, i64* %18, i64 %20
  store i64 0, i64* %22
  %23 = add i64 %20, 1
  store i64 %23, i64* %19
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %24 = sub i64 %2, 1
  %25 = add i64 %3, 1
  %26 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %27 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %26
  %28 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %29 = load i8*, i8** %28
  %30 = call i1 %27(i8* %29, i1* %1, i64 %24, i64 %25)
  %31 = call i64 @to_int366(i1 %30)
  %32 = add i64 %3, 1
  %33 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %34 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %33
  %35 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %36 = load i8*, i8** %35
  %37 = call i1 %34(i8* %36, i1* %1, i64 %2, i64 %32)
  %38 = call i64 @to_int366(i1 %37)
  %39 = add i64 %31, %38
  %40 = add i64 %2, 1
  %41 = add i64 %3, 1
  %42 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %43 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %42
  %44 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %45 = load i8*, i8** %44
  %46 = call i1 %43(i8* %45, i1* %1, i64 %40, i64 %41)
  %47 = call i64 @to_int366(i1 %46)
  %48 = add i64 %39, %47
  %49 = getelementptr i64, i64* %18, i64 0
  store i64 %48, i64* %49
  %50 = call i8* @GC_malloc(i64 0)
  %51 = bitcast i8* %50 to {}*
  %52 = sub i64 %2, 1
  %53 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %54 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %53
  %55 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %56 = load i8*, i8** %55
  %57 = call i1 %54(i8* %56, i1* %1, i64 %52, i64 %3)
  %58 = call i64 @to_int366(i1 %57)
  %59 = add i64 %2, 1
  %60 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %61 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %60
  %62 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %63 = load i8*, i8** %62
  %64 = call i1 %61(i8* %63, i1* %1, i64 %59, i64 %3)
  %65 = call i64 @to_int366(i1 %64)
  %66 = add i64 %58, %65
  %67 = getelementptr i64, i64* %18, i64 0
  %68 = load i64, i64* %67
  %69 = add i64 %66, %68
  %70 = getelementptr i64, i64* %18, i64 0
  store i64 %69, i64* %70
  %71 = call i8* @GC_malloc(i64 0)
  %72 = bitcast i8* %71 to {}*
  %73 = sub i64 %2, 1
  %74 = sub i64 %3, 1
  %75 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %76 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %75
  %77 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %78 = load i8*, i8** %77
  %79 = call i1 %76(i8* %78, i1* %1, i64 %73, i64 %74)
  %80 = call i64 @to_int366(i1 %79)
  %81 = sub i64 %3, 1
  %82 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %83 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %82
  %84 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %85 = load i8*, i8** %84
  %86 = call i1 %83(i8* %85, i1* %1, i64 %2, i64 %81)
  %87 = call i64 @to_int366(i1 %86)
  %88 = add i64 %80, %87
  %89 = add i64 %2, 1
  %90 = sub i64 %3, 1
  %91 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %92 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %91
  %93 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %94 = load i8*, i8** %93
  %95 = call i1 %92(i8* %94, i1* %1, i64 %89, i64 %90)
  %96 = call i64 @to_int366(i1 %95)
  %97 = add i64 %88, %96
  %98 = getelementptr i64, i64* %18, i64 0
  %99 = load i64, i64* %98
  %100 = add i64 %97, %99
  %101 = getelementptr i64, i64* %18, i64 0
  store i64 %100, i64* %101
  %102 = call i8* @GC_malloc(i64 0)
  %103 = bitcast i8* %102 to {}*
  %104 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %105 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %104
  %106 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %107 = load i8*, i8** %106
  %108 = call i1 %105(i8* %107, i1* %1, i64 %2, i64 %3)
  %109 = alloca i1
  br i1 %108, label %then_0, label %else_0

then_0:                                           ; preds = %end_0
  %110 = getelementptr i64, i64* %18, i64 0
  %111 = load i64, i64* %110
  %112 = icmp eq i64 %111, 2
  %113 = getelementptr i64, i64* %18, i64 0
  %114 = load i64, i64* %113
  %115 = icmp eq i64 %114, 3
  %116 = or i1 %112, %115
  store i1 %116, i1* %109
  br label %end_1

else_0:                                           ; preds = %end_0
  %117 = getelementptr i64, i64* %18, i64 0
  %118 = load i64, i64* %117
  %119 = icmp eq i64 %118, 3
  store i1 %119, i1* %109
  br label %end_1

end_1:                                            ; preds = %else_0, %then_0
  %120 = load i1, i1* %109
  ret i1 %120
}

define i64 @to_int366(i1) {
  %2 = alloca i64
  br i1 %0, label %then_0, label %else_0

then_0:                                           ; preds = %1
  store i64 1, i64* %2
  br label %end_0

else_0:                                           ; preds = %1
  store i64 0, i64* %2
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %3 = load i64, i64* %2
  ret i64 %3
}

define {}* @print_cells367(i8*, i1*) {
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
  store {}* (i8*, i1*)* @print_cells367, {}* (i8*, i1*)** %12
  %13 = call i8* @GC_malloc(i64 0)
  %14 = bitcast i8* %13 to {}*
  %15 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %11, i32 0, i32 1
  store i8* %0, i8** %15
  %16 = call i8* @GC_malloc(i64 0)
  %17 = bitcast i8* %16 to {}*
  %18 = call i8* @GC_malloc(i64 ptrtoint ({ i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* getelementptr inbounds ({ i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %19 = bitcast i8* %18 to { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }*
  %20 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %19, i32 0, i32 0
  store i1* %1, i1** %20
  %21 = call i8* @GC_malloc(i64 0)
  %22 = bitcast i8* %21 to {}*
  %23 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %19, i32 0, i32 1
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %5, { i1 (i8*, i1*, i64, i64)*, i8* }** %23
  %24 = call i8* @GC_malloc(i64 0)
  %25 = bitcast i8* %24 to {}*
  %26 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %19, i32 0, i32 2
  store i64 %7, i64* %26
  %27 = call i8* @GC_malloc(i64 0)
  %28 = bitcast i8* %27 to {}*
  %29 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %19, i32 0, i32 3
  store i64 %9, i64* %29
  %30 = call i8* @GC_malloc(i64 0)
  %31 = bitcast i8* %30 to {}*
  %32 = bitcast { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %19 to i8*
  %33 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %34 = bitcast i8* %33 to { {}* (i8*, i64)*, i8* }*
  %35 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %34, i32 0, i32 0
  store {}* (i8*, i64)* @go_y368, {}* (i8*, i64)** %35
  %36 = call i8* @GC_malloc(i64 0)
  %37 = bitcast i8* %36 to {}*
  %38 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %34, i32 0, i32 1
  store i8* %32, i8** %38
  %39 = call i8* @GC_malloc(i64 0)
  %40 = bitcast i8* %39 to {}*
  %41 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %34, i32 0, i32 0
  %42 = load {}* (i8*, i64)*, {}* (i8*, i64)** %41
  %43 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %34, i32 0, i32 1
  %44 = load i8*, i8** %43
  %45 = call {}* %42(i8* %44, i64 0)
  ret {}* %45
}

define {}* @go_y368(i8*, i64) {
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
  store {}* (i8*, i64)* @go_y368, {}* (i8*, i64)** %14
  %15 = call i8* @GC_malloc(i64 0)
  %16 = bitcast i8* %15 to {}*
  %17 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %13, i32 0, i32 1
  store i8* %0, i8** %17
  %18 = call i8* @GC_malloc(i64 0)
  %19 = bitcast i8* %18 to {}*
  %20 = call i8* @GC_malloc(i64 ptrtoint ({ i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* getelementptr inbounds ({ i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* null, i32 1) to i64))
  %21 = bitcast i8* %20 to { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }*
  %22 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %21, i32 0, i32 0
  store i1* %5, i1** %22
  %23 = call i8* @GC_malloc(i64 0)
  %24 = bitcast i8* %23 to {}*
  %25 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %21, i32 0, i32 1
  store { i1 (i8*, i1*, i64, i64)*, i8* }* %7, { i1 (i8*, i1*, i64, i64)*, i8* }** %25
  %26 = call i8* @GC_malloc(i64 0)
  %27 = bitcast i8* %26 to {}*
  %28 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %21, i32 0, i32 2
  store i64 %9, i64* %28
  %29 = call i8* @GC_malloc(i64 0)
  %30 = bitcast i8* %29 to {}*
  %31 = getelementptr { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }, { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %21, i32 0, i32 3
  store i64 %1, i64* %31
  %32 = call i8* @GC_malloc(i64 0)
  %33 = bitcast i8* %32 to {}*
  %34 = bitcast { i1*, { i1 (i8*, i1*, i64, i64)*, i8* }*, i64, i64 }* %21 to i8*
  %35 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %36 = bitcast i8* %35 to { {}* (i8*, i64)*, i8* }*
  %37 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %36, i32 0, i32 0
  store {}* (i8*, i64)* @go_x369, {}* (i8*, i64)** %37
  %38 = call i8* @GC_malloc(i64 0)
  %39 = bitcast i8* %38 to {}*
  %40 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %36, i32 0, i32 1
  store i8* %34, i8** %40
  %41 = call i8* @GC_malloc(i64 0)
  %42 = bitcast i8* %41 to {}*
  %43 = icmp sge i64 %1, %11
  %44 = alloca {}*
  br i1 %43, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %45 = call i8* @GC_malloc(i64 0)
  %46 = bitcast i8* %45 to {}*
  store {}* %46, {}** %44
  br label %end_0

else_0:                                           ; preds = %2
  %47 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %36, i32 0, i32 0
  %48 = load {}* (i8*, i64)*, {}* (i8*, i64)** %47
  %49 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %36, i32 0, i32 1
  %50 = load i8*, i8** %49
  %51 = call {}* %48(i8* %50, i64 0)
  %52 = call {}* @newline379()
  %53 = add i64 %1, 1
  %54 = call {}* @go_y368(i8* %0, i64 %53)
  store {}* %54, {}** %44
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %55 = load {}*, {}** %44
  ret {}* %55
}

define {}* @go_x369(i8*, i64) {
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
  store {}* (i8*, i64)* @go_x369, {}* (i8*, i64)** %14
  %15 = call i8* @GC_malloc(i64 0)
  %16 = bitcast i8* %15 to {}*
  %17 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %13, i32 0, i32 1
  store i8* %0, i8** %17
  %18 = call i8* @GC_malloc(i64 0)
  %19 = bitcast i8* %18 to {}*
  %20 = icmp sge i64 %1, %9
  %21 = alloca {}*
  br i1 %20, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  store {}* %23, {}** %21
  br label %end_0

else_0:                                           ; preds = %2
  %24 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 0
  %25 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %24
  %26 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %7, i32 0, i32 1
  %27 = load i8*, i8** %26
  %28 = call i1 %25(i8* %27, i1* %5, i64 %1, i64 %11)
  %29 = call i8 @to_char370(i1 %28)
  %30 = call {}* @print_char380(i8 %29)
  %31 = add i64 %1, 1
  %32 = call {}* @go_x369(i8* %0, i64 %31)
  store {}* %32, {}** %21
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %33 = load {}*, {}** %21
  ret {}* %33
}

define i8 @to_char370(i1) {
  %2 = alloca i8
  br i1 %0, label %then_0, label %else_0

then_0:                                           ; preds = %1
  store i8 35, i8* %2
  br label %end_0

else_0:                                           ; preds = %1
  store i8 95, i8* %2
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %3 = load i8, i8* %2
  ret i8 %3
}

define {}* @init_cells371(i8*, i1*, i64) {
  %4 = bitcast i8* %0 to { i64, i64 }*
  %5 = getelementptr { i64, i64 }, { i64, i64 }* %4, i32 0, i32 0
  %6 = load i64, i64* %5
  %7 = getelementptr { i64, i64 }, { i64, i64 }* %4, i32 0, i32 1
  %8 = load i64, i64* %7
  %9 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %10 = bitcast i8* %9 to { {}* (i8*, i1*, i64)*, i8* }*
  %11 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %10, i32 0, i32 0
  store {}* (i8*, i1*, i64)* @init_cells371, {}* (i8*, i1*, i64)** %11
  %12 = call i8* @GC_malloc(i64 0)
  %13 = bitcast i8* %12 to {}*
  %14 = getelementptr { {}* (i8*, i1*, i64)*, i8* }, { {}* (i8*, i1*, i64)*, i8* }* %10, i32 0, i32 1
  store i8* %0, i8** %14
  %15 = call i8* @GC_malloc(i64 0)
  %16 = bitcast i8* %15 to {}*
  %17 = mul i64 %6, %8
  %18 = icmp sge i64 %2, %17
  %19 = alloca {}*
  br i1 %18, label %then_0, label %else_0

then_0:                                           ; preds = %3
  %20 = call i8* @GC_malloc(i64 0)
  %21 = bitcast i8* %20 to {}*
  store {}* %21, {}** %19
  br label %end_0

else_0:                                           ; preds = %3
  %22 = call i1 @rand_bool377()
  %23 = getelementptr i1, i1* %1, i64 %2
  store i1 %22, i1* %23
  %24 = call i8* @GC_malloc(i64 0)
  %25 = bitcast i8* %24 to {}*
  %26 = add i64 %2, 1
  %27 = call {}* @init_cells371(i8* %0, i1* %1, i64 %26)
  store {}* %27, {}** %19
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %28 = load {}*, {}** %19
  ret {}* %28
}

define {}* @set372(i8*, i1*, i64, i64, i1) {
  %6 = bitcast i8* %0 to { i64 }*
  %7 = getelementptr { i64 }, { i64 }* %6, i32 0, i32 0
  %8 = load i64, i64* %7
  %9 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %10 = bitcast i8* %9 to { {}* (i8*, i1*, i64, i64, i1)*, i8* }*
  %11 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %10, i32 0, i32 0
  store {}* (i8*, i1*, i64, i64, i1)* @set372, {}* (i8*, i1*, i64, i64, i1)** %11
  %12 = call i8* @GC_malloc(i64 0)
  %13 = bitcast i8* %12 to {}*
  %14 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %10, i32 0, i32 1
  store i8* %0, i8** %14
  %15 = call i8* @GC_malloc(i64 0)
  %16 = bitcast i8* %15 to {}*
  %17 = mul i64 %3, %8
  %18 = add i64 %17, %2
  %19 = getelementptr i1, i1* %1, i64 %18
  store i1 %4, i1* %19
  %20 = call i8* @GC_malloc(i64 0)
  %21 = bitcast i8* %20 to {}*
  ret {}* undef
}

define i1 @view373(i8*, i1*, i64, i64) {
  %5 = bitcast i8* %0 to { i64, i64 }*
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %5, i32 0, i32 0
  %7 = load i64, i64* %6
  %8 = getelementptr { i64, i64 }, { i64, i64 }* %5, i32 0, i32 1
  %9 = load i64, i64* %8
  %10 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %11 = bitcast i8* %10 to { i1 (i8*, i1*, i64, i64)*, i8* }*
  %12 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %11, i32 0, i32 0
  store i1 (i8*, i1*, i64, i64)* @view373, i1 (i8*, i1*, i64, i64)** %12
  %13 = call i8* @GC_malloc(i64 0)
  %14 = bitcast i8* %13 to {}*
  %15 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %0, i8** %15
  %16 = call i8* @GC_malloc(i64 0)
  %17 = bitcast i8* %16 to {}*
  %18 = icmp sge i64 %2, %7
  %19 = icmp sge i64 %3, %9
  %20 = or i1 %18, %19
  %21 = icmp slt i64 %2, 0
  %22 = or i1 %20, %21
  %23 = icmp slt i64 %3, 0
  %24 = or i1 %22, %23
  %25 = alloca i1
  br i1 %24, label %then_0, label %else_0

then_0:                                           ; preds = %4
  store i1 false, i1* %25
  br label %end_0

else_0:                                           ; preds = %4
  %26 = mul i64 %3, %7
  %27 = add i64 %26, %2
  %28 = getelementptr i1, i1* %1, i64 %27
  %29 = load i1, i1* %28
  store i1 %29, i1* %25
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %30 = load i1, i1* %25
  ret i1 %30
}

declare {}* @pulsar(i1*)

define {}* @pulsar374(i1*) {
  %2 = call {}* @pulsar(i1* %0)
  ret {}* %2
}

declare i1* @copy_bool_array(i1*, i64)

define i1* @copy_bool_array375(i1*, i64) {
  %3 = call i1* @copy_bool_array(i1* %0, i64 %1)
  ret i1* %3
}

declare {}* @malgo_sleep(i64)

define {}* @sleep376(i64) {
  %2 = call {}* @malgo_sleep(i64 %0)
  ret {}* %2
}

declare i1 @rand_bool()

define i1 @rand_bool377() {
  %1 = call i1 @rand_bool()
  ret i1 %1
}

declare {}* @gen_seed()

define {}* @gen_seed378() {
  %1 = call {}* @gen_seed()
  ret {}* %1
}

declare {}* @newline()

define {}* @newline379() {
  %1 = call {}* @newline()
  ret {}* %1
}

declare {}* @print_char(i8)

define {}* @print_char380(i8) {
  %2 = call {}* @print_char(i8 %0)
  ret {}* %2
}

define i32 @main() {
  %1 = call i32 @main360()
  ret i32 0
}
