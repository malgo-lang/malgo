; ModuleID = './examples/gen/life.mlg.ll'
source_filename = "./examples/life.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i32 @main360() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 1000)
  br label %copyelem_0

copyelem_0:                                       ; preds = %copyelem_0, %0
  %.01 = phi i64 [ 0, %0 ], [ %31, %copyelem_0 ]
  %2 = getelementptr i8, i8* %1, i64 %.01
  %3 = bitcast i8* %2 to i1*
  store i1 false, i1* %3, align 1
  %4 = or i64 %.01, 1
  %5 = getelementptr i8, i8* %1, i64 %4
  %6 = bitcast i8* %5 to i1*
  store i1 false, i1* %6, align 1
  %7 = add nuw nsw i64 %.01, 2
  %8 = getelementptr i8, i8* %1, i64 %7
  %9 = bitcast i8* %8 to i1*
  store i1 false, i1* %9, align 1
  %10 = add nuw nsw i64 %.01, 3
  %11 = getelementptr i8, i8* %1, i64 %10
  %12 = bitcast i8* %11 to i1*
  store i1 false, i1* %12, align 1
  %13 = add nuw nsw i64 %.01, 4
  %14 = getelementptr i8, i8* %1, i64 %13
  %15 = bitcast i8* %14 to i1*
  store i1 false, i1* %15, align 1
  %16 = add nuw nsw i64 %.01, 5
  %17 = getelementptr i8, i8* %1, i64 %16
  %18 = bitcast i8* %17 to i1*
  store i1 false, i1* %18, align 1
  %19 = add nuw nsw i64 %.01, 6
  %20 = getelementptr i8, i8* %1, i64 %19
  %21 = bitcast i8* %20 to i1*
  store i1 false, i1* %21, align 1
  %22 = add nuw nsw i64 %.01, 7
  %23 = getelementptr i8, i8* %1, i64 %22
  %24 = bitcast i8* %23 to i1*
  store i1 false, i1* %24, align 1
  %25 = add nuw nsw i64 %.01, 8
  %26 = getelementptr i8, i8* %1, i64 %25
  %27 = bitcast i8* %26 to i1*
  store i1 false, i1* %27, align 1
  %28 = add nuw nsw i64 %.01, 9
  %29 = getelementptr i8, i8* %1, i64 %28
  %30 = bitcast i8* %29 to i1*
  store i1 false, i1* %30, align 1
  %31 = add nuw nsw i64 %.01, 10
  %exitcond.9 = icmp eq i64 %31, 1000
  br i1 %exitcond.9, label %end_0, label %copyelem_0

end_0:                                            ; preds = %copyelem_0
  %32 = bitcast i8* %1 to i1*
  %33 = tail call i8* @GC_malloc(i64 16)
  %34 = bitcast i8* %33 to i64*
  store i64 50, i64* %34, align 8
  %35 = tail call i8* @GC_malloc(i64 0)
  %36 = getelementptr i8, i8* %33, i64 8
  %37 = bitcast i8* %36 to i64*
  store i64 20, i64* %37, align 8
  %38 = tail call i8* @GC_malloc(i64 0)
  %39 = tail call i8* @GC_malloc(i64 16)
  %40 = bitcast i8* %39 to i1 (i8*, i1*, i64, i64)**
  store i1 (i8*, i1*, i64, i64)* @view373, i1 (i8*, i1*, i64, i64)** %40, align 8
  %41 = tail call i8* @GC_malloc(i64 0)
  %42 = getelementptr i8, i8* %39, i64 8
  %43 = bitcast i8* %42 to i8**
  store i8* %33, i8** %43, align 8
  %44 = tail call i8* @GC_malloc(i64 0)
  %45 = tail call i8* @GC_malloc(i64 8)
  %46 = bitcast i8* %45 to i64*
  store i64 50, i64* %46, align 8
  %47 = tail call i8* @GC_malloc(i64 0)
  %48 = tail call i8* @GC_malloc(i64 16)
  %49 = bitcast i8* %48 to {}* (i8*, i1*, i64, i64, i1)**
  store {}* (i8*, i1*, i64, i64, i1)* @set372, {}* (i8*, i1*, i64, i64, i1)** %49, align 8
  %50 = tail call i8* @GC_malloc(i64 0)
  %51 = getelementptr i8, i8* %48, i64 8
  %52 = bitcast i8* %51 to i8**
  store i8* %45, i8** %52, align 8
  %53 = tail call i8* @GC_malloc(i64 0)
  %54 = tail call i8* @GC_malloc(i64 16)
  %55 = bitcast i8* %54 to i64*
  store i64 50, i64* %55, align 8
  %56 = tail call i8* @GC_malloc(i64 0)
  %57 = getelementptr i8, i8* %54, i64 8
  %58 = bitcast i8* %57 to i64*
  store i64 20, i64* %58, align 8
  %59 = tail call i8* @GC_malloc(i64 0)
  %60 = tail call i8* @GC_malloc(i64 16)
  %61 = bitcast i8* %60 to {}* (i8*, i1*, i64)**
  store {}* (i8*, i1*, i64)* @init_cells371, {}* (i8*, i1*, i64)** %61, align 8
  %62 = tail call i8* @GC_malloc(i64 0)
  %63 = getelementptr i8, i8* %60, i64 8
  %64 = bitcast i8* %63 to i8**
  store i8* %54, i8** %64, align 8
  %65 = tail call i8* @GC_malloc(i64 0)
  %66 = tail call i8* @GC_malloc(i64 24)
  %67 = bitcast i8* %66 to i8**
  store i8* %39, i8** %67, align 8
  %68 = tail call i8* @GC_malloc(i64 0)
  %69 = getelementptr i8, i8* %66, i64 8
  %70 = bitcast i8* %69 to i64*
  store i64 50, i64* %70, align 8
  %71 = tail call i8* @GC_malloc(i64 0)
  %72 = getelementptr i8, i8* %66, i64 16
  %73 = bitcast i8* %72 to i64*
  store i64 20, i64* %73, align 8
  %74 = tail call i8* @GC_malloc(i64 0)
  %75 = tail call i8* @GC_malloc(i64 16)
  %76 = bitcast i8* %75 to {}* (i8*, i1*)**
  store {}* (i8*, i1*)* @print_cells367, {}* (i8*, i1*)** %76, align 8
  %77 = tail call i8* @GC_malloc(i64 0)
  %78 = getelementptr i8, i8* %75, i64 8
  %79 = bitcast i8* %78 to i8**
  store i8* %66, i8** %79, align 8
  %80 = tail call i8* @GC_malloc(i64 0)
  %81 = tail call i8* @GC_malloc(i64 8)
  %82 = bitcast i8* %81 to i8**
  store i8* %39, i8** %82, align 8
  %83 = tail call i8* @GC_malloc(i64 0)
  %84 = tail call i8* @GC_malloc(i64 16)
  %85 = bitcast i8* %84 to i1 (i8*, i1*, i64, i64)**
  store i1 (i8*, i1*, i64, i64)* @next_state365, i1 (i8*, i1*, i64, i64)** %85, align 8
  %86 = tail call i8* @GC_malloc(i64 0)
  %87 = getelementptr i8, i8* %84, i64 8
  %88 = bitcast i8* %87 to i8**
  store i8* %81, i8** %88, align 8
  %89 = tail call i8* @GC_malloc(i64 0)
  %90 = tail call i8* @GC_malloc(i64 32)
  %91 = bitcast i8* %90 to i8**
  store i8* %84, i8** %91, align 8
  %92 = tail call i8* @GC_malloc(i64 0)
  %93 = getelementptr i8, i8* %90, i64 8
  %94 = bitcast i8* %93 to i8**
  store i8* %48, i8** %94, align 8
  %95 = tail call i8* @GC_malloc(i64 0)
  %96 = getelementptr i8, i8* %90, i64 16
  %97 = bitcast i8* %96 to i64*
  store i64 50, i64* %97, align 8
  %98 = tail call i8* @GC_malloc(i64 0)
  %99 = getelementptr i8, i8* %90, i64 24
  %100 = bitcast i8* %99 to i64*
  store i64 20, i64* %100, align 8
  %101 = tail call i8* @GC_malloc(i64 0)
  %102 = tail call i8* @GC_malloc(i64 16)
  %103 = bitcast i8* %102 to {}* (i8*, i1*)**
  store {}* (i8*, i1*)* @update_cells362, {}* (i8*, i1*)** %103, align 8
  %104 = tail call i8* @GC_malloc(i64 0)
  %105 = getelementptr i8, i8* %102, i64 8
  %106 = bitcast i8* %105 to i8**
  store i8* %90, i8** %106, align 8
  %107 = tail call i8* @GC_malloc(i64 0)
  %108 = tail call {}* @gen_seed()
  %109 = tail call {}* @pulsar(i1* %32)
  %110 = tail call i8* @GC_malloc(i64 24)
  %111 = bitcast i8* %110 to i8**
  store i8* %1, i8** %111, align 8
  %112 = tail call i8* @GC_malloc(i64 0)
  %113 = getelementptr i8, i8* %110, i64 8
  %114 = bitcast i8* %113 to i8**
  store i8* %75, i8** %114, align 8
  %115 = tail call i8* @GC_malloc(i64 0)
  %116 = getelementptr i8, i8* %110, i64 16
  %117 = bitcast i8* %116 to i8**
  store i8* %102, i8** %117, align 8
  %118 = tail call i8* @GC_malloc(i64 0)
  %119 = tail call i8* @GC_malloc(i64 16)
  %120 = bitcast i8* %119 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop361, {}* (i8*, i64)** %120, align 8
  %121 = tail call i8* @GC_malloc(i64 0)
  %122 = getelementptr i8, i8* %119, i64 8
  %123 = bitcast i8* %122 to i8**
  store i8* %110, i8** %123, align 8
  %124 = tail call i8* @GC_malloc(i64 0)
  %125 = load {}* (i8*, i64)*, {}* (i8*, i64)** %120, align 8
  %126 = load i8*, i8** %123, align 8
  %127 = tail call {}* %125(i8* %126, i64 50)
  ret i32 0
}

define {}* @loop361(i8*, i64) {
  %3 = bitcast i8* %0 to i1**
  %4 = load i1*, i1** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to { {}* (i8*, i1*)*, i8* }**
  %7 = load { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }** %6, align 8
  %8 = getelementptr i8, i8* %0, i64 16
  %9 = bitcast i8* %8 to { {}* (i8*, i1*)*, i8* }**
  %10 = load { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }** %9, align 8
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop361, {}* (i8*, i64)** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = getelementptr i8, i8* %11, i64 8
  %15 = bitcast i8* %14 to i8**
  store i8* %0, i8** %15, align 8
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = icmp sgt i64 %1, 0
  br i1 %17, label %then_0, label %else_0

then_0:                                           ; preds = %2, %then_0
  %18 = phi { {}* (i8*, i1*)*, i8* }* [ %36, %then_0 ], [ %10, %2 ]
  %19 = phi { {}* (i8*, i1*)*, i8* }* [ %35, %then_0 ], [ %7, %2 ]
  %20 = phi i1* [ %34, %then_0 ], [ %4, %2 ]
  %.tr23 = phi i64 [ %33, %then_0 ], [ %1, %2 ]
  %21 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %19, i64 0, i32 0
  %22 = load {}* (i8*, i1*)*, {}* (i8*, i1*)** %21, align 8
  %23 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %19, i64 0, i32 1
  %24 = load i8*, i8** %23, align 8
  %25 = tail call {}* %22(i8* %24, i1* %20)
  %26 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %18, i64 0, i32 0
  %27 = load {}* (i8*, i1*)*, {}* (i8*, i1*)** %26, align 8
  %28 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %18, i64 0, i32 1
  %29 = load i8*, i8** %28, align 8
  %30 = tail call {}* %27(i8* %29, i1* %20)
  %31 = tail call {}* @newline()
  %32 = tail call {}* @malgo_sleep(i64 1)
  %33 = add nsw i64 %.tr23, -1
  %34 = load i1*, i1** %3, align 8
  %35 = load { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }** %6, align 8
  %36 = load { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }** %9, align 8
  %37 = tail call i8* @GC_malloc(i64 16)
  %38 = bitcast i8* %37 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop361, {}* (i8*, i64)** %38, align 8
  %39 = tail call i8* @GC_malloc(i64 0)
  %40 = getelementptr i8, i8* %37, i64 8
  %41 = bitcast i8* %40 to i8**
  store i8* %0, i8** %41, align 8
  %42 = tail call i8* @GC_malloc(i64 0)
  %43 = icmp sgt i64 %33, 0
  br i1 %43, label %then_0, label %else_0

else_0:                                           ; preds = %then_0, %2
  %44 = tail call i8* @GC_malloc(i64 0)
  %45 = bitcast i8* %44 to {}*
  ret {}* %45
}

define {}* @update_cells362(i8*, i1*) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i64*
  %7 = load i64, i64* %6, align 8
  %8 = getelementptr i8, i8* %0, i64 16
  %9 = bitcast i8* %8 to i64*
  %10 = load i64, i64* %9, align 8
  %11 = getelementptr i8, i8* %0, i64 24
  %12 = bitcast i8* %11 to i64*
  %13 = load i64, i64* %12, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to {}* (i8*, i1*)**
  store {}* (i8*, i1*)* @update_cells362, {}* (i8*, i1*)** %15, align 8
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = getelementptr i8, i8* %14, i64 8
  %18 = bitcast i8* %17 to i8**
  store i8* %0, i8** %18, align 8
  %19 = tail call i8* @GC_malloc(i64 0)
  %20 = mul i64 %13, %10
  %21 = tail call i1* @copy_bool_array(i1* %1, i64 %20)
  %22 = tail call i8* @GC_malloc(i64 48)
  %23 = bitcast i8* %22 to i1**
  store i1* %1, i1** %23, align 8
  %24 = tail call i8* @GC_malloc(i64 0)
  %25 = getelementptr i8, i8* %22, i64 8
  %26 = bitcast i8* %25 to i1**
  store i1* %21, i1** %26, align 8
  %27 = tail call i8* @GC_malloc(i64 0)
  %28 = getelementptr i8, i8* %22, i64 16
  %29 = bitcast i8* %28 to i64*
  store i64 %4, i64* %29, align 8
  %30 = tail call i8* @GC_malloc(i64 0)
  %31 = getelementptr i8, i8* %22, i64 24
  %32 = bitcast i8* %31 to i64*
  store i64 %7, i64* %32, align 8
  %33 = tail call i8* @GC_malloc(i64 0)
  %34 = getelementptr i8, i8* %22, i64 32
  %35 = bitcast i8* %34 to i64*
  store i64 %10, i64* %35, align 8
  %36 = tail call i8* @GC_malloc(i64 0)
  %37 = getelementptr i8, i8* %22, i64 40
  %38 = bitcast i8* %37 to i64*
  store i64 %13, i64* %38, align 8
  %39 = tail call i8* @GC_malloc(i64 0)
  %40 = tail call i8* @GC_malloc(i64 16)
  %41 = bitcast i8* %40 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_y363, {}* (i8*, i64)** %41, align 8
  %42 = tail call i8* @GC_malloc(i64 0)
  %43 = getelementptr i8, i8* %40, i64 8
  %44 = bitcast i8* %43 to i8**
  store i8* %22, i8** %44, align 8
  %45 = tail call i8* @GC_malloc(i64 0)
  %46 = load {}* (i8*, i64)*, {}* (i8*, i64)** %41, align 8
  %47 = load i8*, i8** %44, align 8
  %48 = tail call {}* %46(i8* %47, i64 0)
  ret {}* %48
}

define {}* @go_y363(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = getelementptr i8, i8* %0, i64 8
  %5 = bitcast i8* %4 to i64*
  %6 = getelementptr i8, i8* %0, i64 16
  %7 = bitcast i8* %6 to i64*
  %8 = getelementptr i8, i8* %0, i64 24
  %9 = bitcast i8* %8 to i64*
  %10 = getelementptr i8, i8* %0, i64 32
  %11 = bitcast i8* %10 to i64*
  %12 = getelementptr i8, i8* %0, i64 40
  %13 = bitcast i8* %12 to i64*
  br label %tailrecurse

tailrecurse:                                      ; preds = %else_0, %2
  %.tr2 = phi i64 [ %1, %2 ], [ %56, %else_0 ]
  %14 = load i64, i64* %3, align 8
  %15 = load i64, i64* %5, align 8
  %16 = load i64, i64* %7, align 8
  %17 = load i64, i64* %9, align 8
  %18 = load i64, i64* %11, align 8
  %19 = load i64, i64* %13, align 8
  %20 = tail call i8* @GC_malloc(i64 16)
  %21 = bitcast i8* %20 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_y363, {}* (i8*, i64)** %21, align 8
  %22 = tail call i8* @GC_malloc(i64 0)
  %23 = getelementptr i8, i8* %20, i64 8
  %24 = bitcast i8* %23 to i8**
  store i8* %0, i8** %24, align 8
  %25 = tail call i8* @GC_malloc(i64 0)
  %26 = tail call i8* @GC_malloc(i64 48)
  %27 = bitcast i8* %26 to i64*
  store i64 %14, i64* %27, align 8
  %28 = tail call i8* @GC_malloc(i64 0)
  %29 = getelementptr i8, i8* %26, i64 8
  %30 = bitcast i8* %29 to i64*
  store i64 %15, i64* %30, align 8
  %31 = tail call i8* @GC_malloc(i64 0)
  %32 = getelementptr i8, i8* %26, i64 16
  %33 = bitcast i8* %32 to i64*
  store i64 %16, i64* %33, align 8
  %34 = tail call i8* @GC_malloc(i64 0)
  %35 = getelementptr i8, i8* %26, i64 24
  %36 = bitcast i8* %35 to i64*
  store i64 %17, i64* %36, align 8
  %37 = tail call i8* @GC_malloc(i64 0)
  %38 = getelementptr i8, i8* %26, i64 32
  %39 = bitcast i8* %38 to i64*
  store i64 %18, i64* %39, align 8
  %40 = tail call i8* @GC_malloc(i64 0)
  %41 = getelementptr i8, i8* %26, i64 40
  %42 = bitcast i8* %41 to i64*
  store i64 %.tr2, i64* %42, align 8
  %43 = tail call i8* @GC_malloc(i64 0)
  %44 = tail call i8* @GC_malloc(i64 16)
  %45 = bitcast i8* %44 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x364, {}* (i8*, i64)** %45, align 8
  %46 = tail call i8* @GC_malloc(i64 0)
  %47 = getelementptr i8, i8* %44, i64 8
  %48 = bitcast i8* %47 to i8**
  store i8* %26, i8** %48, align 8
  %49 = tail call i8* @GC_malloc(i64 0)
  %50 = icmp sgt i64 %19, %.tr2
  br i1 %50, label %else_0, label %then_0

then_0:                                           ; preds = %tailrecurse
  %51 = tail call i8* @GC_malloc(i64 0)
  %52 = bitcast i8* %51 to {}*
  ret {}* %52

else_0:                                           ; preds = %tailrecurse
  %53 = load {}* (i8*, i64)*, {}* (i8*, i64)** %45, align 8
  %54 = load i8*, i8** %48, align 8
  %55 = tail call {}* %53(i8* %54, i64 0)
  %56 = add i64 %.tr2, 1
  br label %tailrecurse
}

define {}* @go_x364(i8*, i64) {
  %3 = bitcast i8* %0 to i1**
  %4 = load i1*, i1** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i1**
  %7 = load i1*, i1** %6, align 8
  %8 = getelementptr i8, i8* %0, i64 16
  %9 = bitcast i8* %8 to { i1 (i8*, i1*, i64, i64)*, i8* }**
  %10 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %9, align 8
  %11 = getelementptr i8, i8* %0, i64 24
  %12 = bitcast i8* %11 to { {}* (i8*, i1*, i64, i64, i1)*, i8* }**
  %13 = load { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }** %12, align 8
  %14 = getelementptr i8, i8* %0, i64 32
  %15 = bitcast i8* %14 to i64*
  %16 = load i64, i64* %15, align 8
  %17 = getelementptr i8, i8* %0, i64 40
  %18 = bitcast i8* %17 to i64*
  %19 = load i64, i64* %18, align 8
  %20 = tail call i8* @GC_malloc(i64 16)
  %21 = bitcast i8* %20 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x364, {}* (i8*, i64)** %21, align 8
  %22 = tail call i8* @GC_malloc(i64 0)
  %23 = getelementptr i8, i8* %20, i64 8
  %24 = bitcast i8* %23 to i8**
  store i8* %0, i8** %24, align 8
  %25 = tail call i8* @GC_malloc(i64 0)
  %26 = icmp sgt i64 %16, %1
  br i1 %26, label %else_0, label %then_0

then_0:                                           ; preds = %else_0, %2
  %27 = tail call i8* @GC_malloc(i64 0)
  %28 = bitcast i8* %27 to {}*
  ret {}* %28

else_0:                                           ; preds = %2, %else_0
  %29 = phi i64 [ %50, %else_0 ], [ %19, %2 ]
  %30 = phi { {}* (i8*, i1*, i64, i64, i1)*, i8* }* [ %48, %else_0 ], [ %13, %2 ]
  %31 = phi { i1 (i8*, i1*, i64, i64)*, i8* }* [ %47, %else_0 ], [ %10, %2 ]
  %32 = phi i1* [ %46, %else_0 ], [ %7, %2 ]
  %33 = phi i1* [ %45, %else_0 ], [ %4, %2 ]
  %.tr23 = phi i64 [ %44, %else_0 ], [ %1, %2 ]
  %34 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %31, i64 0, i32 0
  %35 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %34, align 8
  %36 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %31, i64 0, i32 1
  %37 = load i8*, i8** %36, align 8
  %38 = tail call i1 %35(i8* %37, i1* %32, i64 %.tr23, i64 %29)
  %39 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %30, i64 0, i32 0
  %40 = load {}* (i8*, i1*, i64, i64, i1)*, {}* (i8*, i1*, i64, i64, i1)** %39, align 8
  %41 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %30, i64 0, i32 1
  %42 = load i8*, i8** %41, align 8
  %43 = tail call {}* %40(i8* %42, i1* %33, i64 %.tr23, i64 %29, i1 %38)
  %44 = add i64 %.tr23, 1
  %45 = load i1*, i1** %3, align 8
  %46 = load i1*, i1** %6, align 8
  %47 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %9, align 8
  %48 = load { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }** %12, align 8
  %49 = load i64, i64* %15, align 8
  %50 = load i64, i64* %18, align 8
  %51 = tail call i8* @GC_malloc(i64 16)
  %52 = bitcast i8* %51 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x364, {}* (i8*, i64)** %52, align 8
  %53 = tail call i8* @GC_malloc(i64 0)
  %54 = getelementptr i8, i8* %51, i64 8
  %55 = bitcast i8* %54 to i8**
  store i8* %0, i8** %55, align 8
  %56 = tail call i8* @GC_malloc(i64 0)
  %57 = icmp sgt i64 %49, %44
  br i1 %57, label %else_0, label %then_0
}

define i1 @next_state365(i8*, i1*, i64, i64) {
copyelem_0:
  %4 = bitcast i8* %0 to { i1 (i8*, i1*, i64, i64)*, i8* }**
  %5 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %4, align 8
  %6 = tail call i8* @GC_malloc(i64 16)
  %7 = bitcast i8* %6 to i1 (i8*, i1*, i64, i64)**
  store i1 (i8*, i1*, i64, i64)* @next_state365, i1 (i8*, i1*, i64, i64)** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 0)
  %9 = getelementptr i8, i8* %6, i64 8
  %10 = bitcast i8* %9 to i8**
  store i8* %0, i8** %10, align 8
  %11 = tail call i8* @GC_malloc(i64 0)
  %12 = tail call i8* @GC_malloc(i64 8)
  %13 = bitcast i8* %12 to i64*
  store i64 0, i64* %13, align 8
  %14 = add i64 %2, -1
  %15 = add i64 %3, 1
  %16 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %5, i64 0, i32 0
  %17 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %16, align 8
  %18 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %5, i64 0, i32 1
  %19 = load i8*, i8** %18, align 8
  %20 = tail call i1 %17(i8* %19, i1* %1, i64 %14, i64 %15)
  %..i = zext i1 %20 to i64
  %21 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %16, align 8
  %22 = load i8*, i8** %18, align 8
  %23 = tail call i1 %21(i8* %22, i1* %1, i64 %2, i64 %15)
  %..i1 = zext i1 %23 to i64
  %24 = add nuw nsw i64 %..i1, %..i
  %25 = add i64 %2, 1
  %26 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %16, align 8
  %27 = load i8*, i8** %18, align 8
  %28 = tail call i1 %26(i8* %27, i1* %1, i64 %25, i64 %15)
  %..i3 = zext i1 %28 to i64
  %29 = add nuw nsw i64 %24, %..i3
  store i64 %29, i64* %13, align 8
  %30 = tail call i8* @GC_malloc(i64 0)
  %31 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %16, align 8
  %32 = load i8*, i8** %18, align 8
  %33 = tail call i1 %31(i8* %32, i1* %1, i64 %14, i64 %3)
  %..i5 = zext i1 %33 to i64
  %34 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %16, align 8
  %35 = load i8*, i8** %18, align 8
  %36 = tail call i1 %34(i8* %35, i1* %1, i64 %25, i64 %3)
  %..i7 = zext i1 %36 to i64
  %37 = add nuw nsw i64 %..i7, %..i5
  %38 = load i64, i64* %13, align 8
  %39 = add i64 %37, %38
  store i64 %39, i64* %13, align 8
  %40 = tail call i8* @GC_malloc(i64 0)
  %41 = add i64 %3, -1
  %42 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %16, align 8
  %43 = load i8*, i8** %18, align 8
  %44 = tail call i1 %42(i8* %43, i1* %1, i64 %14, i64 %41)
  %..i6 = zext i1 %44 to i64
  %45 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %16, align 8
  %46 = load i8*, i8** %18, align 8
  %47 = tail call i1 %45(i8* %46, i1* %1, i64 %2, i64 %41)
  %..i4 = zext i1 %47 to i64
  %48 = add nuw nsw i64 %..i4, %..i6
  %49 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %16, align 8
  %50 = load i8*, i8** %18, align 8
  %51 = tail call i1 %49(i8* %50, i1* %1, i64 %25, i64 %41)
  %..i2 = zext i1 %51 to i64
  %52 = add nuw nsw i64 %48, %..i2
  %53 = load i64, i64* %13, align 8
  %54 = add i64 %52, %53
  store i64 %54, i64* %13, align 8
  %55 = tail call i8* @GC_malloc(i64 0)
  %56 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %16, align 8
  %57 = load i8*, i8** %18, align 8
  %58 = tail call i1 %56(i8* %57, i1* %1, i64 %2, i64 %3)
  %59 = load i64, i64* %13, align 8
  %60 = zext i1 %58 to i64
  %storemerge.in = or i64 %59, %60
  %storemerge = icmp eq i64 %storemerge.in, 3
  ret i1 %storemerge
}

; Function Attrs: norecurse nounwind readnone
define i64 @to_int366(i1) local_unnamed_addr #0 {
end_0:
  %. = zext i1 %0 to i64
  ret i64 %.
}

define {}* @print_cells367(i8*, i1*) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i64*
  %7 = load i64, i64* %6, align 8
  %8 = getelementptr i8, i8* %0, i64 16
  %9 = bitcast i8* %8 to i64*
  %10 = load i64, i64* %9, align 8
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to {}* (i8*, i1*)**
  store {}* (i8*, i1*)* @print_cells367, {}* (i8*, i1*)** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = getelementptr i8, i8* %11, i64 8
  %15 = bitcast i8* %14 to i8**
  store i8* %0, i8** %15, align 8
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = tail call i8* @GC_malloc(i64 32)
  %18 = bitcast i8* %17 to i1**
  store i1* %1, i1** %18, align 8
  %19 = tail call i8* @GC_malloc(i64 0)
  %20 = getelementptr i8, i8* %17, i64 8
  %21 = bitcast i8* %20 to i64*
  store i64 %4, i64* %21, align 8
  %22 = tail call i8* @GC_malloc(i64 0)
  %23 = getelementptr i8, i8* %17, i64 16
  %24 = bitcast i8* %23 to i64*
  store i64 %7, i64* %24, align 8
  %25 = tail call i8* @GC_malloc(i64 0)
  %26 = getelementptr i8, i8* %17, i64 24
  %27 = bitcast i8* %26 to i64*
  store i64 %10, i64* %27, align 8
  %28 = tail call i8* @GC_malloc(i64 0)
  %29 = tail call i8* @GC_malloc(i64 16)
  %30 = bitcast i8* %29 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_y368, {}* (i8*, i64)** %30, align 8
  %31 = tail call i8* @GC_malloc(i64 0)
  %32 = getelementptr i8, i8* %29, i64 8
  %33 = bitcast i8* %32 to i8**
  store i8* %17, i8** %33, align 8
  %34 = tail call i8* @GC_malloc(i64 0)
  %35 = load {}* (i8*, i64)*, {}* (i8*, i64)** %30, align 8
  %36 = load i8*, i8** %33, align 8
  %37 = tail call {}* %35(i8* %36, i64 0)
  ret {}* %37
}

define {}* @go_y368(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = getelementptr i8, i8* %0, i64 8
  %5 = bitcast i8* %4 to i64*
  %6 = getelementptr i8, i8* %0, i64 16
  %7 = bitcast i8* %6 to i64*
  %8 = getelementptr i8, i8* %0, i64 24
  %9 = bitcast i8* %8 to i64*
  br label %tailrecurse

tailrecurse:                                      ; preds = %else_0, %2
  %.tr2 = phi i64 [ %1, %2 ], [ %45, %else_0 ]
  %10 = load i64, i64* %3, align 8
  %11 = load i64, i64* %5, align 8
  %12 = load i64, i64* %7, align 8
  %13 = load i64, i64* %9, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_y368, {}* (i8*, i64)** %15, align 8
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = getelementptr i8, i8* %14, i64 8
  %18 = bitcast i8* %17 to i8**
  store i8* %0, i8** %18, align 8
  %19 = tail call i8* @GC_malloc(i64 0)
  %20 = tail call i8* @GC_malloc(i64 32)
  %21 = bitcast i8* %20 to i64*
  store i64 %10, i64* %21, align 8
  %22 = tail call i8* @GC_malloc(i64 0)
  %23 = getelementptr i8, i8* %20, i64 8
  %24 = bitcast i8* %23 to i64*
  store i64 %11, i64* %24, align 8
  %25 = tail call i8* @GC_malloc(i64 0)
  %26 = getelementptr i8, i8* %20, i64 16
  %27 = bitcast i8* %26 to i64*
  store i64 %12, i64* %27, align 8
  %28 = tail call i8* @GC_malloc(i64 0)
  %29 = getelementptr i8, i8* %20, i64 24
  %30 = bitcast i8* %29 to i64*
  store i64 %.tr2, i64* %30, align 8
  %31 = tail call i8* @GC_malloc(i64 0)
  %32 = tail call i8* @GC_malloc(i64 16)
  %33 = bitcast i8* %32 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x369, {}* (i8*, i64)** %33, align 8
  %34 = tail call i8* @GC_malloc(i64 0)
  %35 = getelementptr i8, i8* %32, i64 8
  %36 = bitcast i8* %35 to i8**
  store i8* %20, i8** %36, align 8
  %37 = tail call i8* @GC_malloc(i64 0)
  %38 = icmp sgt i64 %13, %.tr2
  br i1 %38, label %else_0, label %then_0

then_0:                                           ; preds = %tailrecurse
  %39 = tail call i8* @GC_malloc(i64 0)
  %40 = bitcast i8* %39 to {}*
  ret {}* %40

else_0:                                           ; preds = %tailrecurse
  %41 = load {}* (i8*, i64)*, {}* (i8*, i64)** %33, align 8
  %42 = load i8*, i8** %36, align 8
  %43 = tail call {}* %41(i8* %42, i64 0)
  %44 = tail call {}* @newline()
  %45 = add i64 %.tr2, 1
  br label %tailrecurse
}

define {}* @go_x369(i8*, i64) {
  %3 = bitcast i8* %0 to i1**
  %4 = load i1*, i1** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to { i1 (i8*, i1*, i64, i64)*, i8* }**
  %7 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %6, align 8
  %8 = getelementptr i8, i8* %0, i64 16
  %9 = bitcast i8* %8 to i64*
  %10 = load i64, i64* %9, align 8
  %11 = getelementptr i8, i8* %0, i64 24
  %12 = bitcast i8* %11 to i64*
  %13 = load i64, i64* %12, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x369, {}* (i8*, i64)** %15, align 8
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = getelementptr i8, i8* %14, i64 8
  %18 = bitcast i8* %17 to i8**
  store i8* %0, i8** %18, align 8
  %19 = tail call i8* @GC_malloc(i64 0)
  %20 = icmp sgt i64 %10, %1
  br i1 %20, label %else_0, label %then_0

then_0:                                           ; preds = %else_0, %2
  %21 = tail call i8* @GC_malloc(i64 0)
  %22 = bitcast i8* %21 to {}*
  ret {}* %22

else_0:                                           ; preds = %2, %else_0
  %23 = phi i64 [ %36, %else_0 ], [ %13, %2 ]
  %24 = phi { i1 (i8*, i1*, i64, i64)*, i8* }* [ %34, %else_0 ], [ %7, %2 ]
  %25 = phi i1* [ %33, %else_0 ], [ %4, %2 ]
  %.tr23 = phi i64 [ %32, %else_0 ], [ %1, %2 ]
  %26 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %24, i64 0, i32 0
  %27 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %26, align 8
  %28 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %24, i64 0, i32 1
  %29 = load i8*, i8** %28, align 8
  %30 = tail call i1 %27(i8* %29, i1* %25, i64 %.tr23, i64 %23)
  %..i = select i1 %30, i8 35, i8 95
  %31 = tail call {}* @print_char(i8 %..i)
  %32 = add i64 %.tr23, 1
  %33 = load i1*, i1** %3, align 8
  %34 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %6, align 8
  %35 = load i64, i64* %9, align 8
  %36 = load i64, i64* %12, align 8
  %37 = tail call i8* @GC_malloc(i64 16)
  %38 = bitcast i8* %37 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x369, {}* (i8*, i64)** %38, align 8
  %39 = tail call i8* @GC_malloc(i64 0)
  %40 = getelementptr i8, i8* %37, i64 8
  %41 = bitcast i8* %40 to i8**
  store i8* %0, i8** %41, align 8
  %42 = tail call i8* @GC_malloc(i64 0)
  %43 = icmp sgt i64 %35, %32
  br i1 %43, label %else_0, label %then_0
}

; Function Attrs: norecurse nounwind readnone
define i8 @to_char370(i1) local_unnamed_addr #0 {
end_0:
  %. = select i1 %0, i8 35, i8 95
  ret i8 %.
}

define {}* @init_cells371(i8*, i1* nocapture, i64) {
  %4 = bitcast i8* %0 to i64*
  %5 = load i64, i64* %4, align 8
  %6 = getelementptr i8, i8* %0, i64 8
  %7 = bitcast i8* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to {}* (i8*, i1*, i64)**
  store {}* (i8*, i1*, i64)* @init_cells371, {}* (i8*, i1*, i64)** %10, align 8
  %11 = tail call i8* @GC_malloc(i64 0)
  %12 = getelementptr i8, i8* %9, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %0, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 0)
  %15 = mul i64 %8, %5
  %16 = icmp sgt i64 %15, %2
  br i1 %16, label %else_0, label %then_0

then_0:                                           ; preds = %else_0, %3
  %17 = tail call i8* @GC_malloc(i64 0)
  %18 = bitcast i8* %17 to {}*
  ret {}* %18

else_0:                                           ; preds = %3, %else_0
  %.tr34 = phi i64 [ %22, %else_0 ], [ %2, %3 ]
  %19 = tail call i1 @rand_bool()
  %20 = getelementptr i1, i1* %1, i64 %.tr34
  store i1 %19, i1* %20, align 1
  %21 = tail call i8* @GC_malloc(i64 0)
  %22 = add i64 %.tr34, 1
  %23 = load i64, i64* %4, align 8
  %24 = load i64, i64* %7, align 8
  %25 = tail call i8* @GC_malloc(i64 16)
  %26 = bitcast i8* %25 to {}* (i8*, i1*, i64)**
  store {}* (i8*, i1*, i64)* @init_cells371, {}* (i8*, i1*, i64)** %26, align 8
  %27 = tail call i8* @GC_malloc(i64 0)
  %28 = getelementptr i8, i8* %25, i64 8
  %29 = bitcast i8* %28 to i8**
  store i8* %0, i8** %29, align 8
  %30 = tail call i8* @GC_malloc(i64 0)
  %31 = mul i64 %24, %23
  %32 = icmp sgt i64 %31, %22
  br i1 %32, label %else_0, label %then_0
}

define noalias {}* @set372(i8*, i1* nocapture, i64, i64, i1) {
  %6 = bitcast i8* %0 to i64*
  %7 = load i64, i64* %6, align 8
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to {}* (i8*, i1*, i64, i64, i1)**
  store {}* (i8*, i1*, i64, i64, i1)* @set372, {}* (i8*, i1*, i64, i64, i1)** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = getelementptr i8, i8* %8, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %0, i8** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = mul i64 %7, %3
  %15 = add i64 %14, %2
  %16 = getelementptr i1, i1* %1, i64 %15
  store i1 %4, i1* %16, align 1
  %17 = tail call i8* @GC_malloc(i64 0)
  ret {}* undef
}

define i1 @view373(i8*, i1* nocapture readonly, i64, i64) {
  %5 = bitcast i8* %0 to i64*
  %6 = load i64, i64* %5, align 8
  %7 = getelementptr i8, i8* %0, i64 8
  %8 = bitcast i8* %7 to i64*
  %9 = load i64, i64* %8, align 8
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to i1 (i8*, i1*, i64, i64)**
  store i1 (i8*, i1*, i64, i64)* @view373, i1 (i8*, i1*, i64, i64)** %11, align 8
  %12 = tail call i8* @GC_malloc(i64 0)
  %13 = getelementptr i8, i8* %10, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %0, i8** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 0)
  %16 = icmp sle i64 %6, %2
  %17 = icmp sle i64 %9, %3
  %18 = or i1 %16, %17
  %19 = or i64 %3, %2
  %20 = icmp slt i64 %19, 0
  %21 = or i1 %20, %18
  br i1 %21, label %end_0, label %else_0

else_0:                                           ; preds = %4
  %22 = mul i64 %6, %3
  %23 = add i64 %22, %2
  %24 = getelementptr i1, i1* %1, i64 %23
  %25 = load i1, i1* %24, align 1
  br label %end_0

end_0:                                            ; preds = %4, %else_0
  %.0 = phi i1 [ %25, %else_0 ], [ false, %4 ]
  ret i1 %.0
}

declare {}* @pulsar(i1*) local_unnamed_addr

define {}* @pulsar374(i1*) local_unnamed_addr {
  %2 = tail call {}* @pulsar(i1* %0)
  ret {}* %2
}

declare i1* @copy_bool_array(i1*, i64) local_unnamed_addr

define i1* @copy_bool_array375(i1*, i64) local_unnamed_addr {
  %3 = tail call i1* @copy_bool_array(i1* %0, i64 %1)
  ret i1* %3
}

declare {}* @malgo_sleep(i64) local_unnamed_addr

define {}* @sleep376(i64) local_unnamed_addr {
  %2 = tail call {}* @malgo_sleep(i64 %0)
  ret {}* %2
}

declare i1 @rand_bool() local_unnamed_addr

define i1 @rand_bool377() local_unnamed_addr {
  %1 = tail call i1 @rand_bool()
  ret i1 %1
}

declare {}* @gen_seed() local_unnamed_addr

define {}* @gen_seed378() local_unnamed_addr {
  %1 = tail call {}* @gen_seed()
  ret {}* %1
}

declare {}* @newline() local_unnamed_addr

define {}* @newline379() local_unnamed_addr {
  %1 = tail call {}* @newline()
  ret {}* %1
}

declare {}* @print_char(i8) local_unnamed_addr

define {}* @print_char380(i8) local_unnamed_addr {
  %2 = tail call {}* @print_char(i8 %0)
  ret {}* %2
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i32 @main360()
  ret i32 0
}

attributes #0 = { norecurse nounwind readnone }
