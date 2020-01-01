; ModuleID = './examples/gen/life.mlg.ll'
source_filename = "./examples/life.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare {}* @print_char(i8) local_unnamed_addr

define {}* @print_char373(i8) local_unnamed_addr {
  %2 = tail call {}* @print_char(i8 %0)
  ret {}* %2
}

declare {}* @newline() local_unnamed_addr

define {}* @newline372() local_unnamed_addr {
  %1 = tail call {}* @newline()
  ret {}* %1
}

declare {}* @gen_seed() local_unnamed_addr

define {}* @gen_seed371() local_unnamed_addr {
  %1 = tail call {}* @gen_seed()
  ret {}* %1
}

declare i1 @rand_bool() local_unnamed_addr

define i1 @rand_bool370() local_unnamed_addr {
  %1 = tail call i1 @rand_bool()
  ret i1 %1
}

declare {}* @malgo_sleep(i64) local_unnamed_addr

define {}* @sleep369(i64) local_unnamed_addr {
  %2 = tail call {}* @malgo_sleep(i64 %0)
  ret {}* %2
}

declare { i1*, i64 }* @copy_bool_array({ i1*, i64 }*) local_unnamed_addr

define { i1*, i64 }* @copy_bool_array368({ i1*, i64 }*) local_unnamed_addr {
  %2 = tail call { i1*, i64 }* @copy_bool_array({ i1*, i64 }* %0)
  ret { i1*, i64 }* %2
}

declare {}* @pulsar({ i1*, i64 }*) local_unnamed_addr

define {}* @pulsar367({ i1*, i64 }*) local_unnamed_addr {
  %2 = tail call {}* @pulsar({ i1*, i64 }* %0)
  ret {}* %2
}

declare i8* @GC_malloc(i64) local_unnamed_addr

define i1 @view366(i8*, { i1*, i64 }* nocapture readonly, i64, i64) {
  %5 = bitcast i8* %0 to i64*
  %6 = load i64, i64* %5, align 8
  %7 = getelementptr i8, i8* %0, i64 8
  %8 = bitcast i8* %7 to i64*
  %9 = load i64, i64* %8, align 8
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view366, i1 (i8*, { i1*, i64 }*, i64, i64)** %11, align 8
  %12 = tail call i8* @GC_malloc(i64 0)
  %13 = getelementptr i8, i8* %10, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %0, i8** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 0)
  %16 = tail call i8* @GC_malloc(i64 16)
  %17 = bitcast i8* %16 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set365, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 0)
  %19 = getelementptr i8, i8* %16, i64 8
  %20 = bitcast i8* %19 to i8**
  store i8* %0, i8** %20, align 8
  %21 = tail call i8* @GC_malloc(i64 0)
  %22 = tail call i8* @GC_malloc(i64 16)
  %23 = bitcast i8* %22 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells364, {}* (i8*, { i1*, i64 }*, i64)** %23, align 8
  %24 = tail call i8* @GC_malloc(i64 0)
  %25 = getelementptr i8, i8* %22, i64 8
  %26 = bitcast i8* %25 to i8**
  store i8* %0, i8** %26, align 8
  %27 = tail call i8* @GC_malloc(i64 0)
  %28 = tail call i8* @GC_malloc(i64 16)
  %29 = bitcast i8* %28 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char363, i8 (i8*, i1)** %29, align 8
  %30 = tail call i8* @GC_malloc(i64 0)
  %31 = getelementptr i8, i8* %28, i64 8
  %32 = bitcast i8* %31 to i8**
  store i8* %0, i8** %32, align 8
  %33 = tail call i8* @GC_malloc(i64 0)
  %34 = tail call i8* @GC_malloc(i64 16)
  %35 = bitcast i8* %34 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells360, {}* (i8*, { i1*, i64 }*)** %35, align 8
  %36 = tail call i8* @GC_malloc(i64 0)
  %37 = getelementptr i8, i8* %34, i64 8
  %38 = bitcast i8* %37 to i8**
  store i8* %0, i8** %38, align 8
  %39 = tail call i8* @GC_malloc(i64 0)
  %40 = tail call i8* @GC_malloc(i64 16)
  %41 = bitcast i8* %40 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int359, i64 (i8*, i1)** %41, align 8
  %42 = tail call i8* @GC_malloc(i64 0)
  %43 = getelementptr i8, i8* %40, i64 8
  %44 = bitcast i8* %43 to i8**
  store i8* %0, i8** %44, align 8
  %45 = tail call i8* @GC_malloc(i64 0)
  %46 = tail call i8* @GC_malloc(i64 16)
  %47 = bitcast i8* %46 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state358, i1 (i8*, { i1*, i64 }*, i64, i64)** %47, align 8
  %48 = tail call i8* @GC_malloc(i64 0)
  %49 = getelementptr i8, i8* %46, i64 8
  %50 = bitcast i8* %49 to i8**
  store i8* %0, i8** %50, align 8
  %51 = tail call i8* @GC_malloc(i64 0)
  %52 = tail call i8* @GC_malloc(i64 16)
  %53 = bitcast i8* %52 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells355, {}* (i8*, { i1*, i64 }*)** %53, align 8
  %54 = tail call i8* @GC_malloc(i64 0)
  %55 = getelementptr i8, i8* %52, i64 8
  %56 = bitcast i8* %55 to i8**
  store i8* %0, i8** %56, align 8
  %57 = tail call i8* @GC_malloc(i64 0)
  %58 = icmp sle i64 %6, %2
  %59 = icmp sle i64 %9, %3
  %60 = or i1 %58, %59
  %61 = or i64 %3, %2
  %62 = icmp slt i64 %61, 0
  %63 = or i1 %62, %60
  br i1 %63, label %end_0, label %else_0

else_0:                                           ; preds = %4
  %64 = mul i64 %6, %3
  %65 = add i64 %64, %2
  %66 = getelementptr { i1*, i64 }, { i1*, i64 }* %1, i64 0, i32 0
  %67 = load i1*, i1** %66, align 8
  %68 = getelementptr i1, i1* %67, i64 %65
  %69 = load i1, i1* %68, align 1
  br label %end_0

end_0:                                            ; preds = %4, %else_0
  %.0 = phi i1 [ %69, %else_0 ], [ false, %4 ]
  ret i1 %.0
}

define {}* @set365(i8*, { i1*, i64 }* nocapture readonly, i64, i64, i1) {
  %6 = bitcast i8* %0 to i64*
  %7 = load i64, i64* %6, align 8
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view366, i1 (i8*, { i1*, i64 }*, i64, i64)** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = getelementptr i8, i8* %8, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %0, i8** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set365, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %15, align 8
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = getelementptr i8, i8* %14, i64 8
  %18 = bitcast i8* %17 to i8**
  store i8* %0, i8** %18, align 8
  %19 = tail call i8* @GC_malloc(i64 0)
  %20 = tail call i8* @GC_malloc(i64 16)
  %21 = bitcast i8* %20 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells364, {}* (i8*, { i1*, i64 }*, i64)** %21, align 8
  %22 = tail call i8* @GC_malloc(i64 0)
  %23 = getelementptr i8, i8* %20, i64 8
  %24 = bitcast i8* %23 to i8**
  store i8* %0, i8** %24, align 8
  %25 = tail call i8* @GC_malloc(i64 0)
  %26 = tail call i8* @GC_malloc(i64 16)
  %27 = bitcast i8* %26 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char363, i8 (i8*, i1)** %27, align 8
  %28 = tail call i8* @GC_malloc(i64 0)
  %29 = getelementptr i8, i8* %26, i64 8
  %30 = bitcast i8* %29 to i8**
  store i8* %0, i8** %30, align 8
  %31 = tail call i8* @GC_malloc(i64 0)
  %32 = tail call i8* @GC_malloc(i64 16)
  %33 = bitcast i8* %32 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells360, {}* (i8*, { i1*, i64 }*)** %33, align 8
  %34 = tail call i8* @GC_malloc(i64 0)
  %35 = getelementptr i8, i8* %32, i64 8
  %36 = bitcast i8* %35 to i8**
  store i8* %0, i8** %36, align 8
  %37 = tail call i8* @GC_malloc(i64 0)
  %38 = tail call i8* @GC_malloc(i64 16)
  %39 = bitcast i8* %38 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int359, i64 (i8*, i1)** %39, align 8
  %40 = tail call i8* @GC_malloc(i64 0)
  %41 = getelementptr i8, i8* %38, i64 8
  %42 = bitcast i8* %41 to i8**
  store i8* %0, i8** %42, align 8
  %43 = tail call i8* @GC_malloc(i64 0)
  %44 = tail call i8* @GC_malloc(i64 16)
  %45 = bitcast i8* %44 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state358, i1 (i8*, { i1*, i64 }*, i64, i64)** %45, align 8
  %46 = tail call i8* @GC_malloc(i64 0)
  %47 = getelementptr i8, i8* %44, i64 8
  %48 = bitcast i8* %47 to i8**
  store i8* %0, i8** %48, align 8
  %49 = tail call i8* @GC_malloc(i64 0)
  %50 = tail call i8* @GC_malloc(i64 16)
  %51 = bitcast i8* %50 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells355, {}* (i8*, { i1*, i64 }*)** %51, align 8
  %52 = tail call i8* @GC_malloc(i64 0)
  %53 = getelementptr i8, i8* %50, i64 8
  %54 = bitcast i8* %53 to i8**
  store i8* %0, i8** %54, align 8
  %55 = tail call i8* @GC_malloc(i64 0)
  %56 = mul i64 %7, %3
  %57 = add i64 %56, %2
  %58 = getelementptr { i1*, i64 }, { i1*, i64 }* %1, i64 0, i32 0
  %59 = load i1*, i1** %58, align 8
  %60 = getelementptr i1, i1* %59, i64 %57
  store i1 %4, i1* %60, align 1
  %61 = tail call i8* @GC_malloc(i64 0)
  %62 = bitcast i8* %61 to {}*
  ret {}* %62
}

define {}* @init_cells364(i8*, { i1*, i64 }* nocapture readonly, i64) {
  %4 = bitcast i8* %0 to i64*
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i64*
  %7 = getelementptr { i1*, i64 }, { i1*, i64 }* %1, i64 0, i32 0
  br label %tailrecurse

tailrecurse:                                      ; preds = %else_0, %3
  %.tr3 = phi i64 [ %2, %3 ], [ %66, %else_0 ]
  %8 = load i64, i64* %4, align 8
  %9 = load i64, i64* %6, align 8
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view366, i1 (i8*, { i1*, i64 }*, i64, i64)** %11, align 8
  %12 = tail call i8* @GC_malloc(i64 0)
  %13 = getelementptr i8, i8* %10, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %0, i8** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 0)
  %16 = tail call i8* @GC_malloc(i64 16)
  %17 = bitcast i8* %16 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set365, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 0)
  %19 = getelementptr i8, i8* %16, i64 8
  %20 = bitcast i8* %19 to i8**
  store i8* %0, i8** %20, align 8
  %21 = tail call i8* @GC_malloc(i64 0)
  %22 = tail call i8* @GC_malloc(i64 16)
  %23 = bitcast i8* %22 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells364, {}* (i8*, { i1*, i64 }*, i64)** %23, align 8
  %24 = tail call i8* @GC_malloc(i64 0)
  %25 = getelementptr i8, i8* %22, i64 8
  %26 = bitcast i8* %25 to i8**
  store i8* %0, i8** %26, align 8
  %27 = tail call i8* @GC_malloc(i64 0)
  %28 = tail call i8* @GC_malloc(i64 16)
  %29 = bitcast i8* %28 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char363, i8 (i8*, i1)** %29, align 8
  %30 = tail call i8* @GC_malloc(i64 0)
  %31 = getelementptr i8, i8* %28, i64 8
  %32 = bitcast i8* %31 to i8**
  store i8* %0, i8** %32, align 8
  %33 = tail call i8* @GC_malloc(i64 0)
  %34 = tail call i8* @GC_malloc(i64 16)
  %35 = bitcast i8* %34 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells360, {}* (i8*, { i1*, i64 }*)** %35, align 8
  %36 = tail call i8* @GC_malloc(i64 0)
  %37 = getelementptr i8, i8* %34, i64 8
  %38 = bitcast i8* %37 to i8**
  store i8* %0, i8** %38, align 8
  %39 = tail call i8* @GC_malloc(i64 0)
  %40 = tail call i8* @GC_malloc(i64 16)
  %41 = bitcast i8* %40 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int359, i64 (i8*, i1)** %41, align 8
  %42 = tail call i8* @GC_malloc(i64 0)
  %43 = getelementptr i8, i8* %40, i64 8
  %44 = bitcast i8* %43 to i8**
  store i8* %0, i8** %44, align 8
  %45 = tail call i8* @GC_malloc(i64 0)
  %46 = tail call i8* @GC_malloc(i64 16)
  %47 = bitcast i8* %46 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state358, i1 (i8*, { i1*, i64 }*, i64, i64)** %47, align 8
  %48 = tail call i8* @GC_malloc(i64 0)
  %49 = getelementptr i8, i8* %46, i64 8
  %50 = bitcast i8* %49 to i8**
  store i8* %0, i8** %50, align 8
  %51 = tail call i8* @GC_malloc(i64 0)
  %52 = tail call i8* @GC_malloc(i64 16)
  %53 = bitcast i8* %52 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells355, {}* (i8*, { i1*, i64 }*)** %53, align 8
  %54 = tail call i8* @GC_malloc(i64 0)
  %55 = getelementptr i8, i8* %52, i64 8
  %56 = bitcast i8* %55 to i8**
  store i8* %0, i8** %56, align 8
  %57 = tail call i8* @GC_malloc(i64 0)
  %58 = mul i64 %9, %8
  %59 = icmp sgt i64 %58, %.tr3
  br i1 %59, label %else_0, label %then_0

then_0:                                           ; preds = %tailrecurse
  %60 = tail call i8* @GC_malloc(i64 0)
  %61 = bitcast i8* %60 to {}*
  ret {}* %61

else_0:                                           ; preds = %tailrecurse
  %62 = tail call i1 @rand_bool()
  %63 = load i1*, i1** %7, align 8
  %64 = getelementptr i1, i1* %63, i64 %.tr3
  store i1 %62, i1* %64, align 1
  %65 = tail call i8* @GC_malloc(i64 0)
  %66 = add i64 %.tr3, 1
  br label %tailrecurse
}

define i8 @to_char363(i8*, i1) {
end_0:
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view366, i1 (i8*, { i1*, i64 }*, i64, i64)** %3, align 8
  %4 = tail call i8* @GC_malloc(i64 0)
  %5 = getelementptr i8, i8* %2, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set365, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = getelementptr i8, i8* %8, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %0, i8** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells364, {}* (i8*, { i1*, i64 }*, i64)** %15, align 8
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = getelementptr i8, i8* %14, i64 8
  %18 = bitcast i8* %17 to i8**
  store i8* %0, i8** %18, align 8
  %19 = tail call i8* @GC_malloc(i64 0)
  %20 = tail call i8* @GC_malloc(i64 16)
  %21 = bitcast i8* %20 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char363, i8 (i8*, i1)** %21, align 8
  %22 = tail call i8* @GC_malloc(i64 0)
  %23 = getelementptr i8, i8* %20, i64 8
  %24 = bitcast i8* %23 to i8**
  store i8* %0, i8** %24, align 8
  %25 = tail call i8* @GC_malloc(i64 0)
  %26 = tail call i8* @GC_malloc(i64 16)
  %27 = bitcast i8* %26 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells360, {}* (i8*, { i1*, i64 }*)** %27, align 8
  %28 = tail call i8* @GC_malloc(i64 0)
  %29 = getelementptr i8, i8* %26, i64 8
  %30 = bitcast i8* %29 to i8**
  store i8* %0, i8** %30, align 8
  %31 = tail call i8* @GC_malloc(i64 0)
  %32 = tail call i8* @GC_malloc(i64 16)
  %33 = bitcast i8* %32 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int359, i64 (i8*, i1)** %33, align 8
  %34 = tail call i8* @GC_malloc(i64 0)
  %35 = getelementptr i8, i8* %32, i64 8
  %36 = bitcast i8* %35 to i8**
  store i8* %0, i8** %36, align 8
  %37 = tail call i8* @GC_malloc(i64 0)
  %38 = tail call i8* @GC_malloc(i64 16)
  %39 = bitcast i8* %38 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state358, i1 (i8*, { i1*, i64 }*, i64, i64)** %39, align 8
  %40 = tail call i8* @GC_malloc(i64 0)
  %41 = getelementptr i8, i8* %38, i64 8
  %42 = bitcast i8* %41 to i8**
  store i8* %0, i8** %42, align 8
  %43 = tail call i8* @GC_malloc(i64 0)
  %44 = tail call i8* @GC_malloc(i64 16)
  %45 = bitcast i8* %44 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells355, {}* (i8*, { i1*, i64 }*)** %45, align 8
  %46 = tail call i8* @GC_malloc(i64 0)
  %47 = getelementptr i8, i8* %44, i64 8
  %48 = bitcast i8* %47 to i8**
  store i8* %0, i8** %48, align 8
  %49 = tail call i8* @GC_malloc(i64 0)
  %. = select i1 %1, i8 35, i8 95
  ret i8 %.
}

define {}* @go_x362(i8*, i64) {
  %3 = bitcast i8* %0 to { i1*, i64 }**
  %4 = load { i1*, i64 }*, { i1*, i64 }** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to { i8 (i8*, i1)*, i8* }**
  %7 = load { i8 (i8*, i1)*, i8* }*, { i8 (i8*, i1)*, i8* }** %6, align 8
  %8 = getelementptr i8, i8* %0, i64 16
  %9 = bitcast i8* %8 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }**
  %10 = load { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %9, align 8
  %11 = getelementptr i8, i8* %0, i64 24
  %12 = bitcast i8* %11 to i64*
  %13 = load i64, i64* %12, align 8
  %14 = getelementptr i8, i8* %0, i64 32
  %15 = bitcast i8* %14 to i64*
  %16 = load i64, i64* %15, align 8
  %17 = tail call i8* @GC_malloc(i64 16)
  %18 = bitcast i8* %17 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x362, {}* (i8*, i64)** %18, align 8
  %19 = tail call i8* @GC_malloc(i64 0)
  %20 = getelementptr i8, i8* %17, i64 8
  %21 = bitcast i8* %20 to i8**
  store i8* %0, i8** %21, align 8
  %22 = tail call i8* @GC_malloc(i64 0)
  %23 = icmp sgt i64 %13, %1
  br i1 %23, label %else_0, label %then_0

then_0:                                           ; preds = %else_0, %2
  %24 = tail call i8* @GC_malloc(i64 0)
  %25 = bitcast i8* %24 to {}*
  ret {}* %25

else_0:                                           ; preds = %2, %else_0
  %26 = phi i64 [ %46, %else_0 ], [ %16, %2 ]
  %27 = phi { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* [ %44, %else_0 ], [ %10, %2 ]
  %28 = phi { i8 (i8*, i1)*, i8* }* [ %43, %else_0 ], [ %7, %2 ]
  %29 = phi { i1*, i64 }* [ %42, %else_0 ], [ %4, %2 ]
  %.tr23 = phi i64 [ %41, %else_0 ], [ %1, %2 ]
  %30 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %27, i64 0, i32 0
  %31 = load i1 (i8*, { i1*, i64 }*, i64, i64)*, i1 (i8*, { i1*, i64 }*, i64, i64)** %30, align 8
  %32 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %27, i64 0, i32 1
  %33 = load i8*, i8** %32, align 8
  %34 = tail call i1 %31(i8* %33, { i1*, i64 }* %29, i64 %.tr23, i64 %26)
  %35 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %28, i64 0, i32 0
  %36 = load i8 (i8*, i1)*, i8 (i8*, i1)** %35, align 8
  %37 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %28, i64 0, i32 1
  %38 = load i8*, i8** %37, align 8
  %39 = tail call i8 %36(i8* %38, i1 %34)
  %40 = tail call {}* @print_char(i8 %39)
  %41 = add i64 %.tr23, 1
  %42 = load { i1*, i64 }*, { i1*, i64 }** %3, align 8
  %43 = load { i8 (i8*, i1)*, i8* }*, { i8 (i8*, i1)*, i8* }** %6, align 8
  %44 = load { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %9, align 8
  %45 = load i64, i64* %12, align 8
  %46 = load i64, i64* %15, align 8
  %47 = tail call i8* @GC_malloc(i64 16)
  %48 = bitcast i8* %47 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x362, {}* (i8*, i64)** %48, align 8
  %49 = tail call i8* @GC_malloc(i64 0)
  %50 = getelementptr i8, i8* %47, i64 8
  %51 = bitcast i8* %50 to i8**
  store i8* %0, i8** %51, align 8
  %52 = tail call i8* @GC_malloc(i64 0)
  %53 = icmp sgt i64 %45, %41
  br i1 %53, label %else_0, label %then_0
}

define {}* @go_y361(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = getelementptr i8, i8* %0, i64 8
  %5 = bitcast i8* %4 to i64*
  %6 = getelementptr i8, i8* %0, i64 16
  %7 = bitcast i8* %6 to i64*
  %8 = getelementptr i8, i8* %0, i64 24
  %9 = bitcast i8* %8 to i64*
  %10 = getelementptr i8, i8* %0, i64 32
  %11 = bitcast i8* %10 to i64*
  br label %tailrecurse

tailrecurse:                                      ; preds = %else_0, %2
  %.tr2 = phi i64 [ %1, %2 ], [ %51, %else_0 ]
  %12 = load i64, i64* %3, align 8
  %13 = load i64, i64* %5, align 8
  %14 = load i64, i64* %7, align 8
  %15 = load i64, i64* %9, align 8
  %16 = load i64, i64* %11, align 8
  %17 = tail call i8* @GC_malloc(i64 16)
  %18 = bitcast i8* %17 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_y361, {}* (i8*, i64)** %18, align 8
  %19 = tail call i8* @GC_malloc(i64 0)
  %20 = getelementptr i8, i8* %17, i64 8
  %21 = bitcast i8* %20 to i8**
  store i8* %0, i8** %21, align 8
  %22 = tail call i8* @GC_malloc(i64 0)
  %23 = tail call i8* @GC_malloc(i64 40)
  %24 = bitcast i8* %23 to i64*
  store i64 %12, i64* %24, align 8
  %25 = tail call i8* @GC_malloc(i64 0)
  %26 = getelementptr i8, i8* %23, i64 8
  %27 = bitcast i8* %26 to i64*
  store i64 %13, i64* %27, align 8
  %28 = tail call i8* @GC_malloc(i64 0)
  %29 = getelementptr i8, i8* %23, i64 16
  %30 = bitcast i8* %29 to i64*
  store i64 %14, i64* %30, align 8
  %31 = tail call i8* @GC_malloc(i64 0)
  %32 = getelementptr i8, i8* %23, i64 24
  %33 = bitcast i8* %32 to i64*
  store i64 %15, i64* %33, align 8
  %34 = tail call i8* @GC_malloc(i64 0)
  %35 = getelementptr i8, i8* %23, i64 32
  %36 = bitcast i8* %35 to i64*
  store i64 %.tr2, i64* %36, align 8
  %37 = tail call i8* @GC_malloc(i64 0)
  %38 = tail call i8* @GC_malloc(i64 16)
  %39 = bitcast i8* %38 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x362, {}* (i8*, i64)** %39, align 8
  %40 = tail call i8* @GC_malloc(i64 0)
  %41 = getelementptr i8, i8* %38, i64 8
  %42 = bitcast i8* %41 to i8**
  store i8* %23, i8** %42, align 8
  %43 = tail call i8* @GC_malloc(i64 0)
  %44 = icmp sgt i64 %16, %.tr2
  br i1 %44, label %else_0, label %then_0

then_0:                                           ; preds = %tailrecurse
  %45 = tail call i8* @GC_malloc(i64 0)
  %46 = bitcast i8* %45 to {}*
  ret {}* %46

else_0:                                           ; preds = %tailrecurse
  %47 = load {}* (i8*, i64)*, {}* (i8*, i64)** %39, align 8
  %48 = load i8*, i8** %42, align 8
  %49 = tail call {}* %47(i8* %48, i64 0)
  %50 = tail call {}* @newline()
  %51 = add i64 %.tr2, 1
  br label %tailrecurse
}

define {}* @print_cells360(i8*, { i1*, i64 }*) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i64*
  %7 = load i64, i64* %6, align 8
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view366, i1 (i8*, { i1*, i64 }*, i64, i64)** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = getelementptr i8, i8* %8, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %0, i8** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set365, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %15, align 8
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = getelementptr i8, i8* %14, i64 8
  %18 = bitcast i8* %17 to i8**
  store i8* %0, i8** %18, align 8
  %19 = tail call i8* @GC_malloc(i64 0)
  %20 = tail call i8* @GC_malloc(i64 16)
  %21 = bitcast i8* %20 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells364, {}* (i8*, { i1*, i64 }*, i64)** %21, align 8
  %22 = tail call i8* @GC_malloc(i64 0)
  %23 = getelementptr i8, i8* %20, i64 8
  %24 = bitcast i8* %23 to i8**
  store i8* %0, i8** %24, align 8
  %25 = tail call i8* @GC_malloc(i64 0)
  %26 = tail call i8* @GC_malloc(i64 16)
  %27 = bitcast i8* %26 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char363, i8 (i8*, i1)** %27, align 8
  %28 = tail call i8* @GC_malloc(i64 0)
  %29 = getelementptr i8, i8* %26, i64 8
  %30 = bitcast i8* %29 to i8**
  store i8* %0, i8** %30, align 8
  %31 = tail call i8* @GC_malloc(i64 0)
  %32 = tail call i8* @GC_malloc(i64 16)
  %33 = bitcast i8* %32 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells360, {}* (i8*, { i1*, i64 }*)** %33, align 8
  %34 = tail call i8* @GC_malloc(i64 0)
  %35 = getelementptr i8, i8* %32, i64 8
  %36 = bitcast i8* %35 to i8**
  store i8* %0, i8** %36, align 8
  %37 = tail call i8* @GC_malloc(i64 0)
  %38 = tail call i8* @GC_malloc(i64 16)
  %39 = bitcast i8* %38 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int359, i64 (i8*, i1)** %39, align 8
  %40 = tail call i8* @GC_malloc(i64 0)
  %41 = getelementptr i8, i8* %38, i64 8
  %42 = bitcast i8* %41 to i8**
  store i8* %0, i8** %42, align 8
  %43 = tail call i8* @GC_malloc(i64 0)
  %44 = tail call i8* @GC_malloc(i64 16)
  %45 = bitcast i8* %44 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state358, i1 (i8*, { i1*, i64 }*, i64, i64)** %45, align 8
  %46 = tail call i8* @GC_malloc(i64 0)
  %47 = getelementptr i8, i8* %44, i64 8
  %48 = bitcast i8* %47 to i8**
  store i8* %0, i8** %48, align 8
  %49 = tail call i8* @GC_malloc(i64 0)
  %50 = tail call i8* @GC_malloc(i64 16)
  %51 = bitcast i8* %50 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells355, {}* (i8*, { i1*, i64 }*)** %51, align 8
  %52 = tail call i8* @GC_malloc(i64 0)
  %53 = getelementptr i8, i8* %50, i64 8
  %54 = bitcast i8* %53 to i8**
  store i8* %0, i8** %54, align 8
  %55 = tail call i8* @GC_malloc(i64 0)
  %56 = tail call i8* @GC_malloc(i64 40)
  %57 = bitcast i8* %56 to { i1*, i64 }**
  store { i1*, i64 }* %1, { i1*, i64 }** %57, align 8
  %58 = tail call i8* @GC_malloc(i64 0)
  %59 = getelementptr i8, i8* %56, i64 8
  %60 = bitcast i8* %59 to i8**
  store i8* %26, i8** %60, align 8
  %61 = tail call i8* @GC_malloc(i64 0)
  %62 = getelementptr i8, i8* %56, i64 16
  %63 = bitcast i8* %62 to i8**
  store i8* %8, i8** %63, align 8
  %64 = tail call i8* @GC_malloc(i64 0)
  %65 = getelementptr i8, i8* %56, i64 24
  %66 = bitcast i8* %65 to i64*
  store i64 %4, i64* %66, align 8
  %67 = tail call i8* @GC_malloc(i64 0)
  %68 = getelementptr i8, i8* %56, i64 32
  %69 = bitcast i8* %68 to i64*
  store i64 %7, i64* %69, align 8
  %70 = tail call i8* @GC_malloc(i64 0)
  %71 = tail call i8* @GC_malloc(i64 16)
  %72 = bitcast i8* %71 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_y361, {}* (i8*, i64)** %72, align 8
  %73 = tail call i8* @GC_malloc(i64 0)
  %74 = getelementptr i8, i8* %71, i64 8
  %75 = bitcast i8* %74 to i8**
  store i8* %56, i8** %75, align 8
  %76 = tail call i8* @GC_malloc(i64 0)
  %77 = load {}* (i8*, i64)*, {}* (i8*, i64)** %72, align 8
  %78 = load i8*, i8** %75, align 8
  %79 = tail call {}* %77(i8* %78, i64 0)
  ret {}* %79
}

define i64 @to_int359(i8*, i1) {
end_0:
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view366, i1 (i8*, { i1*, i64 }*, i64, i64)** %3, align 8
  %4 = tail call i8* @GC_malloc(i64 0)
  %5 = getelementptr i8, i8* %2, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set365, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = getelementptr i8, i8* %8, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %0, i8** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells364, {}* (i8*, { i1*, i64 }*, i64)** %15, align 8
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = getelementptr i8, i8* %14, i64 8
  %18 = bitcast i8* %17 to i8**
  store i8* %0, i8** %18, align 8
  %19 = tail call i8* @GC_malloc(i64 0)
  %20 = tail call i8* @GC_malloc(i64 16)
  %21 = bitcast i8* %20 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char363, i8 (i8*, i1)** %21, align 8
  %22 = tail call i8* @GC_malloc(i64 0)
  %23 = getelementptr i8, i8* %20, i64 8
  %24 = bitcast i8* %23 to i8**
  store i8* %0, i8** %24, align 8
  %25 = tail call i8* @GC_malloc(i64 0)
  %26 = tail call i8* @GC_malloc(i64 16)
  %27 = bitcast i8* %26 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells360, {}* (i8*, { i1*, i64 }*)** %27, align 8
  %28 = tail call i8* @GC_malloc(i64 0)
  %29 = getelementptr i8, i8* %26, i64 8
  %30 = bitcast i8* %29 to i8**
  store i8* %0, i8** %30, align 8
  %31 = tail call i8* @GC_malloc(i64 0)
  %32 = tail call i8* @GC_malloc(i64 16)
  %33 = bitcast i8* %32 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int359, i64 (i8*, i1)** %33, align 8
  %34 = tail call i8* @GC_malloc(i64 0)
  %35 = getelementptr i8, i8* %32, i64 8
  %36 = bitcast i8* %35 to i8**
  store i8* %0, i8** %36, align 8
  %37 = tail call i8* @GC_malloc(i64 0)
  %38 = tail call i8* @GC_malloc(i64 16)
  %39 = bitcast i8* %38 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state358, i1 (i8*, { i1*, i64 }*, i64, i64)** %39, align 8
  %40 = tail call i8* @GC_malloc(i64 0)
  %41 = getelementptr i8, i8* %38, i64 8
  %42 = bitcast i8* %41 to i8**
  store i8* %0, i8** %42, align 8
  %43 = tail call i8* @GC_malloc(i64 0)
  %44 = tail call i8* @GC_malloc(i64 16)
  %45 = bitcast i8* %44 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells355, {}* (i8*, { i1*, i64 }*)** %45, align 8
  %46 = tail call i8* @GC_malloc(i64 0)
  %47 = getelementptr i8, i8* %44, i64 8
  %48 = bitcast i8* %47 to i8**
  store i8* %0, i8** %48, align 8
  %49 = tail call i8* @GC_malloc(i64 0)
  %. = zext i1 %1 to i64
  ret i64 %.
}

define i1 @next_state358(i8*, { i1*, i64 }* nocapture readonly, i64, i64) {
body_0:
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view366, i1 (i8*, { i1*, i64 }*, i64, i64)** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 0)
  %7 = getelementptr i8, i8* %4, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %0, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 0)
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set365, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %11, align 8
  %12 = tail call i8* @GC_malloc(i64 0)
  %13 = getelementptr i8, i8* %10, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %0, i8** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 0)
  %16 = tail call i8* @GC_malloc(i64 16)
  %17 = bitcast i8* %16 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells364, {}* (i8*, { i1*, i64 }*, i64)** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 0)
  %19 = getelementptr i8, i8* %16, i64 8
  %20 = bitcast i8* %19 to i8**
  store i8* %0, i8** %20, align 8
  %21 = tail call i8* @GC_malloc(i64 0)
  %22 = tail call i8* @GC_malloc(i64 16)
  %23 = bitcast i8* %22 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char363, i8 (i8*, i1)** %23, align 8
  %24 = tail call i8* @GC_malloc(i64 0)
  %25 = getelementptr i8, i8* %22, i64 8
  %26 = bitcast i8* %25 to i8**
  store i8* %0, i8** %26, align 8
  %27 = tail call i8* @GC_malloc(i64 0)
  %28 = tail call i8* @GC_malloc(i64 16)
  %29 = bitcast i8* %28 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells360, {}* (i8*, { i1*, i64 }*)** %29, align 8
  %30 = tail call i8* @GC_malloc(i64 0)
  %31 = getelementptr i8, i8* %28, i64 8
  %32 = bitcast i8* %31 to i8**
  store i8* %0, i8** %32, align 8
  %33 = tail call i8* @GC_malloc(i64 0)
  %34 = tail call i8* @GC_malloc(i64 16)
  %35 = bitcast i8* %34 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int359, i64 (i8*, i1)** %35, align 8
  %36 = tail call i8* @GC_malloc(i64 0)
  %37 = getelementptr i8, i8* %34, i64 8
  %38 = bitcast i8* %37 to i8**
  store i8* %0, i8** %38, align 8
  %39 = tail call i8* @GC_malloc(i64 0)
  %40 = tail call i8* @GC_malloc(i64 16)
  %41 = bitcast i8* %40 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state358, i1 (i8*, { i1*, i64 }*, i64, i64)** %41, align 8
  %42 = tail call i8* @GC_malloc(i64 0)
  %43 = getelementptr i8, i8* %40, i64 8
  %44 = bitcast i8* %43 to i8**
  store i8* %0, i8** %44, align 8
  %45 = tail call i8* @GC_malloc(i64 0)
  %46 = tail call i8* @GC_malloc(i64 16)
  %47 = bitcast i8* %46 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells355, {}* (i8*, { i1*, i64 }*)** %47, align 8
  %48 = tail call i8* @GC_malloc(i64 0)
  %49 = getelementptr i8, i8* %46, i64 8
  %50 = bitcast i8* %49 to i8**
  store i8* %0, i8** %50, align 8
  %51 = tail call i8* @GC_malloc(i64 0)
  %52 = tail call i8* @GC_malloc(i64 8)
  %53 = tail call i8* @GC_malloc(i64 16)
  %54 = bitcast i8* %53 to i64**
  %55 = bitcast i8* %53 to i8**
  store i8* %52, i8** %55, align 8
  %56 = getelementptr i8, i8* %53, i64 8
  %57 = bitcast i8* %56 to i64*
  store i64 1, i64* %57, align 8
  %58 = bitcast i8* %52 to i64*
  store i64 0, i64* %58, align 8
  %59 = tail call i8* @GC_malloc(i64 0)
  %60 = tail call i8* @GC_malloc(i64 0)
  %61 = add i64 %2, -1
  %62 = add i64 %3, 1
  %63 = tail call i1 @view366(i8* %0, { i1*, i64 }* %1, i64 %61, i64 %62)
  %64 = tail call i64 @to_int359(i8* %0, i1 %63)
  %65 = tail call i1 @view366(i8* %0, { i1*, i64 }* %1, i64 %2, i64 %62)
  %66 = tail call i64 @to_int359(i8* %0, i1 %65)
  %67 = add i64 %66, %64
  %68 = add i64 %2, 1
  %69 = tail call i1 @view366(i8* %0, { i1*, i64 }* %1, i64 %68, i64 %62)
  %70 = tail call i64 @to_int359(i8* %0, i1 %69)
  %71 = add i64 %67, %70
  %72 = load i64*, i64** %54, align 8
  store i64 %71, i64* %72, align 8
  %73 = tail call i8* @GC_malloc(i64 0)
  %74 = tail call i1 @view366(i8* %0, { i1*, i64 }* %1, i64 %61, i64 %3)
  %75 = tail call i64 @to_int359(i8* %0, i1 %74)
  %76 = tail call i1 @view366(i8* %0, { i1*, i64 }* %1, i64 %68, i64 %3)
  %77 = tail call i64 @to_int359(i8* %0, i1 %76)
  %78 = add i64 %77, %75
  %79 = load i64*, i64** %54, align 8
  %80 = load i64, i64* %79, align 8
  %81 = add i64 %78, %80
  store i64 %81, i64* %79, align 8
  %82 = tail call i8* @GC_malloc(i64 0)
  %83 = add i64 %3, -1
  %84 = tail call i1 @view366(i8* %0, { i1*, i64 }* %1, i64 %61, i64 %83)
  %85 = tail call i64 @to_int359(i8* %0, i1 %84)
  %86 = tail call i1 @view366(i8* %0, { i1*, i64 }* %1, i64 %2, i64 %83)
  %87 = tail call i64 @to_int359(i8* %0, i1 %86)
  %88 = add i64 %87, %85
  %89 = tail call i1 @view366(i8* %0, { i1*, i64 }* %1, i64 %68, i64 %83)
  %90 = tail call i64 @to_int359(i8* %0, i1 %89)
  %91 = add i64 %88, %90
  %92 = load i64*, i64** %54, align 8
  %93 = load i64, i64* %92, align 8
  %94 = add i64 %91, %93
  store i64 %94, i64* %92, align 8
  %95 = tail call i8* @GC_malloc(i64 0)
  %96 = tail call i1 @view366(i8* %0, { i1*, i64 }* %1, i64 %2, i64 %3)
  %97 = load i64*, i64** %54, align 8
  %98 = load i64, i64* %97, align 8
  %99 = zext i1 %96 to i64
  %storemerge.in = or i64 %98, %99
  %storemerge = icmp eq i64 %storemerge.in, 3
  ret i1 %storemerge
}

define {}* @go_x357(i8*, i64) {
  %3 = bitcast i8* %0 to { i1*, i64 }**
  %4 = load { i1*, i64 }*, { i1*, i64 }** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to { i1*, i64 }**
  %7 = load { i1*, i64 }*, { i1*, i64 }** %6, align 8
  %8 = getelementptr i8, i8* %0, i64 16
  %9 = bitcast i8* %8 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }**
  %10 = load { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %9, align 8
  %11 = getelementptr i8, i8* %0, i64 24
  %12 = bitcast i8* %11 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }**
  %13 = load { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }** %12, align 8
  %14 = getelementptr i8, i8* %0, i64 32
  %15 = bitcast i8* %14 to i64*
  %16 = load i64, i64* %15, align 8
  %17 = getelementptr i8, i8* %0, i64 40
  %18 = bitcast i8* %17 to i64*
  %19 = load i64, i64* %18, align 8
  %20 = tail call i8* @GC_malloc(i64 16)
  %21 = bitcast i8* %20 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x357, {}* (i8*, i64)** %21, align 8
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
  %30 = phi { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* [ %48, %else_0 ], [ %13, %2 ]
  %31 = phi { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* [ %47, %else_0 ], [ %10, %2 ]
  %32 = phi { i1*, i64 }* [ %46, %else_0 ], [ %7, %2 ]
  %33 = phi { i1*, i64 }* [ %45, %else_0 ], [ %4, %2 ]
  %.tr23 = phi i64 [ %44, %else_0 ], [ %1, %2 ]
  %34 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %31, i64 0, i32 0
  %35 = load i1 (i8*, { i1*, i64 }*, i64, i64)*, i1 (i8*, { i1*, i64 }*, i64, i64)** %34, align 8
  %36 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %31, i64 0, i32 1
  %37 = load i8*, i8** %36, align 8
  %38 = tail call i1 %35(i8* %37, { i1*, i64 }* %32, i64 %.tr23, i64 %29)
  %39 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %30, i64 0, i32 0
  %40 = load {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %39, align 8
  %41 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %30, i64 0, i32 1
  %42 = load i8*, i8** %41, align 8
  %43 = tail call {}* %40(i8* %42, { i1*, i64 }* %33, i64 %.tr23, i64 %29, i1 %38)
  %44 = add i64 %.tr23, 1
  %45 = load { i1*, i64 }*, { i1*, i64 }** %3, align 8
  %46 = load { i1*, i64 }*, { i1*, i64 }** %6, align 8
  %47 = load { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %9, align 8
  %48 = load { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }** %12, align 8
  %49 = load i64, i64* %15, align 8
  %50 = load i64, i64* %18, align 8
  %51 = tail call i8* @GC_malloc(i64 16)
  %52 = bitcast i8* %51 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x357, {}* (i8*, i64)** %52, align 8
  %53 = tail call i8* @GC_malloc(i64 0)
  %54 = getelementptr i8, i8* %51, i64 8
  %55 = bitcast i8* %54 to i8**
  store i8* %0, i8** %55, align 8
  %56 = tail call i8* @GC_malloc(i64 0)
  %57 = icmp sgt i64 %49, %44
  br i1 %57, label %else_0, label %then_0
}

define {}* @go_y356(i8*, i64) {
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
  store {}* (i8*, i64)* @go_y356, {}* (i8*, i64)** %21, align 8
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
  store {}* (i8*, i64)* @go_x357, {}* (i8*, i64)** %45, align 8
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

define {}* @update_cells355(i8*, { i1*, i64 }*) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i64*
  %7 = load i64, i64* %6, align 8
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view366, i1 (i8*, { i1*, i64 }*, i64, i64)** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = getelementptr i8, i8* %8, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %0, i8** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set365, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %15, align 8
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = getelementptr i8, i8* %14, i64 8
  %18 = bitcast i8* %17 to i8**
  store i8* %0, i8** %18, align 8
  %19 = tail call i8* @GC_malloc(i64 0)
  %20 = tail call i8* @GC_malloc(i64 16)
  %21 = bitcast i8* %20 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells364, {}* (i8*, { i1*, i64 }*, i64)** %21, align 8
  %22 = tail call i8* @GC_malloc(i64 0)
  %23 = getelementptr i8, i8* %20, i64 8
  %24 = bitcast i8* %23 to i8**
  store i8* %0, i8** %24, align 8
  %25 = tail call i8* @GC_malloc(i64 0)
  %26 = tail call i8* @GC_malloc(i64 16)
  %27 = bitcast i8* %26 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char363, i8 (i8*, i1)** %27, align 8
  %28 = tail call i8* @GC_malloc(i64 0)
  %29 = getelementptr i8, i8* %26, i64 8
  %30 = bitcast i8* %29 to i8**
  store i8* %0, i8** %30, align 8
  %31 = tail call i8* @GC_malloc(i64 0)
  %32 = tail call i8* @GC_malloc(i64 16)
  %33 = bitcast i8* %32 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells360, {}* (i8*, { i1*, i64 }*)** %33, align 8
  %34 = tail call i8* @GC_malloc(i64 0)
  %35 = getelementptr i8, i8* %32, i64 8
  %36 = bitcast i8* %35 to i8**
  store i8* %0, i8** %36, align 8
  %37 = tail call i8* @GC_malloc(i64 0)
  %38 = tail call i8* @GC_malloc(i64 16)
  %39 = bitcast i8* %38 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int359, i64 (i8*, i1)** %39, align 8
  %40 = tail call i8* @GC_malloc(i64 0)
  %41 = getelementptr i8, i8* %38, i64 8
  %42 = bitcast i8* %41 to i8**
  store i8* %0, i8** %42, align 8
  %43 = tail call i8* @GC_malloc(i64 0)
  %44 = tail call i8* @GC_malloc(i64 16)
  %45 = bitcast i8* %44 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state358, i1 (i8*, { i1*, i64 }*, i64, i64)** %45, align 8
  %46 = tail call i8* @GC_malloc(i64 0)
  %47 = getelementptr i8, i8* %44, i64 8
  %48 = bitcast i8* %47 to i8**
  store i8* %0, i8** %48, align 8
  %49 = tail call i8* @GC_malloc(i64 0)
  %50 = tail call i8* @GC_malloc(i64 16)
  %51 = bitcast i8* %50 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells355, {}* (i8*, { i1*, i64 }*)** %51, align 8
  %52 = tail call i8* @GC_malloc(i64 0)
  %53 = getelementptr i8, i8* %50, i64 8
  %54 = bitcast i8* %53 to i8**
  store i8* %0, i8** %54, align 8
  %55 = tail call i8* @GC_malloc(i64 0)
  %56 = tail call { i1*, i64 }* @copy_bool_array({ i1*, i64 }* %1)
  %57 = tail call i8* @GC_malloc(i64 48)
  %58 = bitcast i8* %57 to { i1*, i64 }**
  store { i1*, i64 }* %1, { i1*, i64 }** %58, align 8
  %59 = tail call i8* @GC_malloc(i64 0)
  %60 = getelementptr i8, i8* %57, i64 8
  %61 = bitcast i8* %60 to { i1*, i64 }**
  store { i1*, i64 }* %56, { i1*, i64 }** %61, align 8
  %62 = tail call i8* @GC_malloc(i64 0)
  %63 = getelementptr i8, i8* %57, i64 16
  %64 = bitcast i8* %63 to i8**
  store i8* %44, i8** %64, align 8
  %65 = tail call i8* @GC_malloc(i64 0)
  %66 = getelementptr i8, i8* %57, i64 24
  %67 = bitcast i8* %66 to i8**
  store i8* %14, i8** %67, align 8
  %68 = tail call i8* @GC_malloc(i64 0)
  %69 = getelementptr i8, i8* %57, i64 32
  %70 = bitcast i8* %69 to i64*
  store i64 %4, i64* %70, align 8
  %71 = tail call i8* @GC_malloc(i64 0)
  %72 = getelementptr i8, i8* %57, i64 40
  %73 = bitcast i8* %72 to i64*
  store i64 %7, i64* %73, align 8
  %74 = tail call i8* @GC_malloc(i64 0)
  %75 = tail call i8* @GC_malloc(i64 16)
  %76 = bitcast i8* %75 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_y356, {}* (i8*, i64)** %76, align 8
  %77 = tail call i8* @GC_malloc(i64 0)
  %78 = getelementptr i8, i8* %75, i64 8
  %79 = bitcast i8* %78 to i8**
  store i8* %57, i8** %79, align 8
  %80 = tail call i8* @GC_malloc(i64 0)
  %81 = load {}* (i8*, i64)*, {}* (i8*, i64)** %76, align 8
  %82 = load i8*, i8** %79, align 8
  %83 = tail call {}* %81(i8* %82, i64 0)
  ret {}* %83
}

define {}* @loop354(i8*, i64) {
  %3 = bitcast i8* %0 to { i1*, i64 }**
  %4 = load { i1*, i64 }*, { i1*, i64 }** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to { {}* (i8*, { i1*, i64 }*)*, i8* }**
  %7 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %6, align 8
  %8 = getelementptr i8, i8* %0, i64 16
  %9 = bitcast i8* %8 to { {}* (i8*, { i1*, i64 }*)*, i8* }**
  %10 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %9, align 8
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop354, {}* (i8*, i64)** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = getelementptr i8, i8* %11, i64 8
  %15 = bitcast i8* %14 to i8**
  store i8* %0, i8** %15, align 8
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = icmp sgt i64 %1, 0
  br i1 %17, label %then_0, label %else_0

then_0:                                           ; preds = %2, %then_0
  %18 = phi { {}* (i8*, { i1*, i64 }*)*, i8* }* [ %35, %then_0 ], [ %10, %2 ]
  %19 = phi { {}* (i8*, { i1*, i64 }*)*, i8* }* [ %34, %then_0 ], [ %7, %2 ]
  %20 = phi { i1*, i64 }* [ %33, %then_0 ], [ %4, %2 ]
  %.tr23 = phi i64 [ %32, %then_0 ], [ %1, %2 ]
  %21 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %19, i64 0, i32 0
  %22 = load {}* (i8*, { i1*, i64 }*)*, {}* (i8*, { i1*, i64 }*)** %21, align 8
  %23 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %19, i64 0, i32 1
  %24 = load i8*, i8** %23, align 8
  %25 = tail call {}* %22(i8* %24, { i1*, i64 }* %20)
  %26 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %18, i64 0, i32 0
  %27 = load {}* (i8*, { i1*, i64 }*)*, {}* (i8*, { i1*, i64 }*)** %26, align 8
  %28 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %18, i64 0, i32 1
  %29 = load i8*, i8** %28, align 8
  %30 = tail call {}* %27(i8* %29, { i1*, i64 }* %20)
  %31 = tail call {}* @newline()
  %32 = add nsw i64 %.tr23, -1
  %33 = load { i1*, i64 }*, { i1*, i64 }** %3, align 8
  %34 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %6, align 8
  %35 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %9, align 8
  %36 = tail call i8* @GC_malloc(i64 16)
  %37 = bitcast i8* %36 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop354, {}* (i8*, i64)** %37, align 8
  %38 = tail call i8* @GC_malloc(i64 0)
  %39 = getelementptr i8, i8* %36, i64 8
  %40 = bitcast i8* %39 to i8**
  store i8* %0, i8** %40, align 8
  %41 = tail call i8* @GC_malloc(i64 0)
  %42 = icmp sgt i64 %32, 0
  br i1 %42, label %then_0, label %else_0

else_0:                                           ; preds = %then_0, %2
  %43 = tail call i8* @GC_malloc(i64 0)
  %44 = bitcast i8* %43 to {}*
  ret {}* %44
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 1000)
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to i1**
  %4 = bitcast i8* %2 to i8**
  store i8* %1, i8** %4, align 8
  %5 = getelementptr i8, i8* %2, i64 8
  %6 = bitcast i8* %5 to i64*
  store i64 1000, i64* %6, align 8
  %7 = bitcast i8* %1 to i1*
  store i1 false, i1* %7, align 1
  %8 = tail call i8* @GC_malloc(i64 0)
  br label %body_0.body_0_crit_edge

body_0.body_0_crit_edge:                          ; preds = %0, %body_0.body_0_crit_edge
  %9 = phi i64 [ 1, %0 ], [ %12, %body_0.body_0_crit_edge ]
  %.pre = load i1*, i1** %3, align 8
  %10 = getelementptr i1, i1* %.pre, i64 %9
  store i1 false, i1* %10, align 1
  %11 = tail call i8* @GC_malloc(i64 0)
  %12 = add nuw nsw i64 %9, 1
  %exitcond = icmp eq i64 %12, 1000
  br i1 %exitcond, label %end_0, label %body_0.body_0_crit_edge

end_0:                                            ; preds = %body_0.body_0_crit_edge
  %13 = bitcast i8* %2 to { i1*, i64 }*
  %14 = tail call i8* @GC_malloc(i64 0)
  %15 = tail call i8* @GC_malloc(i64 16)
  %16 = bitcast i8* %15 to i64*
  store i64 50, i64* %16, align 8
  %17 = tail call i8* @GC_malloc(i64 0)
  %18 = getelementptr i8, i8* %15, i64 8
  %19 = bitcast i8* %18 to i64*
  store i64 20, i64* %19, align 8
  %20 = tail call i8* @GC_malloc(i64 0)
  %21 = tail call i8* @GC_malloc(i64 16)
  %22 = bitcast i8* %21 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view366, i1 (i8*, { i1*, i64 }*, i64, i64)** %22, align 8
  %23 = tail call i8* @GC_malloc(i64 0)
  %24 = getelementptr i8, i8* %21, i64 8
  %25 = bitcast i8* %24 to i8**
  store i8* %15, i8** %25, align 8
  %26 = tail call i8* @GC_malloc(i64 0)
  %27 = tail call i8* @GC_malloc(i64 16)
  %28 = bitcast i8* %27 to i64*
  store i64 50, i64* %28, align 8
  %29 = tail call i8* @GC_malloc(i64 0)
  %30 = getelementptr i8, i8* %27, i64 8
  %31 = bitcast i8* %30 to i64*
  store i64 20, i64* %31, align 8
  %32 = tail call i8* @GC_malloc(i64 0)
  %33 = tail call i8* @GC_malloc(i64 16)
  %34 = bitcast i8* %33 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set365, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %34, align 8
  %35 = tail call i8* @GC_malloc(i64 0)
  %36 = getelementptr i8, i8* %33, i64 8
  %37 = bitcast i8* %36 to i8**
  store i8* %27, i8** %37, align 8
  %38 = tail call i8* @GC_malloc(i64 0)
  %39 = tail call i8* @GC_malloc(i64 16)
  %40 = bitcast i8* %39 to i64*
  store i64 50, i64* %40, align 8
  %41 = tail call i8* @GC_malloc(i64 0)
  %42 = getelementptr i8, i8* %39, i64 8
  %43 = bitcast i8* %42 to i64*
  store i64 20, i64* %43, align 8
  %44 = tail call i8* @GC_malloc(i64 0)
  %45 = tail call i8* @GC_malloc(i64 16)
  %46 = bitcast i8* %45 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells364, {}* (i8*, { i1*, i64 }*, i64)** %46, align 8
  %47 = tail call i8* @GC_malloc(i64 0)
  %48 = getelementptr i8, i8* %45, i64 8
  %49 = bitcast i8* %48 to i8**
  store i8* %39, i8** %49, align 8
  %50 = tail call i8* @GC_malloc(i64 0)
  %51 = tail call i8* @GC_malloc(i64 16)
  %52 = bitcast i8* %51 to i64*
  store i64 50, i64* %52, align 8
  %53 = tail call i8* @GC_malloc(i64 0)
  %54 = getelementptr i8, i8* %51, i64 8
  %55 = bitcast i8* %54 to i64*
  store i64 20, i64* %55, align 8
  %56 = tail call i8* @GC_malloc(i64 0)
  %57 = tail call i8* @GC_malloc(i64 16)
  %58 = bitcast i8* %57 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char363, i8 (i8*, i1)** %58, align 8
  %59 = tail call i8* @GC_malloc(i64 0)
  %60 = getelementptr i8, i8* %57, i64 8
  %61 = bitcast i8* %60 to i8**
  store i8* %51, i8** %61, align 8
  %62 = tail call i8* @GC_malloc(i64 0)
  %63 = tail call i8* @GC_malloc(i64 16)
  %64 = bitcast i8* %63 to i64*
  store i64 50, i64* %64, align 8
  %65 = tail call i8* @GC_malloc(i64 0)
  %66 = getelementptr i8, i8* %63, i64 8
  %67 = bitcast i8* %66 to i64*
  store i64 20, i64* %67, align 8
  %68 = tail call i8* @GC_malloc(i64 0)
  %69 = tail call i8* @GC_malloc(i64 16)
  %70 = bitcast i8* %69 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells360, {}* (i8*, { i1*, i64 }*)** %70, align 8
  %71 = tail call i8* @GC_malloc(i64 0)
  %72 = getelementptr i8, i8* %69, i64 8
  %73 = bitcast i8* %72 to i8**
  store i8* %63, i8** %73, align 8
  %74 = tail call i8* @GC_malloc(i64 0)
  %75 = tail call i8* @GC_malloc(i64 16)
  %76 = bitcast i8* %75 to i64*
  store i64 50, i64* %76, align 8
  %77 = tail call i8* @GC_malloc(i64 0)
  %78 = getelementptr i8, i8* %75, i64 8
  %79 = bitcast i8* %78 to i64*
  store i64 20, i64* %79, align 8
  %80 = tail call i8* @GC_malloc(i64 0)
  %81 = tail call i8* @GC_malloc(i64 16)
  %82 = bitcast i8* %81 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int359, i64 (i8*, i1)** %82, align 8
  %83 = tail call i8* @GC_malloc(i64 0)
  %84 = getelementptr i8, i8* %81, i64 8
  %85 = bitcast i8* %84 to i8**
  store i8* %75, i8** %85, align 8
  %86 = tail call i8* @GC_malloc(i64 0)
  %87 = tail call i8* @GC_malloc(i64 16)
  %88 = bitcast i8* %87 to i64*
  store i64 50, i64* %88, align 8
  %89 = tail call i8* @GC_malloc(i64 0)
  %90 = getelementptr i8, i8* %87, i64 8
  %91 = bitcast i8* %90 to i64*
  store i64 20, i64* %91, align 8
  %92 = tail call i8* @GC_malloc(i64 0)
  %93 = tail call i8* @GC_malloc(i64 16)
  %94 = bitcast i8* %93 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state358, i1 (i8*, { i1*, i64 }*, i64, i64)** %94, align 8
  %95 = tail call i8* @GC_malloc(i64 0)
  %96 = getelementptr i8, i8* %93, i64 8
  %97 = bitcast i8* %96 to i8**
  store i8* %87, i8** %97, align 8
  %98 = tail call i8* @GC_malloc(i64 0)
  %99 = tail call i8* @GC_malloc(i64 16)
  %100 = bitcast i8* %99 to i64*
  store i64 50, i64* %100, align 8
  %101 = tail call i8* @GC_malloc(i64 0)
  %102 = getelementptr i8, i8* %99, i64 8
  %103 = bitcast i8* %102 to i64*
  store i64 20, i64* %103, align 8
  %104 = tail call i8* @GC_malloc(i64 0)
  %105 = tail call i8* @GC_malloc(i64 16)
  %106 = bitcast i8* %105 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells355, {}* (i8*, { i1*, i64 }*)** %106, align 8
  %107 = tail call i8* @GC_malloc(i64 0)
  %108 = getelementptr i8, i8* %105, i64 8
  %109 = bitcast i8* %108 to i8**
  store i8* %99, i8** %109, align 8
  %110 = tail call i8* @GC_malloc(i64 0)
  %111 = tail call {}* @gen_seed()
  %112 = tail call {}* @pulsar({ i1*, i64 }* %13)
  %113 = tail call i8* @GC_malloc(i64 24)
  %114 = bitcast i8* %113 to i8**
  store i8* %2, i8** %114, align 8
  %115 = tail call i8* @GC_malloc(i64 0)
  %116 = getelementptr i8, i8* %113, i64 8
  %117 = bitcast i8* %116 to i8**
  store i8* %69, i8** %117, align 8
  %118 = tail call i8* @GC_malloc(i64 0)
  %119 = getelementptr i8, i8* %113, i64 16
  %120 = bitcast i8* %119 to i8**
  store i8* %105, i8** %120, align 8
  %121 = tail call i8* @GC_malloc(i64 0)
  %122 = tail call i8* @GC_malloc(i64 16)
  %123 = bitcast i8* %122 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop354, {}* (i8*, i64)** %123, align 8
  %124 = tail call i8* @GC_malloc(i64 0)
  %125 = getelementptr i8, i8* %122, i64 8
  %126 = bitcast i8* %125 to i8**
  store i8* %113, i8** %126, align 8
  %127 = tail call i8* @GC_malloc(i64 0)
  %128 = load {}* (i8*, i64)*, {}* (i8*, i64)** %123, align 8
  %129 = load i8*, i8** %126, align 8
  %130 = tail call {}* %128(i8* %129, i64 10)
  ret i32 0
}
