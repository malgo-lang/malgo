; ModuleID = './examples/gen/life.mlg.ll'
source_filename = "./examples/life.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i1 @view10(i8*, { i1*, i64 }* nocapture readonly, i64, i64) {
  %5 = bitcast i8* %0 to i64*
  %6 = load i64, i64* %5, align 8
  %7 = getelementptr i8, i8* %0, i64 8
  %8 = bitcast i8* %7 to i64*
  %9 = load i64, i64* %8, align 8
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %11, align 8
  %12 = getelementptr i8, i8* %10, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %0, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %15, align 8
  %16 = getelementptr i8, i8* %14, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %0, i8** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 16)
  %19 = bitcast i8* %18 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %19, align 8
  %20 = getelementptr i8, i8* %18, i64 8
  %21 = bitcast i8* %20 to i8**
  store i8* %0, i8** %21, align 8
  %22 = tail call i8* @GC_malloc(i64 16)
  %23 = bitcast i8* %22 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %23, align 8
  %24 = getelementptr i8, i8* %22, i64 8
  %25 = bitcast i8* %24 to i8**
  store i8* %0, i8** %25, align 8
  %26 = tail call i8* @GC_malloc(i64 16)
  %27 = bitcast i8* %26 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %27, align 8
  %28 = getelementptr i8, i8* %26, i64 8
  %29 = bitcast i8* %28 to i8**
  store i8* %0, i8** %29, align 8
  %30 = tail call i8* @GC_malloc(i64 16)
  %31 = bitcast i8* %30 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %31, align 8
  %32 = getelementptr i8, i8* %30, i64 8
  %33 = bitcast i8* %32 to i8**
  store i8* %0, i8** %33, align 8
  %34 = tail call i8* @GC_malloc(i64 16)
  %35 = bitcast i8* %34 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %35, align 8
  %36 = getelementptr i8, i8* %34, i64 8
  %37 = bitcast i8* %36 to i8**
  store i8* %0, i8** %37, align 8
  %38 = tail call i8* @GC_malloc(i64 16)
  %39 = bitcast i8* %38 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %39, align 8
  %40 = getelementptr i8, i8* %38, i64 8
  %41 = bitcast i8* %40 to i8**
  store i8* %0, i8** %41, align 8
  %42 = icmp sle i64 %6, %2
  %43 = icmp sle i64 %9, %3
  %44 = or i1 %42, %43
  %45 = or i64 %3, %2
  %46 = icmp slt i64 %45, 0
  %47 = or i1 %46, %44
  br i1 %47, label %end_0, label %else_0

else_0:                                           ; preds = %4
  %48 = mul i64 %6, %3
  %49 = add i64 %48, %2
  %50 = getelementptr { i1*, i64 }, { i1*, i64 }* %1, i64 0, i32 0
  %51 = load i1*, i1** %50, align 8
  %52 = getelementptr i1, i1* %51, i64 %49
  %53 = load i1, i1* %52, align 1
  br label %end_0

end_0:                                            ; preds = %4, %else_0
  %.0 = phi i1 [ %53, %else_0 ], [ false, %4 ]
  ret i1 %.0
}

define {}* @update_cells17(i8*, { i1*, i64 }*) {
  %3 = bitcast i8* %0 to <2 x i64>*
  %4 = load <2 x i64>, <2 x i64>* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %6, align 8
  %7 = getelementptr i8, i8* %5, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %0, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %10, align 8
  %11 = getelementptr i8, i8* %9, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %0, i8** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 16)
  %14 = bitcast i8* %13 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %14, align 8
  %15 = getelementptr i8, i8* %13, i64 8
  %16 = bitcast i8* %15 to i8**
  store i8* %0, i8** %16, align 8
  %17 = tail call i8* @GC_malloc(i64 16)
  %18 = bitcast i8* %17 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %18, align 8
  %19 = getelementptr i8, i8* %17, i64 8
  %20 = bitcast i8* %19 to i8**
  store i8* %0, i8** %20, align 8
  %21 = tail call i8* @GC_malloc(i64 16)
  %22 = bitcast i8* %21 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %22, align 8
  %23 = getelementptr i8, i8* %21, i64 8
  %24 = bitcast i8* %23 to i8**
  store i8* %0, i8** %24, align 8
  %25 = tail call i8* @GC_malloc(i64 16)
  %26 = bitcast i8* %25 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %26, align 8
  %27 = getelementptr i8, i8* %25, i64 8
  %28 = bitcast i8* %27 to i8**
  store i8* %0, i8** %28, align 8
  %29 = tail call i8* @GC_malloc(i64 16)
  %30 = bitcast i8* %29 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %30, align 8
  %31 = getelementptr i8, i8* %29, i64 8
  %32 = bitcast i8* %31 to i8**
  store i8* %0, i8** %32, align 8
  %33 = tail call i8* @GC_malloc(i64 16)
  %34 = bitcast i8* %33 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %34, align 8
  %35 = getelementptr i8, i8* %33, i64 8
  %36 = bitcast i8* %35 to i8**
  store i8* %0, i8** %36, align 8
  %37 = tail call { i1*, i64 }* @copy_bool_array({ i1*, i64 }* %1)
  %38 = tail call i8* @GC_malloc(i64 48)
  %39 = bitcast i8* %38 to { i1*, i64 }**
  store { i1*, i64 }* %1, { i1*, i64 }** %39, align 8
  %40 = getelementptr i8, i8* %38, i64 8
  %41 = bitcast i8* %40 to { i1*, i64 }**
  store { i1*, i64 }* %37, { i1*, i64 }** %41, align 8
  %42 = getelementptr i8, i8* %38, i64 16
  %43 = bitcast i8* %42 to i8**
  store i8* %29, i8** %43, align 8
  %44 = getelementptr i8, i8* %38, i64 24
  %45 = bitcast i8* %44 to i8**
  store i8* %9, i8** %45, align 8
  %46 = getelementptr i8, i8* %38, i64 32
  %47 = bitcast i8* %46 to <2 x i64>*
  store <2 x i64> %4, <2 x i64>* %47, align 8
  %48 = tail call i8* @GC_malloc(i64 16)
  %49 = bitcast i8* %48 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_y40, {}* (i8*, i64)** %49, align 8
  %50 = getelementptr i8, i8* %48, i64 8
  %51 = bitcast i8* %50 to i8**
  store i8* %38, i8** %51, align 8
  %52 = tail call {}* @go_y40(i8* %38, i64 0)
  ret {}* %52
}

define i64 @to_int15(i8*, i1) {
end_0:
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %3, align 8
  %4 = getelementptr i8, i8* %2, i64 8
  %5 = bitcast i8* %4 to i8**
  store i8* %0, i8** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 16)
  %7 = bitcast i8* %6 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %7, align 8
  %8 = getelementptr i8, i8* %6, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %0, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %11, align 8
  %12 = getelementptr i8, i8* %10, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %0, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %15, align 8
  %16 = getelementptr i8, i8* %14, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %0, i8** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 16)
  %19 = bitcast i8* %18 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %19, align 8
  %20 = getelementptr i8, i8* %18, i64 8
  %21 = bitcast i8* %20 to i8**
  store i8* %0, i8** %21, align 8
  %22 = tail call i8* @GC_malloc(i64 16)
  %23 = bitcast i8* %22 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %23, align 8
  %24 = getelementptr i8, i8* %22, i64 8
  %25 = bitcast i8* %24 to i8**
  store i8* %0, i8** %25, align 8
  %26 = tail call i8* @GC_malloc(i64 16)
  %27 = bitcast i8* %26 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %27, align 8
  %28 = getelementptr i8, i8* %26, i64 8
  %29 = bitcast i8* %28 to i8**
  store i8* %0, i8** %29, align 8
  %30 = tail call i8* @GC_malloc(i64 16)
  %31 = bitcast i8* %30 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %31, align 8
  %32 = getelementptr i8, i8* %30, i64 8
  %33 = bitcast i8* %32 to i8**
  store i8* %0, i8** %33, align 8
  %. = zext i1 %1 to i64
  ret i64 %.
}

define i8 @to_char13(i8*, i1) {
end_0:
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %3, align 8
  %4 = getelementptr i8, i8* %2, i64 8
  %5 = bitcast i8* %4 to i8**
  store i8* %0, i8** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 16)
  %7 = bitcast i8* %6 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %7, align 8
  %8 = getelementptr i8, i8* %6, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %0, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %11, align 8
  %12 = getelementptr i8, i8* %10, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %0, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %15, align 8
  %16 = getelementptr i8, i8* %14, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %0, i8** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 16)
  %19 = bitcast i8* %18 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %19, align 8
  %20 = getelementptr i8, i8* %18, i64 8
  %21 = bitcast i8* %20 to i8**
  store i8* %0, i8** %21, align 8
  %22 = tail call i8* @GC_malloc(i64 16)
  %23 = bitcast i8* %22 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %23, align 8
  %24 = getelementptr i8, i8* %22, i64 8
  %25 = bitcast i8* %24 to i8**
  store i8* %0, i8** %25, align 8
  %26 = tail call i8* @GC_malloc(i64 16)
  %27 = bitcast i8* %26 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %27, align 8
  %28 = getelementptr i8, i8* %26, i64 8
  %29 = bitcast i8* %28 to i8**
  store i8* %0, i8** %29, align 8
  %30 = tail call i8* @GC_malloc(i64 16)
  %31 = bitcast i8* %30 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %31, align 8
  %32 = getelementptr i8, i8* %30, i64 8
  %33 = bitcast i8* %32 to i8**
  store i8* %0, i8** %33, align 8
  %. = select i1 %1, i8 35, i8 95
  ret i8 %.
}

declare {}* @malgo_sleep(i64) local_unnamed_addr

define {}* @sleep4(i64) local_unnamed_addr {
  %2 = tail call {}* @malgo_sleep(i64 %0)
  ret {}* %2
}

define noalias {}* @set11(i8*, { i1*, i64 }* nocapture readonly, i64, i64, i1) {
  %6 = bitcast i8* %0 to i64*
  %7 = load i64, i64* %6, align 8
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %9, align 8
  %10 = getelementptr i8, i8* %8, i64 8
  %11 = bitcast i8* %10 to i8**
  store i8* %0, i8** %11, align 8
  %12 = tail call i8* @GC_malloc(i64 16)
  %13 = bitcast i8* %12 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %13, align 8
  %14 = getelementptr i8, i8* %12, i64 8
  %15 = bitcast i8* %14 to i8**
  store i8* %0, i8** %15, align 8
  %16 = tail call i8* @GC_malloc(i64 16)
  %17 = bitcast i8* %16 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %17, align 8
  %18 = getelementptr i8, i8* %16, i64 8
  %19 = bitcast i8* %18 to i8**
  store i8* %0, i8** %19, align 8
  %20 = tail call i8* @GC_malloc(i64 16)
  %21 = bitcast i8* %20 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %21, align 8
  %22 = getelementptr i8, i8* %20, i64 8
  %23 = bitcast i8* %22 to i8**
  store i8* %0, i8** %23, align 8
  %24 = tail call i8* @GC_malloc(i64 16)
  %25 = bitcast i8* %24 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %25, align 8
  %26 = getelementptr i8, i8* %24, i64 8
  %27 = bitcast i8* %26 to i8**
  store i8* %0, i8** %27, align 8
  %28 = tail call i8* @GC_malloc(i64 16)
  %29 = bitcast i8* %28 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %29, align 8
  %30 = getelementptr i8, i8* %28, i64 8
  %31 = bitcast i8* %30 to i8**
  store i8* %0, i8** %31, align 8
  %32 = tail call i8* @GC_malloc(i64 16)
  %33 = bitcast i8* %32 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %33, align 8
  %34 = getelementptr i8, i8* %32, i64 8
  %35 = bitcast i8* %34 to i8**
  store i8* %0, i8** %35, align 8
  %36 = tail call i8* @GC_malloc(i64 16)
  %37 = bitcast i8* %36 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %37, align 8
  %38 = getelementptr i8, i8* %36, i64 8
  %39 = bitcast i8* %38 to i8**
  store i8* %0, i8** %39, align 8
  %40 = mul i64 %7, %3
  %41 = add i64 %40, %2
  %42 = getelementptr { i1*, i64 }, { i1*, i64 }* %1, i64 0, i32 0
  %43 = load i1*, i1** %42, align 8
  %44 = getelementptr i1, i1* %43, i64 %41
  store i1 %4, i1* %44, align 1
  ret {}* undef
}

declare i1 @rand_bool() local_unnamed_addr

define i1 @rand_bool3() local_unnamed_addr {
  %1 = tail call i1 @rand_bool()
  ret i1 %1
}

declare {}* @pulsar({ i1*, i64 }*) local_unnamed_addr

define {}* @pulsar6({ i1*, i64 }*) local_unnamed_addr {
  %2 = tail call {}* @pulsar({ i1*, i64 }* %0)
  ret {}* %2
}

declare {}* @print_char(i8) local_unnamed_addr

define {}* @print_char0(i8) local_unnamed_addr {
  %2 = tail call {}* @print_char(i8 %0)
  ret {}* %2
}

define {}* @print_cells14(i8*, { i1*, i64 }*) {
  %3 = bitcast i8* %0 to <2 x i64>*
  %4 = load <2 x i64>, <2 x i64>* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %6, align 8
  %7 = getelementptr i8, i8* %5, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %0, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %10, align 8
  %11 = getelementptr i8, i8* %9, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %0, i8** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 16)
  %14 = bitcast i8* %13 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %14, align 8
  %15 = getelementptr i8, i8* %13, i64 8
  %16 = bitcast i8* %15 to i8**
  store i8* %0, i8** %16, align 8
  %17 = tail call i8* @GC_malloc(i64 16)
  %18 = bitcast i8* %17 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %18, align 8
  %19 = getelementptr i8, i8* %17, i64 8
  %20 = bitcast i8* %19 to i8**
  store i8* %0, i8** %20, align 8
  %21 = tail call i8* @GC_malloc(i64 16)
  %22 = bitcast i8* %21 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %22, align 8
  %23 = getelementptr i8, i8* %21, i64 8
  %24 = bitcast i8* %23 to i8**
  store i8* %0, i8** %24, align 8
  %25 = tail call i8* @GC_malloc(i64 16)
  %26 = bitcast i8* %25 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %26, align 8
  %27 = getelementptr i8, i8* %25, i64 8
  %28 = bitcast i8* %27 to i8**
  store i8* %0, i8** %28, align 8
  %29 = tail call i8* @GC_malloc(i64 16)
  %30 = bitcast i8* %29 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %30, align 8
  %31 = getelementptr i8, i8* %29, i64 8
  %32 = bitcast i8* %31 to i8**
  store i8* %0, i8** %32, align 8
  %33 = tail call i8* @GC_malloc(i64 16)
  %34 = bitcast i8* %33 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %34, align 8
  %35 = getelementptr i8, i8* %33, i64 8
  %36 = bitcast i8* %35 to i8**
  store i8* %0, i8** %36, align 8
  %37 = tail call i8* @GC_malloc(i64 40)
  %38 = bitcast i8* %37 to { i1*, i64 }**
  store { i1*, i64 }* %1, { i1*, i64 }** %38, align 8
  %39 = getelementptr i8, i8* %37, i64 8
  %40 = bitcast i8* %39 to i8**
  store i8* %17, i8** %40, align 8
  %41 = getelementptr i8, i8* %37, i64 16
  %42 = bitcast i8* %41 to i8**
  store i8* %5, i8** %42, align 8
  %43 = getelementptr i8, i8* %37, i64 24
  %44 = bitcast i8* %43 to <2 x i64>*
  store <2 x i64> %4, <2 x i64>* %44, align 8
  %45 = tail call i8* @GC_malloc(i64 16)
  %46 = bitcast i8* %45 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_y29, {}* (i8*, i64)** %46, align 8
  %47 = getelementptr i8, i8* %45, i64 8
  %48 = bitcast i8* %47 to i8**
  store i8* %37, i8** %48, align 8
  %49 = tail call {}* @go_y29(i8* %37, i64 0)
  ret {}* %49
}

define i1 @next_state16(i8*, { i1*, i64 }* nocapture readonly, i64, i64) {
body_0:
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %5, align 8
  %6 = getelementptr i8, i8* %4, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %9, align 8
  %10 = getelementptr i8, i8* %8, i64 8
  %11 = bitcast i8* %10 to i8**
  store i8* %0, i8** %11, align 8
  %12 = tail call i8* @GC_malloc(i64 16)
  %13 = bitcast i8* %12 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %13, align 8
  %14 = getelementptr i8, i8* %12, i64 8
  %15 = bitcast i8* %14 to i8**
  store i8* %0, i8** %15, align 8
  %16 = tail call i8* @GC_malloc(i64 16)
  %17 = bitcast i8* %16 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %17, align 8
  %18 = getelementptr i8, i8* %16, i64 8
  %19 = bitcast i8* %18 to i8**
  store i8* %0, i8** %19, align 8
  %20 = tail call i8* @GC_malloc(i64 16)
  %21 = bitcast i8* %20 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %21, align 8
  %22 = getelementptr i8, i8* %20, i64 8
  %23 = bitcast i8* %22 to i8**
  store i8* %0, i8** %23, align 8
  %24 = tail call i8* @GC_malloc(i64 16)
  %25 = bitcast i8* %24 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %25, align 8
  %26 = getelementptr i8, i8* %24, i64 8
  %27 = bitcast i8* %26 to i8**
  store i8* %0, i8** %27, align 8
  %28 = tail call i8* @GC_malloc(i64 16)
  %29 = bitcast i8* %28 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %29, align 8
  %30 = getelementptr i8, i8* %28, i64 8
  %31 = bitcast i8* %30 to i8**
  store i8* %0, i8** %31, align 8
  %32 = tail call i8* @GC_malloc(i64 16)
  %33 = bitcast i8* %32 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %33, align 8
  %34 = getelementptr i8, i8* %32, i64 8
  %35 = bitcast i8* %34 to i8**
  store i8* %0, i8** %35, align 8
  %36 = tail call i8* @GC_malloc(i64 8)
  %37 = tail call i8* @GC_malloc(i64 16)
  %38 = bitcast i8* %37 to i64**
  %39 = bitcast i8* %37 to i8**
  store i8* %36, i8** %39, align 8
  %40 = getelementptr i8, i8* %37, i64 8
  %41 = bitcast i8* %40 to i64*
  store i64 1, i64* %41, align 8
  %42 = bitcast i8* %36 to i64*
  store i64 0, i64* %42, align 8
  %43 = add i64 %2, -1
  %44 = add i64 %3, 1
  %45 = tail call i1 @view10(i8* %0, { i1*, i64 }* %1, i64 %43, i64 %44)
  %46 = tail call i8* @GC_malloc(i64 16)
  %47 = bitcast i8* %46 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %47, align 8
  %48 = getelementptr i8, i8* %46, i64 8
  %49 = bitcast i8* %48 to i8**
  store i8* %0, i8** %49, align 8
  %50 = tail call i8* @GC_malloc(i64 16)
  %51 = bitcast i8* %50 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %51, align 8
  %52 = getelementptr i8, i8* %50, i64 8
  %53 = bitcast i8* %52 to i8**
  store i8* %0, i8** %53, align 8
  %54 = tail call i8* @GC_malloc(i64 16)
  %55 = bitcast i8* %54 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %55, align 8
  %56 = getelementptr i8, i8* %54, i64 8
  %57 = bitcast i8* %56 to i8**
  store i8* %0, i8** %57, align 8
  %58 = tail call i8* @GC_malloc(i64 16)
  %59 = bitcast i8* %58 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %59, align 8
  %60 = getelementptr i8, i8* %58, i64 8
  %61 = bitcast i8* %60 to i8**
  store i8* %0, i8** %61, align 8
  %62 = tail call i8* @GC_malloc(i64 16)
  %63 = bitcast i8* %62 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %63, align 8
  %64 = getelementptr i8, i8* %62, i64 8
  %65 = bitcast i8* %64 to i8**
  store i8* %0, i8** %65, align 8
  %66 = tail call i8* @GC_malloc(i64 16)
  %67 = bitcast i8* %66 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %67, align 8
  %68 = getelementptr i8, i8* %66, i64 8
  %69 = bitcast i8* %68 to i8**
  store i8* %0, i8** %69, align 8
  %70 = tail call i8* @GC_malloc(i64 16)
  %71 = bitcast i8* %70 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %71, align 8
  %72 = getelementptr i8, i8* %70, i64 8
  %73 = bitcast i8* %72 to i8**
  store i8* %0, i8** %73, align 8
  %74 = tail call i8* @GC_malloc(i64 16)
  %75 = bitcast i8* %74 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %75, align 8
  %76 = getelementptr i8, i8* %74, i64 8
  %77 = bitcast i8* %76 to i8**
  store i8* %0, i8** %77, align 8
  %..i = zext i1 %45 to i64
  %78 = tail call i1 @view10(i8* %0, { i1*, i64 }* %1, i64 %2, i64 %44)
  %79 = tail call i8* @GC_malloc(i64 16)
  %80 = bitcast i8* %79 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %80, align 8
  %81 = getelementptr i8, i8* %79, i64 8
  %82 = bitcast i8* %81 to i8**
  store i8* %0, i8** %82, align 8
  %83 = tail call i8* @GC_malloc(i64 16)
  %84 = bitcast i8* %83 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %84, align 8
  %85 = getelementptr i8, i8* %83, i64 8
  %86 = bitcast i8* %85 to i8**
  store i8* %0, i8** %86, align 8
  %87 = tail call i8* @GC_malloc(i64 16)
  %88 = bitcast i8* %87 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %88, align 8
  %89 = getelementptr i8, i8* %87, i64 8
  %90 = bitcast i8* %89 to i8**
  store i8* %0, i8** %90, align 8
  %91 = tail call i8* @GC_malloc(i64 16)
  %92 = bitcast i8* %91 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %92, align 8
  %93 = getelementptr i8, i8* %91, i64 8
  %94 = bitcast i8* %93 to i8**
  store i8* %0, i8** %94, align 8
  %95 = tail call i8* @GC_malloc(i64 16)
  %96 = bitcast i8* %95 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %96, align 8
  %97 = getelementptr i8, i8* %95, i64 8
  %98 = bitcast i8* %97 to i8**
  store i8* %0, i8** %98, align 8
  %99 = tail call i8* @GC_malloc(i64 16)
  %100 = bitcast i8* %99 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %100, align 8
  %101 = getelementptr i8, i8* %99, i64 8
  %102 = bitcast i8* %101 to i8**
  store i8* %0, i8** %102, align 8
  %103 = tail call i8* @GC_malloc(i64 16)
  %104 = bitcast i8* %103 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %104, align 8
  %105 = getelementptr i8, i8* %103, i64 8
  %106 = bitcast i8* %105 to i8**
  store i8* %0, i8** %106, align 8
  %107 = tail call i8* @GC_malloc(i64 16)
  %108 = bitcast i8* %107 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %108, align 8
  %109 = getelementptr i8, i8* %107, i64 8
  %110 = bitcast i8* %109 to i8**
  store i8* %0, i8** %110, align 8
  %..i1 = zext i1 %78 to i64
  %111 = add nuw nsw i64 %..i1, %..i
  %112 = add i64 %2, 1
  %113 = tail call i1 @view10(i8* %0, { i1*, i64 }* %1, i64 %112, i64 %44)
  %114 = tail call i8* @GC_malloc(i64 16)
  %115 = bitcast i8* %114 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %115, align 8
  %116 = getelementptr i8, i8* %114, i64 8
  %117 = bitcast i8* %116 to i8**
  store i8* %0, i8** %117, align 8
  %118 = tail call i8* @GC_malloc(i64 16)
  %119 = bitcast i8* %118 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %119, align 8
  %120 = getelementptr i8, i8* %118, i64 8
  %121 = bitcast i8* %120 to i8**
  store i8* %0, i8** %121, align 8
  %122 = tail call i8* @GC_malloc(i64 16)
  %123 = bitcast i8* %122 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %123, align 8
  %124 = getelementptr i8, i8* %122, i64 8
  %125 = bitcast i8* %124 to i8**
  store i8* %0, i8** %125, align 8
  %126 = tail call i8* @GC_malloc(i64 16)
  %127 = bitcast i8* %126 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %127, align 8
  %128 = getelementptr i8, i8* %126, i64 8
  %129 = bitcast i8* %128 to i8**
  store i8* %0, i8** %129, align 8
  %130 = tail call i8* @GC_malloc(i64 16)
  %131 = bitcast i8* %130 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %131, align 8
  %132 = getelementptr i8, i8* %130, i64 8
  %133 = bitcast i8* %132 to i8**
  store i8* %0, i8** %133, align 8
  %134 = tail call i8* @GC_malloc(i64 16)
  %135 = bitcast i8* %134 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %135, align 8
  %136 = getelementptr i8, i8* %134, i64 8
  %137 = bitcast i8* %136 to i8**
  store i8* %0, i8** %137, align 8
  %138 = tail call i8* @GC_malloc(i64 16)
  %139 = bitcast i8* %138 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %139, align 8
  %140 = getelementptr i8, i8* %138, i64 8
  %141 = bitcast i8* %140 to i8**
  store i8* %0, i8** %141, align 8
  %142 = tail call i8* @GC_malloc(i64 16)
  %143 = bitcast i8* %142 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %143, align 8
  %144 = getelementptr i8, i8* %142, i64 8
  %145 = bitcast i8* %144 to i8**
  store i8* %0, i8** %145, align 8
  %..i2 = zext i1 %113 to i64
  %146 = add nuw nsw i64 %111, %..i2
  %147 = load i64*, i64** %38, align 8
  store i64 %146, i64* %147, align 8
  %148 = tail call i1 @view10(i8* %0, { i1*, i64 }* %1, i64 %43, i64 %3)
  %149 = tail call i8* @GC_malloc(i64 16)
  %150 = bitcast i8* %149 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %150, align 8
  %151 = getelementptr i8, i8* %149, i64 8
  %152 = bitcast i8* %151 to i8**
  store i8* %0, i8** %152, align 8
  %153 = tail call i8* @GC_malloc(i64 16)
  %154 = bitcast i8* %153 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %154, align 8
  %155 = getelementptr i8, i8* %153, i64 8
  %156 = bitcast i8* %155 to i8**
  store i8* %0, i8** %156, align 8
  %157 = tail call i8* @GC_malloc(i64 16)
  %158 = bitcast i8* %157 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %158, align 8
  %159 = getelementptr i8, i8* %157, i64 8
  %160 = bitcast i8* %159 to i8**
  store i8* %0, i8** %160, align 8
  %161 = tail call i8* @GC_malloc(i64 16)
  %162 = bitcast i8* %161 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %162, align 8
  %163 = getelementptr i8, i8* %161, i64 8
  %164 = bitcast i8* %163 to i8**
  store i8* %0, i8** %164, align 8
  %165 = tail call i8* @GC_malloc(i64 16)
  %166 = bitcast i8* %165 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %166, align 8
  %167 = getelementptr i8, i8* %165, i64 8
  %168 = bitcast i8* %167 to i8**
  store i8* %0, i8** %168, align 8
  %169 = tail call i8* @GC_malloc(i64 16)
  %170 = bitcast i8* %169 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %170, align 8
  %171 = getelementptr i8, i8* %169, i64 8
  %172 = bitcast i8* %171 to i8**
  store i8* %0, i8** %172, align 8
  %173 = tail call i8* @GC_malloc(i64 16)
  %174 = bitcast i8* %173 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %174, align 8
  %175 = getelementptr i8, i8* %173, i64 8
  %176 = bitcast i8* %175 to i8**
  store i8* %0, i8** %176, align 8
  %177 = tail call i8* @GC_malloc(i64 16)
  %178 = bitcast i8* %177 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %178, align 8
  %179 = getelementptr i8, i8* %177, i64 8
  %180 = bitcast i8* %179 to i8**
  store i8* %0, i8** %180, align 8
  %..i3 = zext i1 %148 to i64
  %181 = tail call i1 @view10(i8* %0, { i1*, i64 }* %1, i64 %112, i64 %3)
  %182 = tail call i8* @GC_malloc(i64 16)
  %183 = bitcast i8* %182 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %183, align 8
  %184 = getelementptr i8, i8* %182, i64 8
  %185 = bitcast i8* %184 to i8**
  store i8* %0, i8** %185, align 8
  %186 = tail call i8* @GC_malloc(i64 16)
  %187 = bitcast i8* %186 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %187, align 8
  %188 = getelementptr i8, i8* %186, i64 8
  %189 = bitcast i8* %188 to i8**
  store i8* %0, i8** %189, align 8
  %190 = tail call i8* @GC_malloc(i64 16)
  %191 = bitcast i8* %190 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %191, align 8
  %192 = getelementptr i8, i8* %190, i64 8
  %193 = bitcast i8* %192 to i8**
  store i8* %0, i8** %193, align 8
  %194 = tail call i8* @GC_malloc(i64 16)
  %195 = bitcast i8* %194 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %195, align 8
  %196 = getelementptr i8, i8* %194, i64 8
  %197 = bitcast i8* %196 to i8**
  store i8* %0, i8** %197, align 8
  %198 = tail call i8* @GC_malloc(i64 16)
  %199 = bitcast i8* %198 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %199, align 8
  %200 = getelementptr i8, i8* %198, i64 8
  %201 = bitcast i8* %200 to i8**
  store i8* %0, i8** %201, align 8
  %202 = tail call i8* @GC_malloc(i64 16)
  %203 = bitcast i8* %202 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %203, align 8
  %204 = getelementptr i8, i8* %202, i64 8
  %205 = bitcast i8* %204 to i8**
  store i8* %0, i8** %205, align 8
  %206 = tail call i8* @GC_malloc(i64 16)
  %207 = bitcast i8* %206 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %207, align 8
  %208 = getelementptr i8, i8* %206, i64 8
  %209 = bitcast i8* %208 to i8**
  store i8* %0, i8** %209, align 8
  %210 = tail call i8* @GC_malloc(i64 16)
  %211 = bitcast i8* %210 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %211, align 8
  %212 = getelementptr i8, i8* %210, i64 8
  %213 = bitcast i8* %212 to i8**
  store i8* %0, i8** %213, align 8
  %..i4 = zext i1 %181 to i64
  %214 = add nuw nsw i64 %..i4, %..i3
  %215 = load i64*, i64** %38, align 8
  %216 = load i64, i64* %215, align 8
  %217 = add i64 %214, %216
  store i64 %217, i64* %215, align 8
  %218 = add i64 %3, -1
  %219 = tail call i1 @view10(i8* %0, { i1*, i64 }* %1, i64 %43, i64 %218)
  %220 = tail call i8* @GC_malloc(i64 16)
  %221 = bitcast i8* %220 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %221, align 8
  %222 = getelementptr i8, i8* %220, i64 8
  %223 = bitcast i8* %222 to i8**
  store i8* %0, i8** %223, align 8
  %224 = tail call i8* @GC_malloc(i64 16)
  %225 = bitcast i8* %224 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %225, align 8
  %226 = getelementptr i8, i8* %224, i64 8
  %227 = bitcast i8* %226 to i8**
  store i8* %0, i8** %227, align 8
  %228 = tail call i8* @GC_malloc(i64 16)
  %229 = bitcast i8* %228 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %229, align 8
  %230 = getelementptr i8, i8* %228, i64 8
  %231 = bitcast i8* %230 to i8**
  store i8* %0, i8** %231, align 8
  %232 = tail call i8* @GC_malloc(i64 16)
  %233 = bitcast i8* %232 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %233, align 8
  %234 = getelementptr i8, i8* %232, i64 8
  %235 = bitcast i8* %234 to i8**
  store i8* %0, i8** %235, align 8
  %236 = tail call i8* @GC_malloc(i64 16)
  %237 = bitcast i8* %236 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %237, align 8
  %238 = getelementptr i8, i8* %236, i64 8
  %239 = bitcast i8* %238 to i8**
  store i8* %0, i8** %239, align 8
  %240 = tail call i8* @GC_malloc(i64 16)
  %241 = bitcast i8* %240 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %241, align 8
  %242 = getelementptr i8, i8* %240, i64 8
  %243 = bitcast i8* %242 to i8**
  store i8* %0, i8** %243, align 8
  %244 = tail call i8* @GC_malloc(i64 16)
  %245 = bitcast i8* %244 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %245, align 8
  %246 = getelementptr i8, i8* %244, i64 8
  %247 = bitcast i8* %246 to i8**
  store i8* %0, i8** %247, align 8
  %248 = tail call i8* @GC_malloc(i64 16)
  %249 = bitcast i8* %248 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %249, align 8
  %250 = getelementptr i8, i8* %248, i64 8
  %251 = bitcast i8* %250 to i8**
  store i8* %0, i8** %251, align 8
  %..i5 = zext i1 %219 to i64
  %252 = tail call i1 @view10(i8* %0, { i1*, i64 }* %1, i64 %2, i64 %218)
  %253 = tail call i8* @GC_malloc(i64 16)
  %254 = bitcast i8* %253 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %254, align 8
  %255 = getelementptr i8, i8* %253, i64 8
  %256 = bitcast i8* %255 to i8**
  store i8* %0, i8** %256, align 8
  %257 = tail call i8* @GC_malloc(i64 16)
  %258 = bitcast i8* %257 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %258, align 8
  %259 = getelementptr i8, i8* %257, i64 8
  %260 = bitcast i8* %259 to i8**
  store i8* %0, i8** %260, align 8
  %261 = tail call i8* @GC_malloc(i64 16)
  %262 = bitcast i8* %261 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %262, align 8
  %263 = getelementptr i8, i8* %261, i64 8
  %264 = bitcast i8* %263 to i8**
  store i8* %0, i8** %264, align 8
  %265 = tail call i8* @GC_malloc(i64 16)
  %266 = bitcast i8* %265 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %266, align 8
  %267 = getelementptr i8, i8* %265, i64 8
  %268 = bitcast i8* %267 to i8**
  store i8* %0, i8** %268, align 8
  %269 = tail call i8* @GC_malloc(i64 16)
  %270 = bitcast i8* %269 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %270, align 8
  %271 = getelementptr i8, i8* %269, i64 8
  %272 = bitcast i8* %271 to i8**
  store i8* %0, i8** %272, align 8
  %273 = tail call i8* @GC_malloc(i64 16)
  %274 = bitcast i8* %273 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %274, align 8
  %275 = getelementptr i8, i8* %273, i64 8
  %276 = bitcast i8* %275 to i8**
  store i8* %0, i8** %276, align 8
  %277 = tail call i8* @GC_malloc(i64 16)
  %278 = bitcast i8* %277 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %278, align 8
  %279 = getelementptr i8, i8* %277, i64 8
  %280 = bitcast i8* %279 to i8**
  store i8* %0, i8** %280, align 8
  %281 = tail call i8* @GC_malloc(i64 16)
  %282 = bitcast i8* %281 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %282, align 8
  %283 = getelementptr i8, i8* %281, i64 8
  %284 = bitcast i8* %283 to i8**
  store i8* %0, i8** %284, align 8
  %..i6 = zext i1 %252 to i64
  %285 = add nuw nsw i64 %..i6, %..i5
  %286 = tail call i1 @view10(i8* %0, { i1*, i64 }* %1, i64 %112, i64 %218)
  %287 = tail call i8* @GC_malloc(i64 16)
  %288 = bitcast i8* %287 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %288, align 8
  %289 = getelementptr i8, i8* %287, i64 8
  %290 = bitcast i8* %289 to i8**
  store i8* %0, i8** %290, align 8
  %291 = tail call i8* @GC_malloc(i64 16)
  %292 = bitcast i8* %291 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %292, align 8
  %293 = getelementptr i8, i8* %291, i64 8
  %294 = bitcast i8* %293 to i8**
  store i8* %0, i8** %294, align 8
  %295 = tail call i8* @GC_malloc(i64 16)
  %296 = bitcast i8* %295 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %296, align 8
  %297 = getelementptr i8, i8* %295, i64 8
  %298 = bitcast i8* %297 to i8**
  store i8* %0, i8** %298, align 8
  %299 = tail call i8* @GC_malloc(i64 16)
  %300 = bitcast i8* %299 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %300, align 8
  %301 = getelementptr i8, i8* %299, i64 8
  %302 = bitcast i8* %301 to i8**
  store i8* %0, i8** %302, align 8
  %303 = tail call i8* @GC_malloc(i64 16)
  %304 = bitcast i8* %303 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %304, align 8
  %305 = getelementptr i8, i8* %303, i64 8
  %306 = bitcast i8* %305 to i8**
  store i8* %0, i8** %306, align 8
  %307 = tail call i8* @GC_malloc(i64 16)
  %308 = bitcast i8* %307 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %308, align 8
  %309 = getelementptr i8, i8* %307, i64 8
  %310 = bitcast i8* %309 to i8**
  store i8* %0, i8** %310, align 8
  %311 = tail call i8* @GC_malloc(i64 16)
  %312 = bitcast i8* %311 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %312, align 8
  %313 = getelementptr i8, i8* %311, i64 8
  %314 = bitcast i8* %313 to i8**
  store i8* %0, i8** %314, align 8
  %315 = tail call i8* @GC_malloc(i64 16)
  %316 = bitcast i8* %315 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %316, align 8
  %317 = getelementptr i8, i8* %315, i64 8
  %318 = bitcast i8* %317 to i8**
  store i8* %0, i8** %318, align 8
  %..i7 = zext i1 %286 to i64
  %319 = add nuw nsw i64 %285, %..i7
  %320 = load i64*, i64** %38, align 8
  %321 = load i64, i64* %320, align 8
  %322 = add i64 %319, %321
  store i64 %322, i64* %320, align 8
  %323 = tail call i1 @view10(i8* %0, { i1*, i64 }* %1, i64 %2, i64 %3)
  %324 = load i64*, i64** %38, align 8
  %325 = load i64, i64* %324, align 8
  %326 = zext i1 %323 to i64
  %storemerge.in = or i64 %325, %326
  %storemerge = icmp eq i64 %storemerge.in, 3
  ret i1 %storemerge
}

declare {}* @newline() local_unnamed_addr

define {}* @newline1() local_unnamed_addr {
  %1 = tail call {}* @newline()
  ret {}* %1
}

define {}* @loop44(i8*, i64) {
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
  store {}* (i8*, i64)* @loop44, {}* (i8*, i64)** %12, align 8
  %13 = getelementptr i8, i8* %11, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %0, i8** %14, align 8
  %15 = icmp sgt i64 %1, 0
  br i1 %15, label %then_0, label %else_0

then_0:                                           ; preds = %2, %then_0
  %16 = phi { {}* (i8*, { i1*, i64 }*)*, i8* }* [ %33, %then_0 ], [ %10, %2 ]
  %17 = phi { {}* (i8*, { i1*, i64 }*)*, i8* }* [ %32, %then_0 ], [ %7, %2 ]
  %18 = phi { i1*, i64 }* [ %31, %then_0 ], [ %4, %2 ]
  %.tr23 = phi i64 [ %30, %then_0 ], [ %1, %2 ]
  %19 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %17, i64 0, i32 0
  %20 = load {}* (i8*, { i1*, i64 }*)*, {}* (i8*, { i1*, i64 }*)** %19, align 8
  %21 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %17, i64 0, i32 1
  %22 = load i8*, i8** %21, align 8
  %23 = tail call {}* %20(i8* %22, { i1*, i64 }* %18)
  %24 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %16, i64 0, i32 0
  %25 = load {}* (i8*, { i1*, i64 }*)*, {}* (i8*, { i1*, i64 }*)** %24, align 8
  %26 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %16, i64 0, i32 1
  %27 = load i8*, i8** %26, align 8
  %28 = tail call {}* %25(i8* %27, { i1*, i64 }* %18)
  %29 = tail call {}* @newline()
  %30 = add nsw i64 %.tr23, -1
  %31 = load { i1*, i64 }*, { i1*, i64 }** %3, align 8
  %32 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %6, align 8
  %33 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %9, align 8
  %34 = tail call i8* @GC_malloc(i64 16)
  %35 = bitcast i8* %34 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop44, {}* (i8*, i64)** %35, align 8
  %36 = getelementptr i8, i8* %34, i64 8
  %37 = bitcast i8* %36 to i8**
  store i8* %0, i8** %37, align 8
  %38 = icmp sgt i64 %30, 0
  br i1 %38, label %then_0, label %else_0

else_0:                                           ; preds = %then_0, %2
  %39 = tail call i8* @GC_malloc(i64 0)
  %40 = bitcast i8* %39 to {}*
  ret {}* %40
}

define {}* @init_cells12(i8*, { i1*, i64 }* nocapture readonly, i64) {
  %4 = bitcast i8* %0 to i64*
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i64*
  %7 = getelementptr { i1*, i64 }, { i1*, i64 }* %1, i64 0, i32 0
  br label %tailrecurse

tailrecurse:                                      ; preds = %else_0, %3
  %.tr3 = phi i64 [ %2, %3 ], [ %49, %else_0 ]
  %8 = load i64, i64* %4, align 8
  %9 = load i64, i64* %6, align 8
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %11, align 8
  %12 = getelementptr i8, i8* %10, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %0, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %15, align 8
  %16 = getelementptr i8, i8* %14, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %0, i8** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 16)
  %19 = bitcast i8* %18 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %19, align 8
  %20 = getelementptr i8, i8* %18, i64 8
  %21 = bitcast i8* %20 to i8**
  store i8* %0, i8** %21, align 8
  %22 = tail call i8* @GC_malloc(i64 16)
  %23 = bitcast i8* %22 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %23, align 8
  %24 = getelementptr i8, i8* %22, i64 8
  %25 = bitcast i8* %24 to i8**
  store i8* %0, i8** %25, align 8
  %26 = tail call i8* @GC_malloc(i64 16)
  %27 = bitcast i8* %26 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %27, align 8
  %28 = getelementptr i8, i8* %26, i64 8
  %29 = bitcast i8* %28 to i8**
  store i8* %0, i8** %29, align 8
  %30 = tail call i8* @GC_malloc(i64 16)
  %31 = bitcast i8* %30 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %31, align 8
  %32 = getelementptr i8, i8* %30, i64 8
  %33 = bitcast i8* %32 to i8**
  store i8* %0, i8** %33, align 8
  %34 = tail call i8* @GC_malloc(i64 16)
  %35 = bitcast i8* %34 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %35, align 8
  %36 = getelementptr i8, i8* %34, i64 8
  %37 = bitcast i8* %36 to i8**
  store i8* %0, i8** %37, align 8
  %38 = tail call i8* @GC_malloc(i64 16)
  %39 = bitcast i8* %38 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %39, align 8
  %40 = getelementptr i8, i8* %38, i64 8
  %41 = bitcast i8* %40 to i8**
  store i8* %0, i8** %41, align 8
  %42 = mul i64 %9, %8
  %43 = icmp sgt i64 %42, %.tr3
  br i1 %43, label %else_0, label %then_0

then_0:                                           ; preds = %tailrecurse
  %44 = tail call i8* @GC_malloc(i64 0)
  %45 = bitcast i8* %44 to {}*
  ret {}* %45

else_0:                                           ; preds = %tailrecurse
  %46 = tail call i1 @rand_bool()
  %47 = load i1*, i1** %7, align 8
  %48 = getelementptr i1, i1* %47, i64 %.tr3
  store i1 %46, i1* %48, align 1
  %49 = add i64 %.tr3, 1
  br label %tailrecurse
}

define {}* @go_y40(i8*, i64) {
  %3 = getelementptr i8, i8* %0, i64 16
  %4 = getelementptr i8, i8* %0, i64 32
  %5 = bitcast i8* %4 to i64*
  %6 = getelementptr i8, i8* %0, i64 40
  %7 = bitcast i8* %6 to i64*
  %8 = bitcast i8* %0 to <2 x i64>*
  %9 = bitcast i8* %3 to <2 x i64>*
  br label %tailrecurse

tailrecurse:                                      ; preds = %else_0, %2
  %.tr2 = phi i64 [ %1, %2 ], [ %34, %else_0 ]
  %10 = load <2 x i64>, <2 x i64>* %8, align 8
  %11 = load <2 x i64>, <2 x i64>* %9, align 8
  %12 = load i64, i64* %5, align 8
  %13 = load i64, i64* %7, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_y40, {}* (i8*, i64)** %15, align 8
  %16 = getelementptr i8, i8* %14, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %0, i8** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 48)
  %19 = bitcast i8* %18 to <2 x i64>*
  store <2 x i64> %10, <2 x i64>* %19, align 8
  %20 = getelementptr i8, i8* %18, i64 16
  %21 = bitcast i8* %20 to <2 x i64>*
  store <2 x i64> %11, <2 x i64>* %21, align 8
  %22 = getelementptr i8, i8* %18, i64 32
  %23 = bitcast i8* %22 to i64*
  store i64 %12, i64* %23, align 8
  %24 = getelementptr i8, i8* %18, i64 40
  %25 = bitcast i8* %24 to i64*
  store i64 %.tr2, i64* %25, align 8
  %26 = tail call i8* @GC_malloc(i64 16)
  %27 = bitcast i8* %26 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x42, {}* (i8*, i64)** %27, align 8
  %28 = getelementptr i8, i8* %26, i64 8
  %29 = bitcast i8* %28 to i8**
  store i8* %18, i8** %29, align 8
  %30 = icmp sgt i64 %13, %.tr2
  br i1 %30, label %else_0, label %then_0

then_0:                                           ; preds = %tailrecurse
  %31 = tail call i8* @GC_malloc(i64 0)
  %32 = bitcast i8* %31 to {}*
  ret {}* %32

else_0:                                           ; preds = %tailrecurse
  %33 = tail call {}* @go_x42(i8* %18, i64 0)
  %34 = add i64 %.tr2, 1
  br label %tailrecurse
}

define {}* @go_y29(i8*, i64) {
  %3 = getelementptr i8, i8* %0, i64 16
  %4 = getelementptr i8, i8* %0, i64 32
  %5 = bitcast i8* %4 to i64*
  %6 = bitcast i8* %0 to <2 x i64>*
  %7 = bitcast i8* %3 to <2 x i64>*
  br label %tailrecurse

tailrecurse:                                      ; preds = %else_0, %2
  %.tr2 = phi i64 [ %1, %2 ], [ %30, %else_0 ]
  %8 = load <2 x i64>, <2 x i64>* %6, align 8
  %9 = load <2 x i64>, <2 x i64>* %7, align 8
  %10 = load i64, i64* %5, align 8
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_y29, {}* (i8*, i64)** %12, align 8
  %13 = getelementptr i8, i8* %11, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %0, i8** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 40)
  %16 = bitcast i8* %15 to <2 x i64>*
  store <2 x i64> %8, <2 x i64>* %16, align 8
  %17 = getelementptr i8, i8* %15, i64 16
  %18 = bitcast i8* %17 to <2 x i64>*
  store <2 x i64> %9, <2 x i64>* %18, align 8
  %19 = getelementptr i8, i8* %15, i64 32
  %20 = bitcast i8* %19 to i64*
  store i64 %.tr2, i64* %20, align 8
  %21 = tail call i8* @GC_malloc(i64 16)
  %22 = bitcast i8* %21 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x31, {}* (i8*, i64)** %22, align 8
  %23 = getelementptr i8, i8* %21, i64 8
  %24 = bitcast i8* %23 to i8**
  store i8* %15, i8** %24, align 8
  %25 = icmp sgt i64 %10, %.tr2
  br i1 %25, label %else_0, label %then_0

then_0:                                           ; preds = %tailrecurse
  %26 = tail call i8* @GC_malloc(i64 0)
  %27 = bitcast i8* %26 to {}*
  ret {}* %27

else_0:                                           ; preds = %tailrecurse
  %28 = tail call {}* @go_x31(i8* %15, i64 0)
  %29 = tail call {}* @newline()
  %30 = add i64 %.tr2, 1
  br label %tailrecurse
}

define {}* @go_x42(i8*, i64) {
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
  store {}* (i8*, i64)* @go_x42, {}* (i8*, i64)** %21, align 8
  %22 = getelementptr i8, i8* %20, i64 8
  %23 = bitcast i8* %22 to i8**
  store i8* %0, i8** %23, align 8
  %24 = icmp sgt i64 %16, %1
  br i1 %24, label %else_0, label %then_0

then_0:                                           ; preds = %else_0, %2
  %25 = tail call i8* @GC_malloc(i64 0)
  %26 = bitcast i8* %25 to {}*
  ret {}* %26

else_0:                                           ; preds = %2, %else_0
  %27 = phi i64 [ %48, %else_0 ], [ %19, %2 ]
  %28 = phi { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* [ %46, %else_0 ], [ %13, %2 ]
  %29 = phi { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* [ %45, %else_0 ], [ %10, %2 ]
  %30 = phi { i1*, i64 }* [ %44, %else_0 ], [ %7, %2 ]
  %31 = phi { i1*, i64 }* [ %43, %else_0 ], [ %4, %2 ]
  %.tr23 = phi i64 [ %42, %else_0 ], [ %1, %2 ]
  %32 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %29, i64 0, i32 0
  %33 = load i1 (i8*, { i1*, i64 }*, i64, i64)*, i1 (i8*, { i1*, i64 }*, i64, i64)** %32, align 8
  %34 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %29, i64 0, i32 1
  %35 = load i8*, i8** %34, align 8
  %36 = tail call i1 %33(i8* %35, { i1*, i64 }* %30, i64 %.tr23, i64 %27)
  %37 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %28, i64 0, i32 0
  %38 = load {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %37, align 8
  %39 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %28, i64 0, i32 1
  %40 = load i8*, i8** %39, align 8
  %41 = tail call {}* %38(i8* %40, { i1*, i64 }* %31, i64 %.tr23, i64 %27, i1 %36)
  %42 = add i64 %.tr23, 1
  %43 = load { i1*, i64 }*, { i1*, i64 }** %3, align 8
  %44 = load { i1*, i64 }*, { i1*, i64 }** %6, align 8
  %45 = load { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %9, align 8
  %46 = load { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }** %12, align 8
  %47 = load i64, i64* %15, align 8
  %48 = load i64, i64* %18, align 8
  %49 = tail call i8* @GC_malloc(i64 16)
  %50 = bitcast i8* %49 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x42, {}* (i8*, i64)** %50, align 8
  %51 = getelementptr i8, i8* %49, i64 8
  %52 = bitcast i8* %51 to i8**
  store i8* %0, i8** %52, align 8
  %53 = icmp sgt i64 %47, %42
  br i1 %53, label %else_0, label %then_0
}

define {}* @go_x31(i8*, i64) {
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
  store {}* (i8*, i64)* @go_x31, {}* (i8*, i64)** %18, align 8
  %19 = getelementptr i8, i8* %17, i64 8
  %20 = bitcast i8* %19 to i8**
  store i8* %0, i8** %20, align 8
  %21 = icmp sgt i64 %13, %1
  br i1 %21, label %else_0, label %then_0

then_0:                                           ; preds = %else_0, %2
  %22 = tail call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  ret {}* %23

else_0:                                           ; preds = %2, %else_0
  %24 = phi i64 [ %44, %else_0 ], [ %16, %2 ]
  %25 = phi { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* [ %42, %else_0 ], [ %10, %2 ]
  %26 = phi { i8 (i8*, i1)*, i8* }* [ %41, %else_0 ], [ %7, %2 ]
  %27 = phi { i1*, i64 }* [ %40, %else_0 ], [ %4, %2 ]
  %.tr23 = phi i64 [ %39, %else_0 ], [ %1, %2 ]
  %28 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %25, i64 0, i32 0
  %29 = load i1 (i8*, { i1*, i64 }*, i64, i64)*, i1 (i8*, { i1*, i64 }*, i64, i64)** %28, align 8
  %30 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %25, i64 0, i32 1
  %31 = load i8*, i8** %30, align 8
  %32 = tail call i1 %29(i8* %31, { i1*, i64 }* %27, i64 %.tr23, i64 %24)
  %33 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %26, i64 0, i32 0
  %34 = load i8 (i8*, i1)*, i8 (i8*, i1)** %33, align 8
  %35 = getelementptr { i8 (i8*, i1)*, i8* }, { i8 (i8*, i1)*, i8* }* %26, i64 0, i32 1
  %36 = load i8*, i8** %35, align 8
  %37 = tail call i8 %34(i8* %36, i1 %32)
  %38 = tail call {}* @print_char(i8 %37)
  %39 = add i64 %.tr23, 1
  %40 = load { i1*, i64 }*, { i1*, i64 }** %3, align 8
  %41 = load { i8 (i8*, i1)*, i8* }*, { i8 (i8*, i1)*, i8* }** %6, align 8
  %42 = load { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %9, align 8
  %43 = load i64, i64* %12, align 8
  %44 = load i64, i64* %15, align 8
  %45 = tail call i8* @GC_malloc(i64 16)
  %46 = bitcast i8* %45 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x31, {}* (i8*, i64)** %46, align 8
  %47 = getelementptr i8, i8* %45, i64 8
  %48 = bitcast i8* %47 to i8**
  store i8* %0, i8** %48, align 8
  %49 = icmp sgt i64 %43, %39
  br i1 %49, label %else_0, label %then_0
}

declare {}* @gen_seed() local_unnamed_addr

define {}* @gen_seed2() local_unnamed_addr {
  %1 = tail call {}* @gen_seed()
  ret {}* %1
}

declare { i1*, i64 }* @copy_bool_array({ i1*, i64 }*) local_unnamed_addr

define { i1*, i64 }* @copy_bool_array5({ i1*, i64 }*) local_unnamed_addr {
  %2 = tail call { i1*, i64 }* @copy_bool_array({ i1*, i64 }* %0)
  ret { i1*, i64 }* %2
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
  br label %body_0.body_0_crit_edge

body_0.body_0_crit_edge:                          ; preds = %body_0.body_0_crit_edge, %0
  %8 = phi i64 [ 1, %0 ], [ %14, %body_0.body_0_crit_edge ]
  %.pre = load i1*, i1** %3, align 8
  %9 = getelementptr i1, i1* %.pre, i64 %8
  store i1 false, i1* %9, align 1
  %10 = add nuw nsw i64 %8, 1
  %.pre.1 = load i1*, i1** %3, align 8
  %11 = getelementptr i1, i1* %.pre.1, i64 %10
  store i1 false, i1* %11, align 1
  %12 = add nuw nsw i64 %8, 2
  %.pre.2 = load i1*, i1** %3, align 8
  %13 = getelementptr i1, i1* %.pre.2, i64 %12
  store i1 false, i1* %13, align 1
  %14 = add nuw nsw i64 %8, 3
  %exitcond.2 = icmp eq i64 %14, 1000
  br i1 %exitcond.2, label %end_0, label %body_0.body_0_crit_edge

end_0:                                            ; preds = %body_0.body_0_crit_edge
  %15 = bitcast i8* %2 to { i1*, i64 }*
  %16 = tail call i8* @GC_malloc(i64 16)
  %17 = bitcast i8* %16 to <2 x i64>*
  store <2 x i64> <i64 50, i64 20>, <2 x i64>* %17, align 8
  %18 = tail call i8* @GC_malloc(i64 16)
  %19 = bitcast i8* %18 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %19, align 8
  %20 = getelementptr i8, i8* %18, i64 8
  %21 = bitcast i8* %20 to i8**
  store i8* %16, i8** %21, align 8
  %22 = tail call i8* @GC_malloc(i64 16)
  %23 = bitcast i8* %22 to <2 x i64>*
  store <2 x i64> <i64 50, i64 20>, <2 x i64>* %23, align 8
  %24 = tail call i8* @GC_malloc(i64 16)
  %25 = bitcast i8* %24 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set11, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %25, align 8
  %26 = getelementptr i8, i8* %24, i64 8
  %27 = bitcast i8* %26 to i8**
  store i8* %22, i8** %27, align 8
  %28 = tail call i8* @GC_malloc(i64 16)
  %29 = bitcast i8* %28 to <2 x i64>*
  store <2 x i64> <i64 50, i64 20>, <2 x i64>* %29, align 8
  %30 = tail call i8* @GC_malloc(i64 16)
  %31 = bitcast i8* %30 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells12, {}* (i8*, { i1*, i64 }*, i64)** %31, align 8
  %32 = getelementptr i8, i8* %30, i64 8
  %33 = bitcast i8* %32 to i8**
  store i8* %28, i8** %33, align 8
  %34 = tail call i8* @GC_malloc(i64 16)
  %35 = bitcast i8* %34 to <2 x i64>*
  store <2 x i64> <i64 50, i64 20>, <2 x i64>* %35, align 8
  %36 = tail call i8* @GC_malloc(i64 16)
  %37 = bitcast i8* %36 to i8 (i8*, i1)**
  store i8 (i8*, i1)* @to_char13, i8 (i8*, i1)** %37, align 8
  %38 = getelementptr i8, i8* %36, i64 8
  %39 = bitcast i8* %38 to i8**
  store i8* %34, i8** %39, align 8
  %40 = tail call i8* @GC_malloc(i64 16)
  %41 = bitcast i8* %40 to <2 x i64>*
  store <2 x i64> <i64 50, i64 20>, <2 x i64>* %41, align 8
  %42 = tail call i8* @GC_malloc(i64 16)
  %43 = bitcast i8* %42 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells14, {}* (i8*, { i1*, i64 }*)** %43, align 8
  %44 = getelementptr i8, i8* %42, i64 8
  %45 = bitcast i8* %44 to i8**
  store i8* %40, i8** %45, align 8
  %46 = tail call i8* @GC_malloc(i64 16)
  %47 = bitcast i8* %46 to <2 x i64>*
  store <2 x i64> <i64 50, i64 20>, <2 x i64>* %47, align 8
  %48 = tail call i8* @GC_malloc(i64 16)
  %49 = bitcast i8* %48 to i64 (i8*, i1)**
  store i64 (i8*, i1)* @to_int15, i64 (i8*, i1)** %49, align 8
  %50 = getelementptr i8, i8* %48, i64 8
  %51 = bitcast i8* %50 to i8**
  store i8* %46, i8** %51, align 8
  %52 = tail call i8* @GC_malloc(i64 16)
  %53 = bitcast i8* %52 to <2 x i64>*
  store <2 x i64> <i64 50, i64 20>, <2 x i64>* %53, align 8
  %54 = tail call i8* @GC_malloc(i64 16)
  %55 = bitcast i8* %54 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state16, i1 (i8*, { i1*, i64 }*, i64, i64)** %55, align 8
  %56 = getelementptr i8, i8* %54, i64 8
  %57 = bitcast i8* %56 to i8**
  store i8* %52, i8** %57, align 8
  %58 = tail call i8* @GC_malloc(i64 16)
  %59 = bitcast i8* %58 to <2 x i64>*
  store <2 x i64> <i64 50, i64 20>, <2 x i64>* %59, align 8
  %60 = tail call i8* @GC_malloc(i64 16)
  %61 = bitcast i8* %60 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells17, {}* (i8*, { i1*, i64 }*)** %61, align 8
  %62 = getelementptr i8, i8* %60, i64 8
  %63 = bitcast i8* %62 to i8**
  store i8* %58, i8** %63, align 8
  %64 = tail call {}* @gen_seed()
  %65 = tail call {}* @pulsar({ i1*, i64 }* %15)
  %66 = tail call i8* @GC_malloc(i64 24)
  %67 = bitcast i8* %66 to i8**
  store i8* %2, i8** %67, align 8
  %68 = getelementptr i8, i8* %66, i64 8
  %69 = bitcast i8* %68 to i8**
  store i8* %42, i8** %69, align 8
  %70 = getelementptr i8, i8* %66, i64 16
  %71 = bitcast i8* %70 to i8**
  store i8* %60, i8** %71, align 8
  %72 = tail call i8* @GC_malloc(i64 16)
  %73 = bitcast i8* %72 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop44, {}* (i8*, i64)** %73, align 8
  %74 = getelementptr i8, i8* %72, i64 8
  %75 = bitcast i8* %74 to i8**
  store i8* %66, i8** %75, align 8
  %76 = bitcast i8* %66 to { i1*, i64 }**
  %77 = load { i1*, i64 }*, { i1*, i64 }** %76, align 8
  %78 = bitcast i8* %68 to { {}* (i8*, { i1*, i64 }*)*, i8* }**
  %79 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %78, align 8
  %80 = bitcast i8* %70 to { {}* (i8*, { i1*, i64 }*)*, i8* }**
  %81 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %80, align 8
  %82 = tail call i8* @GC_malloc(i64 16)
  %83 = bitcast i8* %82 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop44, {}* (i8*, i64)** %83, align 8
  %84 = getelementptr i8, i8* %82, i64 8
  %85 = bitcast i8* %84 to i8**
  store i8* %66, i8** %85, align 8
  br label %then_0.i

then_0.i:                                         ; preds = %then_0.i, %end_0
  %86 = phi { {}* (i8*, { i1*, i64 }*)*, i8* }* [ %103, %then_0.i ], [ %81, %end_0 ]
  %87 = phi { {}* (i8*, { i1*, i64 }*)*, i8* }* [ %102, %then_0.i ], [ %79, %end_0 ]
  %88 = phi { i1*, i64 }* [ %101, %then_0.i ], [ %77, %end_0 ]
  %.tr23.i = phi i64 [ %100, %then_0.i ], [ 10, %end_0 ]
  %89 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %87, i64 0, i32 0
  %90 = load {}* (i8*, { i1*, i64 }*)*, {}* (i8*, { i1*, i64 }*)** %89, align 8
  %91 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %87, i64 0, i32 1
  %92 = load i8*, i8** %91, align 8
  %93 = tail call {}* %90(i8* %92, { i1*, i64 }* %88)
  %94 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %86, i64 0, i32 0
  %95 = load {}* (i8*, { i1*, i64 }*)*, {}* (i8*, { i1*, i64 }*)** %94, align 8
  %96 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %86, i64 0, i32 1
  %97 = load i8*, i8** %96, align 8
  %98 = tail call {}* %95(i8* %97, { i1*, i64 }* %88)
  %99 = tail call {}* @newline()
  %100 = add nsw i64 %.tr23.i, -1
  %101 = load { i1*, i64 }*, { i1*, i64 }** %76, align 8
  %102 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %78, align 8
  %103 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %80, align 8
  %104 = tail call i8* @GC_malloc(i64 16)
  %105 = bitcast i8* %104 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop44, {}* (i8*, i64)** %105, align 8
  %106 = getelementptr i8, i8* %104, i64 8
  %107 = bitcast i8* %106 to i8**
  store i8* %66, i8** %107, align 8
  %108 = icmp eq i64 %100, 0
  br i1 %108, label %loop44.exit, label %then_0.i

loop44.exit:                                      ; preds = %then_0.i
  %109 = tail call i8* @GC_malloc(i64 0)
  ret i32 0
}
