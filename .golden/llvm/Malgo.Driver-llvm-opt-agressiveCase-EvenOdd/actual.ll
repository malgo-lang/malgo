; ModuleID = '.malgo-work/test/testcases/malgo/EvenOdd.ll'
source_filename = "test/testcases/malgo/EvenOdd.mlg"

@str4776 = unnamed_addr constant [6 x i8] c"False\00"
@str4777 = unnamed_addr constant [5 x i8] c"True\00"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_sub_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4774"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_sub_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4775"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_sub_int32_t(i32 %x_0, i32 %1)
  ret i32 %3
}

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4778"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_sub_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal noundef ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4779"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %d_0, i64 4
  %d_0.val = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %d_0.val, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4775", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %6 = tail call i32 @malgo_sub_int32_t(i32 %x_0.i.i, i32 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 %6, ptr %8, align 4
  ret ptr %7
}

define internal noundef ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4780"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %d_0, i64 4
  %d_0.val = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %d_0.val, ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4791", ptr %let_func_0.i, align 8
  %6 = tail call i32 @malgo_sub_int32_t(i32 %d_0.val, i32 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 %6, ptr %8, align 4
  ret ptr %7
}

define internal noundef ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4781"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int32#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int32#_0", i64 4
  %"int32#_0.val" = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val", ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4790", ptr %let_func_0.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i, align 4
  %6 = tail call i32 @malgo_sub_int32_t(i32 %p_0.i.i, i32 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 %6, ptr %8, align 4
  ret ptr %7
}

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4786"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %p_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4774", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %4 = tail call i32 @malgo_sub_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %4
}

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4787"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_sub_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal noundef ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4788"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %d_0, i64 4
  %d_0.val = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %d_0.val, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4775", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %6 = tail call i32 @malgo_sub_int32_t(i32 %x_0.i.i, i32 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 %6, ptr %8, align 4
  ret ptr %7
}

define internal noundef ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4789"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %d_0, i64 4
  %d_0.val = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %d_0.val, ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4791", ptr %let_func_0.i, align 8
  %6 = tail call i32 @malgo_sub_int32_t(i32 %d_0.val, i32 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 %6, ptr %8, align 4
  ret ptr %7
}

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4790"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_sub_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4791"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_sub_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 10, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { i32 } }, ptr %5, i64 0, i32 1, i32 0
  store i32 1, ptr %6, align 4
  %7 = load i32, ptr %4, align 4
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %7, ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %8, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4778", ptr %let_func_0.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i, align 4
  %9 = tail call i32 @malgo_sub_int32_t(i32 %p_0.i.i, i32 noundef 1)
  %10 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { i32 } }, ptr %10, i64 0, i32 1, i32 0
  store i32 %9, ptr %11, align 4
  %cond1.i = icmp eq i32 %9, 0
  br i1 %cond1.i, label %"test/testcases/malgo/EvenOdd.mlg.main.exit", label %switch-unboxed_default_1.i

switch-unboxed_default_1.i:                       ; preds = %1
  %12 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { i32 } }, ptr %12, i64 0, i32 1, i32 0
  store i32 1, ptr %13, align 4
  %14 = load i32, ptr %11, align 4
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %14, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %15, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4775", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %16 = tail call i32 @malgo_sub_int32_t(i32 %x_0.i.i, i32 noundef 1)
  %17 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %17, align 1
  %18 = getelementptr { i8, { i32 } }, ptr %17, i64 0, i32 1, i32 0
  store i32 %16, ptr %18, align 4
  %cond2.i = icmp eq i32 %16, 0
  br i1 %cond2.i, label %"test/testcases/malgo/EvenOdd.mlg.main.exit", label %switch-unboxed_default_2.i

switch-unboxed_default_2.i:                       ; preds = %switch-unboxed_default_1.i
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %17, ptr %let_capture_2.i, align 8
  store ptr %let_capture_2.i, ptr %19, align 8
  %let_func_1.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4779", ptr %let_func_1.i, align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { i32 } }, ptr %20, i64 0, i32 1, i32 0
  store i32 1, ptr %21, align 4
  %.val.i = load i32, ptr %18, align 4
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %.val.i, ptr %let_capture_0.i3.i, align 4
  store ptr %let_capture_0.i3.i, ptr %22, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4791", ptr %let_func_0.i4.i, align 8
  %23 = tail call i32 @malgo_sub_int32_t(i32 %.val.i, i32 noundef 1)
  %24 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %24, align 1
  %25 = getelementptr { i8, { i32 } }, ptr %24, i64 0, i32 1, i32 0
  store i32 %23, ptr %25, align 4
  %cond3.i = icmp eq i32 %23, 0
  br i1 %cond3.i, label %"test/testcases/malgo/EvenOdd.mlg.main.exit", label %switch-unboxed_default_3.i

switch-unboxed_default_3.i:                       ; preds = %switch-unboxed_default_2.i
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_4.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %24, ptr %let_capture_4.i, align 8
  store ptr %let_capture_4.i, ptr %26, align 8
  %let_func_2.i = getelementptr { ptr, ptr }, ptr %26, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4780", ptr %let_func_2.i, align 8
  %27 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { i32 } }, ptr %27, i64 0, i32 1, i32 0
  store i32 1, ptr %28, align 4
  %29 = load ptr, ptr %26, align 8
  %30 = load ptr, ptr %let_func_2.i, align 8
  %31 = tail call ptr %30(ptr %29, ptr nonnull %27)
  %32 = getelementptr { i8, { i32 } }, ptr %31, i64 0, i32 1
  %33 = load i32, ptr %32, align 4
  %cond4.i = icmp eq i32 %33, 0
  br i1 %cond4.i, label %"test/testcases/malgo/EvenOdd.mlg.main.exit", label %switch-unboxed_default_4.i

switch-unboxed_default_4.i:                       ; preds = %switch-unboxed_default_3.i
  %34 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i5.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %31, ptr %let_capture_0.i5.i, align 8
  store ptr %let_capture_0.i5.i, ptr %34, align 8
  %let_func_0.i6.i = getelementptr { ptr, ptr }, ptr %34, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4781", ptr %let_func_0.i6.i, align 8
  %35 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %35, align 1
  %36 = getelementptr { i8, { i32 } }, ptr %35, i64 0, i32 1, i32 0
  store i32 1, ptr %36, align 4
  %37 = load ptr, ptr %34, align 8
  %38 = load ptr, ptr %let_func_0.i6.i, align 8
  %39 = tail call ptr %38(ptr %37, ptr nonnull %35)
  %40 = getelementptr { i8, { i32 } }, ptr %39, i64 0, i32 1
  %41 = load i32, ptr %40, align 4
  %cond6.i.i = icmp eq i32 %41, 0
  br i1 %cond6.i.i, label %"test/testcases/malgo/EvenOdd.mlg.main.exit", label %switch-unboxed_default_0.i.i

switch-unboxed_default_0.i.i:                     ; preds = %switch-unboxed_default_4.i, %switch-unboxed_default_5.i.i
  %42 = phi ptr [ %85, %switch-unboxed_default_5.i.i ], [ %40, %switch-unboxed_default_4.i ]
  %43 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %43, align 1
  %44 = getelementptr { i8, { i32 } }, ptr %43, i64 0, i32 1, i32 0
  store i32 1, ptr %44, align 4
  %45 = load i32, ptr %42, align 4
  %46 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i7.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %45, ptr %let_capture_0.i7.i, align 4
  store ptr %let_capture_0.i7.i, ptr %46, align 8
  %let_func_0.i8.i = getelementptr { ptr, ptr }, ptr %46, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4786", ptr %let_func_0.i8.i, align 8
  %47 = tail call i32 @malgo_sub_int32_t(i32 %45, i32 noundef 1)
  %48 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %48, align 1
  %49 = getelementptr { i8, { i32 } }, ptr %48, i64 0, i32 1, i32 0
  store i32 %47, ptr %49, align 4
  %cond1.i.i = icmp eq i32 %47, 0
  br i1 %cond1.i.i, label %"test/testcases/malgo/EvenOdd.mlg.main.exit", label %switch-unboxed_default_1.i.i

switch-unboxed_default_1.i.i:                     ; preds = %switch-unboxed_default_0.i.i
  %50 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %50, align 1
  %51 = getelementptr { i8, { i32 } }, ptr %50, i64 0, i32 1, i32 0
  store i32 1, ptr %51, align 4
  %52 = load i32, ptr %49, align 4
  %53 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %52, ptr %let_capture_2.i.i, align 4
  store ptr %let_capture_2.i.i, ptr %53, align 8
  %let_func_1.i.i = getelementptr { ptr, ptr }, ptr %53, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4787", ptr %let_func_1.i.i, align 8
  %p_0.i.i.i = load i32, ptr %let_capture_2.i.i, align 4
  %54 = tail call i32 @malgo_sub_int32_t(i32 %p_0.i.i.i, i32 noundef 1)
  %55 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %55, align 1
  %56 = getelementptr { i8, { i32 } }, ptr %55, i64 0, i32 1, i32 0
  store i32 %54, ptr %56, align 4
  %cond2.i.i = icmp eq i32 %54, 0
  br i1 %cond2.i.i, label %"test/testcases/malgo/EvenOdd.mlg.main.exit", label %switch-unboxed_default_2.i.i

switch-unboxed_default_2.i.i:                     ; preds = %switch-unboxed_default_1.i.i
  %57 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %57, align 1
  %58 = getelementptr { i8, { i32 } }, ptr %57, i64 0, i32 1, i32 0
  store i32 1, ptr %58, align 4
  %59 = load i32, ptr %56, align 4
  %60 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %59, ptr %let_capture_0.i.i.i, align 4
  store ptr %let_capture_0.i.i.i, ptr %60, align 8
  %let_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %60, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4775", ptr %let_func_0.i.i.i, align 8
  %x_0.i.i.i = load i32, ptr %let_capture_0.i.i.i, align 4
  %61 = tail call i32 @malgo_sub_int32_t(i32 %x_0.i.i.i, i32 noundef 1)
  %62 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %62, align 1
  %63 = getelementptr { i8, { i32 } }, ptr %62, i64 0, i32 1, i32 0
  store i32 %61, ptr %63, align 4
  %cond3.i.i = icmp eq i32 %61, 0
  br i1 %cond3.i.i, label %"test/testcases/malgo/EvenOdd.mlg.main.exit", label %switch-unboxed_default_3.i.i

switch-unboxed_default_3.i.i:                     ; preds = %switch-unboxed_default_2.i.i
  %64 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_4.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %62, ptr %let_capture_4.i.i, align 8
  store ptr %let_capture_4.i.i, ptr %64, align 8
  %let_func_2.i.i = getelementptr { ptr, ptr }, ptr %64, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4788", ptr %let_func_2.i.i, align 8
  %65 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %65, align 1
  %66 = getelementptr { i8, { i32 } }, ptr %65, i64 0, i32 1, i32 0
  store i32 1, ptr %66, align 4
  %.val.i.i = load i32, ptr %63, align 4
  %67 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i2.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %.val.i.i, ptr %let_capture_0.i2.i.i, align 4
  store ptr %let_capture_0.i2.i.i, ptr %67, align 8
  %let_func_0.i3.i.i = getelementptr { ptr, ptr }, ptr %67, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4791", ptr %let_func_0.i3.i.i, align 8
  %68 = tail call i32 @malgo_sub_int32_t(i32 %.val.i.i, i32 noundef 1)
  %69 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %69, align 1
  %70 = getelementptr { i8, { i32 } }, ptr %69, i64 0, i32 1, i32 0
  store i32 %68, ptr %70, align 4
  %cond4.i.i = icmp eq i32 %68, 0
  br i1 %cond4.i.i, label %"test/testcases/malgo/EvenOdd.mlg.main.exit", label %switch-unboxed_default_4.i.i

switch-unboxed_default_4.i.i:                     ; preds = %switch-unboxed_default_3.i.i
  %71 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_6.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %69, ptr %let_capture_6.i.i, align 8
  store ptr %let_capture_6.i.i, ptr %71, align 8
  %let_func_3.i.i = getelementptr { ptr, ptr }, ptr %71, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4789", ptr %let_func_3.i.i, align 8
  %72 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %72, align 1
  %73 = getelementptr { i8, { i32 } }, ptr %72, i64 0, i32 1, i32 0
  store i32 1, ptr %73, align 4
  %74 = load ptr, ptr %71, align 8
  %75 = load ptr, ptr %let_func_3.i.i, align 8
  %76 = tail call ptr %75(ptr %74, ptr nonnull %72)
  %77 = getelementptr { i8, { i32 } }, ptr %76, i64 0, i32 1
  %78 = load i32, ptr %77, align 4
  %cond5.i.i = icmp eq i32 %78, 0
  br i1 %cond5.i.i, label %"test/testcases/malgo/EvenOdd.mlg.main.exit", label %switch-unboxed_default_5.i.i

switch-unboxed_default_5.i.i:                     ; preds = %switch-unboxed_default_4.i.i
  %79 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i4.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %76, ptr %let_capture_0.i4.i.i, align 8
  store ptr %let_capture_0.i4.i.i, ptr %79, align 8
  %let_func_0.i5.i.i = getelementptr { ptr, ptr }, ptr %79, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4781", ptr %let_func_0.i5.i.i, align 8
  %80 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %80, align 1
  %81 = getelementptr { i8, { i32 } }, ptr %80, i64 0, i32 1, i32 0
  store i32 1, ptr %81, align 4
  %82 = load ptr, ptr %79, align 8
  %83 = load ptr, ptr %let_func_0.i5.i.i, align 8
  %84 = tail call ptr %83(ptr %82, ptr nonnull %80)
  %85 = getelementptr { i8, { i32 } }, ptr %84, i64 0, i32 1
  %86 = load i32, ptr %85, align 4
  %cond.i.i = icmp eq i32 %86, 0
  br i1 %cond.i.i, label %"test/testcases/malgo/EvenOdd.mlg.main.exit", label %switch-unboxed_default_0.i.i

"test/testcases/malgo/EvenOdd.mlg.main.exit":     ; preds = %switch-unboxed_default_0.i.i, %switch-unboxed_default_1.i.i, %switch-unboxed_default_2.i.i, %switch-unboxed_default_3.i.i, %switch-unboxed_default_4.i.i, %switch-unboxed_default_5.i.i, %1, %switch-unboxed_default_1.i, %switch-unboxed_default_2.i, %switch-unboxed_default_3.i, %switch-unboxed_default_4.i
  %.sink.i = phi i8 [ 0, %1 ], [ 1, %switch-unboxed_default_1.i ], [ 0, %switch-unboxed_default_2.i ], [ 1, %switch-unboxed_default_3.i ], [ 0, %switch-unboxed_default_4.i ], [ 0, %switch-unboxed_default_3.i.i ], [ 0, %switch-unboxed_default_1.i.i ], [ 0, %switch-unboxed_default_5.i.i ], [ 1, %switch-unboxed_default_4.i.i ], [ 1, %switch-unboxed_default_2.i.i ], [ 1, %switch-unboxed_default_0.i.i ]
  %str4777.sink14.i = phi ptr [ @str4776, %1 ], [ @str4777, %switch-unboxed_default_1.i ], [ @str4776, %switch-unboxed_default_2.i ], [ @str4777, %switch-unboxed_default_3.i ], [ @str4776, %switch-unboxed_default_4.i ], [ @str4776, %switch-unboxed_default_3.i.i ], [ @str4776, %switch-unboxed_default_1.i.i ], [ @str4776, %switch-unboxed_default_5.i.i ], [ @str4777, %switch-unboxed_default_4.i.i ], [ @str4777, %switch-unboxed_default_2.i.i ], [ @str4777, %switch-unboxed_default_0.i.i ]
  %87 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 %.sink.i, ptr %87, align 1
  %88 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %88, align 1
  %89 = getelementptr { i8, { ptr } }, ptr %88, i64 0, i32 1, i32 0
  store ptr %str4777.sink14.i, ptr %89, align 8
  %90 = tail call ptr @malgo_print_string(ptr noundef nonnull %str4777.sink14.i)
  %91 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %91, align 1
  %92 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %92, align 1
  %93 = tail call ptr @malgo_newline(ptr noundef nonnull %92)
  ret i32 0
}
