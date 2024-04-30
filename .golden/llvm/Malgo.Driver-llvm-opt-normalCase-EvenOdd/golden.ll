; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/EvenOdd.ll'
source_filename = "test/testcases/malgo/EvenOdd.mlg"

@str2947 = unnamed_addr constant [6 x i8] c"False\00"
@str2948 = unnamed_addr constant [5 x i8] c"True\00"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_sub_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal i32 @"EvenOdd.#let_closure_2949"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %p_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"EvenOdd.#let_closure_2951", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %4 = tail call i32 @malgo_sub_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %4
}

define internal i32 @"EvenOdd.#let_closure_2950"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %p_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"EvenOdd.#let_closure_2951", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %4 = tail call i32 @malgo_sub_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %4
}

define internal i32 @"EvenOdd.#let_closure_2951"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
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
  br label %switch-unboxed_default_0.i.i

switch-unboxed_default_0.i.i:                     ; preds = %switch-unboxed_default_0.i.i.i, %1
  %5 = phi ptr [ %19, %switch-unboxed_default_0.i.i.i ], [ %4, %1 ]
  %6 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { i32 } }, ptr %6, i64 0, i32 1, i32 0
  store i32 1, ptr %7, align 4
  %8 = load i32, ptr %5, align 4
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %8, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %9, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"EvenOdd.#let_closure_2949", ptr %let_func_0.i.i, align 8
  %10 = tail call i32 @malgo_sub_int32_t(i32 %8, i32 noundef 1)
  %11 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { i32 } }, ptr %11, i64 0, i32 1, i32 0
  store i32 %10, ptr %12, align 4
  %cond.i.i.i = icmp eq i32 %10, 0
  br i1 %cond.i.i.i, label %EvenOdd.main.exit, label %switch-unboxed_default_0.i.i.i

switch-unboxed_default_0.i.i.i:                   ; preds = %switch-unboxed_default_0.i.i
  %13 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { i32 } }, ptr %13, i64 0, i32 1, i32 0
  store i32 1, ptr %14, align 4
  %15 = load i32, ptr %12, align 4
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %15, ptr %let_capture_0.i.i.i, align 4
  store ptr %let_capture_0.i.i.i, ptr %16, align 8
  %let_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"EvenOdd.#let_closure_2950", ptr %let_func_0.i.i.i, align 8
  %17 = tail call i32 @malgo_sub_int32_t(i32 %15, i32 noundef 1)
  %18 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %18, align 1
  %19 = getelementptr { i8, { i32 } }, ptr %18, i64 0, i32 1, i32 0
  store i32 %17, ptr %19, align 4
  %cond.i.i = icmp eq i32 %17, 0
  br i1 %cond.i.i, label %EvenOdd.main.exit, label %switch-unboxed_default_0.i.i

EvenOdd.main.exit:                                ; preds = %switch-unboxed_default_0.i.i, %switch-unboxed_default_0.i.i.i
  %.sink.i = phi i8 [ 1, %switch-unboxed_default_0.i.i.i ], [ 0, %switch-unboxed_default_0.i.i ]
  %spec.select.i.i = phi ptr [ @str2948, %switch-unboxed_default_0.i.i.i ], [ @str2947, %switch-unboxed_default_0.i.i ]
  %20 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 %.sink.i, ptr %20, align 1
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %21, align 1
  %22 = getelementptr { i8, { ptr } }, ptr %21, i64 0, i32 1, i32 0
  store ptr %spec.select.i.i, ptr %22, align 8
  %23 = tail call ptr @malgo_print_string(ptr noundef nonnull %spec.select.i.i)
  %24 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %24, align 1
  %25 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %25, align 1
  %26 = tail call ptr @malgo_newline(ptr noundef nonnull %25)
  ret i32 0
}
