; ModuleID = './test/tmp/malgo_test/noopt/EvenOdd.ll'
source_filename = "./test/testcases/malgo/EvenOdd.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef
@str141 = unnamed_addr constant [1 x i8] zeroinitializer
@str165 = unnamed_addr constant [6 x i8] c"False\00"
@str166 = unnamed_addr constant [5 x i8] c"True\00"
@str289 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i32 @malgo_sub_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"EvenOdd.#let_closure_151"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int32#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int32#_0", i64 4
  %"int32#_0.val" = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val", ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"EvenOdd.#let_closure_209", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"EvenOdd.#let_closure_245", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call i32 @malgo_sub_int32_t(i32 %p_0.i.i.i.i, i32 %.val)
  %8 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { i32 } }, ptr %8, i64 0, i32 1, i32 0
  store i32 %7, ptr %9, align 4
  ret ptr %8
}

define internal i32 @"EvenOdd.#let_closure_209"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"EvenOdd.#let_closure_245", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %4 = tail call i32 @malgo_sub_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %4
}

define internal i32 @"EvenOdd.#let_closure_245"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_sub_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @Builtin.undefined, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { i32 } }, ptr %5, i64 0, i32 1, i32 0
  store i32 10, ptr %6, align 4
  br label %switch-unboxed_default_0.i.i

switch-unboxed_default_0.i.i:                     ; preds = %switch-unboxed_default_0.i.i.i, %1
  %"EvenOdd.$int32#_51_0.tr4.i.i" = phi ptr [ %20, %switch-unboxed_default_0.i.i.i ], [ %5, %1 ]
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"EvenOdd.$int32#_51_0.tr4.i.i", ptr %let_capture_0.i.i.i, align 8
  store ptr %let_capture_0.i.i.i, ptr %7, align 8
  %let_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"EvenOdd.#let_closure_151", ptr %let_func_0.i.i.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { i32 } }, ptr %8, i64 0, i32 1, i32 0
  store i32 1, ptr %9, align 4
  %10 = load ptr, ptr %7, align 8
  %11 = load ptr, ptr %let_func_0.i.i.i, align 8
  %12 = tail call ptr %11(ptr %10, ptr nonnull %8)
  %13 = getelementptr { i8, { i32 } }, ptr %12, i64 0, i32 1
  %14 = load i32, ptr %13, align 4
  %cond.i.i.i = icmp eq i32 %14, 0
  br i1 %cond.i.i.i, label %EvenOdd.main.exit, label %switch-unboxed_default_0.i.i.i

switch-unboxed_default_0.i.i.i:                   ; preds = %switch-unboxed_default_0.i.i
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %12, ptr %let_capture_0.i1.i.i, align 8
  store ptr %let_capture_0.i1.i.i, ptr %15, align 8
  %let_func_0.i2.i.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"EvenOdd.#let_closure_151", ptr %let_func_0.i2.i.i, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %16, align 1
  %17 = getelementptr { i8, { i32 } }, ptr %16, i64 0, i32 1, i32 0
  store i32 1, ptr %17, align 4
  %18 = load ptr, ptr %15, align 8
  %19 = load ptr, ptr %let_func_0.i2.i.i, align 8
  %20 = tail call ptr %19(ptr %18, ptr nonnull %16)
  %21 = getelementptr { i8, { i32 } }, ptr %20, i64 0, i32 1
  %22 = load i32, ptr %21, align 4
  %cond.i.i = icmp eq i32 %22, 0
  br i1 %cond.i.i, label %EvenOdd.main.exit, label %switch-unboxed_default_0.i.i

EvenOdd.main.exit:                                ; preds = %switch-unboxed_default_0.i.i, %switch-unboxed_default_0.i.i.i
  %.sink.i = phi i8 [ 1, %switch-unboxed_default_0.i.i.i ], [ 0, %switch-unboxed_default_0.i.i ]
  %spec.select.i.i = phi ptr [ @str166, %switch-unboxed_default_0.i.i.i ], [ @str165, %switch-unboxed_default_0.i.i ]
  %23 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 %.sink.i, ptr %23, align 1
  %24 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %24, align 1
  %25 = getelementptr { i8, { ptr } }, ptr %24, i64 0, i32 1, i32 0
  store ptr %spec.select.i.i, ptr %25, align 8
  %26 = tail call ptr @malgo_print_string(ptr noundef nonnull %spec.select.i.i)
  %27 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %27, align 1
  %28 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %28, align 1
  %29 = tail call ptr @malgo_newline(ptr noundef nonnull %28)
  ret i32 0
}
