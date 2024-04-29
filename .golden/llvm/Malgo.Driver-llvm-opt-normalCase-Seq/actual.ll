; ModuleID = '/workspaces/malgo/.malgo-work/Seq.ll'
source_filename = "./test/testcases/malgo/Seq.mlg"

@Seq.executeWhenLoaded = local_unnamed_addr global ptr undef

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal i32 @"Seq.#let_closure_3020"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %2, i64 0, i32 1, i32 0
  store i32 1, ptr %3, align 4
  %4 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i32 } }, ptr %4, i64 0, i32 1, i32 0
  store i32 2, ptr %5, align 4
  %6 = load i32, ptr %3, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %6, ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %7, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"Seq.#let_closure_3025", ptr %let_func_0.i, align 8
  %8 = tail call i32 @malgo_add_int32_t(i32 %6, i32 noundef 2)
  %9 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { i32 } }, ptr %9, i64 0, i32 1, i32 0
  store i32 %8, ptr %10, align 4
  %11 = tail call ptr @malgo_int32_t_to_string(i32 %8)
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr %11, ptr %13, align 8
  %14 = tail call ptr @malgo_print_string(ptr %11)
  %15 = load i32, ptr %3, align 4
  %16 = load i32, ptr %5, align 4
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %15, ptr %let_capture_2.i, align 4
  store ptr %let_capture_2.i, ptr %17, align 8
  %let_func_1.i = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"Seq.#let_closure_3026", ptr %let_func_1.i, align 8
  %18 = tail call i32 @malgo_add_int32_t(i32 %15, i32 %16)
  %19 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { i32 } }, ptr %19, i64 0, i32 1, i32 0
  store i32 %18, ptr %20, align 4
  store ptr %19, ptr @Seq.executeWhenLoaded, align 8
  %21 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %21, align 1
  %22 = load ptr, ptr @Seq.executeWhenLoaded, align 8
  %23 = getelementptr { i8, { i32 } }, ptr %22, i64 0, i32 1
  %24 = load i32, ptr %23, align 4
  %25 = tail call ptr @malgo_int32_t_to_string(i32 %24)
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %26, align 1
  %27 = getelementptr { i8, { ptr } }, ptr %26, i64 0, i32 1, i32 0
  store ptr %25, ptr %27, align 8
  %28 = tail call ptr @malgo_print_string(ptr %25)
  ret i32 0
}

define internal i32 @"Seq.#let_closure_3025"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %p_0, ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %3, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Seq.#let_closure_3020", ptr %let_func_0.i, align 8
  %p_0.i = load i32, ptr %let_capture_0.i, align 4
  %4 = tail call i32 @malgo_add_int32_t(i32 %p_0.i, i32 %1)
  ret i32 %4
}

define internal i32 @"Seq.#let_closure_3026"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %p_0, ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %3, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Seq.#let_closure_3020", ptr %let_func_0.i, align 8
  %p_0.i = load i32, ptr %let_capture_0.i, align 4
  %4 = tail call i32 @malgo_add_int32_t(i32 %p_0.i, i32 %1)
  ret i32 %4
}
