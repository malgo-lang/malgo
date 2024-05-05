; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/Seq.ll'
source_filename = "test/testcases/malgo/Seq.mlg"

@"test/testcases/malgo/Seq.mlg.executeWhenLoaded" = local_unnamed_addr global ptr undef

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal i32 @"test/testcases/malgo/Seq.mlg.#let_closure_3020"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_add_int32_t"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$p_1808_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"runtime/malgo/Builtin.mlg.$p_1808_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/Seq.mlg.#let_closure_3020", ptr %let_func_0, align 8
  ret ptr %2
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
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %malgo_add_int32_t_func_0.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_add_int32_t", ptr %malgo_add_int32_t_func_0.i, align 8
  store ptr %8, ptr %let_capture_0.i, align 8
  %p_0.i = getelementptr { ptr, i32 }, ptr %let_capture_0.i, i64 0, i32 1
  store i32 %6, ptr %p_0.i, align 4
  store ptr %let_capture_0.i, ptr %7, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"test/testcases/malgo/Seq.mlg.#let_closure_3025", ptr %let_func_0.i, align 8
  %9 = tail call i32 @malgo_add_int32_t(i32 %6, i32 noundef 2)
  %10 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { i32 } }, ptr %10, i64 0, i32 1, i32 0
  store i32 %9, ptr %11, align 4
  %12 = tail call ptr @malgo_int32_t_to_string(i32 %9)
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { ptr } }, ptr %13, i64 0, i32 1, i32 0
  store ptr %12, ptr %14, align 8
  %15 = tail call ptr @malgo_print_string(ptr %12)
  %16 = load i32, ptr %3, align 4
  %17 = load i32, ptr %5, align 4
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 16)
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %19, align 8
  %malgo_add_int32_t_func_1.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_add_int32_t", ptr %malgo_add_int32_t_func_1.i, align 8
  store ptr %19, ptr %let_capture_2.i, align 8
  %p_1.i = getelementptr { ptr, i32 }, ptr %let_capture_2.i, i64 0, i32 1
  store i32 %16, ptr %p_1.i, align 4
  store ptr %let_capture_2.i, ptr %18, align 8
  %let_func_1.i = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"test/testcases/malgo/Seq.mlg.#let_closure_3026", ptr %let_func_1.i, align 8
  %20 = tail call i32 @malgo_add_int32_t(i32 %16, i32 %17)
  %21 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %21, align 1
  %22 = getelementptr { i8, { i32 } }, ptr %21, i64 0, i32 1, i32 0
  store i32 %20, ptr %22, align 4
  store ptr %21, ptr @"test/testcases/malgo/Seq.mlg.executeWhenLoaded", align 8
  %23 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %23, align 1
  %24 = load ptr, ptr @"test/testcases/malgo/Seq.mlg.executeWhenLoaded", align 8
  %25 = getelementptr { i8, { i32 } }, ptr %24, i64 0, i32 1
  %26 = load i32, ptr %25, align 4
  %27 = tail call ptr @malgo_int32_t_to_string(i32 %26)
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %28, i64 0, i32 1, i32 0
  store ptr %27, ptr %29, align 8
  %30 = tail call ptr @malgo_print_string(ptr %27)
  ret i32 0
}

define internal i32 @"test/testcases/malgo/Seq.mlg.#let_closure_3025"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(12) %0, i32 %1) {
  %malgo_add_int32_t_0 = load ptr, ptr %0, align 8
  %p_addr_0 = getelementptr { ptr, i32 }, ptr %0, i64 0, i32 1
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = load ptr, ptr %malgo_add_int32_t_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %malgo_add_int32_t_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, i32 %p_0)
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call i32 %9(ptr %7, i32 %1)
  ret i32 %10
}

define internal i32 @"test/testcases/malgo/Seq.mlg.#let_closure_3026"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(12) %0, i32 %1) {
  %malgo_add_int32_t_0 = load ptr, ptr %0, align 8
  %p_addr_0 = getelementptr { ptr, i32 }, ptr %0, i64 0, i32 1
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = load ptr, ptr %malgo_add_int32_t_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %malgo_add_int32_t_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, i32 %p_0)
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call i32 %9(ptr %7, i32 %1)
  ret i32 %10
}
