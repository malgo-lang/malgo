; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/TestDot.ll'
source_filename = "test/testcases/malgo/TestDot.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str95 = unnamed_addr constant [1 x i8] zeroinitializer
@str254 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i64 @malgo_add_int64_t(i64, i64) local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal i64 @"test/testcases/malgo/TestDot.mlg.#let_closure_75"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestDot.mlg.#let_closure_145", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %4 = tail call i64 @malgo_add_int64_t(i64 %p_0.i.i, i64 %1)
  ret i64 %4
}

define internal i64 @"test/testcases/malgo/TestDot.mlg.#let_closure_145"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_add_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define internal noundef ptr @"test/testcases/malgo/TestDot.mlg.succ"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %"test/testcases/malgo/TestDot.mlg.$int64#_30_0") {
"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0":
  %1 = getelementptr { i8, { i64 } }, ptr %"test/testcases/malgo/TestDot.mlg.$int64#_30_0", i64 0, i32 1
  %2 = load i64, ptr %1, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %2, ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %3, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestDot.mlg.#let_closure_75", ptr %let_func_0.i, align 8
  %x_0.i = load i64, ptr %let_capture_0.i, align 4
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0.i, ptr %let_capture_0.i.i.i, align 4
  store ptr %let_capture_0.i.i.i, ptr %4, align 8
  %let_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestDot.mlg.#let_closure_145", ptr %let_func_0.i.i.i, align 8
  %p_0.i.i.i = load i64, ptr %let_capture_0.i.i.i, align 4
  %5 = tail call i64 @malgo_add_int64_t(i64 %p_0.i.i.i, i64 noundef 1)
  %6 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { i64 } }, ptr %6, i64 0, i32 1, i32 0
  store i64 %5, ptr %7, align 4
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/TestDot.mlg.#fun_closure_222"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr %1) {
  %f_0 = load ptr, ptr %0, align 8
  %g_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %g_0 = load ptr, ptr %g_addr_0, align 8
  %g_0.val = load ptr, ptr %g_0, align 8
  %3 = getelementptr i8, ptr %g_0, i64 8
  %g_0.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %g_0.val1(ptr %g_0.val, ptr %1)
  %5 = load ptr, ptr %f_0, align 8
  %6 = getelementptr { ptr, ptr }, ptr %f_0, i64 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = tail call ptr %7(ptr %5, ptr %4)
  ret ptr %8
}

define internal ptr @"test/testcases/malgo/TestDot.mlg.#let_closure_242"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %f_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %f_0, ptr %fun_capture_0.i, align 8
  %g_0.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %1, ptr %g_0.i, align 8
  store ptr %fun_capture_0.i, ptr %3, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestDot.mlg.#fun_closure_222", ptr %fun_func_0.i, align 8
  ret ptr %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @"runtime/malgo/Builtin.mlg.undefined", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %succ_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestDot.mlg.succ", ptr %succ_func_0.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %6, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestDot.mlg.#let_closure_242", ptr %let_func_0.i.i, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %succ_func_1.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestDot.mlg.succ", ptr %succ_func_1.i, align 8
  %f_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %f_0.i.i, ptr %fun_capture_0.i.i.i, align 8
  %g_0.i.i.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i.i.i, i64 0, i32 1
  store ptr %7, ptr %g_0.i.i.i, align 8
  store ptr %fun_capture_0.i.i.i, ptr %8, align 8
  %fun_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestDot.mlg.#fun_closure_222", ptr %fun_func_0.i.i.i, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { i64 } }, ptr %9, i64 0, i32 1, i32 0
  store i64 0, ptr %10, align 4
  %11 = load ptr, ptr %8, align 8
  %12 = load ptr, ptr %fun_func_0.i.i.i, align 8
  %13 = tail call ptr %12(ptr %11, ptr nonnull %9)
  %14 = getelementptr i8, ptr %13, i64 4
  %.val.i = load i64, ptr %14, align 4
  %15 = tail call ptr @malgo_int64_t_to_string(i64 %.val.i)
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %16, align 1
  %17 = getelementptr { i8, { ptr } }, ptr %16, i64 0, i32 1, i32 0
  store ptr %15, ptr %17, align 8
  %18 = tail call ptr @malgo_print_string(ptr %15)
  ret i32 0
}
