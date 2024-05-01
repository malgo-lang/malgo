; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/Test0.ll'
source_filename = "test/testcases/malgo/Test0.mlg"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"test/testcases/malgo/Test0.mlg.#let_closure_2364"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/Test0.mlg.#fun_closure_2365"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %1) {
"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0":
  %2 = getelementptr { i8, { i64 } }, ptr %1, i64 0, i32 1
  %3 = load i64, ptr %2, align 4
  %4 = tail call ptr @malgo_int64_t_to_string(i64 %3)
  %5 = tail call ptr @malgo_print_string(ptr %4)
  ret ptr %5
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %3, i64 0, i32 1, i32 0
  store i64 1, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/Test0.mlg.#let_closure_2364", ptr %let_func_0.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %6, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/Test0.mlg.#fun_closure_2365", ptr %fun_func_0.i, align 8
  %7 = load ptr, ptr %5, align 8
  %8 = load ptr, ptr %let_func_0.i, align 8
  %9 = tail call ptr %8(ptr %7, ptr nonnull %6)
  ret i32 0
}
