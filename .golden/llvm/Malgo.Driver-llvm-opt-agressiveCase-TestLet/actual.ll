; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/TestLet.ll'
source_filename = "test/testcases/malgo/TestLet.mlg"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal i32 @"test/testcases/malgo/TestLet.mlg.#let_closure_2401"(ptr nocapture nofree readnone %0, i32 %1) {
  %3 = tail call i32 @malgo_add_int32_t(i32 noundef 1, i32 %1)
  ret i32 %3
}

define internal i32 @"test/testcases/malgo/TestLet.mlg.#let_closure_2402"(ptr nocapture nofree readnone %0, i32 %1) {
  %3 = tail call i32 @malgo_add_int32_t(i32 noundef 1, i32 %1)
  ret i32 %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %let_capture_0.i, ptr %3, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestLet.mlg.#let_closure_2401", ptr %let_func_0.i, align 8
  %4 = tail call i32 @malgo_add_int32_t(i32 noundef 1, i32 noundef 2)
  %5 = tail call ptr @malgo_int32_t_to_string(i32 %4)
  %6 = tail call ptr @malgo_print_string(ptr %5)
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %let_capture_2.i, ptr %7, align 8
  %let_func_1.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestLet.mlg.#let_closure_2402", ptr %let_func_1.i, align 8
  %8 = tail call i32 @malgo_add_int32_t(i32 noundef 1, i32 noundef 2)
  %9 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { i32 } }, ptr %9, i64 0, i32 1, i32 0
  store i32 %8, ptr %10, align 4
  %11 = tail call ptr @malgo_int32_t_to_string(i32 %8)
  %12 = tail call ptr @malgo_print_string(ptr %11)
  ret i32 0
}
