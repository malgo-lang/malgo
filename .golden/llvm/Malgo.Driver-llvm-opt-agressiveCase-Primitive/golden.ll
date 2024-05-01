; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/Primitive.ll'
source_filename = "test/testcases/malgo/Primitive.mlg"

declare void @GC_init() local_unnamed_addr

declare i64 @malgo_add_int64_t(i64, i64) local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal i64 @"test/testcases/malgo/Primitive.mlg.#let_closure_2315"(ptr nocapture nofree readnone %0, i64 %1) {
  %3 = tail call i64 @malgo_add_int64_t(i64 noundef 40, i64 %1)
  ret i64 %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %let_capture_0.i, ptr %3, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/Primitive.mlg.#let_closure_2315", ptr %let_func_0.i, align 8
  %4 = tail call i64 @malgo_add_int64_t(i64 noundef 40, i64 noundef 2)
  %5 = tail call ptr @malgo_int64_t_to_string(i64 %4)
  %6 = tail call ptr @malgo_print_string(ptr %5)
  ret i32 0
}
