; ModuleID = './test/tmp/malgo_test/noopt/Test0.ll'
source_filename = "./test/testcases/malgo/Test0.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"Test0.#fun_closure_127"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %1) {
  %3 = getelementptr i8, ptr %1, i64 4
  %.val = load i64, ptr %3, align 4
  %4 = tail call ptr @malgo_int64_t_to_string(i64 %.val)
  %5 = tail call ptr @malgo_print_string(ptr %4)
  ret ptr %5
}

define internal ptr @"Test0.#let_closure_223"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %x_0 = load ptr, ptr %0, align 8
  %.val = load ptr, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %.val1(ptr %.val, ptr %x_0)
  ret ptr %4
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @Builtin.undefined, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { i64 } }, ptr %5, i64 0, i32 1, i32 0
  store i64 1, ptr %6, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %7, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"Test0.#let_closure_223", ptr %let_func_0.i.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %8, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"Test0.#fun_closure_127", ptr %fun_func_0.i, align 8
  %9 = load ptr, ptr %7, align 8
  %10 = load ptr, ptr %let_func_0.i.i, align 8
  %11 = tail call ptr %10(ptr %9, ptr nonnull %8)
  ret i32 0
}
