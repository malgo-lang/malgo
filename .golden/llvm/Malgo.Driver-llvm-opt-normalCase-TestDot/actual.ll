; ModuleID = './test/tmp/malgo_test/normal/TestDot.ll'
source_filename = "./test/testcases/malgo/TestDot.mlg"

declare void @GC_init() local_unnamed_addr

declare i64 @malgo_add_int64_t(i64, i64) local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal i64 @"TestDot.#let_closure_2890"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_add_int64_t(i64 %x_0, i64 %1)
  ret i64 %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %3, i64 0, i32 1, i32 0
  store i64 0, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"TestDot.#let_closure_2890", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %6 = tail call i64 @malgo_add_int64_t(i64 %x_0.i.i, i64 noundef 1)
  %7 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i64 } }, ptr %7, i64 0, i32 1, i32 0
  store i64 %6, ptr %8, align 4
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %6, ptr %let_capture_0.i1.i, align 4
  store ptr %let_capture_0.i1.i, ptr %9, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"TestDot.#let_closure_2890", ptr %let_func_0.i2.i, align 8
  %x_0.i3.i = load i64, ptr %let_capture_0.i1.i, align 4
  %10 = tail call i64 @malgo_add_int64_t(i64 %x_0.i3.i, i64 noundef 1)
  %11 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { i64 } }, ptr %11, i64 0, i32 1, i32 0
  store i64 %10, ptr %12, align 4
  %13 = tail call ptr @malgo_int64_t_to_string(i64 %10)
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %14, align 1
  %15 = getelementptr { i8, { ptr } }, ptr %14, i64 0, i32 1, i32 0
  store ptr %13, ptr %15, align 8
  %16 = tail call ptr @malgo_print_string(ptr %13)
  ret i32 0
}
