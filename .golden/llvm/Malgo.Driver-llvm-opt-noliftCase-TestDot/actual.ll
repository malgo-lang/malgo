; ModuleID = './test/tmp/malgo_test/nolift/TestDot.ll'
source_filename = "./test/testcases/malgo/TestDot.mlg"

declare void @GC_init() local_unnamed_addr

declare i64 @malgo_add_int64_t(i64, i64) local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"Builtin.Int64#"(ptr nocapture nofree readnone %0, i64 %"Builtin.$p_1794_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i64 } }, ptr %2, i64 0, i32 1, i32 0
  store i64 %"Builtin.$p_1794_0", ptr %3, align 4
  ret ptr %2
}

define internal i64 @"TestDot.#let_closure_2889"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_add_int64_t(i64 %x_0, i64 %1)
  ret i64 %3
}

define internal ptr @"Builtin.addInt64#"(ptr nocapture nofree readnone %0, i64 %"Builtin.$x_3995_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"Builtin.$x_3995_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"TestDot.#let_closure_2889", ptr %let_func_0, align 8
  ret ptr %2
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
  store ptr null, ptr %5, align 8
  %"addInt64#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Builtin.addInt64#", ptr %"addInt64#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %6, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"TestDot.#let_closure_2889", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %7 = tail call i64 @malgo_add_int64_t(i64 %x_0.i.i, i64 noundef 1)
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %"Int64#_func_0.i" = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"Builtin.Int64#", ptr %"Int64#_func_0.i", align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { i64 } }, ptr %9, i64 0, i32 1, i32 0
  store i64 %7, ptr %10, align 4
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %"addInt64#_func_1.i" = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"Builtin.addInt64#", ptr %"addInt64#_func_1.i", align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %7, ptr %let_capture_0.i1.i, align 4
  store ptr %let_capture_0.i1.i, ptr %12, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"TestDot.#let_closure_2889", ptr %let_func_0.i2.i, align 8
  %x_0.i3.i = load i64, ptr %let_capture_0.i1.i, align 4
  %13 = tail call i64 @malgo_add_int64_t(i64 %x_0.i3.i, i64 noundef 1)
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %"Int64#_func_1.i" = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"Builtin.Int64#", ptr %"Int64#_func_1.i", align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i64 } }, ptr %15, i64 0, i32 1, i32 0
  store i64 %13, ptr %16, align 4
  %17 = tail call ptr @malgo_int64_t_to_string(i64 %13)
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %18, align 1
  %19 = getelementptr { i8, { ptr } }, ptr %18, i64 0, i32 1, i32 0
  store ptr %17, ptr %19, align 8
  %20 = tail call ptr @malgo_print_string(ptr %17)
  ret i32 0
}
