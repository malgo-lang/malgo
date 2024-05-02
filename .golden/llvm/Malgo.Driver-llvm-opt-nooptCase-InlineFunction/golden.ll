; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/InlineFunction.ll'
source_filename = "test/testcases/malgo/InlineFunction.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str325 = unnamed_addr constant [1 x i8] zeroinitializer
@str488 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare i32 @malgo_sub_int32_t(i32, i32) local_unnamed_addr

declare i32 @malgo_le_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_313"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_420", ptr %let_func_0.i.i, align 8
  %"int32#_0.i.i" = load ptr, ptr %let_capture_0.i.i, align 8
  %4 = getelementptr i8, ptr %"int32#_0.i.i", i64 4
  %"int32#_0.val.i.i" = load i32, ptr %4, align 4
  %5 = getelementptr i8, ptr %1, i64 4
  %.val.i.i = load i32, ptr %5, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val.i.i", ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_440", ptr %let_func_0.i.i.i.i, align 8
  %x_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i.i.i, ptr %let_capture_0.i.i.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i.i.i, ptr %7, align 8
  %let_func_0.i.i.i.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_427", ptr %let_func_0.i.i.i.i.i.i, align 8
  %p_0.i.i.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i.i.i, align 4
  %8 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i.i.i.i.i, i32 %.val.i.i)
  %9 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { i32 } }, ptr %9, i64 0, i32 1, i32 0
  store i32 %8, ptr %10, align 4
  ret ptr %9
}

define internal noundef ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_348"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_507", ptr %let_func_0.i.i, align 8
  %"int32#_0.i.i" = load ptr, ptr %let_capture_0.i.i, align 8
  %4 = getelementptr i8, ptr %"int32#_0.i.i", i64 4
  %"int32#_0.val.i.i" = load i32, ptr %4, align 4
  %5 = getelementptr i8, ptr %1, i64 4
  %.val.i.i = load i32, ptr %5, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val.i.i", ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_382", ptr %let_func_0.i.i.i.i, align 8
  %x_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i.i.i, ptr %let_capture_0.i.i.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i.i.i, ptr %7, align 8
  %let_func_0.i.i.i.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_403", ptr %let_func_0.i.i.i.i.i.i, align 8
  %p_0.i.i.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i.i.i, align 4
  %8 = tail call i32 @malgo_sub_int32_t(i32 %p_0.i.i.i.i.i.i, i32 %.val.i.i)
  %9 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { i32 } }, ptr %9, i64 0, i32 1, i32 0
  store i32 %8, ptr %10, align 4
  ret ptr %9
}

define internal noundef ptr @"runtime/malgo/Builtin.mlg.toStringInt32"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %"runtime/malgo/Builtin.mlg.$int32#_2181_0") {
"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0":
  %1 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2181_0", i64 0, i32 1
  %2 = load i32, ptr %1, align 4
  %3 = tail call ptr @malgo_int32_t_to_string(i32 %2)
  %4 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %4, i64 0, i32 1, i32 0
  store ptr %3, ptr %5, align 8
  ret ptr %4
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_356"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_le_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Prelude.mlg.putStrLn"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"runtime/malgo/Prelude.mlg.$str_716_0") {
  %2 = getelementptr i8, ptr %"runtime/malgo/Prelude.mlg.$str_716_0", i64 8
  %"runtime/malgo/Prelude.mlg.$str_716_0.val" = load ptr, ptr %2, align 8
  %3 = tail call ptr @malgo_print_string(ptr %"runtime/malgo/Prelude.mlg.$str_716_0.val")
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_newline(ptr noundef nonnull %5)
  ret ptr %6
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_382"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_403", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %4 = tail call i32 @malgo_sub_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %4
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_391"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readonly %1) {
  %true_0 = load ptr, ptr %0, align 8
  %t_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %t_0 = load ptr, ptr %t_addr_0, align 8
  %true_0.val = load i8, ptr %true_0, align 1
  %switch.i = icmp eq i8 %true_0.val, 0
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %spec.select.i = select i1 %switch.i, ptr %1, ptr %t_0
  %4 = load ptr, ptr %spec.select.i, align 8
  %5 = getelementptr { ptr, ptr }, ptr %spec.select.i, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %3)
  ret ptr %7
}

define internal noundef ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_395"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
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
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_414", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_356", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call i32 @malgo_le_int32_t(i32 %p_0.i.i.i.i, i32 %.val)
  %cond.i.i = icmp eq i32 %7, 1
  %8 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  %spec.select.i.i = zext i1 %cond.i.i to i8
  store i8 %spec.select.i.i, ptr %8, align 1
  ret ptr %8
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_396"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %x_0 = load ptr, ptr %0, align 8
  %.val = load ptr, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %.val1(ptr %.val, ptr %x_0)
  ret ptr %4
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_403"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_sub_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_413"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %f_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %1, ptr %let_capture_0.i.i.i, align 8
  store ptr %let_capture_0.i.i.i, ptr %3, align 8
  %let_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_416", ptr %let_func_0.i.i.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i32 } }, ptr %4, i64 0, i32 1, i32 0
  store i32 1, ptr %5, align 4
  %6 = load ptr, ptr %3, align 8
  %7 = load ptr, ptr %let_func_0.i.i.i, align 8
  %8 = tail call ptr %7(ptr %6, ptr nonnull %4)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %8, ptr %let_capture_0.i1.i.i, align 8
  store ptr %let_capture_0.i1.i.i, ptr %9, align 8
  %let_func_0.i2.i.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_492", ptr %let_func_0.i2.i.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i.i, ptr %10, align 8
  %fun_func_0.i.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_499", ptr %fun_func_0.i.i, align 8
  %11 = load ptr, ptr %9, align 8
  %12 = load ptr, ptr %let_func_0.i2.i.i, align 8
  %13 = tail call ptr %12(ptr %11, ptr nonnull %10)
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2.i.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %f_0, ptr %fun_capture_2.i.i, align 8
  %n_0.i.i = getelementptr { ptr, ptr }, ptr %fun_capture_2.i.i, i64 0, i32 1
  store ptr %1, ptr %n_0.i.i, align 8
  store ptr %fun_capture_2.i.i, ptr %14, align 8
  %fun_func_1.i.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_500", ptr %fun_func_1.i.i, align 8
  %15 = load ptr, ptr %13, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = tail call ptr %17(ptr %15, ptr nonnull %14)
  ret ptr %18
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_414"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_356", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %4 = tail call i32 @malgo_le_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %4
}

define internal noundef ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_416"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_395", ptr %let_func_0.i.i, align 8
  %"int32#_0.i.i" = load ptr, ptr %let_capture_0.i.i, align 8
  %4 = getelementptr i8, ptr %"int32#_0.i.i", i64 4
  %"int32#_0.val.i.i" = load i32, ptr %4, align 4
  %5 = getelementptr i8, ptr %1, i64 4
  %.val.i.i = load i32, ptr %5, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val.i.i", ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_414", ptr %let_func_0.i.i.i.i, align 8
  %x_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i.i.i, ptr %let_capture_0.i.i.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i.i.i, ptr %7, align 8
  %let_func_0.i.i.i.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_356", ptr %let_func_0.i.i.i.i.i.i, align 8
  %p_0.i.i.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i.i.i, align 4
  %8 = tail call i32 @malgo_le_int32_t(i32 %p_0.i.i.i.i.i.i, i32 %.val.i.i)
  %cond.i.i.i.i = icmp eq i32 %8, 1
  %9 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  %spec.select.i.i.i.i = zext i1 %cond.i.i.i.i to i8
  store i8 %spec.select.i.i.i.i, ptr %9, align 1
  ret ptr %9
}

define internal noundef ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_420"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
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
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_440", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_427", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i.i.i, i32 %.val)
  %8 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { i32 } }, ptr %8, i64 0, i32 1, i32 0
  store i32 %7, ptr %9, align 4
  ret ptr %8
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_427"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_440"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_427", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %4 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %4
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_455"(ptr nocapture nofree readnone %0, ptr nofree %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %1, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %3, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_413", ptr %let_func_0.i, align 8
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_492"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %true_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %true_0, ptr %let_capture_0.i, align 8
  %t_0.i = getelementptr { ptr, ptr }, ptr %let_capture_0.i, i64 0, i32 1
  store ptr %1, ptr %t_0.i, align 8
  store ptr %let_capture_0.i, ptr %3, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_391", ptr %let_func_0.i, align 8
  ret ptr %3
}

define internal noundef ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_499"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_500"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readnone %1) {
  %f_0 = load ptr, ptr %0, align 8
  %n_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %n_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_348", ptr %let_func_0.i.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i32 } }, ptr %4, i64 0, i32 1, i32 0
  store i32 1, ptr %5, align 4
  %6 = load ptr, ptr %3, align 8
  %7 = load ptr, ptr %let_func_0.i.i, align 8
  %8 = tail call ptr %7(ptr %6, ptr nonnull %4)
  %9 = load ptr, ptr %f_0, align 8
  %10 = getelementptr { ptr, ptr }, ptr %f_0, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = tail call ptr %11(ptr %9, ptr %8)
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %12, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %13, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_313", ptr %let_func_0.i2.i, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %n_0, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %14, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_348", ptr %let_func_0.i4.i, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i32 } }, ptr %15, i64 0, i32 1, i32 0
  store i32 2, ptr %16, align 4
  %17 = load ptr, ptr %14, align 8
  %18 = load ptr, ptr %let_func_0.i4.i, align 8
  %19 = tail call ptr %18(ptr %17, ptr nonnull %15)
  %20 = load ptr, ptr %f_0, align 8
  %21 = load ptr, ptr %10, align 8
  %22 = tail call ptr %21(ptr %20, ptr %19)
  %23 = load ptr, ptr %13, align 8
  %24 = load ptr, ptr %let_func_0.i2.i, align 8
  %25 = tail call ptr %24(ptr %23, ptr %22)
  ret ptr %25
}

define internal noundef ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_507"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
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
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_382", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_403", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call i32 @malgo_sub_int32_t(i32 %p_0.i.i.i.i, i32 %.val)
  %8 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { i32 } }, ptr %8, i64 0, i32 1, i32 0
  store i32 %7, ptr %9, align 4
  ret ptr %8
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_509"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %f_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %f_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_509", ptr %let_func_0.i.i, align 8
  %4 = load ptr, ptr %f_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %f_0, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %3)
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, ptr %1)
  ret ptr %11
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
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %5, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_455", ptr %fun_func_0.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %6, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_509", ptr %let_func_0.i.i, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 5, ptr %8, align 4
  %9 = load ptr, ptr %6, align 8
  %10 = load ptr, ptr %let_func_0.i.i, align 8
  %11 = tail call ptr %10(ptr %9, ptr nonnull %7)
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %11, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %12, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_396", ptr %let_func_0.i2.i, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %toStringInt32_func_0.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32", ptr %toStringInt32_func_0.i, align 8
  %x_0.i.i = load ptr, ptr %let_capture_0.i1.i, align 8
  %14 = getelementptr { i8, { i32 } }, ptr %x_0.i.i, i64 0, i32 1
  %15 = load i32, ptr %14, align 4
  %16 = tail call ptr @malgo_int32_t_to_string(i32 %15)
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %17, align 1
  %18 = getelementptr { i8, { ptr } }, ptr %17, i64 0, i32 1, i32 0
  store ptr %16, ptr %18, align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %17, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %19, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_396", ptr %let_func_0.i4.i, align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %20, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0.i, align 8
  %x_0.i5.i = load ptr, ptr %let_capture_0.i3.i, align 8
  %21 = getelementptr i8, ptr %x_0.i5.i, i64 8
  %"runtime/malgo/Prelude.mlg.$str_716_0.val.i.i" = load ptr, ptr %21, align 8
  %22 = tail call ptr @malgo_print_string(ptr %"runtime/malgo/Prelude.mlg.$str_716_0.val.i.i")
  %23 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %23, align 1
  %24 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %24, align 1
  %25 = tail call ptr @malgo_newline(ptr noundef nonnull %24)
  ret i32 0
}
