; ModuleID = '/workspaces/malgo/.malgo-work/TestLet.ll'
source_filename = "./test/testcases/malgo/TestLet.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal i32 @"TestLet.#let_closure_190"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"TestLet.#let_closure_251", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %4 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %4
}

define internal i32 @"TestLet.#let_closure_251"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @Builtin.undefined, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 1, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"TestLet.#let_closure_190", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"TestLet.#let_closure_251", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i.i.i, i32 noundef 2)
  %8 = tail call ptr @malgo_int32_t_to_string(i32 %7)
  %9 = tail call ptr @malgo_print_string(ptr %8)
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 1, ptr %let_capture_0.i1.i, align 4
  store ptr %let_capture_0.i1.i, ptr %10, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"TestLet.#let_closure_190", ptr %let_func_0.i2.i, align 8
  %x_0.i3.i = load i32, ptr %let_capture_0.i1.i, align 4
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i4.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i3.i, ptr %let_capture_0.i.i.i4.i, align 4
  store ptr %let_capture_0.i.i.i4.i, ptr %11, align 8
  %let_func_0.i.i.i5.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"TestLet.#let_closure_251", ptr %let_func_0.i.i.i5.i, align 8
  %p_0.i.i.i6.i = load i32, ptr %let_capture_0.i.i.i4.i, align 4
  %12 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i.i6.i, i32 noundef 2)
  %13 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { i32 } }, ptr %13, i64 0, i32 1, i32 0
  store i32 %12, ptr %14, align 4
  %15 = tail call ptr @malgo_int32_t_to_string(i32 %12)
  %16 = tail call ptr @malgo_print_string(ptr %15)
  ret i32 0
}
