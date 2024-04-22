; ModuleID = './test/tmp/malgo_test/aggressive/InlineFunction.ll'
source_filename = "./test/testcases/malgo/InlineFunction.mlg"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare i32 @malgo_sub_int32_t(i32, i32) local_unnamed_addr

declare i32 @malgo_le_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"InlineFunction.#let_closure_4129"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %n_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %n_0, i64 4
  %n_0.val = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %n_0.val, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4146", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %6 = tail call i32 @malgo_sub_int32_t(i32 %x_0.i.i, i32 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 %6, ptr %8, align 4
  ret ptr %7
}

define internal noundef ptr @"InlineFunction.#let_closure_4130"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %n_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %n_0, i64 4
  %n_0.val = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %n_0.val, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4146", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %6 = tail call i32 @malgo_sub_int32_t(i32 %x_0.i.i, i32 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 %6, ptr %8, align 4
  ret ptr %7
}

define internal noundef ptr @"InlineFunction.#let_closure_4131"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %d_0, i64 4
  %d_0.val = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %d_0.val, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4148", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %6 = tail call i32 @malgo_add_int32_t(i32 %x_0.i.i, i32 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 %6, ptr %8, align 4
  ret ptr %7
}

define internal ptr @"InlineFunction.#let_closure_4133"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %f_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %f_0, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %3, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4138", ptr %let_func_0.i, align 8
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

define internal i32 @"InlineFunction.#let_closure_4135"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_le_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"InlineFunction.#let_closure_4136"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %f_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %f_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4133", ptr %let_func_0.i.i, align 8
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

define internal ptr @"InlineFunction.#let_closure_4138"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %f_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %f_0, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %3, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4136", ptr %let_func_0.i, align 8
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

define internal ptr @Prelude.putStrLn(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"Prelude.$str_716_0") {
"switch_branch_Builtin.String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %"Prelude.$str_716_0", i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_print_string(ptr %2)
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_newline(ptr noundef nonnull %5)
  ret ptr %6
}

define internal noundef ptr @"InlineFunction.#let_closure_4139"(ptr nocapture nofree readnone %0, ptr nofree %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"InlineFunction.$fun_245", ptr %fun_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4133", ptr %let_func_0.i.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %4, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %5, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4150", ptr %let_func_0.i2.i, align 8
  %6 = tail call noundef ptr @"InlineFunction.#let_closure_4150"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i1.i, ptr nofree %1)
  ret ptr %6
}

define internal ptr @"InlineFunction.#let_closure_4140"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal noundef ptr @"InlineFunction.#let_closure_4141"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %n_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %n_0, i64 4
  %n_0.val = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %n_0.val, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4147", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %6 = tail call i32 @malgo_le_int32_t(i32 %x_0.i.i, i32 %.val)
  %cond.i.i = icmp eq i32 %6, 1
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  %spec.select.i.i = zext i1 %cond.i.i to i8
  store i8 %spec.select.i.i, ptr %7, align 1
  ret ptr %7
}

define internal noundef ptr @"InlineFunction.#fun_closure_4142"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readnone %1) {
  %f_0 = load ptr, ptr %0, align 8
  %n_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %n_0, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4129", ptr %let_func_0.i, align 8
  %6 = getelementptr i8, ptr %n_0, i64 4
  %"InlineFunction.$n_166_0.val1.i" = load i32, ptr %6, align 4
  %.val2.i = load i32, ptr %4, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"InlineFunction.$n_166_0.val1.i", ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %7, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4151", ptr %let_func_0.i.i, align 8
  %8 = tail call i32 @malgo_sub_int32_t(i32 %"InlineFunction.$n_166_0.val1.i", i32 %.val2.i)
  %9 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { i32 } }, ptr %9, i64 0, i32 1, i32 0
  store i32 %8, ptr %10, align 4
  %11 = load ptr, ptr %f_0, align 8
  %12 = getelementptr { ptr, ptr }, ptr %f_0, i64 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = tail call ptr %13(ptr %11, ptr nonnull %9)
  %15 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i32 } }, ptr %15, i64 0, i32 1, i32 0
  store i32 2, ptr %16, align 4
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %n_0, ptr %let_capture_2.i, align 8
  store ptr %let_capture_2.i, ptr %17, align 8
  %let_func_1.i = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4130", ptr %let_func_1.i, align 8
  %"InlineFunction.$n_166_0.val.i" = load i32, ptr %6, align 4
  %.val.i = load i32, ptr %16, align 4
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i5.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"InlineFunction.$n_166_0.val.i", ptr %let_capture_0.i5.i, align 4
  store ptr %let_capture_0.i5.i, ptr %18, align 8
  %let_func_0.i6.i = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4151", ptr %let_func_0.i6.i, align 8
  %19 = tail call i32 @malgo_sub_int32_t(i32 %"InlineFunction.$n_166_0.val.i", i32 %.val.i)
  %20 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { i32 } }, ptr %20, i64 0, i32 1, i32 0
  store i32 %19, ptr %21, align 4
  %22 = load ptr, ptr %f_0, align 8
  %23 = load ptr, ptr %12, align 8
  %24 = tail call ptr %23(ptr %22, ptr nonnull %20)
  %25 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_4.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %14, ptr %let_capture_4.i, align 8
  store ptr %let_capture_4.i, ptr %25, align 8
  %let_func_2.i = getelementptr { ptr, ptr }, ptr %25, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4131", ptr %let_func_2.i, align 8
  %26 = getelementptr i8, ptr %14, i64 4
  %.val3.i = load i32, ptr %26, align 4
  %27 = getelementptr i8, ptr %24, i64 4
  %.val4.i = load i32, ptr %27, align 4
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i7.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %.val3.i, ptr %let_capture_0.i7.i, align 4
  store ptr %let_capture_0.i7.i, ptr %28, align 8
  %let_func_0.i8.i = getelementptr { ptr, ptr }, ptr %28, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4153", ptr %let_func_0.i8.i, align 8
  %29 = tail call i32 @malgo_add_int32_t(i32 %.val3.i, i32 %.val4.i)
  %30 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %30, align 1
  %31 = getelementptr { i8, { i32 } }, ptr %30, i64 0, i32 1, i32 0
  store i32 %29, ptr %31, align 4
  ret ptr %30
}

define internal noundef ptr @"InlineFunction.#let_closure_4143"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %n_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %n_0, i64 4
  %n_0.val = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %n_0.val, ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4151", ptr %let_func_0.i, align 8
  %6 = tail call i32 @malgo_sub_int32_t(i32 %n_0.val, i32 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 %6, ptr %8, align 4
  ret ptr %7
}

define internal noundef ptr @"InlineFunction.#let_closure_4144"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %n_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %n_0, i64 4
  %n_0.val = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %n_0.val, ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4151", ptr %let_func_0.i, align 8
  %6 = tail call i32 @malgo_sub_int32_t(i32 %n_0.val, i32 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 %6, ptr %8, align 4
  ret ptr %7
}

define internal i32 @"InlineFunction.#let_closure_4146"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_sub_int32_t(i32 %x_0, i32 %1)
  ret i32 %3
}

define internal i32 @"InlineFunction.#let_closure_4147"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_le_int32_t(i32 %x_0, i32 %1)
  ret i32 %3
}

define internal i32 @"InlineFunction.#let_closure_4148"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %x_0, i32 %1)
  ret i32 %3
}

define internal noundef ptr @"InlineFunction.#let_closure_4150"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %f_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %1, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4141", ptr %let_func_0.i, align 8
  %6 = getelementptr i8, ptr %1, i64 4
  %"InlineFunction.$n_166_0.val.i" = load i32, ptr %6, align 4
  %.val.i = load i32, ptr %4, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"InlineFunction.$n_166_0.val.i", ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %7, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4135", ptr %let_func_0.i.i, align 8
  %8 = tail call i32 @malgo_le_int32_t(i32 %"InlineFunction.$n_166_0.val.i", i32 %.val.i)
  %cond.i.i = icmp eq i32 %8, 1
  %9 = tail call ptr @malgo_malloc(i64 noundef 1)
  %spec.select.i.i = zext i1 %cond.i.i to i8
  store i8 %spec.select.i.i, ptr %9, align 1
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %f_0, ptr %fun_capture_0.i, align 8
  %n_1.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %1, ptr %n_1.i, align 8
  store ptr %fun_capture_0.i, ptr %10, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"InlineFunction.#fun_closure_4142", ptr %fun_func_0.i, align 8
  %11 = load i8, ptr %9, align 1
  %switch.i = icmp eq i8 %11, 0
  %12 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %12, align 1
  %13 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { i32 } }, ptr %13, i64 0, i32 1, i32 0
  store i32 1, ptr %14, align 4
  br i1 %switch.i, label %switch_branch_Builtin.False_0.i, label %"InlineFunction.$raw_let_4127.exit"

switch_branch_Builtin.False_0.i:                  ; preds = %2
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %1, ptr %let_capture_2.i, align 8
  store ptr %let_capture_2.i, ptr %15, align 8
  %let_func_1.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4143", ptr %let_func_1.i, align 8
  %n_0.i.i = load ptr, ptr %let_capture_2.i, align 8
  %16 = getelementptr i8, ptr %n_0.i.i, i64 4
  %n_0.val.i.i = load i32, ptr %16, align 4
  %.val.i.i = load i32, ptr %14, align 4
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %n_0.val.i.i, ptr %let_capture_0.i.i.i, align 4
  store ptr %let_capture_0.i.i.i, ptr %17, align 8
  %let_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4151", ptr %let_func_0.i.i.i, align 8
  %18 = tail call i32 @malgo_sub_int32_t(i32 %n_0.val.i.i, i32 %.val.i.i)
  %19 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { i32 } }, ptr %19, i64 0, i32 1, i32 0
  store i32 %18, ptr %20, align 4
  %21 = load ptr, ptr %f_0, align 8
  %22 = getelementptr { ptr, ptr }, ptr %f_0, i64 0, i32 1
  %23 = load ptr, ptr %22, align 8
  %24 = tail call ptr %23(ptr %21, ptr nonnull %19)
  %25 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %25, align 1
  %26 = getelementptr { i8, { i32 } }, ptr %25, i64 0, i32 1, i32 0
  store i32 2, ptr %26, align 4
  %27 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_4.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %1, ptr %let_capture_4.i, align 8
  store ptr %let_capture_4.i, ptr %27, align 8
  %let_func_2.i = getelementptr { ptr, ptr }, ptr %27, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4144", ptr %let_func_2.i, align 8
  %n_0.i3.i = load ptr, ptr %let_capture_4.i, align 8
  %28 = getelementptr i8, ptr %n_0.i3.i, i64 4
  %n_0.val.i4.i = load i32, ptr %28, align 4
  %.val.i5.i = load i32, ptr %26, align 4
  %29 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i6.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %n_0.val.i4.i, ptr %let_capture_0.i.i6.i, align 4
  store ptr %let_capture_0.i.i6.i, ptr %29, align 8
  %let_func_0.i.i7.i = getelementptr { ptr, ptr }, ptr %29, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4151", ptr %let_func_0.i.i7.i, align 8
  %30 = tail call i32 @malgo_sub_int32_t(i32 %n_0.val.i4.i, i32 %.val.i5.i)
  %31 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %31, align 1
  %32 = getelementptr { i8, { i32 } }, ptr %31, i64 0, i32 1, i32 0
  store i32 %30, ptr %32, align 4
  %33 = load ptr, ptr %f_0, align 8
  %34 = load ptr, ptr %22, align 8
  %35 = tail call ptr %34(ptr %33, ptr nonnull %31)
  %36 = getelementptr i8, ptr %24, i64 4
  %.val1.i = load i32, ptr %36, align 4
  %37 = getelementptr i8, ptr %35, i64 4
  %.val2.i = load i32, ptr %37, align 4
  %38 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i8.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %.val1.i, ptr %let_capture_0.i8.i, align 4
  store ptr %let_capture_0.i8.i, ptr %38, align 8
  %let_func_0.i9.i = getelementptr { ptr, ptr }, ptr %38, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4153", ptr %let_func_0.i9.i, align 8
  %39 = tail call i32 @malgo_add_int32_t(i32 %.val1.i, i32 %.val2.i)
  %40 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %40, align 1
  %41 = getelementptr { i8, { i32 } }, ptr %40, i64 0, i32 1, i32 0
  store i32 %39, ptr %41, align 4
  br label %"InlineFunction.$raw_let_4127.exit"

"InlineFunction.$raw_let_4127.exit":              ; preds = %2, %switch_branch_Builtin.False_0.i
  %common.ret.op.i = phi ptr [ %40, %switch_branch_Builtin.False_0.i ], [ %13, %2 ]
  ret ptr %common.ret.op.i
}

define internal ptr @"InlineFunction.$fun_245"(ptr nocapture nofree readnone %0, ptr nofree %"InlineFunction.$f_165_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"InlineFunction.$f_165_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4150", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"InlineFunction.#let_closure_4151"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_sub_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal i32 @"InlineFunction.#let_closure_4153"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 4
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 5, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4139", ptr %let_func_0.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %6, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4150", ptr %let_func_0.i.i, align 8
  %7 = tail call ptr @"InlineFunction.#let_closure_4150"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i.i, ptr nofree noundef nonnull align 4 dereferenceable(1) %3)
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1
  %9 = load i32, ptr %8, align 4
  %10 = tail call ptr @malgo_int32_t_to_string(i32 %9)
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr %10, ptr %12, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %11, ptr %let_capture_2.i, align 8
  store ptr %let_capture_2.i, ptr %13, align 8
  %let_func_1.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"InlineFunction.#let_closure_4140", ptr %let_func_1.i, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @Prelude.putStrLn, ptr %putStrLn_func_0.i, align 8
  %d_0.i.i = load ptr, ptr %let_capture_2.i, align 8
  %15 = getelementptr { i8, { ptr } }, ptr %d_0.i.i, i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr @malgo_print_string(ptr %16)
  %18 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %18, align 1
  %19 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %19, align 1
  %20 = tail call ptr @malgo_newline(ptr noundef nonnull %19)
  ret i32 0
}
