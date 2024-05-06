; ModuleID = '.malgo-work/test/testcases/malgo/InlineFunction.ll'
source_filename = "test/testcases/malgo/InlineFunction.mlg"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare i32 @malgo_sub_int32_t(i32, i32) local_unnamed_addr

declare i32 @malgo_le_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3412"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3413"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_le_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3414"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_sub_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal noundef ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$p_1792_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %2, i64 0, i32 1, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1792_0", ptr %3, align 4
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$p_2155_0") {
  %2 = tail call ptr @malgo_int32_t_to_string(i32 %"runtime/malgo/Builtin.mlg.$p_2155_0")
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3415"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_sub_int32_t(i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$x_2287_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"runtime/malgo/Builtin.mlg.$x_2287_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3415", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3416"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_le_int32_t(i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.leInt32#"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$x_2727_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"runtime/malgo/Builtin.mlg.$x_2727_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3416", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"runtime/malgo/Builtin.mlg.isTrue#"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$unboxed_2777_0") {
common.ret:
  %cond = icmp eq i32 %"runtime/malgo/Builtin.mlg.$unboxed_2777_0", 1
  %1 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  %spec.select = zext i1 %cond to i8
  store i8 %spec.select, ptr %1, align 1
  ret ptr %1
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3417"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.addInt32#"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$x_4027_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"runtime/malgo/Builtin.mlg.$x_4027_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3417", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Prelude.mlg.putStrLn"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"runtime/malgo/Prelude.mlg.$str_716_0") {
"switch_branch_runtime/malgo/Builtin.mlg.String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Prelude.mlg.$str_716_0", i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_print_string(ptr %2)
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_newline(ptr noundef nonnull %5)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3420"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr %1) {
  %fix_0 = load ptr, ptr %0, align 8
  %f_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %f_0 = load ptr, ptr %f_addr_0, align 8
  %3 = load ptr, ptr %fix_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %fix_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %f_0)
  %7 = load ptr, ptr %f_0, align 8
  %8 = getelementptr { ptr, ptr }, ptr %f_0, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call ptr %9(ptr %7, ptr %6)
  %11 = load ptr, ptr %10, align 8
  %12 = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = tail call ptr %13(ptr %11, ptr %1)
  ret ptr %14
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3419"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr %1) {
  %fix_0 = load ptr, ptr %0, align 8
  %f_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %f_0 = load ptr, ptr %f_addr_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %fix_0, ptr %let_capture_0, align 8
  %f_1 = getelementptr { ptr, ptr }, ptr %let_capture_0, i64 0, i32 1
  store ptr %f_0, ptr %f_1, align 8
  store ptr %let_capture_0, ptr %3, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3420", ptr %let_func_0, align 8
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

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3418"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr %1) {
  %fix_0 = load ptr, ptr %0, align 8
  %f_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %f_0 = load ptr, ptr %f_addr_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %fix_0, ptr %let_capture_0, align 8
  %f_1 = getelementptr { ptr, ptr }, ptr %let_capture_0, i64 0, i32 1
  store ptr %f_0, ptr %f_1, align 8
  store ptr %let_capture_0, ptr %3, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3419", ptr %let_func_0, align 8
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

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.fix"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/InlineFunction.mlg.$f_112_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 16)
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %fix_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.fix", ptr %fix_func_0, align 8
  store ptr %3, ptr %let_capture_0, align 8
  %f_0 = getelementptr { ptr, ptr }, ptr %let_capture_0, i64 0, i32 1
  store ptr %"test/testcases/malgo/InlineFunction.mlg.$f_112_0", ptr %f_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3418", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3429"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr nocapture nofree readonly align 4 %1) {
"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0":
  %"leInt32#_0" = load ptr, ptr %0, align 8
  %"isTrue#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"isTrue#_0" = load ptr, ptr %"isTrue#_addr_0", align 8
  %n_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %2 = getelementptr { i8, { i32 } }, ptr %n_0, i64 0, i32 1
  %3 = load i32, ptr %2, align 4
  %4 = getelementptr { i8, { i32 } }, ptr %1, i64 0, i32 1
  %5 = load i32, ptr %4, align 4
  %6 = load ptr, ptr %"leInt32#_0", align 8
  %7 = getelementptr { ptr, ptr }, ptr %"leInt32#_0", i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, i32 %3)
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call i32 %12(ptr %10, i32 %5)
  %14 = load ptr, ptr %"isTrue#_0", align 8
  %15 = getelementptr { ptr, ptr }, ptr %"isTrue#_0", i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr %16(ptr %14, i32 %13)
  ret ptr %17
}

define internal noundef ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_3430"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3432"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr nocapture nofree readonly align 4 %1) {
"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0":
  %"subInt32#_0" = load ptr, ptr %0, align 8
  %"Int32#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"Int32#_0" = load ptr, ptr %"Int32#_addr_0", align 8
  %n_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %2 = getelementptr { i8, { i32 } }, ptr %n_0, i64 0, i32 1
  %3 = load i32, ptr %2, align 4
  %4 = getelementptr { i8, { i32 } }, ptr %1, i64 0, i32 1
  %5 = load i32, ptr %4, align 4
  %6 = load ptr, ptr %"subInt32#_0", align 8
  %7 = getelementptr { ptr, ptr }, ptr %"subInt32#_0", i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, i32 %3)
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call i32 %12(ptr %10, i32 %5)
  %14 = load ptr, ptr %"Int32#_0", align 8
  %15 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr %16(ptr %14, i32 %13)
  ret ptr %17
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3433"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr nocapture nofree readonly align 4 %1) {
"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0":
  %"subInt32#_0" = load ptr, ptr %0, align 8
  %"Int32#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"Int32#_0" = load ptr, ptr %"Int32#_addr_0", align 8
  %n_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %2 = getelementptr { i8, { i32 } }, ptr %n_0, i64 0, i32 1
  %3 = load i32, ptr %2, align 4
  %4 = getelementptr { i8, { i32 } }, ptr %1, i64 0, i32 1
  %5 = load i32, ptr %4, align 4
  %6 = load ptr, ptr %"subInt32#_0", align 8
  %7 = getelementptr { ptr, ptr }, ptr %"subInt32#_0", i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, i32 %3)
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call i32 %12(ptr %10, i32 %5)
  %14 = load ptr, ptr %"Int32#_0", align 8
  %15 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr %16(ptr %14, i32 %13)
  ret ptr %17
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3434"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr nocapture nofree readonly align 4 %1) {
"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0":
  %"addInt32#_0" = load ptr, ptr %0, align 8
  %"Int32#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"Int32#_0" = load ptr, ptr %"Int32#_addr_0", align 8
  %d_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %2 = getelementptr { i8, { i32 } }, ptr %d_0, i64 0, i32 1
  %3 = load i32, ptr %2, align 4
  %4 = getelementptr { i8, { i32 } }, ptr %1, i64 0, i32 1
  %5 = load i32, ptr %4, align 4
  %6 = load ptr, ptr %"addInt32#_0", align 8
  %7 = getelementptr { ptr, ptr }, ptr %"addInt32#_0", i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, i32 %3)
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call i32 %12(ptr %10, i32 %5)
  %14 = load ptr, ptr %"Int32#_0", align 8
  %15 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr %16(ptr %14, i32 %13)
  ret ptr %17
}

define internal noundef ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_3431"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(40) %0, ptr nocapture nofree readnone %1) {
  %"addInt32#_0" = load ptr, ptr %0, align 8
  %"subInt32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"subInt32#_0" = load ptr, ptr %"subInt32#_addr_0", align 8
  %"Int32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %"Int32#_0" = load ptr, ptr %"Int32#_addr_0", align 8
  %f_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 3
  %f_0 = load ptr, ptr %f_addr_0, align 8
  %n_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 4
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 24)
  store ptr %"subInt32#_0", ptr %let_capture_0, align 8
  %"Int32#_1" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 1
  store ptr %"Int32#_0", ptr %"Int32#_1", align 8
  %n_1 = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 2
  store ptr %n_0, ptr %n_1, align 8
  store ptr %let_capture_0, ptr %5, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3432", ptr %let_func_0, align 8
  %6 = getelementptr i8, ptr %n_0, i64 4
  %n_0.val = load i32, ptr %6, align 4
  %.val = load i32, ptr %4, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %n_0.val, ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %7, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3414", ptr %let_func_0.i, align 8
  %8 = tail call i32 @malgo_sub_int32_t(i32 %n_0.val, i32 %.val)
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
  %let_capture_2 = tail call ptr @malgo_malloc(i64 noundef 24)
  store ptr %"subInt32#_0", ptr %let_capture_2, align 8
  %"Int32#_2" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_2, i64 0, i32 1
  store ptr %"Int32#_0", ptr %"Int32#_2", align 8
  %n_2 = getelementptr { ptr, ptr, ptr }, ptr %let_capture_2, i64 0, i32 2
  store ptr %n_0, ptr %n_2, align 8
  store ptr %let_capture_2, ptr %17, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3433", ptr %let_func_1, align 8
  %n_0.val1 = load i32, ptr %6, align 4
  %.val2 = load i32, ptr %16, align 4
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i5 = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %n_0.val1, ptr %let_capture_0.i5, align 4
  store ptr %let_capture_0.i5, ptr %18, align 8
  %let_func_0.i6 = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3414", ptr %let_func_0.i6, align 8
  %19 = tail call i32 @malgo_sub_int32_t(i32 %n_0.val1, i32 %.val2)
  %20 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { i32 } }, ptr %20, i64 0, i32 1, i32 0
  store i32 %19, ptr %21, align 4
  %22 = load ptr, ptr %f_0, align 8
  %23 = load ptr, ptr %12, align 8
  %24 = tail call ptr %23(ptr %22, ptr nonnull %20)
  %25 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_4 = tail call ptr @malgo_malloc(i64 noundef 24)
  store ptr %"addInt32#_0", ptr %let_capture_4, align 8
  %"Int32#_3" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_4, i64 0, i32 1
  store ptr %"Int32#_0", ptr %"Int32#_3", align 8
  %d_0 = getelementptr { ptr, ptr, ptr }, ptr %let_capture_4, i64 0, i32 2
  store ptr %14, ptr %d_0, align 8
  store ptr %let_capture_4, ptr %25, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %25, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3434", ptr %let_func_2, align 8
  %26 = getelementptr i8, ptr %14, i64 4
  %.val3 = load i32, ptr %26, align 4
  %27 = getelementptr i8, ptr %24, i64 4
  %.val4 = load i32, ptr %27, align 4
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i7 = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %.val3, ptr %let_capture_0.i7, align 4
  store ptr %let_capture_0.i7, ptr %28, align 8
  %let_func_0.i8 = getelementptr { ptr, ptr }, ptr %28, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3412", ptr %let_func_0.i8, align 8
  %29 = tail call i32 @malgo_add_int32_t(i32 %.val3, i32 %.val4)
  %30 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %30, align 1
  %31 = getelementptr { i8, { i32 } }, ptr %30, i64 0, i32 1, i32 0
  store i32 %29, ptr %31, align 4
  ret ptr %30
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3428"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(48) %0, ptr nofree %1) {
  %"addInt32#_0" = load ptr, ptr %0, align 8
  %"leInt32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"leInt32#_0" = load ptr, ptr %"leInt32#_addr_0", align 8
  %"subInt32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %"subInt32#_0" = load ptr, ptr %"subInt32#_addr_0", align 8
  %"Int32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 3
  %"Int32#_0" = load ptr, ptr %"Int32#_addr_0", align 8
  %"isTrue#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 4
  %"isTrue#_0" = load ptr, ptr %"isTrue#_addr_0", align 8
  %f_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 5
  %f_0 = load ptr, ptr %f_addr_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 24)
  store ptr %"leInt32#_0", ptr %let_capture_0, align 8
  %"isTrue#_1" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 1
  store ptr %"isTrue#_0", ptr %"isTrue#_1", align 8
  %n_0 = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 2
  store ptr %1, ptr %n_0, align 8
  store ptr %let_capture_0, ptr %5, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3429", ptr %let_func_0, align 8
  %6 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %6, align 4
  %.val1 = load i32, ptr %4, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %.val, ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %7, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3413", ptr %let_func_0.i, align 8
  %8 = tail call i32 @malgo_le_int32_t(i32 %.val, i32 %.val1)
  %cond.i = icmp eq i32 %8, 1
  %9 = tail call ptr @malgo_malloc(i64 noundef 1)
  %spec.select.i = zext i1 %cond.i to i8
  store i8 %spec.select.i, ptr %9, align 1
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0 = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0, ptr %10, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_3430", ptr %fun_func_0, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2 = tail call ptr @malgo_malloc(i64 noundef 40)
  store ptr %"addInt32#_0", ptr %fun_capture_2, align 8
  %"subInt32#_1" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i64 0, i32 1
  store ptr %"subInt32#_0", ptr %"subInt32#_1", align 8
  %"Int32#_1" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i64 0, i32 2
  store ptr %"Int32#_0", ptr %"Int32#_1", align 8
  %f_1 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i64 0, i32 3
  store ptr %f_0, ptr %f_1, align 8
  %n_1 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i64 0, i32 4
  store ptr %1, ptr %n_1, align 8
  store ptr %fun_capture_2, ptr %11, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_3431", ptr %fun_func_1, align 8
  %12 = load i8, ptr %9, align 1
  %switch = icmp eq i8 %12, 0
  %13 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %13, align 1
  br i1 %switch, label %"switch_branch_runtime/malgo/Builtin.mlg.False_0", label %"switch_branch_runtime/malgo/Builtin.mlg.True_0"

common.ret:                                       ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.True_0", %"switch_branch_runtime/malgo/Builtin.mlg.False_0"
  %common.ret.op = phi ptr [ %16, %"switch_branch_runtime/malgo/Builtin.mlg.False_0" ], [ %17, %"switch_branch_runtime/malgo/Builtin.mlg.True_0" ]
  ret ptr %common.ret.op

"switch_branch_runtime/malgo/Builtin.mlg.False_0": ; preds = %2
  %14 = load ptr, ptr %11, align 8
  %15 = load ptr, ptr %fun_func_1, align 8
  %16 = tail call ptr %15(ptr %14, ptr nonnull %13)
  br label %common.ret

"switch_branch_runtime/malgo/Builtin.mlg.True_0": ; preds = %2
  %17 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %17, align 1
  %18 = getelementptr { i8, { i32 } }, ptr %17, i64 0, i32 1, i32 0
  store i32 1, ptr %18, align 4
  br label %common.ret
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_3427"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(40) %0, ptr nofree %1) {
  %"addInt32#_0" = load ptr, ptr %0, align 8
  %"leInt32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"leInt32#_0" = load ptr, ptr %"leInt32#_addr_0", align 8
  %"subInt32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %"subInt32#_0" = load ptr, ptr %"subInt32#_addr_0", align 8
  %"Int32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 3
  %"Int32#_0" = load ptr, ptr %"Int32#_addr_0", align 8
  %"isTrue#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 4
  %"isTrue#_0" = load ptr, ptr %"isTrue#_addr_0", align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 48)
  store ptr %"addInt32#_0", ptr %let_capture_0, align 8
  %"leInt32#_1" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 1
  store ptr %"leInt32#_0", ptr %"leInt32#_1", align 8
  %"subInt32#_1" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 2
  store ptr %"subInt32#_0", ptr %"subInt32#_1", align 8
  %"Int32#_1" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 3
  store ptr %"Int32#_0", ptr %"Int32#_1", align 8
  %"isTrue#_1" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 4
  store ptr %"isTrue#_0", ptr %"isTrue#_1", align 8
  %f_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 5
  store ptr %1, ptr %f_0, align 8
  store ptr %let_capture_0, ptr %3, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3428", ptr %let_func_0, align 8
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3435"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr %1) {
  %fix_0 = load ptr, ptr %0, align 8
  %fun_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %fun_0 = load ptr, ptr %fun_addr_0, align 8
  %3 = load ptr, ptr %fix_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %fix_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %fun_0)
  %7 = load ptr, ptr %fun_0, align 8
  %8 = getelementptr { ptr, ptr }, ptr %fun_0, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call ptr %9(ptr %7, ptr %6)
  %11 = load ptr, ptr %10, align 8
  %12 = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = tail call ptr %13(ptr %11, ptr %1)
  ret ptr %14
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3436"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 40)
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %"addInt32#_func_0.i" = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addInt32#", ptr %"addInt32#_func_0.i", align 8
  store ptr %4, ptr %fun_capture_0.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"leInt32#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.leInt32#", ptr %"leInt32#_func_0.i", align 8
  %"leInt32#_0.i" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %5, ptr %"leInt32#_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %"subInt32#_func_0.i" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.subInt32#", ptr %"subInt32#_func_0.i", align 8
  %"subInt32#_0.i" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 2
  store ptr %6, ptr %"subInt32#_0.i", align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %"Int32#_func_0.i" = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0.i", align 8
  %"Int32#_0.i" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 3
  store ptr %7, ptr %"Int32#_0.i", align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %"isTrue#_func_0.i" = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0.i", align 8
  %"isTrue#_0.i" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 4
  store ptr %8, ptr %"isTrue#_0.i", align 8
  store ptr %fun_capture_0.i, ptr %3, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_3427", ptr %fun_func_0.i, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { i32 } }, ptr %9, i64 0, i32 1, i32 0
  store i32 5, ptr %10, align 4
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %fix_func_0.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.fix", ptr %fix_func_0.i, align 8
  store ptr %12, ptr %let_capture_0.i, align 8
  %fun_0.i = getelementptr { ptr, ptr }, ptr %let_capture_0.i, i64 0, i32 1
  store ptr %3, ptr %fun_0.i, align 8
  store ptr %let_capture_0.i, ptr %11, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3435", ptr %let_func_0.i, align 8
  %13 = load ptr, ptr %3, align 8
  %14 = load ptr, ptr %fun_func_0.i, align 8
  %15 = tail call ptr %14(ptr %13, ptr nonnull %11)
  %16 = load ptr, ptr %15, align 8
  %17 = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  %18 = load ptr, ptr %17, align 8
  %19 = tail call ptr %18(ptr %16, ptr nonnull %9)
  %20 = getelementptr { i8, { i32 } }, ptr %19, i64 0, i32 1
  %21 = load i32, ptr %20, align 4
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %22, align 8
  %malgo_int32_t_to_string_func_0.i = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string", ptr %malgo_int32_t_to_string_func_0.i, align 8
  %23 = tail call ptr @malgo_int32_t_to_string(i32 %21)
  %24 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %24, align 1
  %25 = getelementptr { i8, { ptr } }, ptr %24, i64 0, i32 1, i32 0
  store ptr %23, ptr %25, align 8
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %24, ptr %let_capture_2.i, align 8
  store ptr %let_capture_2.i, ptr %26, align 8
  %let_func_1.i = getelementptr { ptr, ptr }, ptr %26, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3436", ptr %let_func_1.i, align 8
  %27 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %27, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %27, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0.i, align 8
  %d_0.i.i = load ptr, ptr %let_capture_2.i, align 8
  %28 = getelementptr { i8, { ptr } }, ptr %d_0.i.i, i64 0, i32 1
  %29 = load ptr, ptr %28, align 8
  %30 = tail call ptr @malgo_print_string(ptr %29)
  %31 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %31, align 1
  %32 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %32, align 1
  %33 = tail call ptr @malgo_newline(ptr noundef nonnull %32)
  ret i32 0
}
