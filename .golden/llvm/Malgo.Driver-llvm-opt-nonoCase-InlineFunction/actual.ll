; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/InlineFunction.ll'
source_filename = "test/testcases/malgo/InlineFunction.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str265 = unnamed_addr constant [1 x i8] zeroinitializer
@str285 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare i32 @malgo_sub_int32_t(i32, i32) local_unnamed_addr

declare i32 @malgo_le_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_271"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %x_0 = load ptr, ptr %0, align 8
  %.val = load ptr, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %.val1(ptr %.val, ptr %x_0)
  ret ptr %4
}

define internal ptr @"runtime/malgo/Prelude.mlg.|>"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Prelude.mlg.$x_699_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"runtime/malgo/Prelude.mlg.$x_699_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_271", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Prelude.mlg.putStrLn"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly %"runtime/malgo/Prelude.mlg.$str_716_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %printString_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0, align 8
  %3 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Prelude.mlg.$str_716_0", i64 0, i32 1
  %4 = load ptr, ptr %3, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"printString#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString#", ptr %"printString#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %malgo_print_string_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_string", ptr %malgo_print_string_func_0.i.i, align 8
  %7 = tail call ptr @malgo_print_string(ptr %4)
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %newline_func_0 = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.newline", ptr %newline_func_0, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %10, align 1
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %malgo_newline_func_0.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_newline", ptr %malgo_newline_func_0.i, align 8
  %12 = tail call ptr @malgo_newline(ptr noundef nonnull %10)
  ret ptr %12
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_275"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readonly %1) {
  %t_0 = load ptr, ptr %0, align 8
  %true_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %true_0 = load ptr, ptr %true_addr_0, align 8
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

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_274"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %true_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %1, ptr %let_capture_0, align 8
  %true_1 = getelementptr { ptr, ptr }, ptr %let_capture_0, i64 0, i32 1
  store ptr %true_0, ptr %true_1, align 8
  store ptr %let_capture_0, ptr %3, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_275", ptr %let_func_0, align 8
  ret ptr %3
}

define internal ptr @"runtime/malgo/Prelude.mlg.if"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Prelude.mlg.$true_817_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"runtime/malgo/Prelude.mlg.$true_817_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_274", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$p_1792_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %2, i64 0, i32 1, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1792_0", ptr %3, align 4
  ret ptr %2
}

define internal noundef ptr @"runtime/malgo/Builtin.mlg.String#"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Builtin.mlg.$p_1802_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$p_1802_0", ptr %3, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_unsafe_cast"(ptr nocapture nofree readnone %0, ptr %"runtime/malgo/Builtin.mlg.$p_1807_0") {
  %2 = tail call ptr @malgo_unsafe_cast(ptr %"runtime/malgo/Builtin.mlg.$p_1807_0")
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_291"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_add_int32_t"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$p_1808_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"runtime/malgo/Builtin.mlg.$p_1808_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_291", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_292"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_sub_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_sub_int32_t"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$p_1814_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"runtime/malgo/Builtin.mlg.$p_1814_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_292", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_311"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_le_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_le_int32_t"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$p_1930_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"runtime/malgo/Builtin.mlg.$p_1930_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_311", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$p_2155_0") {
  %2 = tail call ptr @malgo_int32_t_to_string(i32 %"runtime/malgo/Builtin.mlg.$p_2155_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_newline"(ptr nocapture nofree readnone %0, ptr %"runtime/malgo/Builtin.mlg.$p_2161_0") {
  %2 = tail call ptr @malgo_newline(ptr %"runtime/malgo/Builtin.mlg.$p_2161_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_print_string"(ptr nocapture nofree readnone %0, ptr %"runtime/malgo/Builtin.mlg.$p_2163_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"runtime/malgo/Builtin.mlg.$p_2163_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.toStringInt32#"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$x_2179_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_int32_t_to_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string", ptr %malgo_int32_t_to_string_func_0, align 8
  %3 = tail call ptr @malgo_int32_t_to_string(i32 %"runtime/malgo/Builtin.mlg.$x_2179_0")
  ret ptr %3
}

define internal noundef ptr @"runtime/malgo/Builtin.mlg.toStringInt32"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %"runtime/malgo/Builtin.mlg.$int32#_2181_0") {
"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0":
  %1 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2181_0", i64 0, i32 1
  %2 = load i32, ptr %1, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"toStringInt32#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32#", ptr %"toStringInt32#_func_0", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_int32_t_to_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string", ptr %malgo_int32_t_to_string_func_0.i, align 8
  %5 = tail call ptr @malgo_int32_t_to_string(i32 %2)
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %5, ptr %8, align 8
  ret ptr %7
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_354"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_sub_int32_t_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_sub_int32_t", ptr %malgo_sub_int32_t_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_292", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %5 = tail call i32 @malgo_sub_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %5
}

define internal ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$x_2287_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"runtime/malgo/Builtin.mlg.$x_2287_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_354", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_355"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int32#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int32#_0", i64 4
  %"int32#_0.val" = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"subInt32#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.subInt32#", ptr %"subInt32#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val", ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %6, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_354", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %malgo_sub_int32_t_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_sub_int32_t", ptr %malgo_sub_int32_t_func_0.i.i.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %8, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_292", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %9 = tail call i32 @malgo_sub_int32_t(i32 %p_0.i.i.i.i, i32 %.val)
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %"Int32#_func_0.i" = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0.i", align 8
  %11 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { i32 } }, ptr %11, i64 0, i32 1, i32 0
  store i32 %9, ptr %12, align 4
  ret ptr %11
}

define internal ptr @"runtime/malgo/Builtin.mlg.subInt32"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Builtin.mlg.$int32#_2299_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"runtime/malgo/Builtin.mlg.$int32#_2299_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_355", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.printString#"(ptr nocapture nofree readnone %0, ptr %"runtime/malgo/Builtin.mlg.$x_2399_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_print_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_string", ptr %malgo_print_string_func_0, align 8
  %3 = tail call ptr @malgo_print_string(ptr %"runtime/malgo/Builtin.mlg.$x_2399_0")
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.printString"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"runtime/malgo/Builtin.mlg.$string#_2401_0") {
"switch_branch_runtime/malgo/Builtin.mlg.String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_2401_0", i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"printString#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString#", ptr %"printString#_func_0", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_print_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_string", ptr %malgo_print_string_func_0.i, align 8
  %5 = tail call ptr @malgo_print_string(ptr %2)
  ret ptr %5
}

define internal ptr @"runtime/malgo/Builtin.mlg.newline"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %"runtime/malgo/Builtin.mlg.$__2420_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_newline_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_newline", ptr %malgo_newline_func_0, align 8
  %4 = tail call ptr @malgo_newline(ptr noundef nonnull %2)
  ret ptr %4
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_382"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_le_int32_t_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_le_int32_t", ptr %malgo_le_int32_t_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_311", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %5 = tail call i32 @malgo_le_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %5
}

define internal ptr @"runtime/malgo/Builtin.mlg.leInt32#"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$x_2727_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"runtime/malgo/Builtin.mlg.$x_2727_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_382", ptr %let_func_0, align 8
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

define internal noundef ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_389"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int32#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int32#_0", i64 4
  %"int32#_0.val" = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"leInt32#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.leInt32#", ptr %"leInt32#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val", ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %6, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_382", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %malgo_le_int32_t_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_le_int32_t", ptr %malgo_le_int32_t_func_0.i.i.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %8, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_311", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %9 = tail call i32 @malgo_le_int32_t(i32 %p_0.i.i.i.i, i32 %.val)
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %"isTrue#_func_0.i" = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0.i", align 8
  %cond.i.i = icmp eq i32 %9, 1
  %11 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  %spec.select.i.i = zext i1 %cond.i.i to i8
  store i8 %spec.select.i.i, ptr %11, align 1
  ret ptr %11
}

define internal ptr @"runtime/malgo/Builtin.mlg.leInt32"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Builtin.mlg.$int32#_2844_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"runtime/malgo/Builtin.mlg.$int32#_2844_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_389", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_456"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_add_int32_t_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_add_int32_t", ptr %malgo_add_int32_t_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_291", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %5 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %5
}

define internal ptr @"runtime/malgo/Builtin.mlg.addInt32#"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$x_4027_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"runtime/malgo/Builtin.mlg.$x_4027_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_456", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_457"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int32#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int32#_0", i64 4
  %"int32#_0.val" = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"addInt32#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addInt32#", ptr %"addInt32#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val", ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %6, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_456", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %malgo_add_int32_t_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_add_int32_t", ptr %malgo_add_int32_t_func_0.i.i.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %8, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_291", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %9 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i.i.i, i32 %.val)
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %"Int32#_func_0.i" = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0.i", align 8
  %11 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { i32 } }, ptr %11, i64 0, i32 1, i32 0
  store i32 %9, ptr %12, align 4
  ret ptr %11
}

define internal ptr @"runtime/malgo/Builtin.mlg.addInt32"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Builtin.mlg.$int32#_4039_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"runtime/malgo/Builtin.mlg.$int32#_4039_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_457", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_462"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %f_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %fix_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.fix", ptr %fix_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %f_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_462", ptr %let_func_0.i.i, align 8
  %5 = load ptr, ptr %f_0, align 8
  %6 = getelementptr { ptr, ptr }, ptr %f_0, i64 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = tail call ptr %7(ptr %5, ptr nonnull %4)
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = tail call ptr %11(ptr %9, ptr %1)
  ret ptr %12
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.fix"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/InlineFunction.mlg.$f_112_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/InlineFunction.mlg.$f_112_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_462", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_463"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %leInt32_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.leInt32", ptr %leInt32_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_389", ptr %let_func_0.i.i, align 8
  %5 = tail call noundef ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_389"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i.i, ptr nocapture nofree readonly align 4 %1)
  ret ptr %5
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.<="(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/InlineFunction.mlg.$x_128_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/InlineFunction.mlg.$x_128_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_463", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_464"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %subInt32_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.subInt32", ptr %subInt32_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_355", ptr %let_func_0.i.i, align 8
  %5 = tail call noundef ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_355"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i.i, ptr nocapture nofree readonly align 4 %1)
  ret ptr %5
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.-"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/InlineFunction.mlg.$x_140_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/InlineFunction.mlg.$x_140_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_464", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_465"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %addInt32_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addInt32", ptr %addInt32_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_457", ptr %let_func_0.i.i, align 8
  %5 = tail call noundef ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_457"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i.i, ptr nocapture nofree readonly align 4 %1)
  ret ptr %5
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.+"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/InlineFunction.mlg.$x_152_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/InlineFunction.mlg.$x_152_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_465", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_469"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) {
  %"Int32#_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %"Int32#_0", align 8
  %4 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, i32 1)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_470"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(40) %0, ptr nocapture nofree readnone %1) {
  %f_0 = load ptr, ptr %0, align 8
  %n_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %"Int32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %"Int32#_0" = load ptr, ptr %"Int32#_addr_0", align 8
  %-_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 3
  %-_0 = load ptr, ptr %-_addr_0, align 8
  %"+_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 4
  %"+_0" = load ptr, ptr %"+_addr_0", align 8
  %3 = load ptr, ptr %-_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %-_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %n_0)
  %7 = load ptr, ptr %"Int32#_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call ptr %9(ptr %7, i32 1)
  %11 = load ptr, ptr %6, align 8
  %12 = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = tail call ptr %13(ptr %11, ptr %10)
  %15 = load ptr, ptr %f_0, align 8
  %16 = getelementptr { ptr, ptr }, ptr %f_0, i64 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = tail call ptr %17(ptr %15, ptr %14)
  %19 = load ptr, ptr %"+_0", align 8
  %20 = getelementptr { ptr, ptr }, ptr %"+_0", i64 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = tail call ptr %21(ptr %19, ptr %18)
  %23 = load ptr, ptr %-_0, align 8
  %24 = load ptr, ptr %4, align 8
  %25 = tail call ptr %24(ptr %23, ptr %n_0)
  %26 = load ptr, ptr %"Int32#_0", align 8
  %27 = load ptr, ptr %8, align 8
  %28 = tail call ptr %27(ptr %26, i32 2)
  %29 = load ptr, ptr %25, align 8
  %30 = getelementptr { ptr, ptr }, ptr %25, i64 0, i32 1
  %31 = load ptr, ptr %30, align 8
  %32 = tail call ptr %31(ptr %29, ptr %28)
  %33 = load ptr, ptr %f_0, align 8
  %34 = load ptr, ptr %16, align 8
  %35 = tail call ptr %34(ptr %33, ptr %32)
  %36 = load ptr, ptr %22, align 8
  %37 = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  %38 = load ptr, ptr %37, align 8
  %39 = tail call ptr %38(ptr %36, ptr %35)
  ret ptr %39
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#inner_curry_closure_468"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(40) %0, ptr nofree %1, ptr %2) {
  %"Int32#_0" = load ptr, ptr %0, align 8
  %"<=_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"<=_0" = load ptr, ptr %"<=_addr_0", align 8
  %if_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %if_0 = load ptr, ptr %if_addr_0, align 8
  %-_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 3
  %-_0 = load ptr, ptr %-_addr_0, align 8
  %"+_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 4
  %"+_0" = load ptr, ptr %"+_addr_0", align 8
  %4 = load ptr, ptr %"<=_0", align 8
  %5 = getelementptr { ptr, ptr }, ptr %"<=_0", i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr %2)
  %8 = load ptr, ptr %"Int32#_0", align 8
  %9 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, i32 1)
  %12 = load ptr, ptr %7, align 8
  %13 = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = tail call ptr %14(ptr %12, ptr %11)
  %16 = load ptr, ptr %if_0, align 8
  %17 = getelementptr { ptr, ptr }, ptr %if_0, i64 0, i32 1
  %18 = load ptr, ptr %17, align 8
  %19 = tail call ptr %18(ptr %16, ptr %15)
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Int32#_0", ptr %fun_capture_0, align 8
  store ptr %fun_capture_0, ptr %20, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_469", ptr %fun_func_0, align 8
  %21 = load ptr, ptr %19, align 8
  %22 = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  %23 = load ptr, ptr %22, align 8
  %24 = tail call ptr %23(ptr %21, ptr nonnull %20)
  %25 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2 = tail call ptr @malgo_malloc(i64 noundef 40)
  store ptr %1, ptr %fun_capture_2, align 8
  %n_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i64 0, i32 1
  store ptr %2, ptr %n_0, align 8
  %"Int32#_2" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i64 0, i32 2
  store ptr %"Int32#_0", ptr %"Int32#_2", align 8
  %-_1 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i64 0, i32 3
  store ptr %-_0, ptr %-_1, align 8
  %"+_1" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i64 0, i32 4
  store ptr %"+_0", ptr %"+_1", align 8
  store ptr %fun_capture_2, ptr %25, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %25, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_470", ptr %fun_func_1, align 8
  %26 = load ptr, ptr %24, align 8
  %27 = getelementptr { ptr, ptr }, ptr %24, i64 0, i32 1
  %28 = load ptr, ptr %27, align 8
  %29 = tail call ptr %28(ptr %26, ptr nonnull %25)
  ret ptr %29
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_467"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(48) %0, ptr %1) {
  %f_0 = load ptr, ptr %0, align 8
  %"Int32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"Int32#_0" = load ptr, ptr %"Int32#_addr_0", align 8
  %"<=_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %"<=_0" = load ptr, ptr %"<=_addr_0", align 8
  %if_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 3
  %if_0 = load ptr, ptr %if_addr_0, align 8
  %-_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 4
  %-_0 = load ptr, ptr %-_addr_0, align 8
  %"+_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 5
  %"+_0" = load ptr, ptr %"+_addr_0", align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %inner_curry_capture_0 = tail call ptr @malgo_malloc(i64 noundef 40)
  store ptr %"Int32#_0", ptr %inner_curry_capture_0, align 8
  %"<=_1" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %inner_curry_capture_0, i64 0, i32 1
  store ptr %"<=_0", ptr %"<=_1", align 8
  %if_1 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %inner_curry_capture_0, i64 0, i32 2
  store ptr %if_0, ptr %if_1, align 8
  %-_1 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %inner_curry_capture_0, i64 0, i32 3
  store ptr %-_0, ptr %-_1, align 8
  %"+_1" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %inner_curry_capture_0, i64 0, i32 4
  store ptr %"+_0", ptr %"+_1", align 8
  store ptr %inner_curry_capture_0, ptr %3, align 8
  %inner_curry_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#inner_curry_closure_468", ptr %inner_curry_func_0, align 8
  %"Int32#_0.i" = load ptr, ptr %inner_curry_capture_0, align 8
  %"<=_0.i" = load ptr, ptr %"<=_1", align 8
  %if_0.i = load ptr, ptr %if_1, align 8
  %-_0.i = load ptr, ptr %-_1, align 8
  %"+_0.i" = load ptr, ptr %"+_1", align 8
  %4 = load ptr, ptr %"<=_0.i", align 8
  %5 = getelementptr { ptr, ptr }, ptr %"<=_0.i", i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr %1)
  %8 = load ptr, ptr %"Int32#_0.i", align 8
  %9 = getelementptr { ptr, ptr }, ptr %"Int32#_0.i", i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, i32 1)
  %12 = load ptr, ptr %7, align 8
  %13 = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = tail call ptr %14(ptr %12, ptr %11)
  %16 = load ptr, ptr %if_0.i, align 8
  %17 = getelementptr { ptr, ptr }, ptr %if_0.i, i64 0, i32 1
  %18 = load ptr, ptr %17, align 8
  %19 = tail call ptr %18(ptr %16, ptr %15)
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Int32#_0.i", ptr %fun_capture_0.i, align 8
  store ptr %fun_capture_0.i, ptr %20, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_469", ptr %fun_func_0.i, align 8
  %21 = load ptr, ptr %19, align 8
  %22 = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  %23 = load ptr, ptr %22, align 8
  %24 = tail call ptr %23(ptr %21, ptr nonnull %20)
  %25 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 40)
  store ptr %f_0, ptr %fun_capture_2.i, align 8
  %n_0.i = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 1
  store ptr %1, ptr %n_0.i, align 8
  %"Int32#_2.i" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 2
  store ptr %"Int32#_0.i", ptr %"Int32#_2.i", align 8
  %-_1.i = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 3
  store ptr %-_0.i, ptr %-_1.i, align 8
  %"+_1.i" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 4
  store ptr %"+_0.i", ptr %"+_1.i", align 8
  store ptr %fun_capture_2.i, ptr %25, align 8
  %fun_func_1.i = getelementptr { ptr, ptr }, ptr %25, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_470", ptr %fun_func_1.i, align 8
  %26 = load ptr, ptr %24, align 8
  %27 = getelementptr { ptr, ptr }, ptr %24, i64 0, i32 1
  %28 = load ptr, ptr %27, align 8
  %29 = tail call ptr %28(ptr %26, ptr nonnull %25)
  ret ptr %29
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_466"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(40) %0, ptr nofree %1) {
  %"Int32#_0" = load ptr, ptr %0, align 8
  %"<=_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"<=_0" = load ptr, ptr %"<=_addr_0", align 8
  %if_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %if_0 = load ptr, ptr %if_addr_0, align 8
  %-_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 3
  %-_0 = load ptr, ptr %-_addr_0, align 8
  %"+_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 4
  %"+_0" = load ptr, ptr %"+_addr_0", align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 48)
  store ptr %1, ptr %let_capture_0, align 8
  %"Int32#_1" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 1
  store ptr %"Int32#_0", ptr %"Int32#_1", align 8
  %"<=_1" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 2
  store ptr %"<=_0", ptr %"<=_1", align 8
  %if_1 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 3
  store ptr %if_0, ptr %if_1, align 8
  %-_1 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 4
  store ptr %-_0, ptr %-_1, align 8
  %"+_1" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 5
  store ptr %"+_0", ptr %"+_1", align 8
  store ptr %let_capture_0, ptr %3, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_467", ptr %let_func_0, align 8
  ret ptr %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_unsafe_cast_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_unsafe_cast", ptr %malgo_unsafe_cast_func_0.i, align 8
  %4 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %4, ptr @"runtime/malgo/Builtin.mlg.undefined", align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 40)
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %"Int32#_func_0.i" = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0.i", align 8
  store ptr %7, ptr %fun_capture_0.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %"<=_func_0.i" = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.<=", ptr %"<=_func_0.i", align 8
  %"<=_0.i" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %8, ptr %"<=_0.i", align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %if_func_0.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.if", ptr %if_func_0.i, align 8
  %if_0.i = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 2
  store ptr %9, ptr %if_0.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %-_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.-", ptr %-_func_0.i, align 8
  %-_0.i = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 3
  store ptr %10, ptr %-_0.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %"+_func_0.i" = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.+", ptr %"+_func_0.i", align 8
  %"+_0.i" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 4
  store ptr %11, ptr %"+_0.i", align 8
  store ptr %fun_capture_0.i, ptr %6, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_466", ptr %fun_func_0.i, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %fix_func_0.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.fix", ptr %fix_func_0.i, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %6, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %13, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_462", ptr %let_func_0.i.i, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %"Int32#_func_1.i" = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_1.i", align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i32 } }, ptr %15, i64 0, i32 1, i32 0
  store i32 5, ptr %16, align 4
  %17 = load ptr, ptr %13, align 8
  %18 = load ptr, ptr %let_func_0.i.i, align 8
  %19 = tail call ptr %18(ptr %17, ptr nonnull %15)
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %20, align 8
  %"|>_func_0.i" = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.|>", ptr %"|>_func_0.i", align 8
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %19, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %21, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %21, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_271", ptr %let_func_0.i2.i, align 8
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %22, align 8
  %toStringInt32_func_0.i = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32", ptr %toStringInt32_func_0.i, align 8
  %x_0.i.i = load ptr, ptr %let_capture_0.i1.i, align 8
  %23 = getelementptr { i8, { i32 } }, ptr %x_0.i.i, i64 0, i32 1
  %24 = load i32, ptr %23, align 4
  %25 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %25, align 8
  %"toStringInt32#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %25, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32#", ptr %"toStringInt32#_func_0.i.i", align 8
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %26, align 8
  %malgo_int32_t_to_string_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %26, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string", ptr %malgo_int32_t_to_string_func_0.i.i.i, align 8
  %27 = tail call ptr @malgo_int32_t_to_string(i32 %24)
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %28, align 8
  %"String#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %28, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0.i.i", align 8
  %29 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %29, align 1
  %30 = getelementptr { i8, { ptr } }, ptr %29, i64 0, i32 1, i32 0
  store ptr %27, ptr %30, align 8
  %31 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %31, align 8
  %"|>_func_1.i" = getelementptr { ptr, ptr }, ptr %31, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.|>", ptr %"|>_func_1.i", align 8
  %32 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %29, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %32, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %32, i64 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_271", ptr %let_func_0.i4.i, align 8
  %33 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %33, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %33, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0.i, align 8
  %x_0.i5.i = load ptr, ptr %let_capture_0.i3.i, align 8
  %34 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %34, align 8
  %printString_func_0.i.i = getelementptr { ptr, ptr }, ptr %34, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0.i.i, align 8
  %35 = getelementptr { i8, { ptr } }, ptr %x_0.i5.i, i64 0, i32 1
  %36 = load ptr, ptr %35, align 8
  %37 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %37, align 8
  %"printString#_func_0.i.i.i" = getelementptr { ptr, ptr }, ptr %37, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString#", ptr %"printString#_func_0.i.i.i", align 8
  %38 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %38, align 8
  %malgo_print_string_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %38, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_string", ptr %malgo_print_string_func_0.i.i.i.i, align 8
  %39 = tail call ptr @malgo_print_string(ptr %36)
  %40 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %40, align 1
  %41 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %41, align 8
  %newline_func_0.i.i = getelementptr { ptr, ptr }, ptr %41, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.newline", ptr %newline_func_0.i.i, align 8
  %42 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %42, align 1
  %43 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %43, align 8
  %malgo_newline_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %43, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_newline", ptr %malgo_newline_func_0.i.i.i, align 8
  %44 = tail call ptr @malgo_newline(ptr noundef nonnull %42)
  ret i32 0
}
