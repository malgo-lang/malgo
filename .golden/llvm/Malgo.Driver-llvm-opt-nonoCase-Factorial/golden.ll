; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/Factorial.ll'
source_filename = "test/testcases/malgo/Factorial.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef
@str222 = unnamed_addr constant [1 x i8] zeroinitializer
@str242 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i64 @malgo_sub_int64_t(i64, i64) local_unnamed_addr

declare i64 @malgo_mul_int64_t(i64, i64) local_unnamed_addr

declare i32 @malgo_eq_int64_t(i64, i64) local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"Factorial.#let_closure_228"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %x_0 = load ptr, ptr %0, align 8
  %.val = load ptr, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %.val1(ptr %.val, ptr %x_0)
  ret ptr %4
}

define internal ptr @"Prelude.|>"(ptr nocapture nofree readnone %0, ptr nofree %"Prelude.$x_699_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Prelude.$x_699_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Factorial.#let_closure_228", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Prelude.putStrLn(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly %"Prelude.$str_716_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %printString_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @Builtin.printString, ptr %printString_func_0, align 8
  %3 = getelementptr { i8, { ptr } }, ptr %"Prelude.$str_716_0", i64 0, i32 1
  %4 = load ptr, ptr %3, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"printString#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %malgo_print_string_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0.i.i, align 8
  %7 = tail call ptr @malgo_print_string(ptr %4)
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %newline_func_0 = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @Builtin.newline, ptr %newline_func_0, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %10, align 1
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %malgo_newline_func_0.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @Builtin.malgo_newline, ptr %malgo_newline_func_0.i, align 8
  %12 = tail call ptr @malgo_newline(ptr noundef nonnull %10)
  ret ptr %12
}

define internal ptr @"Factorial.#let_closure_232"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readonly %1) {
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

define internal ptr @"Factorial.#let_closure_231"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %true_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %true_0, ptr %let_capture_0, align 8
  %t_0 = getelementptr { ptr, ptr }, ptr %let_capture_0, i64 0, i32 1
  store ptr %1, ptr %t_0, align 8
  store ptr %let_capture_0, ptr %3, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Factorial.#let_closure_232", ptr %let_func_0, align 8
  ret ptr %3
}

define internal ptr @Prelude.if(ptr nocapture nofree readnone %0, ptr nofree %"Prelude.$true_817_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Prelude.$true_817_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Factorial.#let_closure_231", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"Builtin.Int64#"(ptr nocapture nofree readnone %0, i64 %"Builtin.$p_1794_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i64 } }, ptr %2, i64 0, i32 1, i32 0
  store i64 %"Builtin.$p_1794_0", ptr %3, align 4
  ret ptr %2
}

define internal noundef ptr @"Builtin.String#"(ptr nocapture nofree readnone %0, ptr nofree %"Builtin.$p_1802_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"Builtin.$p_1802_0", ptr %3, align 8
  ret ptr %2
}

define internal ptr @Builtin.malgo_unsafe_cast(ptr nocapture nofree readnone %0, ptr %"Builtin.$p_1807_0") {
  %2 = tail call ptr @malgo_unsafe_cast(ptr %"Builtin.$p_1807_0")
  ret ptr %2
}

define internal i64 @"Factorial.#let_closure_253"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_sub_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @Builtin.malgo_sub_int64_t(ptr nocapture nofree readnone %0, i64 %"Builtin.$p_1838_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"Builtin.$p_1838_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Factorial.#let_closure_253", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"Factorial.#let_closure_254"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_mul_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @Builtin.malgo_mul_int64_t(ptr nocapture nofree readnone %0, i64 %"Builtin.$p_1844_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"Builtin.$p_1844_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Factorial.#let_closure_254", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Factorial.#let_closure_270"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call i32 @malgo_eq_int64_t(i64 %p_0, i64 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_eq_int64_t(ptr nocapture nofree readnone %0, i64 %"Builtin.$p_1942_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"Builtin.$p_1942_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Factorial.#let_closure_270", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Builtin.malgo_int64_t_to_string(ptr nocapture nofree readnone %0, i64 %"Builtin.$p_2156_0") {
  %2 = tail call ptr @malgo_int64_t_to_string(i64 %"Builtin.$p_2156_0")
  ret ptr %2
}

define internal ptr @Builtin.malgo_newline(ptr nocapture nofree readnone %0, ptr %"Builtin.$p_2161_0") {
  %2 = tail call ptr @malgo_newline(ptr %"Builtin.$p_2161_0")
  ret ptr %2
}

define internal ptr @Builtin.malgo_print_string(ptr nocapture nofree readnone %0, ptr %"Builtin.$p_2163_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"Builtin.$p_2163_0")
  ret ptr %2
}

define internal ptr @"Builtin.toStringInt64#"(ptr nocapture nofree readnone %0, i64 %"Builtin.$x_2172_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_int64_t_to_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @Builtin.malgo_int64_t_to_string, ptr %malgo_int64_t_to_string_func_0, align 8
  %3 = tail call ptr @malgo_int64_t_to_string(i64 %"Builtin.$x_2172_0")
  ret ptr %3
}

define internal noundef ptr @Builtin.toStringInt64(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %"Builtin.$int64#_2174_0") {
"switch_branch_Builtin.Int64#_0":
  %1 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_2174_0", i64 0, i32 1
  %2 = load i64, ptr %1, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"toStringInt64#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Builtin.toStringInt64#", ptr %"toStringInt64#_func_0", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_int64_t_to_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @Builtin.malgo_int64_t_to_string, ptr %malgo_int64_t_to_string_func_0.i, align 8
  %5 = tail call ptr @malgo_int64_t_to_string(i64 %2)
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_0", align 8
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %5, ptr %8, align 8
  ret ptr %7
}

define internal i64 @"Factorial.#let_closure_309"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_sub_int64_t_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Builtin.malgo_sub_int64_t, ptr %malgo_sub_int64_t_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"Factorial.#let_closure_253", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %5 = tail call i64 @malgo_sub_int64_t(i64 %p_0.i.i, i64 %1)
  ret i64 %5
}

define internal ptr @"Builtin.subInt64#"(ptr nocapture nofree readnone %0, i64 %"Builtin.$x_2255_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"Builtin.$x_2255_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Factorial.#let_closure_309", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"Factorial.#let_closure_310"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int64#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int64#_0", i64 4
  %"int64#_0.val" = load i64, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i64, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"subInt64#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Builtin.subInt64#", ptr %"subInt64#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"int64#_0.val", ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %6, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Factorial.#let_closure_309", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %malgo_sub_int64_t_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @Builtin.malgo_sub_int64_t, ptr %malgo_sub_int64_t_func_0.i.i.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %8, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"Factorial.#let_closure_253", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i, align 4
  %9 = tail call i64 @malgo_sub_int64_t(i64 %p_0.i.i.i.i, i64 %.val)
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %"Int64#_func_0.i" = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"Builtin.Int64#", ptr %"Int64#_func_0.i", align 8
  %11 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { i64 } }, ptr %11, i64 0, i32 1, i32 0
  store i64 %9, ptr %12, align 4
  ret ptr %11
}

define internal ptr @Builtin.subInt64(ptr nocapture nofree readnone %0, ptr nofree %"Builtin.$int64#_2267_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Builtin.$int64#_2267_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Factorial.#let_closure_310", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.printString#"(ptr nocapture nofree readnone %0, ptr %"Builtin.$x_2399_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_print_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0, align 8
  %3 = tail call ptr @malgo_print_string(ptr %"Builtin.$x_2399_0")
  ret ptr %3
}

define internal ptr @Builtin.printString(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"Builtin.$string#_2401_0") {
"switch_branch_Builtin.String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_2401_0", i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"printString#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_0", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_print_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0.i, align 8
  %5 = tail call ptr @malgo_print_string(ptr %2)
  ret ptr %5
}

define internal ptr @Builtin.newline(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %"Builtin.$__2420_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_newline_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Builtin.malgo_newline, ptr %malgo_newline_func_0, align 8
  %4 = tail call ptr @malgo_newline(ptr noundef nonnull %2)
  ret ptr %4
}

define internal i64 @"Factorial.#let_closure_323"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_mul_int64_t_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Builtin.malgo_mul_int64_t, ptr %malgo_mul_int64_t_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"Factorial.#let_closure_254", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %5 = tail call i64 @malgo_mul_int64_t(i64 %p_0.i.i, i64 %1)
  ret i64 %5
}

define internal ptr @"Builtin.mulInt64#"(ptr nocapture nofree readnone %0, i64 %"Builtin.$x_2496_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"Builtin.$x_2496_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Factorial.#let_closure_323", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"Factorial.#let_closure_324"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int64#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int64#_0", i64 4
  %"int64#_0.val" = load i64, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i64, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"mulInt64#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Builtin.mulInt64#", ptr %"mulInt64#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"int64#_0.val", ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %6, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Factorial.#let_closure_323", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %malgo_mul_int64_t_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @Builtin.malgo_mul_int64_t, ptr %malgo_mul_int64_t_func_0.i.i.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %8, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"Factorial.#let_closure_254", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i, align 4
  %9 = tail call i64 @malgo_mul_int64_t(i64 %p_0.i.i.i.i, i64 %.val)
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %"Int64#_func_0.i" = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"Builtin.Int64#", ptr %"Int64#_func_0.i", align 8
  %11 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { i64 } }, ptr %11, i64 0, i32 1, i32 0
  store i64 %9, ptr %12, align 4
  ret ptr %11
}

define internal ptr @Builtin.mulInt64(ptr nocapture nofree readnone %0, ptr nofree %"Builtin.$int64#_2508_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Builtin.$int64#_2508_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Factorial.#let_closure_324", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"Builtin.isTrue#"(ptr nocapture nofree readnone %0, i32 %"Builtin.$unboxed_2777_0") {
common.ret:
  %cond = icmp eq i32 %"Builtin.$unboxed_2777_0", 1
  %1 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  %spec.select = zext i1 %cond to i8
  store i8 %spec.select, ptr %1, align 1
  ret ptr %1
}

define internal i32 @"Factorial.#let_closure_387"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_eq_int64_t_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Builtin.malgo_eq_int64_t, ptr %malgo_eq_int64_t_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"Factorial.#let_closure_270", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %5 = tail call i32 @malgo_eq_int64_t(i64 %p_0.i.i, i64 %1)
  ret i32 %5
}

define internal ptr @"Builtin.eqInt64#"(ptr nocapture nofree readnone %0, i64 %"Builtin.$x_3607_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"Builtin.$x_3607_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Factorial.#let_closure_387", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"Factorial.#let_closure_388"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int64#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int64#_0", i64 4
  %"int64#_0.val" = load i64, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i64, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"eqInt64#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Builtin.eqInt64#", ptr %"eqInt64#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"int64#_0.val", ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %6, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Factorial.#let_closure_387", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %malgo_eq_int64_t_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @Builtin.malgo_eq_int64_t, ptr %malgo_eq_int64_t_func_0.i.i.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %8, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"Factorial.#let_closure_270", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i, align 4
  %9 = tail call i32 @malgo_eq_int64_t(i64 %p_0.i.i.i.i, i64 %.val)
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %"isTrue#_func_0.i" = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"Builtin.isTrue#", ptr %"isTrue#_func_0.i", align 8
  %cond.i.i = icmp eq i32 %9, 1
  %11 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  %spec.select.i.i = zext i1 %cond.i.i to i8
  store i8 %spec.select.i.i, ptr %11, align 1
  ret ptr %11
}

define internal ptr @Builtin.eqInt64(ptr nocapture nofree readnone %0, ptr nofree %"Builtin.$int64#_3619_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Builtin.$int64#_3619_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Factorial.#let_closure_388", ptr %let_func_0, align 8
  ret ptr %2
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define internal ptr @"Factorial.#fun_closure_419"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) #0 {
  %acc_0 = load ptr, ptr %0, align 8
  ret ptr %acc_0
}

define internal ptr @"Factorial.#fun_closure_420"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(48) %0, ptr nocapture nofree readnone %1) {
  %acc_0 = load ptr, ptr %0, align 8
  %"Int64#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"Int64#_0" = load ptr, ptr %"Int64#_addr_0", align 8
  %-_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %-_0 = load ptr, ptr %-_addr_0, align 8
  %factAcc_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 3
  %factAcc_0 = load ptr, ptr %factAcc_addr_0, align 8
  %n_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 4
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %"*_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 5
  %"*_0" = load ptr, ptr %"*_addr_0", align 8
  %3 = load ptr, ptr %-_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %-_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %n_0)
  %7 = load ptr, ptr %"Int64#_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %"Int64#_0", i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call ptr %9(ptr %7, i64 1)
  %11 = load ptr, ptr %6, align 8
  %12 = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = tail call ptr %13(ptr %11, ptr %10)
  %15 = load ptr, ptr %factAcc_0, align 8
  %16 = getelementptr { ptr, ptr }, ptr %factAcc_0, i64 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = tail call ptr %17(ptr %15, ptr %14)
  %19 = load ptr, ptr %"*_0", align 8
  %20 = getelementptr { ptr, ptr }, ptr %"*_0", i64 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = tail call ptr %21(ptr %19, ptr %n_0)
  %23 = load ptr, ptr %22, align 8
  %24 = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  %25 = load ptr, ptr %24, align 8
  %26 = tail call ptr %25(ptr %23, ptr %acc_0)
  %27 = load ptr, ptr %18, align 8
  %28 = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  %29 = load ptr, ptr %28, align 8
  %30 = tail call ptr %29(ptr %27, ptr %26)
  ret ptr %30
}

define internal noundef ptr @"Factorial.#let_closure_421"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %eqInt64_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Builtin.eqInt64, ptr %eqInt64_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"Factorial.#let_closure_388", ptr %let_func_0.i.i, align 8
  %5 = tail call noundef ptr @"Factorial.#let_closure_388"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i.i, ptr nocapture nofree readonly align 4 %1)
  ret ptr %5
}

define internal ptr @"Factorial.=="(ptr nocapture nofree readnone %0, ptr nofree %"Factorial.$x_98_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Factorial.$x_98_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Factorial.#let_closure_421", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"Factorial.#let_closure_422"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %subInt64_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Builtin.subInt64, ptr %subInt64_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"Factorial.#let_closure_310", ptr %let_func_0.i.i, align 8
  %5 = tail call noundef ptr @"Factorial.#let_closure_310"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i.i, ptr nocapture nofree readonly align 4 %1)
  ret ptr %5
}

define internal ptr @Factorial.-(ptr nocapture nofree readnone %0, ptr nofree %"Factorial.$x_110_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Factorial.$x_110_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Factorial.#let_closure_422", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"Factorial.#let_closure_423"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %mulInt64_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Builtin.mulInt64, ptr %mulInt64_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"Factorial.#let_closure_324", ptr %let_func_0.i.i, align 8
  %5 = tail call noundef ptr @"Factorial.#let_closure_324"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i.i, ptr nocapture nofree readonly align 4 %1)
  ret ptr %5
}

define internal ptr @"Factorial.*"(ptr nocapture nofree readnone %0, ptr nofree %"Factorial.$x_122_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Factorial.$x_122_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Factorial.#let_closure_423", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Factorial.#let_closure_424"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %n_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"==_func_0.i" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Factorial.==", ptr %"==_func_0.i", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %n_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"Factorial.#let_closure_421", ptr %let_func_0.i.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"Int64#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Builtin.Int64#", ptr %"Int64#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { i64 } }, ptr %6, i64 0, i32 1, i32 0
  store i64 0, ptr %7, align 4
  %8 = load ptr, ptr %4, align 8
  %9 = load ptr, ptr %let_func_0.i.i, align 8
  %10 = tail call ptr %9(ptr %8, ptr nonnull %6)
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %if_func_0.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @Prelude.if, ptr %if_func_0.i, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %10, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %12, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"Factorial.#let_closure_231", ptr %let_func_0.i2.i, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %1, ptr %fun_capture_0.i, align 8
  store ptr %fun_capture_0.i, ptr %13, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"Factorial.#fun_closure_419", ptr %fun_func_0.i, align 8
  %14 = load ptr, ptr %12, align 8
  %15 = load ptr, ptr %let_func_0.i2.i, align 8
  %16 = tail call ptr %15(ptr %14, ptr nonnull %13)
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 48)
  store ptr %1, ptr %fun_capture_2.i, align 8
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %18, align 8
  %"Int64#_func_1.i" = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"Builtin.Int64#", ptr %"Int64#_func_1.i", align 8
  %"Int64#_0.i" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 1
  store ptr %18, ptr %"Int64#_0.i", align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %19, align 8
  %-_func_0.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @Factorial.-, ptr %-_func_0.i, align 8
  %-_0.i = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 2
  store ptr %19, ptr %-_0.i, align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %20, align 8
  %factAcc_func_0.i = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @Factorial.factAcc, ptr %factAcc_func_0.i, align 8
  %factAcc_0.i = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 3
  store ptr %20, ptr %factAcc_0.i, align 8
  %n_0.i = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 4
  store ptr %n_0, ptr %n_0.i, align 8
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %21, align 8
  %"*_func_0.i" = getelementptr { ptr, ptr }, ptr %21, i64 0, i32 1
  store ptr @"Factorial.*", ptr %"*_func_0.i", align 8
  %"*_0.i" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 5
  store ptr %21, ptr %"*_0.i", align 8
  store ptr %fun_capture_2.i, ptr %17, align 8
  %fun_func_1.i = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"Factorial.#fun_closure_420", ptr %fun_func_1.i, align 8
  %22 = load ptr, ptr %16, align 8
  %23 = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  %24 = load ptr, ptr %23, align 8
  %25 = tail call ptr %24(ptr %22, ptr nonnull %17)
  ret ptr %25
}

define internal ptr @Factorial.factAcc(ptr nocapture nofree readnone %0, ptr nofree %"Factorial.$n_134_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Factorial.$n_134_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Factorial.#let_closure_424", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Factorial.fact(ptr nocapture nofree readnone %0, ptr nofree %"Factorial.$n_200_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %factAcc_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @Factorial.factAcc, ptr %factAcc_func_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Factorial.$n_200_0", ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %3, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Factorial.#let_closure_424", ptr %let_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %"Int64#_func_0" = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"Builtin.Int64#", ptr %"Int64#_func_0", align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { i64 } }, ptr %5, i64 0, i32 1, i32 0
  store i64 1, ptr %6, align 4
  %7 = load ptr, ptr %3, align 8
  %8 = load ptr, ptr %let_func_0.i, align 8
  %9 = tail call ptr %8(ptr %7, ptr nonnull %5)
  ret ptr %9
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_unsafe_cast_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Builtin.malgo_unsafe_cast, ptr %malgo_unsafe_cast_func_0.i, align 8
  %4 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %4, ptr @Builtin.undefined, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %"Int64#_func_0.i" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Builtin.Int64#", ptr %"Int64#_func_0.i", align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i64 } }, ptr %7, i64 0, i32 1, i32 0
  store i64 5, ptr %8, align 4
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %fact_func_0.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @Factorial.fact, ptr %fact_func_0.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %factAcc_func_0.i.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @Factorial.factAcc, ptr %factAcc_func_0.i.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %7, ptr %let_capture_0.i.i.i, align 8
  store ptr %let_capture_0.i.i.i, ptr %11, align 8
  %let_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"Factorial.#let_closure_424", ptr %let_func_0.i.i.i, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %"Int64#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"Builtin.Int64#", ptr %"Int64#_func_0.i.i", align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { i64 } }, ptr %13, i64 0, i32 1, i32 0
  store i64 1, ptr %14, align 4
  %15 = load ptr, ptr %11, align 8
  %16 = load ptr, ptr %let_func_0.i.i.i, align 8
  %17 = tail call ptr %16(ptr %15, ptr nonnull %13)
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %18, align 8
  %"|>_func_0.i" = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"Prelude.|>", ptr %"|>_func_0.i", align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %17, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %19, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"Factorial.#let_closure_228", ptr %let_func_0.i.i, align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %20, align 8
  %toStringInt64_func_0.i = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @Builtin.toStringInt64, ptr %toStringInt64_func_0.i, align 8
  %x_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %21 = getelementptr { i8, { i64 } }, ptr %x_0.i.i, i64 0, i32 1
  %22 = load i64, ptr %21, align 4
  %23 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %23, align 8
  %"toStringInt64#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %23, i64 0, i32 1
  store ptr @"Builtin.toStringInt64#", ptr %"toStringInt64#_func_0.i.i", align 8
  %24 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %24, align 8
  %malgo_int64_t_to_string_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %24, i64 0, i32 1
  store ptr @Builtin.malgo_int64_t_to_string, ptr %malgo_int64_t_to_string_func_0.i.i.i, align 8
  %25 = tail call ptr @malgo_int64_t_to_string(i64 %22)
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %26, align 8
  %"String#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %26, i64 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_0.i.i", align 8
  %27 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %27, i64 0, i32 1, i32 0
  store ptr %25, ptr %28, align 8
  %29 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %29, align 8
  %"|>_func_1.i" = getelementptr { ptr, ptr }, ptr %29, i64 0, i32 1
  store ptr @"Prelude.|>", ptr %"|>_func_1.i", align 8
  %30 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %27, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %30, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %30, i64 0, i32 1
  store ptr @"Factorial.#let_closure_228", ptr %let_func_0.i2.i, align 8
  %31 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %31, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %31, i64 0, i32 1
  store ptr @Prelude.putStrLn, ptr %putStrLn_func_0.i, align 8
  %x_0.i3.i = load ptr, ptr %let_capture_0.i1.i, align 8
  %32 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %32, align 8
  %printString_func_0.i.i = getelementptr { ptr, ptr }, ptr %32, i64 0, i32 1
  store ptr @Builtin.printString, ptr %printString_func_0.i.i, align 8
  %33 = getelementptr { i8, { ptr } }, ptr %x_0.i3.i, i64 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %35, align 8
  %"printString#_func_0.i.i.i" = getelementptr { ptr, ptr }, ptr %35, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_0.i.i.i", align 8
  %36 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %36, align 8
  %malgo_print_string_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %36, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0.i.i.i.i, align 8
  %37 = tail call ptr @malgo_print_string(ptr %34)
  %38 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %38, align 1
  %39 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %39, align 8
  %newline_func_0.i.i = getelementptr { ptr, ptr }, ptr %39, i64 0, i32 1
  store ptr @Builtin.newline, ptr %newline_func_0.i.i, align 8
  %40 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %40, align 1
  %41 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %41, align 8
  %malgo_newline_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %41, i64 0, i32 1
  store ptr @Builtin.malgo_newline, ptr %malgo_newline_func_0.i.i.i, align 8
  %42 = tail call ptr @malgo_newline(ptr noundef nonnull %40)
  ret i32 0
}

attributes #0 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn }
