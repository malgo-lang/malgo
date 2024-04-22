; ModuleID = './test/tmp/malgo_test/nono/With.ll'
source_filename = "./test/testcases/malgo/With.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef
@str149 = unnamed_addr constant [1 x i8] zeroinitializer
@str169 = unnamed_addr constant [10 x i8] c"no branch\00"
@str349 = unnamed_addr constant [4 x i8] c"end\00"
@str352 = unnamed_addr constant [4 x i8] c"foo\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

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

define internal ptr @Builtin.malgo_print_string(ptr nocapture nofree readnone %0, ptr %"Builtin.$p_2163_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"Builtin.$p_2163_0")
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

define internal ptr @With.twice(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly %"With.$k_79_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = load ptr, ptr %"With.$k_79_0", align 8
  %4 = getelementptr { ptr, ptr }, ptr %"With.$k_79_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr nonnull %2)
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %7, align 1
  %8 = load ptr, ptr %"With.$k_79_0", align 8
  %9 = load ptr, ptr %4, align 8
  %10 = tail call ptr %9(ptr %8, ptr nonnull %7)
  ret ptr %10
}

define internal ptr @"With.#let_closure_346"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %str_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %printString_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Builtin.printString, ptr %printString_func_0.i, align 8
  %4 = getelementptr { i8, { ptr } }, ptr %str_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %"printString#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_0.i.i", align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %malgo_print_string_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0.i.i.i, align 8
  %8 = tail call ptr @malgo_print_string(ptr %5)
  %9 = load ptr, ptr %1, align 8
  %10 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = tail call ptr %11(ptr %9, ptr %str_0)
  ret ptr %12
}

define internal ptr @With.printAndReturn(ptr nocapture nofree readnone %0, ptr nofree %"With.$str_87_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"With.$str_87_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"With.#let_closure_346", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"With.#let_closure_347"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %finalizer_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = load ptr, ptr %1, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %3)
  %8 = load ptr, ptr %finalizer_0, align 8
  %9 = getelementptr { ptr, ptr }, ptr %finalizer_0, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, ptr %7)
  ret ptr %11
}

define internal ptr @With.finally(ptr nocapture nofree readnone %0, ptr nofree %"With.$finalizer_99_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"With.$finalizer_99_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"With.#let_closure_347", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"With.#fun_closure_348"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readnone %1) {
"switch_branch_Tuple#_0":
  %"String#_0" = load ptr, ptr %0, align 8
  %printString_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %printString_0 = load ptr, ptr %printString_addr_0, align 8
  %2 = load ptr, ptr %"String#_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %"String#_0", i64 0, i32 1
  %4 = load ptr, ptr %3, align 8
  %5 = tail call ptr %4(ptr %2, ptr nonnull @str349)
  %6 = load ptr, ptr %printString_0, align 8
  %7 = getelementptr { ptr, ptr }, ptr %printString_0, i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, ptr %5)
  ret ptr %9
}

define internal ptr @"With.#fun_closure_353"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %printString_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %printString_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %printString_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %1)
  ret ptr %6
}

define internal ptr @"With.#fun_closure_351"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr nocapture nofree readnone %1) {
  %"String#_0" = load ptr, ptr %0, align 8
  %printAndReturn_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %printAndReturn_0 = load ptr, ptr %printAndReturn_addr_0, align 8
  %printString_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %printString_0 = load ptr, ptr %printString_addr_0, align 8
  %3 = load ptr, ptr %"String#_0", align 8
  %4 = getelementptr { ptr, ptr }, ptr %"String#_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr nonnull @str352)
  %7 = load ptr, ptr %printAndReturn_0, align 8
  %8 = getelementptr { ptr, ptr }, ptr %printAndReturn_0, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call ptr %9(ptr %7, ptr %6)
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %printString_0, ptr %fun_capture_0, align 8
  store ptr %fun_capture_0, ptr %11, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"With.#fun_closure_353", ptr %fun_func_0, align 8
  %12 = load ptr, ptr %10, align 8
  %13 = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = tail call ptr %14(ptr %12, ptr nonnull %11)
  ret ptr %15
}

define internal ptr @"With.#fun_closure_350"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(32) %0, ptr nocapture nofree readnone %1) {
  %"String#_0" = load ptr, ptr %0, align 8
  %printAndReturn_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %printAndReturn_0 = load ptr, ptr %printAndReturn_addr_0, align 8
  %printString_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %printString_0 = load ptr, ptr %printString_addr_0, align 8
  %twice_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 3
  %twice_0 = load ptr, ptr %twice_addr_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0 = tail call ptr @malgo_malloc(i64 noundef 24)
  store ptr %"String#_0", ptr %fun_capture_0, align 8
  %printAndReturn_1 = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_0, i64 0, i32 1
  store ptr %printAndReturn_0, ptr %printAndReturn_1, align 8
  %printString_1 = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_0, i64 0, i32 2
  store ptr %printString_0, ptr %printString_1, align 8
  store ptr %fun_capture_0, ptr %3, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"With.#fun_closure_351", ptr %fun_func_0, align 8
  %4 = load ptr, ptr %twice_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %twice_0, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %3)
  ret ptr %7
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
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %"String#_func_0.i" = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_0.i", align 8
  store ptr %7, ptr %fun_capture_0.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %printString_func_0.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @Builtin.printString, ptr %printString_func_0.i, align 8
  %printString_0.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %8, ptr %printString_0.i, align 8
  store ptr %fun_capture_0.i, ptr %6, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"With.#fun_closure_348", ptr %fun_func_0.i, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %finally_func_0.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @With.finally, ptr %finally_func_0.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %6, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %10, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"With.#let_closure_347", ptr %let_func_0.i.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 32)
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %"String#_func_1.i" = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_1.i", align 8
  store ptr %12, ptr %fun_capture_2.i, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %printAndReturn_func_0.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @With.printAndReturn, ptr %printAndReturn_func_0.i, align 8
  %printAndReturn_0.i = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 1
  store ptr %13, ptr %printAndReturn_0.i, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %printString_func_1.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @Builtin.printString, ptr %printString_func_1.i, align 8
  %printString_1.i = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 2
  store ptr %14, ptr %printString_1.i, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %twice_func_0.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @With.twice, ptr %twice_func_0.i, align 8
  %twice_0.i = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 3
  store ptr %15, ptr %twice_0.i, align 8
  store ptr %fun_capture_2.i, ptr %11, align 8
  %fun_func_1.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"With.#fun_closure_350", ptr %fun_func_1.i, align 8
  %16 = load ptr, ptr %10, align 8
  %17 = load ptr, ptr %let_func_0.i.i, align 8
  %18 = tail call ptr %17(ptr %16, ptr nonnull %11)
  ret i32 0
}
