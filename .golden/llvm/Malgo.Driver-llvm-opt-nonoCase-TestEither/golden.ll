; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/TestEither.ll'
source_filename = "test/testcases/malgo/TestEither.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str382 = unnamed_addr constant [1 x i8] zeroinitializer
@str402 = unnamed_addr constant [10 x i8] c"no branch\00"
@str581 = unnamed_addr constant [12 x i8] c"unreachable\00"
@str585 = unnamed_addr constant [6 x i8] c"error\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_388"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
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
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_388", ptr %let_func_0, align 8
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

define internal ptr @"runtime/malgo/Prelude.mlg.putStr"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly %"runtime/malgo/Prelude.mlg.$str_723_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %printString_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0, align 8
  %3 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Prelude.mlg.$str_723_0", i64 0, i32 1
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
  ret ptr %7
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

define internal i32 @"test/testcases/malgo/TestEither.mlg.#let_closure_408"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
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
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_408", ptr %let_func_0, align 8
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

define internal i32 @"test/testcases/malgo/TestEither.mlg.#let_closure_573"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
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
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_408", ptr %let_func_0.i.i, align 8
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
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_573", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_574"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
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
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_573", ptr %let_func_0.i.i, align 8
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
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_408", ptr %let_func_0.i.i.i.i, align 8
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
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_574", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"test/testcases/malgo/TestEither.mlg.Left"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/TestEither.mlg.$p_249_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/TestEither.mlg.$p_249_0", ptr %3, align 8
  ret ptr %2
}

define internal noundef ptr @"test/testcases/malgo/TestEither.mlg.Right"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/TestEither.mlg.$p_251_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/TestEither.mlg.$p_251_0", ptr %3, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_579"(ptr nocapture nofree readonly %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 8
  %switch.i = icmp eq i8 %3, 0
  %4 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  br i1 %switch.i, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0.i", label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0.i"

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0.i": ; preds = %2
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %Left_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.Left", ptr %Left_func_0.i, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %5, ptr %8, align 8
  br label %"test/testcases/malgo/TestEither.mlg.$andThen_curry_260.exit"

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0.i": ; preds = %2
  %__0 = load ptr, ptr %0, align 8
  %9 = load ptr, ptr %__0, align 8
  %10 = getelementptr { ptr, ptr }, ptr %__0, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = tail call ptr %11(ptr %9, ptr %5)
  br label %"test/testcases/malgo/TestEither.mlg.$andThen_curry_260.exit"

"test/testcases/malgo/TestEither.mlg.$andThen_curry_260.exit": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0.i", %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0.i"
  %common.ret.op.i = phi ptr [ %7, %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0.i" ], [ %12, %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0.i" ]
  ret ptr %common.ret.op.i
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.andThen"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/TestEither.mlg.$__254_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/TestEither.mlg.$__254_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_579", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_580"(ptr nocapture nofree readonly align 8 %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch = icmp eq i8 %3, 0
  br i1 %switch, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0", label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"

common.ret:                                       ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0", %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %putStr_0.sink4 = phi ptr [ %putStr_0, %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0" ], [ %putStrLn_0, %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0" ]
  %.sink1 = phi ptr [ %17, %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0" ], [ %11, %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0" ]
  %4 = load ptr, ptr %putStr_0.sink4, align 8
  %5 = getelementptr { ptr, ptr }, ptr %putStr_0.sink4, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr %.sink1)
  ret ptr %7

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %2
  %"String#_addr_0" = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 3
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %putStrLn_0 = load ptr, ptr %0, align 8
  %8 = load ptr, ptr %"String#_0", align 8
  %9 = getelementptr { ptr, ptr }, ptr %"String#_0", i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, ptr nonnull @str581)
  br label %common.ret

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %2
  %toStringInt32_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %toStringInt32_0 = load ptr, ptr %toStringInt32_addr_0, align 8
  %putStr_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %putStr_0 = load ptr, ptr %putStr_addr_0, align 8
  %12 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = load ptr, ptr %toStringInt32_0, align 8
  %15 = getelementptr { ptr, ptr }, ptr %toStringInt32_0, i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr %16(ptr %14, ptr %13)
  br label %common.ret
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_582"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr %1) {
  %Right_0 = load ptr, ptr %0, align 8
  %"Int32#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"Int32#_0" = load ptr, ptr %"Int32#_addr_0", align 8
  %addInt32_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %addInt32_0 = load ptr, ptr %addInt32_addr_0, align 8
  %3 = load ptr, ptr %addInt32_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %addInt32_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %1)
  %7 = load ptr, ptr %"Int32#_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call ptr %9(ptr %7, i32 1)
  %11 = load ptr, ptr %6, align 8
  %12 = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = tail call ptr %13(ptr %11, ptr %10)
  %15 = load ptr, ptr %Right_0, align 8
  %16 = getelementptr { ptr, ptr }, ptr %Right_0, i64 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = tail call ptr %17(ptr %15, ptr %14)
  ret ptr %18
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_583"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(32) %0, ptr %1) {
  %Right_0 = load ptr, ptr %0, align 8
  %"|>_addr_0" = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"|>_0" = load ptr, ptr %"|>_addr_0", align 8
  %putStr_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %putStr_0 = load ptr, ptr %putStr_addr_0, align 8
  %toStringInt32_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 3
  %toStringInt32_0 = load ptr, ptr %toStringInt32_addr_0, align 8
  %3 = load ptr, ptr %"|>_0", align 8
  %4 = getelementptr { ptr, ptr }, ptr %"|>_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %1)
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call ptr %9(ptr %7, ptr %toStringInt32_0)
  %11 = load ptr, ptr %"|>_0", align 8
  %12 = load ptr, ptr %4, align 8
  %13 = tail call ptr %12(ptr %11, ptr %10)
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr %16(ptr %14, ptr %putStr_0)
  %18 = load ptr, ptr %"|>_0", align 8
  %19 = load ptr, ptr %4, align 8
  %20 = tail call ptr %19(ptr %18, ptr %17)
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  %23 = load ptr, ptr %22, align 8
  %24 = tail call ptr %23(ptr %21, ptr %Right_0)
  ret ptr %24
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_584"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readnone %1) {
  %Left_0 = load ptr, ptr %0, align 8
  %"String#_addr_0" = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %3 = load ptr, ptr %"String#_0", align 8
  %4 = getelementptr { ptr, ptr }, ptr %"String#_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr nonnull @str585)
  %7 = load ptr, ptr %Left_0, align 8
  %8 = getelementptr { ptr, ptr }, ptr %Left_0, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call ptr %9(ptr %7, ptr %6)
  ret ptr %10
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_586"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr %1) {
  %Right_0 = load ptr, ptr %0, align 8
  %"Int32#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"Int32#_0" = load ptr, ptr %"Int32#_addr_0", align 8
  %addInt32_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %addInt32_0 = load ptr, ptr %addInt32_addr_0, align 8
  %3 = load ptr, ptr %addInt32_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %addInt32_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %1)
  %7 = load ptr, ptr %"Int32#_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call ptr %9(ptr %7, i32 1)
  %11 = load ptr, ptr %6, align 8
  %12 = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = tail call ptr %13(ptr %11, ptr %10)
  %15 = load ptr, ptr %Right_0, align 8
  %16 = getelementptr { ptr, ptr }, ptr %Right_0, i64 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = tail call ptr %17(ptr %15, ptr %14)
  ret ptr %18
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_587"(ptr nocapture nofree readonly align 8 %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %1) {
  %putStrLn_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %putStrLn_0 = load ptr, ptr %putStrLn_addr_0, align 8
  %3 = load i8, ptr %1, align 8
  %switch = icmp eq i8 %3, 0
  br i1 %switch, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0", label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"

common.ret:                                       ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0", %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %.sink4 = phi ptr [ %22, %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0" ], [ %putStrLn_0, %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0" ]
  %putStrLn_0.sink = phi ptr [ %putStrLn_0, %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0" ], [ %9, %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0" ]
  %4 = load ptr, ptr %.sink4, align 8
  %5 = getelementptr { ptr, ptr }, ptr %.sink4, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr %putStrLn_0.sink)
  ret ptr %7

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %2
  %8 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  br label %common.ret

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %2
  %toStringInt32_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %toStringInt32_0 = load ptr, ptr %toStringInt32_addr_0, align 8
  %"|>_0" = load ptr, ptr %0, align 8
  %10 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = load ptr, ptr %"|>_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %"|>_0", i64 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = tail call ptr %14(ptr %12, ptr %11)
  %16 = load ptr, ptr %15, align 8
  %17 = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  %18 = load ptr, ptr %17, align 8
  %19 = tail call ptr %18(ptr %16, ptr %toStringInt32_0)
  %20 = load ptr, ptr %"|>_0", align 8
  %21 = load ptr, ptr %13, align 8
  %22 = tail call ptr %21(ptr %20, ptr %19)
  br label %common.ret
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
  store ptr null, ptr %6, align 8
  %"Int32#_func_0.i" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0.i", align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 1, ptr %8, align 4
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %Right_func_0.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.Right", ptr %Right_func_0.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %10, align 1
  %11 = getelementptr { i8, { ptr } }, ptr %10, i64 0, i32 1, i32 0
  store ptr %7, ptr %11, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %"|>_func_0.i" = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.|>", ptr %"|>_func_0.i", align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %10, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %13, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_388", ptr %let_func_0.i.i, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 32)
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0.i, align 8
  store ptr %15, ptr %fun_capture_0.i, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %16, align 8
  %putStr_func_0.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStr", ptr %putStr_func_0.i, align 8
  %putStr_0.i = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %16, ptr %putStr_0.i, align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %17, align 8
  %toStringInt32_func_0.i = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32", ptr %toStringInt32_func_0.i, align 8
  %toStringInt32_0.i = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 2
  store ptr %17, ptr %toStringInt32_0.i, align 8
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %18, align 8
  %"String#_func_0.i" = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0.i", align 8
  %"String#_0.i" = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 3
  store ptr %18, ptr %"String#_0.i", align 8
  store ptr %fun_capture_0.i, ptr %14, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_580", ptr %fun_func_0.i, align 8
  %19 = load ptr, ptr %13, align 8
  %20 = load ptr, ptr %let_func_0.i.i, align 8
  %21 = tail call ptr %20(ptr %19, ptr nonnull %14)
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %22, align 8
  %"Int32#_func_1.i" = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_1.i", align 8
  %23 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %23, align 1
  %24 = getelementptr { i8, { i32 } }, ptr %23, i64 0, i32 1, i32 0
  store i32 1, ptr %24, align 4
  %25 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %25, align 8
  %Right_func_1.i = getelementptr { ptr, ptr }, ptr %25, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.Right", ptr %Right_func_1.i, align 8
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %26, align 1
  %27 = getelementptr { i8, { ptr } }, ptr %26, i64 0, i32 1, i32 0
  store ptr %23, ptr %27, align 8
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %28, align 8
  %"|>_func_1.i" = getelementptr { ptr, ptr }, ptr %28, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.|>", ptr %"|>_func_1.i", align 8
  %29 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %26, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %29, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %29, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_388", ptr %let_func_0.i2.i, align 8
  %30 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 24)
  %31 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %31, align 8
  %Right_func_2.i = getelementptr { ptr, ptr }, ptr %31, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.Right", ptr %Right_func_2.i, align 8
  store ptr %31, ptr %fun_capture_2.i, align 8
  %32 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %32, align 8
  %"Int32#_func_2.i" = getelementptr { ptr, ptr }, ptr %32, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_2.i", align 8
  %"Int32#_0.i" = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 1
  store ptr %32, ptr %"Int32#_0.i", align 8
  %33 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %33, align 8
  %addInt32_func_0.i = getelementptr { ptr, ptr }, ptr %33, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addInt32", ptr %addInt32_func_0.i, align 8
  %addInt32_0.i = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 2
  store ptr %33, ptr %addInt32_0.i, align 8
  store ptr %fun_capture_2.i, ptr %30, align 8
  %fun_func_1.i = getelementptr { ptr, ptr }, ptr %30, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_582", ptr %fun_func_1.i, align 8
  %34 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %34, align 8
  %andThen_func_0.i = getelementptr { ptr, ptr }, ptr %34, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.andThen", ptr %andThen_func_0.i, align 8
  %35 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %30, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %35, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %35, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_579", ptr %let_func_0.i4.i, align 8
  %36 = load ptr, ptr %29, align 8
  %37 = load ptr, ptr %let_func_0.i2.i, align 8
  %38 = tail call ptr %37(ptr %36, ptr nonnull %35)
  %39 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %39, align 8
  %"|>_func_2.i" = getelementptr { ptr, ptr }, ptr %39, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.|>", ptr %"|>_func_2.i", align 8
  %40 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i5.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %38, ptr %let_capture_0.i5.i, align 8
  store ptr %let_capture_0.i5.i, ptr %40, align 8
  %let_func_0.i6.i = getelementptr { ptr, ptr }, ptr %40, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_388", ptr %let_func_0.i6.i, align 8
  %41 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_4.i = tail call ptr @malgo_malloc(i64 noundef 32)
  %42 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %42, align 8
  %Right_func_3.i = getelementptr { ptr, ptr }, ptr %42, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.Right", ptr %Right_func_3.i, align 8
  store ptr %42, ptr %fun_capture_4.i, align 8
  %43 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %43, align 8
  %"|>_func_3.i" = getelementptr { ptr, ptr }, ptr %43, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.|>", ptr %"|>_func_3.i", align 8
  %"|>_0.i" = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_4.i, i64 0, i32 1
  store ptr %43, ptr %"|>_0.i", align 8
  %44 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %44, align 8
  %putStr_func_1.i = getelementptr { ptr, ptr }, ptr %44, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStr", ptr %putStr_func_1.i, align 8
  %putStr_1.i = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_4.i, i64 0, i32 2
  store ptr %44, ptr %putStr_1.i, align 8
  %45 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %45, align 8
  %toStringInt32_func_1.i = getelementptr { ptr, ptr }, ptr %45, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32", ptr %toStringInt32_func_1.i, align 8
  %toStringInt32_1.i = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_4.i, i64 0, i32 3
  store ptr %45, ptr %toStringInt32_1.i, align 8
  store ptr %fun_capture_4.i, ptr %41, align 8
  %fun_func_2.i = getelementptr { ptr, ptr }, ptr %41, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_583", ptr %fun_func_2.i, align 8
  %46 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %46, align 8
  %andThen_func_1.i = getelementptr { ptr, ptr }, ptr %46, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.andThen", ptr %andThen_func_1.i, align 8
  %47 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i7.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %41, ptr %let_capture_0.i7.i, align 8
  store ptr %let_capture_0.i7.i, ptr %47, align 8
  %let_func_0.i8.i = getelementptr { ptr, ptr }, ptr %47, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_579", ptr %let_func_0.i8.i, align 8
  %48 = load ptr, ptr %40, align 8
  %49 = load ptr, ptr %let_func_0.i6.i, align 8
  %50 = tail call ptr %49(ptr %48, ptr nonnull %47)
  %51 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %51, align 8
  %"|>_func_4.i" = getelementptr { ptr, ptr }, ptr %51, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.|>", ptr %"|>_func_4.i", align 8
  %52 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i9.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %26, ptr %let_capture_0.i9.i, align 8
  store ptr %let_capture_0.i9.i, ptr %52, align 8
  %let_func_0.i10.i = getelementptr { ptr, ptr }, ptr %52, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_388", ptr %let_func_0.i10.i, align 8
  %53 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_6.i = tail call ptr @malgo_malloc(i64 noundef 16)
  %54 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %54, align 8
  %Left_func_0.i = getelementptr { ptr, ptr }, ptr %54, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.Left", ptr %Left_func_0.i, align 8
  store ptr %54, ptr %fun_capture_6.i, align 8
  %55 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %55, align 8
  %"String#_func_1.i" = getelementptr { ptr, ptr }, ptr %55, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_1.i", align 8
  %"String#_1.i" = getelementptr { ptr, ptr }, ptr %fun_capture_6.i, i64 0, i32 1
  store ptr %55, ptr %"String#_1.i", align 8
  store ptr %fun_capture_6.i, ptr %53, align 8
  %fun_func_3.i = getelementptr { ptr, ptr }, ptr %53, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_584", ptr %fun_func_3.i, align 8
  %56 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %56, align 8
  %andThen_func_2.i = getelementptr { ptr, ptr }, ptr %56, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.andThen", ptr %andThen_func_2.i, align 8
  %57 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i11.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %53, ptr %let_capture_0.i11.i, align 8
  store ptr %let_capture_0.i11.i, ptr %57, align 8
  %let_func_0.i12.i = getelementptr { ptr, ptr }, ptr %57, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_579", ptr %let_func_0.i12.i, align 8
  %58 = load ptr, ptr %52, align 8
  %59 = load ptr, ptr %let_func_0.i10.i, align 8
  %60 = tail call ptr %59(ptr %58, ptr nonnull %57)
  %61 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %61, align 8
  %"|>_func_5.i" = getelementptr { ptr, ptr }, ptr %61, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.|>", ptr %"|>_func_5.i", align 8
  %62 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i13.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %60, ptr %let_capture_0.i13.i, align 8
  store ptr %let_capture_0.i13.i, ptr %62, align 8
  %let_func_0.i14.i = getelementptr { ptr, ptr }, ptr %62, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_388", ptr %let_func_0.i14.i, align 8
  %63 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_8.i = tail call ptr @malgo_malloc(i64 noundef 24)
  %64 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %64, align 8
  %Right_func_4.i = getelementptr { ptr, ptr }, ptr %64, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.Right", ptr %Right_func_4.i, align 8
  store ptr %64, ptr %fun_capture_8.i, align 8
  %65 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %65, align 8
  %"Int32#_func_3.i" = getelementptr { ptr, ptr }, ptr %65, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_3.i", align 8
  %"Int32#_1.i" = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_8.i, i64 0, i32 1
  store ptr %65, ptr %"Int32#_1.i", align 8
  %66 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %66, align 8
  %addInt32_func_1.i = getelementptr { ptr, ptr }, ptr %66, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addInt32", ptr %addInt32_func_1.i, align 8
  %addInt32_1.i = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_8.i, i64 0, i32 2
  store ptr %66, ptr %addInt32_1.i, align 8
  store ptr %fun_capture_8.i, ptr %63, align 8
  %fun_func_4.i = getelementptr { ptr, ptr }, ptr %63, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_586", ptr %fun_func_4.i, align 8
  %67 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %67, align 8
  %andThen_func_3.i = getelementptr { ptr, ptr }, ptr %67, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.andThen", ptr %andThen_func_3.i, align 8
  %68 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i15.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %63, ptr %let_capture_0.i15.i, align 8
  store ptr %let_capture_0.i15.i, ptr %68, align 8
  %let_func_0.i16.i = getelementptr { ptr, ptr }, ptr %68, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_579", ptr %let_func_0.i16.i, align 8
  %69 = load ptr, ptr %62, align 8
  %70 = load ptr, ptr %let_func_0.i14.i, align 8
  %71 = tail call ptr %70(ptr %69, ptr nonnull %68)
  %72 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %72, align 8
  %"|>_func_6.i" = getelementptr { ptr, ptr }, ptr %72, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.|>", ptr %"|>_func_6.i", align 8
  %73 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i17.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %71, ptr %let_capture_0.i17.i, align 8
  store ptr %let_capture_0.i17.i, ptr %73, align 8
  %let_func_0.i18.i = getelementptr { ptr, ptr }, ptr %73, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_388", ptr %let_func_0.i18.i, align 8
  %74 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_10.i = tail call ptr @malgo_malloc(i64 noundef 24)
  %75 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %75, align 8
  %"|>_func_7.i" = getelementptr { ptr, ptr }, ptr %75, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.|>", ptr %"|>_func_7.i", align 8
  store ptr %75, ptr %fun_capture_10.i, align 8
  %76 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %76, align 8
  %putStrLn_func_1.i = getelementptr { ptr, ptr }, ptr %76, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_1.i, align 8
  %putStrLn_1.i = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_10.i, i64 0, i32 1
  store ptr %76, ptr %putStrLn_1.i, align 8
  %77 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %77, align 8
  %toStringInt32_func_2.i = getelementptr { ptr, ptr }, ptr %77, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32", ptr %toStringInt32_func_2.i, align 8
  %toStringInt32_2.i = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_10.i, i64 0, i32 2
  store ptr %77, ptr %toStringInt32_2.i, align 8
  store ptr %fun_capture_10.i, ptr %74, align 8
  %fun_func_5.i = getelementptr { ptr, ptr }, ptr %74, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_587", ptr %fun_func_5.i, align 8
  %78 = load ptr, ptr %73, align 8
  %79 = load ptr, ptr %let_func_0.i18.i, align 8
  %80 = tail call ptr %79(ptr %78, ptr nonnull %74)
  ret i32 0
}
