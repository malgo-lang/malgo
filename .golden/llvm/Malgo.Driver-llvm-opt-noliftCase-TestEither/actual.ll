; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/TestEither.ll'
source_filename = "test/testcases/malgo/TestEither.mlg"

@str3549 = unnamed_addr constant [12 x i8] c"unreachable\00"
@str3559 = unnamed_addr constant [6 x i8] c"error\00"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

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

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$p_2155_0") {
  %2 = tail call ptr @malgo_int32_t_to_string(i32 %"runtime/malgo/Builtin.mlg.$p_2155_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.toStringInt32#"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$x_2179_0") {
  %2 = tail call ptr @malgo_int32_t_to_string(i32 %"runtime/malgo/Builtin.mlg.$x_2179_0")
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/TestEither.mlg.#let_closure_3545"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
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
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3545", ptr %let_func_0, align 8
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

define internal ptr @"runtime/malgo/Prelude.mlg.putStr"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"runtime/malgo/Prelude.mlg.$str_723_0") {
"switch_branch_runtime/malgo/Builtin.mlg.String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Prelude.mlg.$str_723_0", i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_print_string(ptr %2)
  ret ptr %3
}

define internal noundef ptr @"test/testcases/malgo/TestEither.mlg.Right"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/TestEither.mlg.$p_251_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/TestEither.mlg.$p_251_0", ptr %3, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3547"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3548"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch = icmp eq i8 %3, 0
  br i1 %switch, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0", label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %2
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %4, i64 0, i32 1, i32 0
  store ptr @str3549, ptr %5, align 8
  %6 = tail call ptr @malgo_print_string(ptr noundef nonnull @str3549)
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %7, align 1
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = tail call ptr @malgo_newline(ptr noundef nonnull %8)
  br label %common.ret

common.ret:                                       ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0", %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %common.ret.op = phi ptr [ %9, %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0" ], [ %17, %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0" ]
  ret ptr %common.ret.op

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %2
  %10 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = getelementptr { i8, { i32 } }, ptr %11, i64 0, i32 1
  %13 = load i32, ptr %12, align 4
  %14 = tail call ptr @malgo_int32_t_to_string(i32 %13)
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %15, i64 0, i32 1, i32 0
  store ptr %14, ptr %16, align 8
  %17 = tail call ptr @malgo_print_string(ptr %14)
  br label %common.ret
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3567"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3568"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %cast_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %cast_0)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3566"(ptr nocapture nofree readonly %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 8
  %switch = icmp eq i8 %3, 0
  br i1 %switch, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0", label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"

common.ret:                                       ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0", %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %common.ret.op = phi ptr [ %6, %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0" ], [ %29, %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0" ]
  ret ptr %common.ret.op

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %2
  %4 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %6, i64 0, i32 1, i32 0
  store ptr %5, ptr %7, align 8
  br label %common.ret

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %2
  %putStr_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 3
  %putStr_0 = load ptr, ptr %putStr_addr_0, align 8
  %"toStringInt32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %"toStringInt32#_0" = load ptr, ptr %"toStringInt32#_addr_0", align 8
  %"String#_addr_0" = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %Right_0 = load ptr, ptr %0, align 8
  %8 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { i8, { i32 } }, ptr %9, i64 0, i32 1
  %11 = load i32, ptr %10, align 4
  %12 = load ptr, ptr %"toStringInt32#_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %"toStringInt32#_0", i64 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = tail call ptr %14(ptr %12, i32 %11)
  %16 = load ptr, ptr %"String#_0", align 8
  %17 = getelementptr { ptr, ptr }, ptr %"String#_0", i64 0, i32 1
  %18 = load ptr, ptr %17, align 8
  %19 = tail call ptr %18(ptr %16, ptr %15)
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %19, ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %20, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3567", ptr %let_func_0, align 8
  %d_0.i = load ptr, ptr %let_capture_0, align 8
  %21 = load ptr, ptr %putStr_0, align 8
  %22 = getelementptr { ptr, ptr }, ptr %putStr_0, i64 0, i32 1
  %23 = load ptr, ptr %22, align 8
  %24 = tail call ptr %23(ptr %21, ptr %d_0.i)
  %25 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %24, ptr %let_capture_2, align 8
  store ptr %let_capture_2, ptr %25, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %25, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3568", ptr %let_func_1, align 8
  %cast_0.i = load ptr, ptr %let_capture_2, align 8
  %26 = load ptr, ptr %Right_0, align 8
  %27 = getelementptr { ptr, ptr }, ptr %Right_0, i64 0, i32 1
  %28 = load ptr, ptr %27, align 8
  %29 = tail call ptr %28(ptr %26, ptr %cast_0.i)
  br label %common.ret
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3569"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3571"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3570"(ptr nocapture nofree readonly %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 8
  %switch = icmp eq i8 %3, 0
  br i1 %switch, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0", label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %2
  %4 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = tail call ptr @malgo_print_string(ptr %7)
  %9 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %9, align 1
  %10 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %10, align 1
  %11 = tail call ptr @malgo_newline(ptr noundef nonnull %10)
  br label %common.ret

common.ret:                                       ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0", %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %common.ret.op = phi ptr [ %11, %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0" ], [ %26, %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0" ]
  ret ptr %common.ret.op

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %2
  %malgo_int32_t_to_string_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %malgo_int32_t_to_string_0 = load ptr, ptr %malgo_int32_t_to_string_addr_0, align 8
  %putStrLn_0 = load ptr, ptr %0, align 8
  %12 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = getelementptr { i8, { i32 } }, ptr %13, i64 0, i32 1
  %15 = load i32, ptr %14, align 4
  %16 = load ptr, ptr %malgo_int32_t_to_string_0, align 8
  %17 = getelementptr { ptr, ptr }, ptr %malgo_int32_t_to_string_0, i64 0, i32 1
  %18 = load ptr, ptr %17, align 8
  %19 = tail call ptr %18(ptr %16, i32 %15)
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { ptr } }, ptr %20, i64 0, i32 1, i32 0
  store ptr %19, ptr %21, align 8
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %20, ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %22, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3571", ptr %let_func_0, align 8
  %d_0.i = load ptr, ptr %let_capture_0, align 8
  %23 = load ptr, ptr %putStrLn_0, align 8
  %24 = getelementptr { ptr, ptr }, ptr %putStrLn_0, i64 0, i32 1
  %25 = load ptr, ptr %24, align 8
  %26 = tail call ptr %25(ptr %23, ptr %d_0.i)
  br label %common.ret
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3575"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3577"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3576"(ptr nocapture nofree readonly %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 8
  %switch = icmp eq i8 %3, 0
  br i1 %switch, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0", label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %2
  %4 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = tail call ptr @malgo_print_string(ptr %7)
  %9 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %9, align 1
  %10 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %10, align 1
  %11 = tail call ptr @malgo_newline(ptr noundef nonnull %10)
  br label %common.ret

common.ret:                                       ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0", %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %common.ret.op = phi ptr [ %11, %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0" ], [ %26, %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0" ]
  ret ptr %common.ret.op

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %2
  %malgo_int32_t_to_string_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %malgo_int32_t_to_string_0 = load ptr, ptr %malgo_int32_t_to_string_addr_0, align 8
  %putStrLn_0 = load ptr, ptr %0, align 8
  %12 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = getelementptr { i8, { i32 } }, ptr %13, i64 0, i32 1
  %15 = load i32, ptr %14, align 4
  %16 = load ptr, ptr %malgo_int32_t_to_string_0, align 8
  %17 = getelementptr { ptr, ptr }, ptr %malgo_int32_t_to_string_0, i64 0, i32 1
  %18 = load ptr, ptr %17, align 8
  %19 = tail call ptr %18(ptr %16, i32 %15)
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { ptr } }, ptr %20, i64 0, i32 1, i32 0
  store ptr %19, ptr %21, align 8
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %20, ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %22, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3577", ptr %let_func_0, align 8
  %d_0.i = load ptr, ptr %let_capture_0, align 8
  %23 = load ptr, ptr %putStrLn_0, align 8
  %24 = getelementptr { ptr, ptr }, ptr %putStrLn_0, i64 0, i32 1
  %25 = load ptr, ptr %24, align 8
  %26 = tail call ptr %25(ptr %23, ptr %d_0.i)
  br label %common.ret
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %3, ptr %6, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %7, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3547", ptr %let_func_0.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %8, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3548", ptr %fun_func_0.i, align 8
  %9 = load ptr, ptr %7, align 8
  %10 = load ptr, ptr %let_func_0.i, align 8
  %11 = tail call ptr %10(ptr %9, ptr nonnull %8)
  %12 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { i32 } }, ptr %12, i64 0, i32 1, i32 0
  store i32 1, ptr %13, align 4
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %14, align 1
  %15 = getelementptr { i8, { ptr } }, ptr %14, i64 0, i32 1, i32 0
  store ptr %12, ptr %15, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %16, align 1
  %17 = getelementptr { i8, { i32 } }, ptr %16, i64 0, i32 1, i32 0
  store i32 1, ptr %17, align 4
  %18 = load i32, ptr %13, align 4
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %19, align 8
  %"addInt32#_func_2.i" = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addInt32#", ptr %"addInt32#_func_2.i", align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %18, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %20, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3545", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %21 = tail call i32 @malgo_add_int32_t(i32 %x_0.i.i, i32 noundef 1)
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %22, align 8
  %"Int32#_func_2.i" = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_2.i", align 8
  %23 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %23, align 1
  %24 = getelementptr { i8, { i32 } }, ptr %23, i64 0, i32 1, i32 0
  store i32 %21, ptr %24, align 4
  %25 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %25, align 8
  %26 = getelementptr { i8, { ptr } }, ptr %25, i64 0, i32 1, i32 0
  store ptr %23, ptr %26, align 8
  %27 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_12.i = tail call ptr @malgo_malloc(i64 noundef 32)
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %28, align 8
  %Right_func_1.i = getelementptr { ptr, ptr }, ptr %28, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.Right", ptr %Right_func_1.i, align 8
  store ptr %28, ptr %let_capture_12.i, align 8
  %29 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %29, align 8
  %"String#_func_1.i" = getelementptr { ptr, ptr }, ptr %29, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_1.i", align 8
  %"String#_1.i" = getelementptr { ptr, ptr, ptr, ptr }, ptr %let_capture_12.i, i64 0, i32 1
  store ptr %29, ptr %"String#_1.i", align 8
  %30 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %30, align 8
  %"toStringInt32#_func_1.i" = getelementptr { ptr, ptr }, ptr %30, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32#", ptr %"toStringInt32#_func_1.i", align 8
  %"toStringInt32#_1.i" = getelementptr { ptr, ptr, ptr, ptr }, ptr %let_capture_12.i, i64 0, i32 2
  store ptr %30, ptr %"toStringInt32#_1.i", align 8
  %31 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %31, align 8
  %putStr_func_1.i = getelementptr { ptr, ptr }, ptr %31, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStr", ptr %putStr_func_1.i, align 8
  %putStr_1.i = getelementptr { ptr, ptr, ptr, ptr }, ptr %let_capture_12.i, i64 0, i32 3
  store ptr %31, ptr %putStr_1.i, align 8
  store ptr %let_capture_12.i, ptr %27, align 8
  %let_func_6.i = getelementptr { ptr, ptr }, ptr %27, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3566", ptr %let_func_6.i, align 8
  %32 = tail call ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3566"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(32) %let_capture_12.i, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %25)
  %33 = load i8, ptr %14, align 1
  %switch.i = icmp eq i8 %33, 0
  br i1 %switch.i, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_4.i", label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_5.i"

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_4.i": ; preds = %1
  %34 = load ptr, ptr %15, align 8
  %35 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %35, align 1
  %36 = getelementptr { i8, { ptr } }, ptr %35, i64 0, i32 1, i32 0
  store ptr %34, ptr %36, align 8
  %37 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %37, align 1
  %38 = getelementptr { i8, { ptr } }, ptr %37, i64 0, i32 1, i32 0
  store ptr %34, ptr %38, align 8
  %39 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_14.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %37, ptr %let_capture_14.i, align 8
  store ptr %let_capture_14.i, ptr %39, align 8
  %let_func_7.i = getelementptr { ptr, ptr }, ptr %39, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3569", ptr %let_func_7.i, align 8
  %40 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_10.i = tail call ptr @malgo_malloc(i64 noundef 16)
  %41 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %41, align 8
  %putStrLn_func_4.i = getelementptr { ptr, ptr }, ptr %41, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_4.i, align 8
  store ptr %41, ptr %fun_capture_10.i, align 8
  %42 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %42, align 8
  %malgo_int32_t_to_string_func_4.i = getelementptr { ptr, ptr }, ptr %42, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string", ptr %malgo_int32_t_to_string_func_4.i, align 8
  %malgo_int32_t_to_string_4.i = getelementptr { ptr, ptr }, ptr %fun_capture_10.i, i64 0, i32 1
  store ptr %42, ptr %malgo_int32_t_to_string_4.i, align 8
  store ptr %fun_capture_10.i, ptr %40, align 8
  br label %"test/testcases/malgo/TestEither.mlg.main.exit"

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_5.i": ; preds = %1
  %43 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %43, align 1
  %44 = getelementptr { i8, { ptr } }, ptr %43, i64 0, i32 1, i32 0
  store ptr @str3559, ptr %44, align 8
  %45 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %45, align 1
  %46 = getelementptr { i8, { ptr } }, ptr %45, i64 0, i32 1, i32 0
  store ptr %43, ptr %46, align 8
  %47 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %47, align 1
  %48 = getelementptr { i8, { ptr } }, ptr %47, i64 0, i32 1, i32 0
  store ptr %43, ptr %48, align 8
  %49 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_18.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %47, ptr %let_capture_18.i, align 8
  store ptr %let_capture_18.i, ptr %49, align 8
  %let_func_9.i = getelementptr { ptr, ptr }, ptr %49, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3575", ptr %let_func_9.i, align 8
  %50 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_14.i = tail call ptr @malgo_malloc(i64 noundef 16)
  %51 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %51, align 8
  %putStrLn_func_6.i = getelementptr { ptr, ptr }, ptr %51, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_6.i, align 8
  store ptr %51, ptr %fun_capture_14.i, align 8
  %52 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %52, align 8
  %malgo_int32_t_to_string_func_6.i = getelementptr { ptr, ptr }, ptr %52, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string", ptr %malgo_int32_t_to_string_func_6.i, align 8
  %malgo_int32_t_to_string_6.i = getelementptr { ptr, ptr }, ptr %fun_capture_14.i, i64 0, i32 1
  store ptr %52, ptr %malgo_int32_t_to_string_6.i, align 8
  store ptr %fun_capture_14.i, ptr %50, align 8
  br label %"test/testcases/malgo/TestEither.mlg.main.exit"

"test/testcases/malgo/TestEither.mlg.main.exit":  ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_4.i", %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_5.i"
  %.sink.i = phi ptr [ %50, %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_5.i" ], [ %40, %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_4.i" ]
  %"test/testcases/malgo/TestEither.mlg.#fun_closure_3576.sink.i" = phi ptr [ @"test/testcases/malgo/TestEither.mlg.#fun_closure_3576", %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_5.i" ], [ @"test/testcases/malgo/TestEither.mlg.#fun_closure_3570", %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_4.i" ]
  %.sink3.i = phi ptr [ %49, %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_5.i" ], [ %39, %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_4.i" ]
  %let_func_9.sink.i = phi ptr [ %let_func_9.i, %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_5.i" ], [ %let_func_7.i, %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_4.i" ]
  %fun_func_7.i = getelementptr { ptr, ptr }, ptr %.sink.i, i64 0, i32 1
  store ptr %"test/testcases/malgo/TestEither.mlg.#fun_closure_3576.sink.i", ptr %fun_func_7.i, align 8
  %53 = load ptr, ptr %.sink3.i, align 8
  %54 = load ptr, ptr %let_func_9.sink.i, align 8
  %55 = tail call ptr %54(ptr %53, ptr nonnull %.sink.i)
  ret i32 0
}
