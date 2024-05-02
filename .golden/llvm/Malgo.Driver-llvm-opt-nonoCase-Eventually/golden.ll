; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/Eventually.ll'
source_filename = "test/testcases/malgo/Eventually.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@"test/testcases/malgo/Eventually.mlg.eventually" = local_unnamed_addr global ptr undef
@"test/testcases/malgo/Eventually.mlg.comp" = local_unnamed_addr global ptr undef
@str566 = unnamed_addr constant [1 x i8] zeroinitializer
@str586 = unnamed_addr constant [10 x i8] c"no branch\00"
@str593 = unnamed_addr constant [7 x i8] c"return\00"
@str594 = unnamed_addr constant [5 x i8] c"bind\00"
@str597 = unnamed_addr constant [8 x i8] c"not yet\00"
@str603 = unnamed_addr constant [2 x i8] c"1\00"
@str606 = unnamed_addr constant [2 x i8] c"2\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

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

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_unsafe_cast"(ptr nocapture nofree readnone %0, ptr %"runtime/malgo/Builtin.mlg.$p_1807_0") {
  %2 = tail call ptr @malgo_unsafe_cast(ptr %"runtime/malgo/Builtin.mlg.$p_1807_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$p_2155_0") {
  %2 = tail call ptr @malgo_int32_t_to_string(i32 %"runtime/malgo/Builtin.mlg.$p_2155_0")
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

define internal ptr @"test/testcases/malgo/Eventually.mlg.#let_closure_572"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
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
  store ptr @"test/testcases/malgo/Eventually.mlg.#let_closure_572", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_592"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr nocapture nofree readnone %1) {
  %p_0 = load ptr, ptr %0, align 8
  %k_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %k_0 = load ptr, ptr %k_addr_0, align 8
  %eventuallyBind_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %eventuallyBind_0 = load ptr, ptr %eventuallyBind_addr_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = load ptr, ptr %p_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %p_0, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %3)
  %8 = load ptr, ptr %eventuallyBind_0, align 8
  %9 = getelementptr { ptr, ptr }, ptr %eventuallyBind_0, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, ptr %7)
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = tail call ptr %14(ptr %12, ptr %k_0)
  ret ptr %15
}

define internal noundef ptr @"test/testcases/malgo/Eventually.mlg.Done"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/Eventually.mlg.$p_258_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/Eventually.mlg.$p_258_0", ptr %3, align 8
  ret ptr %2
}

define internal noundef ptr @"test/testcases/malgo/Eventually.mlg.NotYetDone"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/Eventually.mlg.$p_260_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/Eventually.mlg.$p_260_0", ptr %3, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Eventually.mlg.step"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %"test/testcases/malgo/Eventually.mlg.$done_262_0") {
  %2 = load i8, ptr %"test/testcases/malgo/Eventually.mlg.$done_262_0", align 8
  %switch = icmp eq i8 %2, 0
  %3 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/Eventually.mlg.$done_262_0", i64 0, i32 1
  %4 = load ptr, ptr %3, align 8
  br i1 %switch, label %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0", label %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0"

common.ret:                                       ; preds = %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0", %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0"
  %common.ret.op = phi ptr [ %6, %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0" ], [ %12, %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0" ]
  ret ptr %common.ret.op

"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0": ; preds = %1
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %Done_func_0 = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.Done", ptr %Done_func_0, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %6, i64 0, i32 1, i32 0
  store ptr %4, ptr %7, align 8
  br label %common.ret

"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0": ; preds = %1
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = load ptr, ptr %4, align 8
  %10 = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = tail call ptr %11(ptr %9, ptr nonnull %8)
  br label %common.ret
}

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

define internal ptr @"test/testcases/malgo/Eventually.mlg.return"(ptr nocapture nofree readnone %0, ptr %"test/testcases/malgo/Eventually.mlg.$record_269_0") {
  %2 = tail call ptr @malgo_hash_table_get(ptr %"test/testcases/malgo/Eventually.mlg.$record_269_0", ptr noundef nonnull @str593)
  %3 = tail call ptr @malgo_hash_table_get(ptr %"test/testcases/malgo/Eventually.mlg.$record_269_0", ptr noundef nonnull @str594)
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Eventually.mlg.#let_closure_595"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %done_0 = load ptr, ptr %0, align 8
  %3 = load i8, ptr %done_0, align 8
  %switch.i = icmp eq i8 %3, 0
  %4 = getelementptr { i8, { ptr } }, ptr %done_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  br i1 %switch.i, label %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i", label %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i"

"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i": ; preds = %2
  %6 = load ptr, ptr %1, align 8
  %7 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, ptr %5)
  br label %"test/testcases/malgo/Eventually.mlg.$eventuallyBind_curry_288.exit"

"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i": ; preds = %2
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 24)
  store ptr %5, ptr %fun_capture_0.i, align 8
  %k_0.i = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %1, ptr %k_0.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %eventuallyBind_func_0.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.eventuallyBind", ptr %eventuallyBind_func_0.i, align 8
  %eventuallyBind_0.i = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 2
  store ptr %11, ptr %eventuallyBind_0.i, align 8
  store ptr %fun_capture_0.i, ptr %10, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_592", ptr %fun_func_0.i, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %NotYetDone_func_0.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.NotYetDone", ptr %NotYetDone_func_0.i, align 8
  %13 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %13, align 1
  %14 = getelementptr { i8, { ptr } }, ptr %13, i64 0, i32 1, i32 0
  store ptr %10, ptr %14, align 8
  br label %"test/testcases/malgo/Eventually.mlg.$eventuallyBind_curry_288.exit"

"test/testcases/malgo/Eventually.mlg.$eventuallyBind_curry_288.exit": ; preds = %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i", %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i"
  %common.ret.op.i = phi ptr [ %9, %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i" ], [ %13, %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i" ]
  ret ptr %common.ret.op.i
}

define internal ptr @"test/testcases/malgo/Eventually.mlg.eventuallyBind"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/Eventually.mlg.$done_272_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/Eventually.mlg.$done_272_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#let_closure_595", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Eventually.mlg.bind"(ptr nocapture nofree readnone %0, ptr %"test/testcases/malgo/Eventually.mlg.$record_308_0") {
  %2 = tail call ptr @malgo_hash_table_get(ptr %"test/testcases/malgo/Eventually.mlg.$record_308_0", ptr noundef nonnull @str593)
  %3 = tail call ptr @malgo_hash_table_get(ptr %"test/testcases/malgo/Eventually.mlg.$record_308_0", ptr noundef nonnull @str594)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_596"(ptr nocapture nofree readonly align 8 %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %printString_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %printString_0 = load ptr, ptr %printString_addr_0, align 8
  %3 = load i8, ptr %1, align 1
  %switch = icmp eq i8 %3, 0
  br i1 %switch, label %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0", label %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0"

common.ret:                                       ; preds = %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0", %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0"
  %.sink1 = phi ptr [ %17, %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0" ], [ %13, %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0" ]
  %4 = load ptr, ptr %printString_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %printString_0, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr %.sink1)
  ret ptr %7

"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0": ; preds = %2
  %toStringInt32_0 = load ptr, ptr %0, align 8
  %8 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = load ptr, ptr %toStringInt32_0, align 8
  %11 = getelementptr { ptr, ptr }, ptr %toStringInt32_0, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call ptr %12(ptr %10, ptr %9)
  br label %common.ret

"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0": ; preds = %2
  %"String#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %14 = load ptr, ptr %"String#_0", align 8
  %15 = getelementptr { ptr, ptr }, ptr %"String#_0", i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr %16(ptr %14, ptr nonnull @str597)
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
  %5 = tail call ptr @malgo_hash_table_new()
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %Done_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.Done", ptr %Done_func_0.i, align 8
  tail call void @malgo_hash_table_insert(ptr %5, ptr noundef nonnull @str593, ptr noundef nonnull %6)
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %eventuallyBind_func_0.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.eventuallyBind", ptr %eventuallyBind_func_0.i, align 8
  tail call void @malgo_hash_table_insert(ptr %5, ptr noundef nonnull @str594, ptr noundef nonnull %7)
  store ptr %5, ptr @"test/testcases/malgo/Eventually.mlg.eventually", align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %bind_func_0.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.bind", ptr %bind_func_0.i, align 8
  %9 = load ptr, ptr @"test/testcases/malgo/Eventually.mlg.eventually", align 8
  %10 = tail call ptr @malgo_hash_table_get(ptr %9, ptr noundef nonnull @str593)
  %11 = tail call ptr @malgo_hash_table_get(ptr %9, ptr noundef nonnull @str594)
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 24)
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %Done_func_1.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.Done", ptr %Done_func_1.i, align 8
  store ptr %13, ptr %fun_capture_0.i, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %printString_func_0.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0.i, align 8
  %printString_0.i = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %14, ptr %printString_0.i, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %"String#_func_0.i" = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0.i", align 8
  %"String#_0.i" = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 2
  store ptr %15, ptr %"String#_0.i", align 8
  store ptr %fun_capture_0.i, ptr %12, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_602", ptr %fun_func_0.i, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %16, align 8
  %NotYetDone_func_0.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.NotYetDone", ptr %NotYetDone_func_0.i, align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %17, align 1
  %18 = getelementptr { i8, { ptr } }, ptr %17, i64 0, i32 1, i32 0
  store ptr %12, ptr %18, align 8
  %19 = load ptr, ptr %11, align 8
  %20 = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = tail call ptr %21(ptr %19, ptr nonnull %17)
  %23 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 56)
  %24 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %24, align 8
  %Done_func_2.i = getelementptr { ptr, ptr }, ptr %24, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.Done", ptr %Done_func_2.i, align 8
  store ptr %24, ptr %fun_capture_2.i, align 8
  %25 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %25, align 8
  %bind_func_1.i = getelementptr { ptr, ptr }, ptr %25, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.bind", ptr %bind_func_1.i, align 8
  %bind_0.i = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 1
  store ptr %25, ptr %bind_0.i, align 8
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %26, align 8
  %printString_func_1.i = getelementptr { ptr, ptr }, ptr %26, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_1.i, align 8
  %printString_1.i = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 2
  store ptr %26, ptr %printString_1.i, align 8
  %27 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %27, align 8
  %"String#_func_1.i" = getelementptr { ptr, ptr }, ptr %27, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_1.i", align 8
  %"String#_1.i" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 3
  store ptr %27, ptr %"String#_1.i", align 8
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %28, align 8
  %"Int32#_func_0.i" = getelementptr { ptr, ptr }, ptr %28, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0.i", align 8
  %"Int32#_0.i" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 4
  store ptr %28, ptr %"Int32#_0.i", align 8
  %29 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %29, align 8
  %return_func_0.i = getelementptr { ptr, ptr }, ptr %29, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.return", ptr %return_func_0.i, align 8
  %return_0.i = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 5
  store ptr %29, ptr %return_0.i, align 8
  %30 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %30, align 8
  %NotYetDone_func_1.i = getelementptr { ptr, ptr }, ptr %30, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.NotYetDone", ptr %NotYetDone_func_1.i, align 8
  %NotYetDone_0.i = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 6
  store ptr %30, ptr %NotYetDone_0.i, align 8
  store ptr %fun_capture_2.i, ptr %23, align 8
  %fun_func_1.i = getelementptr { ptr, ptr }, ptr %23, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_604", ptr %fun_func_1.i, align 8
  %31 = load ptr, ptr %22, align 8
  %32 = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  %33 = load ptr, ptr %32, align 8
  %34 = tail call ptr %33(ptr %31, ptr nonnull %23)
  store ptr %34, ptr @"test/testcases/malgo/Eventually.mlg.comp", align 8
  %35 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %35, align 1
  %36 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %36, align 8
  %"|>_func_0.i" = getelementptr { ptr, ptr }, ptr %36, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.|>", ptr %"|>_func_0.i", align 8
  %37 = load ptr, ptr @"test/testcases/malgo/Eventually.mlg.comp", align 8
  %38 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %37, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %38, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %38, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#let_closure_572", ptr %let_func_0.i.i, align 8
  %39 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %39, align 8
  %step_func_0.i = getelementptr { ptr, ptr }, ptr %39, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.step", ptr %step_func_0.i, align 8
  %x_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %40 = load i8, ptr %x_0.i.i, align 8
  %switch.i.i = icmp eq i8 %40, 0
  %41 = getelementptr { i8, { ptr } }, ptr %x_0.i.i, i64 0, i32 1
  %42 = load ptr, ptr %41, align 8
  br i1 %switch.i.i, label %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i.i", label %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i.i"

"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i.i": ; preds = %1
  %43 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %43, align 8
  %Done_func_0.i.i = getelementptr { ptr, ptr }, ptr %43, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.Done", ptr %Done_func_0.i.i, align 8
  %44 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %44, align 1
  %45 = getelementptr { i8, { ptr } }, ptr %44, i64 0, i32 1, i32 0
  store ptr %42, ptr %45, align 8
  br label %"test/testcases/malgo/Eventually.mlg.step.exit.i"

"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i.i": ; preds = %1
  %46 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %46, align 1
  %47 = load ptr, ptr %42, align 8
  %48 = getelementptr { ptr, ptr }, ptr %42, i64 0, i32 1
  %49 = load ptr, ptr %48, align 8
  %50 = tail call ptr %49(ptr %47, ptr nonnull %46)
  br label %"test/testcases/malgo/Eventually.mlg.step.exit.i"

"test/testcases/malgo/Eventually.mlg.step.exit.i": ; preds = %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i.i", %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i.i"
  %51 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %51, align 8
  %"|>_func_1.i" = getelementptr { ptr, ptr }, ptr %51, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.|>", ptr %"|>_func_1.i", align 8
  %52 = load ptr, ptr @"test/testcases/malgo/Eventually.mlg.comp", align 8
  %53 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %52, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %53, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %53, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#let_closure_572", ptr %let_func_0.i2.i, align 8
  %54 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %54, align 8
  %step_func_1.i = getelementptr { ptr, ptr }, ptr %54, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.step", ptr %step_func_1.i, align 8
  %x_0.i9.i = load ptr, ptr %let_capture_0.i1.i, align 8
  %55 = load i8, ptr %x_0.i9.i, align 8
  %switch.i18.i = icmp eq i8 %55, 0
  %56 = getelementptr { i8, { ptr } }, ptr %x_0.i9.i, i64 0, i32 1
  %57 = load ptr, ptr %56, align 8
  br i1 %switch.i18.i, label %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i21.i", label %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i22.i"

"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i21.i": ; preds = %"test/testcases/malgo/Eventually.mlg.step.exit.i"
  %58 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %58, align 8
  %Done_func_0.i20.i = getelementptr { ptr, ptr }, ptr %58, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.Done", ptr %Done_func_0.i20.i, align 8
  %59 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %59, align 1
  %60 = getelementptr { i8, { ptr } }, ptr %59, i64 0, i32 1, i32 0
  store ptr %57, ptr %60, align 8
  br label %"test/testcases/malgo/Eventually.mlg.step.exit23.i"

"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i22.i": ; preds = %"test/testcases/malgo/Eventually.mlg.step.exit.i"
  %61 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %61, align 1
  %62 = load ptr, ptr %57, align 8
  %63 = getelementptr { ptr, ptr }, ptr %57, i64 0, i32 1
  %64 = load ptr, ptr %63, align 8
  %65 = tail call ptr %64(ptr %62, ptr nonnull %61)
  br label %"test/testcases/malgo/Eventually.mlg.step.exit23.i"

"test/testcases/malgo/Eventually.mlg.step.exit23.i": ; preds = %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i22.i", %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i21.i"
  %common.ret.op.i19.i = phi ptr [ %59, %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i21.i" ], [ %65, %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i22.i" ]
  %66 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %66, align 8
  %"|>_func_2.i" = getelementptr { ptr, ptr }, ptr %66, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.|>", ptr %"|>_func_2.i", align 8
  %67 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %common.ret.op.i19.i, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %67, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %67, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#let_closure_572", ptr %let_func_0.i4.i, align 8
  %68 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %68, align 8
  %step_func_2.i = getelementptr { ptr, ptr }, ptr %68, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.step", ptr %step_func_2.i, align 8
  %x_0.i12.i = load ptr, ptr %let_capture_0.i3.i, align 8
  %69 = load i8, ptr %x_0.i12.i, align 8
  %switch.i24.i = icmp eq i8 %69, 0
  %70 = getelementptr { i8, { ptr } }, ptr %x_0.i12.i, i64 0, i32 1
  %71 = load ptr, ptr %70, align 8
  br i1 %switch.i24.i, label %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i27.i", label %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i28.i"

"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i27.i": ; preds = %"test/testcases/malgo/Eventually.mlg.step.exit23.i"
  %72 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %72, align 8
  %Done_func_0.i26.i = getelementptr { ptr, ptr }, ptr %72, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.Done", ptr %Done_func_0.i26.i, align 8
  %73 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %73, align 1
  %74 = getelementptr { i8, { ptr } }, ptr %73, i64 0, i32 1, i32 0
  store ptr %71, ptr %74, align 8
  br label %"test/testcases/malgo/Eventually.mlg.step.exit29.i"

"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i28.i": ; preds = %"test/testcases/malgo/Eventually.mlg.step.exit23.i"
  %75 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %75, align 1
  %76 = load ptr, ptr %71, align 8
  %77 = getelementptr { ptr, ptr }, ptr %71, i64 0, i32 1
  %78 = load ptr, ptr %77, align 8
  %79 = tail call ptr %78(ptr %76, ptr nonnull %75)
  br label %"test/testcases/malgo/Eventually.mlg.step.exit29.i"

"test/testcases/malgo/Eventually.mlg.step.exit29.i": ; preds = %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i28.i", %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i27.i"
  %common.ret.op.i25.i = phi ptr [ %73, %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i27.i" ], [ %79, %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i28.i" ]
  %80 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %80, align 8
  %"|>_func_3.i" = getelementptr { ptr, ptr }, ptr %80, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.|>", ptr %"|>_func_3.i", align 8
  %81 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i5.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %common.ret.op.i25.i, ptr %let_capture_0.i5.i, align 8
  store ptr %let_capture_0.i5.i, ptr %81, align 8
  %let_func_0.i6.i = getelementptr { ptr, ptr }, ptr %81, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#let_closure_572", ptr %let_func_0.i6.i, align 8
  %82 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %82, align 8
  %step_func_3.i = getelementptr { ptr, ptr }, ptr %82, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.step", ptr %step_func_3.i, align 8
  %x_0.i15.i = load ptr, ptr %let_capture_0.i5.i, align 8
  %83 = load i8, ptr %x_0.i15.i, align 8
  %switch.i30.i = icmp eq i8 %83, 0
  %84 = getelementptr { i8, { ptr } }, ptr %x_0.i15.i, i64 0, i32 1
  %85 = load ptr, ptr %84, align 8
  br i1 %switch.i30.i, label %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i33.i", label %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i34.i"

"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i33.i": ; preds = %"test/testcases/malgo/Eventually.mlg.step.exit29.i"
  %86 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %86, align 8
  %Done_func_0.i32.i = getelementptr { ptr, ptr }, ptr %86, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.Done", ptr %Done_func_0.i32.i, align 8
  %87 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %87, align 1
  %88 = getelementptr { i8, { ptr } }, ptr %87, i64 0, i32 1, i32 0
  store ptr %85, ptr %88, align 8
  br label %"test/testcases/malgo/Eventually.mlg.main.exit"

"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i34.i": ; preds = %"test/testcases/malgo/Eventually.mlg.step.exit29.i"
  %89 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %89, align 1
  %90 = load ptr, ptr %85, align 8
  %91 = getelementptr { ptr, ptr }, ptr %85, i64 0, i32 1
  %92 = load ptr, ptr %91, align 8
  %93 = tail call ptr %92(ptr %90, ptr nonnull %89)
  br label %"test/testcases/malgo/Eventually.mlg.main.exit"

"test/testcases/malgo/Eventually.mlg.main.exit":  ; preds = %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i33.i", %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i34.i"
  %common.ret.op.i31.i = phi ptr [ %87, %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i33.i" ], [ %93, %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i34.i" ]
  %94 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %94, align 8
  %"|>_func_4.i" = getelementptr { ptr, ptr }, ptr %94, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.|>", ptr %"|>_func_4.i", align 8
  %95 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i7.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %common.ret.op.i31.i, ptr %let_capture_0.i7.i, align 8
  store ptr %let_capture_0.i7.i, ptr %95, align 8
  %let_func_0.i8.i = getelementptr { ptr, ptr }, ptr %95, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#let_closure_572", ptr %let_func_0.i8.i, align 8
  %96 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i1 = tail call ptr @malgo_malloc(i64 noundef 24)
  %97 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %97, align 8
  %toStringInt32_func_0.i = getelementptr { ptr, ptr }, ptr %97, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32", ptr %toStringInt32_func_0.i, align 8
  store ptr %97, ptr %fun_capture_0.i1, align 8
  %98 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %98, align 8
  %printString_func_0.i2 = getelementptr { ptr, ptr }, ptr %98, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0.i2, align 8
  %printString_0.i3 = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_0.i1, i64 0, i32 1
  store ptr %98, ptr %printString_0.i3, align 8
  %99 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %99, align 8
  %"String#_func_0.i4" = getelementptr { ptr, ptr }, ptr %99, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0.i4", align 8
  %"String#_0.i5" = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_0.i1, i64 0, i32 2
  store ptr %99, ptr %"String#_0.i5", align 8
  store ptr %fun_capture_0.i1, ptr %96, align 8
  %fun_func_0.i6 = getelementptr { ptr, ptr }, ptr %96, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_596", ptr %fun_func_0.i6, align 8
  %100 = load ptr, ptr %95, align 8
  %101 = load ptr, ptr %let_func_0.i8.i, align 8
  %102 = tail call ptr %101(ptr %100, ptr nonnull %96)
  ret i32 0
}

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr

define internal ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_602"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr nocapture nofree readnone %1) {
  %Done_0 = load ptr, ptr %0, align 8
  %printString_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %printString_0 = load ptr, ptr %printString_addr_0, align 8
  %"String#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %3 = load ptr, ptr %"String#_0", align 8
  %4 = getelementptr { ptr, ptr }, ptr %"String#_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr nonnull @str603)
  %7 = load ptr, ptr %printString_0, align 8
  %8 = getelementptr { ptr, ptr }, ptr %printString_0, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call ptr %9(ptr %7, ptr %6)
  %11 = load ptr, ptr %Done_0, align 8
  %12 = getelementptr { ptr, ptr }, ptr %Done_0, i64 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = tail call ptr %13(ptr %11, ptr %10)
  ret ptr %14
}

define internal ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_605"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr nocapture nofree readnone %1) {
  %Done_0 = load ptr, ptr %0, align 8
  %printString_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %printString_0 = load ptr, ptr %printString_addr_0, align 8
  %"String#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %3 = load ptr, ptr %"String#_0", align 8
  %4 = getelementptr { ptr, ptr }, ptr %"String#_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr nonnull @str606)
  %7 = load ptr, ptr %printString_0, align 8
  %8 = getelementptr { ptr, ptr }, ptr %printString_0, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call ptr %9(ptr %7, ptr %6)
  %11 = load ptr, ptr %Done_0, align 8
  %12 = getelementptr { ptr, ptr }, ptr %Done_0, i64 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = tail call ptr %13(ptr %11, ptr %10)
  ret ptr %14
}

define internal ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_607"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readnone %1) {
  %"Int32#_0" = load ptr, ptr %0, align 8
  %return_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %return_0 = load ptr, ptr %return_addr_0, align 8
  %3 = load ptr, ptr %return_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %return_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = load ptr, ptr @"test/testcases/malgo/Eventually.mlg.eventually", align 8
  %7 = tail call ptr %5(ptr %3, ptr %6)
  %8 = load ptr, ptr %"Int32#_0", align 8
  %9 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, i32 3)
  %12 = load ptr, ptr %7, align 8
  %13 = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = tail call ptr %14(ptr %12, ptr %11)
  ret ptr %15
}

define internal ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_604"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(56) %0, ptr nocapture nofree readnone %1) {
  %Done_0 = load ptr, ptr %0, align 8
  %bind_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %bind_0 = load ptr, ptr %bind_addr_0, align 8
  %printString_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %printString_0 = load ptr, ptr %printString_addr_0, align 8
  %"String#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 3
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %"Int32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 4
  %"Int32#_0" = load ptr, ptr %"Int32#_addr_0", align 8
  %return_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 5
  %return_0 = load ptr, ptr %return_addr_0, align 8
  %NotYetDone_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 6
  %NotYetDone_0 = load ptr, ptr %NotYetDone_addr_0, align 8
  %3 = load ptr, ptr %bind_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %bind_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = load ptr, ptr @"test/testcases/malgo/Eventually.mlg.eventually", align 8
  %7 = tail call ptr %5(ptr %3, ptr %6)
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0 = tail call ptr @malgo_malloc(i64 noundef 24)
  store ptr %Done_0, ptr %fun_capture_0, align 8
  %printString_1 = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_0, i64 0, i32 1
  store ptr %printString_0, ptr %printString_1, align 8
  %"String#_1" = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_0, i64 0, i32 2
  store ptr %"String#_0", ptr %"String#_1", align 8
  store ptr %fun_capture_0, ptr %8, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_605", ptr %fun_func_0, align 8
  %9 = load ptr, ptr %NotYetDone_0, align 8
  %10 = getelementptr { ptr, ptr }, ptr %NotYetDone_0, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = tail call ptr %11(ptr %9, ptr nonnull %8)
  %13 = load ptr, ptr %7, align 8
  %14 = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  %15 = load ptr, ptr %14, align 8
  %16 = tail call ptr %15(ptr %13, ptr %12)
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %"Int32#_0", ptr %fun_capture_2, align 8
  %return_1 = getelementptr { ptr, ptr }, ptr %fun_capture_2, i64 0, i32 1
  store ptr %return_0, ptr %return_1, align 8
  store ptr %fun_capture_2, ptr %17, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_607", ptr %fun_func_1, align 8
  %18 = load ptr, ptr %16, align 8
  %19 = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  %20 = load ptr, ptr %19, align 8
  %21 = tail call ptr %20(ptr %18, ptr nonnull %17)
  ret ptr %21
}
