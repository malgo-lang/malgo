; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/TestPatSynRecon.ll'
source_filename = "test/testcases/malgo/TestPatSynRecon.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str144 = unnamed_addr constant [1 x i8] zeroinitializer
@str164 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i64 @malgo_add_int64_t(i64, i64) local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_149"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr %p_0, ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 1
  store ptr %1, ptr %5, align 8
  ret ptr %3
}

define internal ptr @"runtime/malgo/Prelude.mlg.Cons"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Prelude.mlg.$p_691_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"runtime/malgo/Prelude.mlg.$p_691_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_149", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_150"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
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
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_150", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Prelude.mlg.printInt64"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly %"runtime/malgo/Prelude.mlg.$i_769_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %toStringInt64_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt64", ptr %toStringInt64_func_0, align 8
  %3 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Prelude.mlg.$i_769_0", i64 0, i32 1
  %4 = load i64, ptr %3, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"toStringInt64#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt64#", ptr %"toStringInt64#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %malgo_int64_t_to_string_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int64_t_to_string", ptr %malgo_int64_t_to_string_func_0.i.i, align 8
  %7 = tail call ptr @malgo_int64_t_to_string(i64 %4)
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %"String#_func_0.i" = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0.i", align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %7, ptr %10, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %printString_func_0 = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0, align 8
  %12 = load ptr, ptr %10, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %"printString#_func_0.i" = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString#", ptr %"printString#_func_0.i", align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %malgo_print_string_func_0.i.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_string", ptr %malgo_print_string_func_0.i.i, align 8
  %15 = tail call ptr @malgo_print_string(ptr %12)
  ret ptr %15
}

define internal noundef ptr @"runtime/malgo/Builtin.mlg.Int64#"(ptr nocapture nofree readnone %0, i64 %"runtime/malgo/Builtin.mlg.$p_1794_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i64 } }, ptr %2, i64 0, i32 1, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$p_1794_0", ptr %3, align 4
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

define internal i64 @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_174"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_add_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_add_int64_t"(ptr nocapture nofree readnone %0, i64 %"runtime/malgo/Builtin.mlg.$p_1832_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"runtime/malgo/Builtin.mlg.$p_1832_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_174", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_int64_t_to_string"(ptr nocapture nofree readnone %0, i64 %"runtime/malgo/Builtin.mlg.$p_2156_0") {
  %2 = tail call ptr @malgo_int64_t_to_string(i64 %"runtime/malgo/Builtin.mlg.$p_2156_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_print_string"(ptr nocapture nofree readnone %0, ptr %"runtime/malgo/Builtin.mlg.$p_2163_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"runtime/malgo/Builtin.mlg.$p_2163_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.toStringInt64#"(ptr nocapture nofree readnone %0, i64 %"runtime/malgo/Builtin.mlg.$x_2172_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_int64_t_to_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int64_t_to_string", ptr %malgo_int64_t_to_string_func_0, align 8
  %3 = tail call ptr @malgo_int64_t_to_string(i64 %"runtime/malgo/Builtin.mlg.$x_2172_0")
  ret ptr %3
}

define internal noundef ptr @"runtime/malgo/Builtin.mlg.toStringInt64"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %"runtime/malgo/Builtin.mlg.$int64#_2174_0") {
"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0":
  %1 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2174_0", i64 0, i32 1
  %2 = load i64, ptr %1, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"toStringInt64#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt64#", ptr %"toStringInt64#_func_0", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_int64_t_to_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int64_t_to_string", ptr %malgo_int64_t_to_string_func_0.i, align 8
  %5 = tail call ptr @malgo_int64_t_to_string(i64 %2)
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

define internal i64 @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_333"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_add_int64_t_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_add_int64_t", ptr %malgo_add_int64_t_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_174", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %5 = tail call i64 @malgo_add_int64_t(i64 %p_0.i.i, i64 %1)
  ret i64 %5
}

define internal ptr @"runtime/malgo/Builtin.mlg.addInt64#"(ptr nocapture nofree readnone %0, i64 %"runtime/malgo/Builtin.mlg.$x_3995_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"runtime/malgo/Builtin.mlg.$x_3995_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_333", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_341"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"addInt64#_func_0.i" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addInt64#", ptr %"addInt64#_func_0.i", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_333", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %malgo_add_int64_t_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_add_int64_t", ptr %malgo_add_int64_t_func_0.i.i.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_174", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call i64 @malgo_add_int64_t(i64 %p_0.i.i.i.i, i64 %1)
  ret i64 %7
}

define internal ptr @"test/testcases/malgo/TestPatSynRecon.mlg.+#"(ptr nocapture nofree readnone %0, i64 %"test/testcases/malgo/TestPatSynRecon.mlg.$x_83_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"test/testcases/malgo/TestPatSynRecon.mlg.$x_83_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_341", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#fun_closure_342"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr nocapture nofree readonly align 4 %1) {
"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0":
  %"Int64#_0" = load ptr, ptr %0, align 8
  %"+#_addr_0" = getelementptr { ptr, ptr, i64 }, ptr %0, i64 0, i32 1
  %"+#_0" = load ptr, ptr %"+#_addr_0", align 8
  %p_addr_0 = getelementptr { ptr, ptr, i64 }, ptr %0, i64 0, i32 2
  %p_0 = load i64, ptr %p_addr_0, align 4
  %2 = getelementptr { i8, { i64 } }, ptr %1, i64 0, i32 1
  %3 = load i64, ptr %2, align 4
  %4 = load ptr, ptr %"+#_0", align 8
  %5 = getelementptr { ptr, ptr }, ptr %"+#_0", i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, i64 %p_0)
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call i64 %10(ptr %8, i64 %3)
  %12 = load ptr, ptr %"Int64#_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %"Int64#_0", i64 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = tail call ptr %14(ptr %12, i64 %11)
  ret ptr %15
}

define internal ptr @"test/testcases/malgo/TestPatSynRecon.mlg.sum"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %"test/testcases/malgo/TestPatSynRecon.mlg.$cons_95_0") {
  %2 = load i8, ptr %"test/testcases/malgo/TestPatSynRecon.mlg.$cons_95_0", align 1
  %switch = icmp eq i8 %2, 0
  br i1 %switch, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0", label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"

common.ret:                                       ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0", %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0"
  %common.ret.op = phi ptr [ %4, %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0" ], [ %21, %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0" ]
  ret ptr %common.ret.op

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0":  ; preds = %1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"Int64#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int64#", ptr %"Int64#_func_0", align 8
  %4 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i64 } }, ptr %4, i64 0, i32 1, i32 0
  store i64 0, ptr %5, align 4
  br label %common.ret

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0": ; preds = %1
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %"test/testcases/malgo/TestPatSynRecon.mlg.$cons_95_0", i64 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, { ptr, ptr } }, ptr %"test/testcases/malgo/TestPatSynRecon.mlg.$cons_95_0", i64 0, i32 1, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { i8, { i64 } }, ptr %7, i64 0, i32 1
  %11 = load i64, ptr %10, align 4
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %sum_func_0 = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.sum", ptr %sum_func_0, align 8
  %13 = tail call ptr @"test/testcases/malgo/TestPatSynRecon.mlg.sum"(ptr poison, ptr nocapture nofree readonly %9)
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %"|>_func_0" = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.|>", ptr %"|>_func_0", align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %13, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %15, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_150", ptr %let_func_0.i, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0 = tail call ptr @malgo_malloc(i64 noundef 24)
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %17, align 8
  %"Int64#_func_1" = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int64#", ptr %"Int64#_func_1", align 8
  store ptr %17, ptr %fun_capture_0, align 8
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %18, align 8
  %"+#_func_0" = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.+#", ptr %"+#_func_0", align 8
  %"+#_0" = getelementptr { ptr, ptr, i64 }, ptr %fun_capture_0, i64 0, i32 1
  store ptr %18, ptr %"+#_0", align 8
  %p_0 = getelementptr { ptr, ptr, i64 }, ptr %fun_capture_0, i64 0, i32 2
  store i64 %11, ptr %p_0, align 4
  store ptr %fun_capture_0, ptr %16, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#fun_closure_342", ptr %fun_func_0, align 8
  %19 = load ptr, ptr %15, align 8
  %20 = load ptr, ptr %let_func_0.i, align 8
  %21 = tail call ptr %20(ptr %19, ptr nonnull %16)
  br label %common.ret
}

define internal ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#fun_closure_343"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %printInt64_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %printInt64_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %printInt64_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %1)
  ret ptr %6
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
  %"Int64#_func_0.i" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int64#", ptr %"Int64#_func_0.i", align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i64 } }, ptr %7, i64 0, i32 1, i32 0
  store i64 1, ptr %8, align 4
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %Cons_func_0.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_0.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %7, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %10, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_149", ptr %let_func_0.i.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %"Int64#_func_1.i" = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int64#", ptr %"Int64#_func_1.i", align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { i64 } }, ptr %12, i64 0, i32 1, i32 0
  store i64 2, ptr %13, align 4
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %Cons_func_1.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_1.i, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %12, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %15, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_149", ptr %let_func_0.i2.i, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %16, align 1
  %17 = load ptr, ptr %15, align 8
  %18 = load ptr, ptr %let_func_0.i2.i, align 8
  %19 = tail call ptr %18(ptr %17, ptr nonnull %16)
  %20 = load ptr, ptr %10, align 8
  %21 = load ptr, ptr %let_func_0.i.i, align 8
  %22 = tail call ptr %21(ptr %20, ptr %19)
  %23 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %23, align 8
  %sum_func_0.i = getelementptr { ptr, ptr }, ptr %23, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.sum", ptr %sum_func_0.i, align 8
  %24 = tail call ptr @"test/testcases/malgo/TestPatSynRecon.mlg.sum"(ptr poison, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %22)
  %25 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %25, align 8
  %"|>_func_0.i" = getelementptr { ptr, ptr }, ptr %25, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.|>", ptr %"|>_func_0.i", align 8
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %24, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %26, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %26, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_150", ptr %let_func_0.i4.i, align 8
  %27 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %28, align 8
  %printInt64_func_0.i = getelementptr { ptr, ptr }, ptr %28, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.printInt64", ptr %printInt64_func_0.i, align 8
  store ptr %28, ptr %fun_capture_0.i, align 8
  store ptr %fun_capture_0.i, ptr %27, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %27, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#fun_closure_343", ptr %fun_func_0.i, align 8
  %29 = load ptr, ptr %26, align 8
  %30 = load ptr, ptr %let_func_0.i4.i, align 8
  %31 = tail call ptr %30(ptr %29, ptr nonnull %27)
  ret i32 0
}
