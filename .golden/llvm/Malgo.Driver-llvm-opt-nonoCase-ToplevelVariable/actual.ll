; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/ToplevelVariable.ll'
source_filename = "test/testcases/malgo/ToplevelVariable.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@"test/testcases/malgo/ToplevelVariable.mlg.one" = local_unnamed_addr global ptr undef
@"test/testcases/malgo/ToplevelVariable.mlg.comp" = local_unnamed_addr global ptr undef
@str129 = unnamed_addr constant [1 x i8] zeroinitializer
@str149 = unnamed_addr constant [10 x i8] c"no branch\00"
@str327 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"runtime/malgo/Prelude.mlg.Just"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Prelude.mlg.$p_688_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$p_688_0", ptr %3, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Prelude.mlg.printInt32"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly %"runtime/malgo/Prelude.mlg.$i_773_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %toStringInt32_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32", ptr %toStringInt32_func_0, align 8
  %3 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Prelude.mlg.$i_773_0", i64 0, i32 1
  %4 = load i32, ptr %3, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"toStringInt32#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32#", ptr %"toStringInt32#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %malgo_int32_t_to_string_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string", ptr %malgo_int32_t_to_string_func_0.i.i, align 8
  %7 = tail call ptr @malgo_int32_t_to_string(i32 %4)
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

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define internal ptr @"runtime/malgo/Prelude.mlg.identity"(ptr nocapture nofree readnone %0, ptr nofree readnone returned %"runtime/malgo/Prelude.mlg.$x_890_0") #0 {
  ret ptr %"runtime/malgo/Prelude.mlg.$x_890_0"
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_148"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) #1 {
  %a_0 = load ptr, ptr %0, align 8
  ret ptr %a_0
}

define internal ptr @"runtime/malgo/Prelude.mlg.const"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Prelude.mlg.$a_956_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"runtime/malgo/Prelude.mlg.$a_956_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_148", ptr %let_func_0, align 8
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

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_155"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
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
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_155", ptr %let_func_0, align 8
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

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_320"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
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
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_155", ptr %let_func_0.i.i, align 8
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
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_320", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_321"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
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
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_320", ptr %let_func_0.i.i, align 8
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
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_155", ptr %let_func_0.i.i.i.i, align 8
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
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_321", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.constId"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %"test/testcases/malgo/ToplevelVariable.mlg.$eta_87_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %identity_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.identity", ptr %identity_func_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %const_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.const", ptr %const_func_0, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %2, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %4, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_148", ptr %let_func_0.i, align 8
  %a_0.i = load ptr, ptr %let_capture_0.i, align 8
  ret ptr %a_0.i
}

define internal noundef ptr @"test/testcases/malgo/ToplevelVariable.mlg.addOne"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly %"test/testcases/malgo/ToplevelVariable.mlg.$eta_102_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %addInt32_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addInt32", ptr %addInt32_func_0, align 8
  %3 = load ptr, ptr @"test/testcases/malgo/ToplevelVariable.mlg.one", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %4, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_321", ptr %let_func_0.i, align 8
  %5 = tail call noundef ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_321"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i, ptr nocapture nofree readonly align 4 %"test/testcases/malgo/ToplevelVariable.mlg.$eta_102_0")
  ret ptr %5
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_326"(ptr nocapture nofree readonly %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch = icmp eq i8 %3, 0
  br i1 %switch, label %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0", label %"switch_branch_runtime/malgo/Prelude.mlg.Just_0"

common.ret:                                       ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Just_0", %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0"
  %common.ret.op = phi ptr [ %11, %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0" ], [ %12, %"switch_branch_runtime/malgo/Prelude.mlg.Just_0" ]
  ret ptr %common.ret.op

"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0": ; preds = %2
  %"String#_addr_0" = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %printString_0 = load ptr, ptr %0, align 8
  %4 = load ptr, ptr %"String#_0", align 8
  %5 = getelementptr { ptr, ptr }, ptr %"String#_0", i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull @str327)
  %8 = load ptr, ptr %printString_0, align 8
  %9 = getelementptr { ptr, ptr }, ptr %printString_0, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, ptr %7)
  br label %common.ret

"switch_branch_runtime/malgo/Prelude.mlg.Just_0": ; preds = %2
  %12 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %12, align 1
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
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"Int32#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { i32 } }, ptr %6, i64 0, i32 1, i32 0
  store i32 1, ptr %7, align 4
  store ptr %6, ptr @"test/testcases/malgo/ToplevelVariable.mlg.one", align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %8, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_332", ptr %fun_func_0.i, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %9, align 1
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %Just_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Just", ptr %Just_func_0.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr %9, ptr %12, align 8
  %13 = load ptr, ptr %8, align 8
  %14 = load ptr, ptr %fun_func_0.i, align 8
  %15 = tail call ptr %14(ptr %13, ptr nonnull %11)
  store ptr %15, ptr @"test/testcases/malgo/ToplevelVariable.mlg.comp", align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %16, align 1
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i1 = tail call ptr @malgo_malloc(i64 noundef 16)
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %18, align 8
  %printString_func_0.i = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0.i, align 8
  store ptr %18, ptr %fun_capture_0.i1, align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %19, align 8
  %"String#_func_0.i" = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0.i", align 8
  %"String#_0.i" = getelementptr { ptr, ptr }, ptr %fun_capture_0.i1, i64 0, i32 1
  store ptr %19, ptr %"String#_0.i", align 8
  store ptr %fun_capture_0.i1, ptr %17, align 8
  %fun_func_0.i2 = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_326", ptr %fun_func_0.i2, align 8
  %20 = load ptr, ptr @"test/testcases/malgo/ToplevelVariable.mlg.comp", align 8
  %21 = load i8, ptr %20, align 1
  %switch.i.i = icmp eq i8 %21, 0
  br i1 %switch.i.i, label %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0.i.i", label %"switch_branch_runtime/malgo/Prelude.mlg.Just_0.i.i"

"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0.i.i": ; preds = %1
  %"String#_0.i.i" = load ptr, ptr %"String#_0.i", align 8
  %printString_0.i.i = load ptr, ptr %fun_capture_0.i1, align 8
  %22 = load ptr, ptr %"String#_0.i.i", align 8
  %23 = getelementptr { ptr, ptr }, ptr %"String#_0.i.i", i64 0, i32 1
  %24 = load ptr, ptr %23, align 8
  %25 = tail call ptr %24(ptr %22, ptr nonnull @str327)
  %26 = load ptr, ptr %printString_0.i.i, align 8
  %27 = getelementptr { ptr, ptr }, ptr %printString_0.i.i, i64 0, i32 1
  %28 = load ptr, ptr %27, align 8
  %29 = tail call ptr %28(ptr %26, ptr %25)
  br label %"test/testcases/malgo/ToplevelVariable.mlg.main.exit"

"switch_branch_runtime/malgo/Prelude.mlg.Just_0.i.i": ; preds = %1
  %30 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %30, align 1
  br label %"test/testcases/malgo/ToplevelVariable.mlg.main.exit"

"test/testcases/malgo/ToplevelVariable.mlg.main.exit": ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0.i.i", %"switch_branch_runtime/malgo/Prelude.mlg.Just_0.i.i"
  %31 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %31, align 8
  %const_func_0.i = getelementptr { ptr, ptr }, ptr %31, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.const", ptr %const_func_0.i, align 8
  %32 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %32, align 8
  %constId_func_0.i = getelementptr { ptr, ptr }, ptr %32, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.constId", ptr %constId_func_0.i, align 8
  %33 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %33, align 8
  %identity_func_0.i.i = getelementptr { ptr, ptr }, ptr %33, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.identity", ptr %identity_func_0.i.i, align 8
  %34 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %34, align 8
  %const_func_0.i.i = getelementptr { ptr, ptr }, ptr %34, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.const", ptr %const_func_0.i.i, align 8
  %35 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %33, ptr %let_capture_0.i.i.i, align 8
  store ptr %let_capture_0.i.i.i, ptr %35, align 8
  %let_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %35, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_148", ptr %let_func_0.i.i.i, align 8
  %a_0.i.i.i = load ptr, ptr %let_capture_0.i.i.i, align 8
  %36 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %36, align 8
  %addOne_func_0.i = getelementptr { ptr, ptr }, ptr %36, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.addOne", ptr %addOne_func_0.i, align 8
  %37 = load ptr, ptr @"test/testcases/malgo/ToplevelVariable.mlg.one", align 8
  %38 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %38, align 8
  %addInt32_func_0.i.i = getelementptr { ptr, ptr }, ptr %38, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addInt32", ptr %addInt32_func_0.i.i, align 8
  %39 = load ptr, ptr @"test/testcases/malgo/ToplevelVariable.mlg.one", align 8
  %40 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %39, ptr %let_capture_0.i.i1.i, align 8
  store ptr %let_capture_0.i.i1.i, ptr %40, align 8
  %let_func_0.i.i2.i = getelementptr { ptr, ptr }, ptr %40, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_321", ptr %let_func_0.i.i2.i, align 8
  %41 = tail call ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_321"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i.i1.i, ptr nocapture nofree readonly align 4 %37)
  %42 = load ptr, ptr %a_0.i.i.i, align 8
  %43 = getelementptr { ptr, ptr }, ptr %a_0.i.i.i, i64 0, i32 1
  %44 = load ptr, ptr %43, align 8
  %45 = tail call ptr %44(ptr %42, ptr %41)
  %46 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %46, align 8
  %printInt32_func_0.i = getelementptr { ptr, ptr }, ptr %46, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.printInt32", ptr %printInt32_func_0.i, align 8
  %47 = tail call ptr @"runtime/malgo/Prelude.mlg.printInt32"(ptr poison, ptr nocapture nofree readonly %45)
  ret i32 0
}

define internal noundef ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_332"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
common.ret:
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  ret ptr %2
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind readnone willreturn }
attributes #1 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn }
