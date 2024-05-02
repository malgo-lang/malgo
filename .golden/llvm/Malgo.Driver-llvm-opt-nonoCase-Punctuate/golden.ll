; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/Punctuate.ll'
source_filename = "test/testcases/malgo/Punctuate.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str381 = unnamed_addr constant [1 x i8] zeroinitializer
@str401 = unnamed_addr constant [10 x i8] c"no branch\00"
@str407 = unnamed_addr constant [8 x i8] c"Symbol \00"
@str408 = unnamed_addr constant [6 x i8] c"SInt \00"
@str409 = unnamed_addr constant [8 x i8] c"SList [\00"
@str410 = unnamed_addr constant [3 x i8] c", \00"
@str411 = unnamed_addr constant [2 x i8] c"]\00"
@str412 = unnamed_addr constant [2 x i8] c"x\00"
@str413 = unnamed_addr constant [2 x i8] c"y\00"
@str414 = unnamed_addr constant [2 x i8] c"z\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_string_append(ptr, ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

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

define internal ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_260"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_string_append(ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_string_append"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Builtin.mlg.$p_2140_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"runtime/malgo/Builtin.mlg.$p_2140_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_260", ptr %let_func_0, align 8
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

define internal ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_367"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_string_append_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_string_append", ptr %malgo_string_append_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_260", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %5 = tail call ptr @malgo_string_append(ptr %p_0.i.i, ptr %1)
  ret ptr %5
}

define internal ptr @"runtime/malgo/Builtin.mlg.appendString#"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Builtin.mlg.$x_3963_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"runtime/malgo/Builtin.mlg.$x_3963_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_367", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_368"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 8 %1) {
  %"string#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"string#_0", i64 8
  %"string#_0.val" = load ptr, ptr %3, align 8
  %4 = getelementptr i8, ptr %1, i64 8
  %.val = load ptr, ptr %4, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"appendString#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.appendString#", ptr %"appendString#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"string#_0.val", ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %6, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_367", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %malgo_string_append_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_string_append", ptr %malgo_string_append_func_0.i.i.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 8
  store ptr %let_capture_0.i.i.i.i, ptr %8, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_260", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load ptr, ptr %let_capture_0.i.i.i.i, align 8
  %9 = tail call ptr @malgo_string_append(ptr %p_0.i.i.i.i, ptr %.val)
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %"String#_func_0.i" = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0.i", align 8
  %11 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr %9, ptr %12, align 8
  ret ptr %11
}

define internal ptr @"runtime/malgo/Builtin.mlg.appendString"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Builtin.mlg.$string#_3975_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"runtime/malgo/Builtin.mlg.$string#_3975_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_368", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_386"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
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
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_386", ptr %let_func_0, align 8
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

define internal ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_388"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %__0 = load ptr, ptr %0, align 8
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i", label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i"

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i": ; preds = %2
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  br label %common.ret1

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i": ; preds = %2
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = load i8, ptr %8, align 1
  %switch1.i = icmp eq i8 %9, 0
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %Cons_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_0.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %6, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %11, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_386", ptr %let_func_0.i.i, align 8
  br i1 %switch1.i, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_1.i", label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_1.i"

"switch_branch_runtime/malgo/Prelude.mlg.Nil_1.i": ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i"
  %12 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %12, align 1
  %13 = load ptr, ptr %11, align 8
  %14 = load ptr, ptr %let_func_0.i.i, align 8
  %15 = tail call ptr %14(ptr %13, ptr nonnull %12)
  br label %common.ret1

common.ret1:                                      ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i", %"switch_branch_runtime/malgo/Prelude.mlg.Nil_1.i", %"switch_branch_runtime/malgo/Prelude.mlg.Cons_1.i"
  %common.ret1.op = phi ptr [ %26, %"switch_branch_runtime/malgo/Prelude.mlg.Cons_1.i" ], [ %4, %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i" ], [ %15, %"switch_branch_runtime/malgo/Prelude.mlg.Nil_1.i" ]
  ret ptr %common.ret1.op

"switch_branch_runtime/malgo/Prelude.mlg.Cons_1.i": ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i"
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %16, align 8
  %Cons_func_2.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_2.i, align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i4.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %__0, ptr %let_capture_0.i4.i, align 8
  store ptr %let_capture_0.i4.i, ptr %17, align 8
  %let_func_0.i5.i = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_386", ptr %let_func_0.i5.i, align 8
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %18, align 8
  %punctuate_func_0.i = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.punctuate", ptr %punctuate_func_0.i, align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i6.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %__0, ptr %let_capture_0.i6.i, align 8
  store ptr %let_capture_0.i6.i, ptr %19, align 8
  %let_func_0.i7.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_388", ptr %let_func_0.i7.i, align 8
  %20 = tail call ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_388"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i6.i, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %8)
  %21 = load ptr, ptr %17, align 8
  %22 = load ptr, ptr %let_func_0.i5.i, align 8
  %23 = tail call ptr %22(ptr %21, ptr %20)
  %24 = load ptr, ptr %11, align 8
  %25 = load ptr, ptr %let_func_0.i.i, align 8
  %26 = tail call ptr %25(ptr %24, ptr %23)
  br label %common.ret1
}

define internal ptr @"runtime/malgo/Prelude.mlg.punctuate"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Prelude.mlg.$__725_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"runtime/malgo/Prelude.mlg.$__725_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_388", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_389"(ptr nocapture nofree readonly %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i", label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i"

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i": ; preds = %2
  %4 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  br label %common.ret1

common.ret1:                                      ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i", %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i"
  %common.ret1.op = phi ptr [ %4, %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i" ], [ %20, %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i" ]
  ret ptr %common.ret1.op

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i": ; preds = %2
  %__0 = load ptr, ptr %0, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = load ptr, ptr %__0, align 8
  %10 = getelementptr { ptr, ptr }, ptr %__0, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = tail call ptr %11(ptr %9, ptr %6)
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %Cons_func_0.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_0.i, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %12, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %14, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_386", ptr %let_func_0.i.i, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %mapList_func_0.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.mapList", ptr %mapList_func_0.i, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %__0, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %16, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_389", ptr %let_func_0.i2.i, align 8
  %17 = tail call ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_389"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i1.i, ptr nocapture nofree readonly %8)
  %18 = load ptr, ptr %14, align 8
  %19 = load ptr, ptr %let_func_0.i.i, align 8
  %20 = tail call ptr %19(ptr %18, ptr %17)
  br label %common.ret1
}

define internal ptr @"runtime/malgo/Prelude.mlg.mapList"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Prelude.mlg.$__777_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"runtime/malgo/Prelude.mlg.$__777_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_389", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Prelude.mlg.concatString"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %"runtime/malgo/Prelude.mlg.$nil_974_0") {
  %2 = load i8, ptr %"runtime/malgo/Prelude.mlg.$nil_974_0", align 1
  %switch = icmp eq i8 %2, 0
  br i1 %switch, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0", label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"

common.ret:                                       ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0", %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0"
  %common.ret.op = phi ptr [ %4, %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0" ], [ %16, %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0" ]
  ret ptr %common.ret.op

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0":  ; preds = %1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %4 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %4, i64 0, i32 1, i32 0
  store ptr @str381, ptr %5, align 8
  br label %common.ret

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0": ; preds = %1
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$nil_974_0", i64 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$nil_974_0", i64 0, i32 1, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %appendString_func_0 = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.appendString", ptr %appendString_func_0, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %7, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %11, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_368", ptr %let_func_0.i, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %concatString_func_0 = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.concatString", ptr %concatString_func_0, align 8
  %13 = tail call ptr @"runtime/malgo/Prelude.mlg.concatString"(ptr poison, ptr nocapture nofree readonly %9)
  %14 = load ptr, ptr %11, align 8
  %15 = load ptr, ptr %let_func_0.i, align 8
  %16 = tail call ptr %15(ptr %14, ptr %13)
  br label %common.ret
}

define internal noundef ptr @"test/testcases/malgo/Punctuate.mlg.Symbol"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/Punctuate.mlg.$p_113_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/Punctuate.mlg.$p_113_0", ptr %3, align 8
  ret ptr %2
}

define internal noundef ptr @"test/testcases/malgo/Punctuate.mlg.SList"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/Punctuate.mlg.$p_117_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 2, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/Punctuate.mlg.$p_117_0", ptr %3, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Punctuate.mlg.<>"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/Punctuate.mlg.$eta_119_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %appendString_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.appendString", ptr %appendString_func_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/Punctuate.mlg.$eta_119_0", ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %3, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_368", ptr %let_func_0.i, align 8
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Punctuate.mlg.show"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %"test/testcases/malgo/Punctuate.mlg.$symbol_120_0") {
  %2 = load i8, ptr %"test/testcases/malgo/Punctuate.mlg.$symbol_120_0", align 1
  switch i8 %2, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/Punctuate.mlg.Symbol_0"
    i8 1, label %"switch_branch_test/testcases/malgo/Punctuate.mlg.SInt_0"
    i8 2, label %"switch_branch_test/testcases/malgo/Punctuate.mlg.SList_0"
  ]

common.ret:                                       ; preds = %"switch_branch_test/testcases/malgo/Punctuate.mlg.SList_0", %"switch_branch_test/testcases/malgo/Punctuate.mlg.SInt_0", %"switch_branch_test/testcases/malgo/Punctuate.mlg.Symbol_0"
  %common.ret.op = phi ptr [ %11, %"switch_branch_test/testcases/malgo/Punctuate.mlg.Symbol_0" ], [ %31, %"switch_branch_test/testcases/malgo/Punctuate.mlg.SInt_0" ], [ %65, %"switch_branch_test/testcases/malgo/Punctuate.mlg.SList_0" ]
  ret ptr %common.ret.op

"switch_branch_test/testcases/malgo/Punctuate.mlg.Symbol_0": ; preds = %1
  %3 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/Punctuate.mlg.$symbol_120_0", i64 0, i32 1
  %4 = load ptr, ptr %3, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %6, i64 0, i32 1, i32 0
  store ptr @str407, ptr %7, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %"<>_func_0" = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.<>", ptr %"<>_func_0", align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %appendString_func_0.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.appendString", ptr %appendString_func_0.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %6, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %10, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_368", ptr %let_func_0.i.i, align 8
  %11 = tail call ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_368"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i.i, ptr nocapture nofree readonly align 8 %4)
  br label %common.ret

"switch_branch_test/testcases/malgo/Punctuate.mlg.SInt_0": ; preds = %1
  %12 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/Punctuate.mlg.$symbol_120_0", i64 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %"String#_func_1" = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_1", align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %15, i64 0, i32 1, i32 0
  store ptr @str408, ptr %16, align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %17, align 8
  %"<>_func_1" = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.<>", ptr %"<>_func_1", align 8
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %18, align 8
  %appendString_func_0.i1 = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.appendString", ptr %appendString_func_0.i1, align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i2 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %15, ptr %let_capture_0.i.i2, align 8
  store ptr %let_capture_0.i.i2, ptr %19, align 8
  %let_func_0.i.i3 = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_368", ptr %let_func_0.i.i3, align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %20, align 8
  %toStringInt32_func_0 = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32", ptr %toStringInt32_func_0, align 8
  %21 = getelementptr { i8, { i32 } }, ptr %13, i64 0, i32 1
  %22 = load i32, ptr %21, align 4
  %23 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %23, align 8
  %"toStringInt32#_func_0.i" = getelementptr { ptr, ptr }, ptr %23, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32#", ptr %"toStringInt32#_func_0.i", align 8
  %24 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %24, align 8
  %malgo_int32_t_to_string_func_0.i.i = getelementptr { ptr, ptr }, ptr %24, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string", ptr %malgo_int32_t_to_string_func_0.i.i, align 8
  %25 = tail call ptr @malgo_int32_t_to_string(i32 %22)
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %26, align 8
  %"String#_func_0.i" = getelementptr { ptr, ptr }, ptr %26, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0.i", align 8
  %27 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %27, i64 0, i32 1, i32 0
  store ptr %25, ptr %28, align 8
  %29 = load ptr, ptr %19, align 8
  %30 = load ptr, ptr %let_func_0.i.i3, align 8
  %31 = tail call ptr %30(ptr %29, ptr nonnull %27)
  br label %common.ret

"switch_branch_test/testcases/malgo/Punctuate.mlg.SList_0": ; preds = %1
  %32 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/Punctuate.mlg.$symbol_120_0", i64 0, i32 1
  %33 = load ptr, ptr %32, align 8
  %34 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %34, align 8
  %"String#_func_2" = getelementptr { ptr, ptr }, ptr %34, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_2", align 8
  %35 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %35, align 1
  %36 = getelementptr { i8, { ptr } }, ptr %35, i64 0, i32 1, i32 0
  store ptr @str409, ptr %36, align 8
  %37 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %37, align 8
  %"<>_func_2" = getelementptr { ptr, ptr }, ptr %37, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.<>", ptr %"<>_func_2", align 8
  %38 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %38, align 8
  %appendString_func_0.i4 = getelementptr { ptr, ptr }, ptr %38, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.appendString", ptr %appendString_func_0.i4, align 8
  %39 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i5 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %35, ptr %let_capture_0.i.i5, align 8
  store ptr %let_capture_0.i.i5, ptr %39, align 8
  %let_func_0.i.i6 = getelementptr { ptr, ptr }, ptr %39, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_368", ptr %let_func_0.i.i6, align 8
  %40 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %40, align 8
  %"String#_func_3" = getelementptr { ptr, ptr }, ptr %40, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_3", align 8
  %41 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %41, align 1
  %42 = getelementptr { i8, { ptr } }, ptr %41, i64 0, i32 1, i32 0
  store ptr @str410, ptr %42, align 8
  %43 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %43, align 8
  %punctuate_func_0 = getelementptr { ptr, ptr }, ptr %43, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.punctuate", ptr %punctuate_func_0, align 8
  %44 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %41, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %44, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %44, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_388", ptr %let_func_0.i, align 8
  %45 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %45, align 8
  %mapList_func_0 = getelementptr { ptr, ptr }, ptr %45, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.mapList", ptr %mapList_func_0, align 8
  %46 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %46, align 8
  %show_func_0 = getelementptr { ptr, ptr }, ptr %46, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.show", ptr %show_func_0, align 8
  %47 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i7 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %46, ptr %let_capture_0.i7, align 8
  store ptr %let_capture_0.i7, ptr %47, align 8
  %let_func_0.i8 = getelementptr { ptr, ptr }, ptr %47, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_389", ptr %let_func_0.i8, align 8
  %48 = tail call ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_389"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i7, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %33)
  %49 = load ptr, ptr %44, align 8
  %50 = load ptr, ptr %let_func_0.i, align 8
  %51 = tail call ptr %50(ptr %49, ptr %48)
  %52 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %52, align 8
  %concatString_func_0 = getelementptr { ptr, ptr }, ptr %52, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.concatString", ptr %concatString_func_0, align 8
  %53 = tail call ptr @"runtime/malgo/Prelude.mlg.concatString"(ptr poison, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %51)
  %54 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %54, align 8
  %"<>_func_3" = getelementptr { ptr, ptr }, ptr %54, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.<>", ptr %"<>_func_3", align 8
  %55 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %55, align 8
  %appendString_func_0.i9 = getelementptr { ptr, ptr }, ptr %55, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.appendString", ptr %appendString_func_0.i9, align 8
  %56 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i10 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %53, ptr %let_capture_0.i.i10, align 8
  store ptr %let_capture_0.i.i10, ptr %56, align 8
  %let_func_0.i.i11 = getelementptr { ptr, ptr }, ptr %56, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_368", ptr %let_func_0.i.i11, align 8
  %57 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %57, align 8
  %"String#_func_4" = getelementptr { ptr, ptr }, ptr %57, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_4", align 8
  %58 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %58, align 1
  %59 = getelementptr { i8, { ptr } }, ptr %58, i64 0, i32 1, i32 0
  store ptr @str411, ptr %59, align 8
  %60 = load ptr, ptr %56, align 8
  %61 = load ptr, ptr %let_func_0.i.i11, align 8
  %62 = tail call ptr %61(ptr %60, ptr nonnull %58)
  %63 = load ptr, ptr %39, align 8
  %64 = load ptr, ptr %let_func_0.i.i6, align 8
  %65 = tail call ptr %64(ptr %63, ptr %62)
  br label %common.ret

switch_default_0:                                 ; preds = %1
  unreachable
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
  %"String#_func_0.i" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0.i", align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr @str412, ptr %8, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %Symbol_func_0.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.Symbol", ptr %Symbol_func_0.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { ptr } }, ptr %10, i64 0, i32 1, i32 0
  store ptr %7, ptr %11, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %Cons_func_0.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_0.i, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %10, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %13, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_386", ptr %let_func_0.i.i, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %"String#_func_1.i" = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_1.i", align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %15, i64 0, i32 1, i32 0
  store ptr @str413, ptr %16, align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %17, align 8
  %Symbol_func_1.i = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.Symbol", ptr %Symbol_func_1.i, align 8
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %18, align 1
  %19 = getelementptr { i8, { ptr } }, ptr %18, i64 0, i32 1, i32 0
  store ptr %15, ptr %19, align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %20, align 8
  %Cons_func_1.i = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_1.i, align 8
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %18, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %21, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %21, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_386", ptr %let_func_0.i2.i, align 8
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %22, align 8
  %"String#_func_2.i" = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_2.i", align 8
  %23 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %23, align 1
  %24 = getelementptr { i8, { ptr } }, ptr %23, i64 0, i32 1, i32 0
  store ptr @str414, ptr %24, align 8
  %25 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %25, align 8
  %Symbol_func_2.i = getelementptr { ptr, ptr }, ptr %25, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.Symbol", ptr %Symbol_func_2.i, align 8
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %26, align 1
  %27 = getelementptr { i8, { ptr } }, ptr %26, i64 0, i32 1, i32 0
  store ptr %23, ptr %27, align 8
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %28, align 8
  %Cons_func_2.i = getelementptr { ptr, ptr }, ptr %28, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_2.i, align 8
  %29 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %26, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %29, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %29, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_386", ptr %let_func_0.i4.i, align 8
  %30 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %30, align 1
  %31 = load ptr, ptr %29, align 8
  %32 = load ptr, ptr %let_func_0.i4.i, align 8
  %33 = tail call ptr %32(ptr %31, ptr nonnull %30)
  %34 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %34, align 8
  %SList_func_0.i = getelementptr { ptr, ptr }, ptr %34, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.SList", ptr %SList_func_0.i, align 8
  %35 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 2, ptr %35, align 1
  %36 = getelementptr { i8, { ptr } }, ptr %35, i64 0, i32 1, i32 0
  store ptr %33, ptr %36, align 8
  %37 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %37, align 8
  %Cons_func_3.i = getelementptr { ptr, ptr }, ptr %37, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_3.i, align 8
  %38 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i5.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %35, ptr %let_capture_0.i5.i, align 8
  store ptr %let_capture_0.i5.i, ptr %38, align 8
  %let_func_0.i6.i = getelementptr { ptr, ptr }, ptr %38, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_386", ptr %let_func_0.i6.i, align 8
  %39 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %39, align 1
  %40 = load ptr, ptr %38, align 8
  %41 = load ptr, ptr %let_func_0.i6.i, align 8
  %42 = tail call ptr %41(ptr %40, ptr nonnull %39)
  %43 = load ptr, ptr %21, align 8
  %44 = load ptr, ptr %let_func_0.i2.i, align 8
  %45 = tail call ptr %44(ptr %43, ptr %42)
  %46 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %46, align 8
  %SList_func_1.i = getelementptr { ptr, ptr }, ptr %46, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.SList", ptr %SList_func_1.i, align 8
  %47 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 2, ptr %47, align 1
  %48 = getelementptr { i8, { ptr } }, ptr %47, i64 0, i32 1, i32 0
  store ptr %45, ptr %48, align 8
  %49 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %49, align 8
  %Cons_func_4.i = getelementptr { ptr, ptr }, ptr %49, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_4.i, align 8
  %50 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i7.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %47, ptr %let_capture_0.i7.i, align 8
  store ptr %let_capture_0.i7.i, ptr %50, align 8
  %let_func_0.i8.i = getelementptr { ptr, ptr }, ptr %50, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_386", ptr %let_func_0.i8.i, align 8
  %51 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %51, align 1
  %52 = load ptr, ptr %50, align 8
  %53 = load ptr, ptr %let_func_0.i8.i, align 8
  %54 = tail call ptr %53(ptr %52, ptr nonnull %51)
  %55 = load ptr, ptr %13, align 8
  %56 = load ptr, ptr %let_func_0.i.i, align 8
  %57 = tail call ptr %56(ptr %55, ptr %54)
  %58 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %58, align 8
  %SList_func_2.i = getelementptr { ptr, ptr }, ptr %58, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.SList", ptr %SList_func_2.i, align 8
  %59 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 2, ptr %59, align 8
  %60 = getelementptr { i8, { ptr } }, ptr %59, i64 0, i32 1, i32 0
  store ptr %57, ptr %60, align 8
  %61 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %61, align 8
  %show_func_0.i = getelementptr { ptr, ptr }, ptr %61, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.show", ptr %show_func_0.i, align 8
  %62 = tail call ptr @"test/testcases/malgo/Punctuate.mlg.show"(ptr poison, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %59)
  %63 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %63, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %63, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0.i, align 8
  %64 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %64, align 8
  %printString_func_0.i.i = getelementptr { ptr, ptr }, ptr %64, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0.i.i, align 8
  %65 = getelementptr { i8, { ptr } }, ptr %62, i64 0, i32 1
  %66 = load ptr, ptr %65, align 8
  %67 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %67, align 8
  %"printString#_func_0.i.i.i" = getelementptr { ptr, ptr }, ptr %67, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString#", ptr %"printString#_func_0.i.i.i", align 8
  %68 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %68, align 8
  %malgo_print_string_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %68, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_string", ptr %malgo_print_string_func_0.i.i.i.i, align 8
  %69 = tail call ptr @malgo_print_string(ptr %66)
  %70 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %70, align 1
  %71 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %71, align 8
  %newline_func_0.i.i = getelementptr { ptr, ptr }, ptr %71, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.newline", ptr %newline_func_0.i.i, align 8
  %72 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %72, align 1
  %73 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %73, align 8
  %malgo_newline_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %73, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_newline", ptr %malgo_newline_func_0.i.i.i, align 8
  %74 = tail call ptr @malgo_newline(ptr noundef nonnull %72)
  ret i32 0
}
