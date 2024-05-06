; ModuleID = '.malgo-work/test/testcases/malgo/EvenOdd.ll'
source_filename = "test/testcases/malgo/EvenOdd.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str266 = unnamed_addr constant [1 x i8] zeroinitializer
@str286 = unnamed_addr constant [10 x i8] c"no branch\00"
@str293 = unnamed_addr constant [6 x i8] c"False\00"
@str294 = unnamed_addr constant [5 x i8] c"True\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i32 @malgo_sub_int32_t(i32, i32) local_unnamed_addr

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

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_unsafe_cast"(ptr nocapture nofree readnone %0, ptr %"runtime/malgo/Builtin.mlg.$p_1807_0") {
  %2 = tail call ptr @malgo_unsafe_cast(ptr %"runtime/malgo/Builtin.mlg.$p_1807_0")
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.#let_closure_92"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
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
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_92", ptr %let_func_0, align 8
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

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.#let_closure_154"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
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
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_92", ptr %let_func_0.i.i, align 8
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
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_154", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_155"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
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
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_154", ptr %let_func_0.i.i, align 8
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
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_92", ptr %let_func_0.i.i.i.i, align 8
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
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_155", ptr %let_func_0, align 8
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

define internal noundef ptr @"test/testcases/malgo/EvenOdd.mlg.even"(ptr nocapture nofree readnone %0, ptr nofree align 4 %"test/testcases/malgo/EvenOdd.mlg.$int32#_51_0") {
"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0":
  %1 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_51_0", i64 0, i32 1
  %2 = load i32, ptr %1, align 4
  %cond = icmp eq i32 %2, 0
  br i1 %cond, label %switch-unboxed_branch_0_i32_0, label %switch-unboxed_default_0

common.ret:                                       ; preds = %switch-unboxed_default_0, %switch-unboxed_branch_0_i32_0
  %common.ret.op = phi ptr [ %3, %switch-unboxed_branch_0_i32_0 ], [ %13, %switch-unboxed_default_0 ]
  ret ptr %common.ret.op

switch-unboxed_branch_0_i32_0:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %3, align 1
  br label %common.ret

switch-unboxed_default_0:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %subInt32_func_0 = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.subInt32", ptr %subInt32_func_0, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_51_0", ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_155", ptr %let_func_0.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %"Int32#_func_0" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0", align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 1, ptr %8, align 4
  %9 = load ptr, ptr %5, align 8
  %10 = load ptr, ptr %let_func_0.i, align 8
  %11 = tail call ptr %10(ptr %9, ptr nonnull %7)
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %odd_func_0 = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.odd", ptr %odd_func_0, align 8
  %13 = tail call noundef ptr @"test/testcases/malgo/EvenOdd.mlg.odd"(ptr poison, ptr nofree align 4 %11)
  br label %common.ret
}

define internal noundef ptr @"test/testcases/malgo/EvenOdd.mlg.odd"(ptr nocapture nofree readnone %0, ptr nofree align 4 %"test/testcases/malgo/EvenOdd.mlg.$int32#_61_0") {
"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0":
  %1 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_61_0", i64 0, i32 1
  %2 = load i32, ptr %1, align 4
  %cond = icmp eq i32 %2, 0
  br i1 %cond, label %switch-unboxed_branch_0_i32_0, label %switch-unboxed_default_0

common.ret:                                       ; preds = %switch-unboxed_default_0, %switch-unboxed_branch_0_i32_0
  %common.ret.op = phi ptr [ %3, %switch-unboxed_branch_0_i32_0 ], [ %13, %switch-unboxed_default_0 ]
  ret ptr %common.ret.op

switch-unboxed_branch_0_i32_0:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  br label %common.ret

switch-unboxed_default_0:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %subInt32_func_0 = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.subInt32", ptr %subInt32_func_0, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_61_0", ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_155", ptr %let_func_0.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %"Int32#_func_0" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0", align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 1, ptr %8, align 4
  %9 = load ptr, ptr %5, align 8
  %10 = load ptr, ptr %let_func_0.i, align 8
  %11 = tail call ptr %10(ptr %9, ptr nonnull %7)
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %even_func_0 = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.even", ptr %even_func_0, align 8
  %13 = tail call noundef ptr @"test/testcases/malgo/EvenOdd.mlg.even"(ptr poison, ptr nofree align 4 %11)
  br label %common.ret
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.#fun_closure_292"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
common.ret:
  %"String#_0" = load ptr, ptr %0, align 8
  %putStrLn_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %putStrLn_0 = load ptr, ptr %putStrLn_addr_0, align 8
  %2 = load i8, ptr %1, align 1
  %switch = icmp eq i8 %2, 0
  %3 = load ptr, ptr %"String#_0", align 8
  %4 = getelementptr { ptr, ptr }, ptr %"String#_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %spec.select = select i1 %switch, ptr @str293, ptr @str294
  %6 = tail call ptr %5(ptr %3, ptr nonnull %spec.select)
  %7 = load ptr, ptr %putStrLn_0, align 8
  %8 = getelementptr { ptr, ptr }, ptr %putStrLn_0, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call ptr %9(ptr %7, ptr %6)
  ret ptr %10
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
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %"String#_func_0.i" = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0.i", align 8
  store ptr %7, ptr %fun_capture_0.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0.i, align 8
  %putStrLn_0.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %8, ptr %putStrLn_0.i, align 8
  store ptr %fun_capture_0.i, ptr %6, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#fun_closure_292", ptr %fun_func_0.i, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %"Int32#_func_0.i" = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0.i", align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %10, align 4
  %11 = getelementptr { i8, { i32 } }, ptr %10, i64 0, i32 1, i32 0
  store i32 10, ptr %11, align 4
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %even_func_0.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.even", ptr %even_func_0.i, align 8
  %13 = tail call ptr @"test/testcases/malgo/EvenOdd.mlg.even"(ptr poison, ptr nofree noundef nonnull align 4 dereferenceable(1) %10)
  %14 = load ptr, ptr %6, align 8
  %15 = load ptr, ptr %fun_func_0.i, align 8
  %16 = tail call ptr %15(ptr %14, ptr %13)
  ret i32 0
}
