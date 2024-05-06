; ModuleID = '.malgo-work/test/testcases/malgo/RecordTest.ll'
source_filename = "test/testcases/malgo/RecordTest.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str262 = unnamed_addr constant [1 x i8] zeroinitializer
@str282 = unnamed_addr constant [10 x i8] c"no branch\00"
@str288 = unnamed_addr constant [2 x i8] c"a\00"
@str289 = unnamed_addr constant [2 x i8] c"b\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

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

define internal i32 @"test/testcases/malgo/RecordTest.mlg.#let_closure_87"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
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
  store ptr @"test/testcases/malgo/RecordTest.mlg.#let_closure_87", ptr %let_func_0, align 8
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

define internal i32 @"test/testcases/malgo/RecordTest.mlg.#let_closure_252"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
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
  store ptr @"test/testcases/malgo/RecordTest.mlg.#let_closure_87", ptr %let_func_0.i.i, align 8
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
  store ptr @"test/testcases/malgo/RecordTest.mlg.#let_closure_252", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"test/testcases/malgo/RecordTest.mlg.#let_closure_253"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
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
  store ptr @"test/testcases/malgo/RecordTest.mlg.#let_closure_252", ptr %let_func_0.i.i, align 8
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
  store ptr @"test/testcases/malgo/RecordTest.mlg.#let_closure_87", ptr %let_func_0.i.i.i.i, align 8
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
  store ptr @"test/testcases/malgo/RecordTest.mlg.#let_closure_253", ptr %let_func_0, align 8
  ret ptr %2
}

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

define internal ptr @"test/testcases/malgo/RecordTest.mlg.f"(ptr nocapture nofree readnone %0, ptr %"test/testcases/malgo/RecordTest.mlg.$record_56_0") {
  %2 = tail call ptr @malgo_hash_table_get(ptr %"test/testcases/malgo/RecordTest.mlg.$record_56_0", ptr noundef nonnull @str288)
  %3 = tail call ptr @malgo_hash_table_get(ptr %"test/testcases/malgo/RecordTest.mlg.$record_56_0", ptr noundef nonnull @str289)
  ret ptr %2
}

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr

define internal ptr @"test/testcases/malgo/RecordTest.mlg.#fun_closure_290"(ptr nocapture nofree readnone %0, ptr %1) {
  %3 = tail call ptr @malgo_hash_table_get(ptr %1, ptr noundef nonnull @str288)
  %4 = tail call ptr @malgo_hash_table_get(ptr %1, ptr noundef nonnull @str289)
  ret ptr %4
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
  store i32 32, ptr %8, align 4
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %"Int32#_func_1.i" = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_1.i", align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { i32 } }, ptr %10, i64 0, i32 1, i32 0
  store i32 10, ptr %11, align 4
  %12 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %12, ptr noundef nonnull @str288, ptr noundef nonnull %7)
  tail call void @malgo_hash_table_insert(ptr %12, ptr noundef nonnull @str289, ptr noundef nonnull %10)
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %f_func_0.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"test/testcases/malgo/RecordTest.mlg.f", ptr %f_func_0.i, align 8
  %14 = tail call ptr @malgo_hash_table_get(ptr %12, ptr noundef nonnull @str288)
  %15 = tail call ptr @malgo_hash_table_get(ptr %12, ptr noundef nonnull @str289)
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %16, align 8
  %addInt32_func_0.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addInt32", ptr %addInt32_func_0.i, align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %14, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %17, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"test/testcases/malgo/RecordTest.mlg.#let_closure_253", ptr %let_func_0.i.i, align 8
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %18, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"test/testcases/malgo/RecordTest.mlg.#fun_closure_290", ptr %fun_func_0.i, align 8
  %19 = tail call ptr @malgo_hash_table_get(ptr %12, ptr noundef nonnull @str288)
  %20 = tail call ptr @malgo_hash_table_get(ptr %12, ptr noundef nonnull @str289)
  %21 = load ptr, ptr %17, align 8
  %22 = load ptr, ptr %let_func_0.i.i, align 8
  %23 = tail call ptr %22(ptr %21, ptr %20)
  %24 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %24, align 8
  %toStringInt32_func_0.i = getelementptr { ptr, ptr }, ptr %24, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32", ptr %toStringInt32_func_0.i, align 8
  %25 = getelementptr { i8, { i32 } }, ptr %23, i64 0, i32 1
  %26 = load i32, ptr %25, align 4
  %27 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %27, align 8
  %"toStringInt32#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %27, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32#", ptr %"toStringInt32#_func_0.i.i", align 8
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %28, align 8
  %malgo_int32_t_to_string_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %28, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string", ptr %malgo_int32_t_to_string_func_0.i.i.i, align 8
  %29 = tail call ptr @malgo_int32_t_to_string(i32 %26)
  %30 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %30, align 8
  %"String#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %30, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0.i.i", align 8
  %31 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %31, align 1
  %32 = getelementptr { i8, { ptr } }, ptr %31, i64 0, i32 1, i32 0
  store ptr %29, ptr %32, align 8
  %33 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %33, align 8
  %printString_func_0.i = getelementptr { ptr, ptr }, ptr %33, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0.i, align 8
  %34 = load ptr, ptr %32, align 8
  %35 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %35, align 8
  %"printString#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %35, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString#", ptr %"printString#_func_0.i.i", align 8
  %36 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %36, align 8
  %malgo_print_string_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %36, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_string", ptr %malgo_print_string_func_0.i.i.i, align 8
  %37 = tail call ptr @malgo_print_string(ptr %34)
  ret i32 0
}
