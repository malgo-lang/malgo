; ModuleID = '.malgo-work/test/testcases/malgo/Show.ll'
source_filename = "test/testcases/malgo/Show.mlg"

@"test/testcases/malgo/Show.mlg.showInt32" = local_unnamed_addr global ptr undef
@str3189 = unnamed_addr constant [2 x i8] c"(\00"
@str3190 = unnamed_addr constant [5 x i8] c"show\00"
@str3191 = unnamed_addr constant [3 x i8] c", \00"
@str3192 = unnamed_addr constant [2 x i8] c")\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_string_append(ptr, ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_3186"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_string_append(ptr %p_0, ptr %1)
  ret ptr %3
}

define internal noundef ptr @"runtime/malgo/Builtin.mlg.String#"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Builtin.mlg.$p_1802_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$p_1802_0", ptr %3, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_3187"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_string_append(ptr %x_0, ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.appendString#"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Builtin.mlg.$x_3963_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"runtime/malgo/Builtin.mlg.$x_3963_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3187", ptr %let_func_0, align 8
  ret ptr %2
}

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_3193"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr nocapture nofree readonly align 8 %1) {
"switch_branch_runtime/malgo/Builtin.mlg.String#_0":
  %"appendString#_0" = load ptr, ptr %0, align 8
  %"String#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %d_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %2 = getelementptr { i8, { ptr } }, ptr %d_0, i64 0, i32 1
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = load ptr, ptr %"appendString#_0", align 8
  %7 = getelementptr { ptr, ptr }, ptr %"appendString#_0", i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, ptr %3)
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call ptr %12(ptr %10, ptr %5)
  %14 = load ptr, ptr %"String#_0", align 8
  %15 = getelementptr { ptr, ptr }, ptr %"String#_0", i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr %16(ptr %14, ptr %13)
  ret ptr %17
}

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_3194"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr nocapture nofree readonly align 8 %1) {
"switch_branch_runtime/malgo/Builtin.mlg.String#_0":
  %"appendString#_0" = load ptr, ptr %0, align 8
  %"String#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %d_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %2 = getelementptr { i8, { ptr } }, ptr %d_0, i64 0, i32 1
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = load ptr, ptr %"appendString#_0", align 8
  %7 = getelementptr { ptr, ptr }, ptr %"appendString#_0", i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, ptr %3)
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call ptr %12(ptr %10, ptr %5)
  %14 = load ptr, ptr %"String#_0", align 8
  %15 = getelementptr { ptr, ptr }, ptr %"String#_0", i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr %16(ptr %14, ptr %13)
  ret ptr %17
}

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_3195"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr nocapture nofree readonly align 8 %1) {
"switch_branch_runtime/malgo/Builtin.mlg.String#_0":
  %"appendString#_0" = load ptr, ptr %0, align 8
  %"String#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %d_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %2 = getelementptr { i8, { ptr } }, ptr %d_0, i64 0, i32 1
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = load ptr, ptr %"appendString#_0", align 8
  %7 = getelementptr { ptr, ptr }, ptr %"appendString#_0", i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, ptr %3)
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call ptr %12(ptr %10, ptr %5)
  %14 = load ptr, ptr %"String#_0", align 8
  %15 = getelementptr { ptr, ptr }, ptr %"String#_0", i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr %16(ptr %14, ptr %13)
  ret ptr %17
}

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_3196"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr nocapture nofree readonly align 8 %1) {
"switch_branch_runtime/malgo/Builtin.mlg.String#_0":
  %"appendString#_0" = load ptr, ptr %0, align 8
  %"String#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %d_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %2 = getelementptr { i8, { ptr } }, ptr %d_0, i64 0, i32 1
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = load ptr, ptr %"appendString#_0", align 8
  %7 = getelementptr { ptr, ptr }, ptr %"appendString#_0", i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, ptr %3)
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call ptr %12(ptr %10, ptr %5)
  %14 = load ptr, ptr %"String#_0", align 8
  %15 = getelementptr { ptr, ptr }, ptr %"String#_0", i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr %16(ptr %14, ptr %13)
  ret ptr %17
}

define internal noundef ptr @"test/testcases/malgo/Show.mlg.#fun_closure_3188"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(32) %0, ptr nocapture nofree readonly align 8 %1) {
"switch_branch_Tuple#_0":
  %"appendString#_0" = load ptr, ptr %0, align 8
  %"String#_addr_0" = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %showDictA_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %showDictA_0 = load ptr, ptr %showDictA_addr_0, align 8
  %showDictB_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 3
  %showDictB_0 = load ptr, ptr %showDictB_addr_0, align 8
  %2 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %6, i64 0, i32 1, i32 0
  store ptr @str3189, ptr %7, align 8
  %8 = tail call ptr @malgo_hash_table_get(ptr %showDictA_0, ptr noundef nonnull @str3190)
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = tail call ptr %11(ptr %9, ptr %3)
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { ptr } }, ptr %13, i64 0, i32 1, i32 0
  store ptr @str3191, ptr %14, align 8
  %15 = tail call ptr @malgo_hash_table_get(ptr %showDictB_0, ptr noundef nonnull @str3190)
  %16 = load ptr, ptr %15, align 8
  %17 = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  %18 = load ptr, ptr %17, align 8
  %19 = tail call ptr %18(ptr %16, ptr %5)
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { ptr } }, ptr %20, i64 0, i32 1, i32 0
  store ptr @str3192, ptr %21, align 8
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 24)
  store ptr %"appendString#_0", ptr %let_capture_0, align 8
  %"String#_1" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 1
  store ptr %"String#_0", ptr %"String#_1", align 8
  %d_0 = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 2
  store ptr %19, ptr %d_0, align 8
  store ptr %let_capture_0, ptr %22, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3193", ptr %let_func_0, align 8
  %23 = getelementptr i8, ptr %19, i64 8
  %.val = load ptr, ptr %23, align 8
  %.val1 = load ptr, ptr %21, align 8
  %24 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %.val, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %24, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %24, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3186", ptr %let_func_0.i, align 8
  %25 = tail call ptr @malgo_string_append(ptr %.val, ptr %.val1)
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %26, align 1
  %27 = getelementptr { i8, { ptr } }, ptr %26, i64 0, i32 1, i32 0
  store ptr %25, ptr %27, align 8
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2 = tail call ptr @malgo_malloc(i64 noundef 24)
  store ptr %"appendString#_0", ptr %let_capture_2, align 8
  %"String#_2" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_2, i64 0, i32 1
  store ptr %"String#_0", ptr %"String#_2", align 8
  %d_1 = getelementptr { ptr, ptr, ptr }, ptr %let_capture_2, i64 0, i32 2
  store ptr %13, ptr %d_1, align 8
  store ptr %let_capture_2, ptr %28, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %28, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3194", ptr %let_func_1, align 8
  %.val2 = load ptr, ptr %14, align 8
  %.val3 = load ptr, ptr %27, align 8
  %29 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i8 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %.val2, ptr %let_capture_0.i8, align 8
  store ptr %let_capture_0.i8, ptr %29, align 8
  %let_func_0.i9 = getelementptr { ptr, ptr }, ptr %29, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3186", ptr %let_func_0.i9, align 8
  %30 = tail call ptr @malgo_string_append(ptr %.val2, ptr %.val3)
  %31 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %31, align 1
  %32 = getelementptr { i8, { ptr } }, ptr %31, i64 0, i32 1, i32 0
  store ptr %30, ptr %32, align 8
  %33 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_4 = tail call ptr @malgo_malloc(i64 noundef 24)
  store ptr %"appendString#_0", ptr %let_capture_4, align 8
  %"String#_3" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_4, i64 0, i32 1
  store ptr %"String#_0", ptr %"String#_3", align 8
  %d_2 = getelementptr { ptr, ptr, ptr }, ptr %let_capture_4, i64 0, i32 2
  store ptr %12, ptr %d_2, align 8
  store ptr %let_capture_4, ptr %33, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %33, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3195", ptr %let_func_2, align 8
  %34 = getelementptr i8, ptr %12, i64 8
  %.val4 = load ptr, ptr %34, align 8
  %.val5 = load ptr, ptr %32, align 8
  %35 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i10 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %.val4, ptr %let_capture_0.i10, align 8
  store ptr %let_capture_0.i10, ptr %35, align 8
  %let_func_0.i11 = getelementptr { ptr, ptr }, ptr %35, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3186", ptr %let_func_0.i11, align 8
  %36 = tail call ptr @malgo_string_append(ptr %.val4, ptr %.val5)
  %37 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %37, align 1
  %38 = getelementptr { i8, { ptr } }, ptr %37, i64 0, i32 1, i32 0
  store ptr %36, ptr %38, align 8
  %39 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_6 = tail call ptr @malgo_malloc(i64 noundef 24)
  store ptr %"appendString#_0", ptr %let_capture_6, align 8
  %"String#_4" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_6, i64 0, i32 1
  store ptr %"String#_0", ptr %"String#_4", align 8
  %d_3 = getelementptr { ptr, ptr, ptr }, ptr %let_capture_6, i64 0, i32 2
  store ptr %6, ptr %d_3, align 8
  store ptr %let_capture_6, ptr %39, align 8
  %let_func_3 = getelementptr { ptr, ptr }, ptr %39, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3196", ptr %let_func_3, align 8
  %.val6 = load ptr, ptr %7, align 8
  %.val7 = load ptr, ptr %38, align 8
  %40 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i12 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %.val6, ptr %let_capture_0.i12, align 8
  store ptr %let_capture_0.i12, ptr %40, align 8
  %let_func_0.i13 = getelementptr { ptr, ptr }, ptr %40, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3186", ptr %let_func_0.i13, align 8
  %41 = tail call ptr @malgo_string_append(ptr %.val6, ptr %.val7)
  %42 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %42, align 1
  %43 = getelementptr { i8, { ptr } }, ptr %42, i64 0, i32 1, i32 0
  store ptr %41, ptr %43, align 8
  ret ptr %42
}

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_3201"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
"switch_branch_runtime/malgo/Builtin.mlg.String#_0":
  %cast_0 = load ptr, ptr %0, align 8
  %2 = tail call ptr @malgo_hash_table_get(ptr %cast_0, ptr noundef nonnull @str3190)
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %1)
  %7 = getelementptr { i8, { ptr } }, ptr %6, i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr @malgo_print_string(ptr %8)
  ret ptr %9
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %2, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#fun_closure_3206", ptr %fun_func_0.i, align 8
  %3 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %3, ptr noundef nonnull @str3190, ptr noundef nonnull %2)
  store ptr %3, ptr @"test/testcases/malgo/Show.mlg.showInt32", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = load ptr, ptr @"test/testcases/malgo/Show.mlg.showInt32", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 32)
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %"appendString#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.appendString#", ptr %"appendString#_func_0.i.i", align 8
  store ptr %7, ptr %fun_capture_0.i.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %"String#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0.i.i", align 8
  %"String#_0.i.i" = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i.i, i64 0, i32 1
  store ptr %8, ptr %"String#_0.i.i", align 8
  %showDictA_0.i.i = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i.i, i64 0, i32 2
  store ptr %5, ptr %showDictA_0.i.i, align 8
  %showDictB_0.i.i = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i.i, i64 0, i32 3
  store ptr %5, ptr %showDictB_0.i.i, align 8
  store ptr %fun_capture_0.i.i, ptr %6, align 8
  %fun_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#fun_closure_3188", ptr %fun_func_0.i.i, align 8
  %9 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %9, ptr noundef nonnull @str3190, ptr noundef nonnull %6)
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %9, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %10, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3201", ptr %let_func_0.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { i32 } }, ptr %11, i64 0, i32 1, i32 0
  store i32 1, ptr %12, align 4
  %13 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { i32 } }, ptr %13, i64 0, i32 1, i32 0
  store i32 2, ptr %14, align 4
  %15 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr, ptr } }, ptr %15, i64 0, i32 1, i32 0
  store ptr %11, ptr %16, align 8
  %17 = getelementptr { i8, { ptr, ptr } }, ptr %15, i64 0, i32 1, i32 1
  store ptr %13, ptr %17, align 8
  %18 = load ptr, ptr %10, align 8
  %19 = load ptr, ptr %let_func_0.i, align 8
  %20 = tail call ptr %19(ptr %18, ptr nonnull %15)
  ret i32 0
}

define internal noundef ptr @"test/testcases/malgo/Show.mlg.#fun_closure_3206"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %1) {
"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0":
  %2 = getelementptr { i8, { i32 } }, ptr %1, i64 0, i32 1
  %3 = load i32, ptr %2, align 4
  %4 = tail call ptr @malgo_int32_t_to_string(i32 %3)
  %5 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %4, ptr %6, align 8
  ret ptr %5
}
