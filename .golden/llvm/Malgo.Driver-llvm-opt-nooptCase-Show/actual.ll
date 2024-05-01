; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/Show.ll'
source_filename = "test/testcases/malgo/Show.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@"test/testcases/malgo/Show.mlg.showInt32" = local_unnamed_addr global ptr undef
@str282 = unnamed_addr constant [1 x i8] zeroinitializer
@str300 = unnamed_addr constant [5 x i8] c"show\00"
@str354 = unnamed_addr constant [2 x i8] c"(\00"
@str355 = unnamed_addr constant [3 x i8] c", \00"
@str356 = unnamed_addr constant [2 x i8] c")\00"
@str458 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_string_append(ptr, ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"test/testcases/malgo/Show.mlg.#let_closure_284"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 8 %1) {
  %"string#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"string#_0", i64 8
  %"string#_0.val" = load ptr, ptr %3, align 8
  %4 = getelementptr i8, ptr %1, i64 8
  %.val = load ptr, ptr %4, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"string#_0.val", ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_421", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 8
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_383", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load ptr, ptr %let_capture_0.i.i.i.i, align 8
  %7 = tail call ptr @malgo_string_append(ptr %p_0.i.i.i.i, ptr %.val)
  %8 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %8, i64 0, i32 1, i32 0
  store ptr %7, ptr %9, align 8
  ret ptr %8
}

define internal ptr @"test/testcases/malgo/Show.mlg.#fun_closure_299"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readonly align 8 %1) {
  %showDictB_0 = load ptr, ptr %0, align 8
  %showDictA_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %showDictA_0 = load ptr, ptr %showDictA_addr_0, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val = load ptr, ptr %3, align 8
  %4 = getelementptr i8, ptr %1, i64 16
  %.val1 = load ptr, ptr %4, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr @str354, ptr %6, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %7, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_303", ptr %let_func_0.i.i, align 8
  %8 = tail call ptr @malgo_hash_table_get(ptr %showDictA_0, ptr noundef nonnull @str300)
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = tail call ptr %11(ptr %9, ptr %.val)
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %12, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %13, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_303", ptr %let_func_0.i2.i, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %14, align 1
  %15 = getelementptr { i8, { ptr } }, ptr %14, i64 0, i32 1, i32 0
  store ptr @str355, ptr %15, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %14, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %16, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_303", ptr %let_func_0.i4.i, align 8
  %17 = tail call ptr @malgo_hash_table_get(ptr %showDictB_0, ptr noundef nonnull @str300)
  %18 = load ptr, ptr %17, align 8
  %19 = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  %20 = load ptr, ptr %19, align 8
  %21 = tail call ptr %20(ptr %18, ptr %.val1)
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i5.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %21, ptr %let_capture_0.i5.i, align 8
  store ptr %let_capture_0.i5.i, ptr %22, align 8
  %let_func_0.i6.i = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_303", ptr %let_func_0.i6.i, align 8
  %23 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %23, align 1
  %24 = getelementptr { i8, { ptr } }, ptr %23, i64 0, i32 1, i32 0
  store ptr @str356, ptr %24, align 8
  %25 = load ptr, ptr %22, align 8
  %26 = load ptr, ptr %let_func_0.i6.i, align 8
  %27 = tail call ptr %26(ptr %25, ptr nonnull %23)
  %28 = load ptr, ptr %16, align 8
  %29 = load ptr, ptr %let_func_0.i4.i, align 8
  %30 = tail call ptr %29(ptr %28, ptr %27)
  %31 = load ptr, ptr %13, align 8
  %32 = load ptr, ptr %let_func_0.i2.i, align 8
  %33 = tail call ptr %32(ptr %31, ptr %30)
  %34 = load ptr, ptr %7, align 8
  %35 = load ptr, ptr %let_func_0.i.i, align 8
  %36 = tail call ptr %35(ptr %34, ptr %33)
  ret ptr %36
}

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr

define internal noundef ptr @"test/testcases/malgo/Show.mlg.#let_closure_303"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_284", ptr %let_func_0.i.i, align 8
  %"string#_0.i.i" = load ptr, ptr %let_capture_0.i.i, align 8
  %4 = getelementptr i8, ptr %"string#_0.i.i", i64 8
  %"string#_0.val.i.i" = load ptr, ptr %4, align 8
  %5 = getelementptr i8, ptr %1, i64 8
  %.val.i.i = load ptr, ptr %5, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"string#_0.val.i.i", ptr %let_capture_0.i.i.i.i, align 8
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_421", ptr %let_func_0.i.i.i.i, align 8
  %x_0.i.i.i.i = load ptr, ptr %let_capture_0.i.i.i.i, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0.i.i.i.i, ptr %let_capture_0.i.i.i.i.i.i, align 8
  store ptr %let_capture_0.i.i.i.i.i.i, ptr %7, align 8
  %let_func_0.i.i.i.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_383", ptr %let_func_0.i.i.i.i.i.i, align 8
  %p_0.i.i.i.i.i.i = load ptr, ptr %let_capture_0.i.i.i.i.i.i, align 8
  %8 = tail call ptr @malgo_string_append(ptr %p_0.i.i.i.i.i.i, ptr %.val.i.i)
  %9 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %8, ptr %10, align 8
  ret ptr %9
}

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_321"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %showDictA_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %1, ptr %fun_capture_0.i, align 8
  %showDictA_0.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %showDictA_0, ptr %showDictA_0.i, align 8
  store ptr %fun_capture_0.i, ptr %3, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#fun_closure_299", ptr %fun_func_0.i, align 8
  %4 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %4, ptr noundef nonnull @str300, ptr noundef nonnull %3)
  ret ptr %4
}

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_331"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %showDict_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_hash_table_get(ptr %showDict_0, ptr noundef nonnull @str300)
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr %1)
  %8 = getelementptr i8, ptr %7, i64 8
  %.val.i = load ptr, ptr %8, align 8
  %9 = tail call ptr @malgo_print_string(ptr %.val.i)
  ret ptr %9
}

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_383"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_string_append(ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_421"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_383", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %4 = tail call ptr @malgo_string_append(ptr %p_0.i.i, ptr %1)
  ret ptr %4
}

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @"runtime/malgo/Builtin.mlg.undefined", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %4, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#fun_closure_482", ptr %fun_func_0.i, align 8
  %5 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %5, ptr noundef nonnull @str300, ptr noundef nonnull %4)
  store ptr %5, ptr @"test/testcases/malgo/Show.mlg.showInt32", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %6, align 1
  %7 = load ptr, ptr @"test/testcases/malgo/Show.mlg.showInt32", align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %7, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %8, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_321", ptr %let_func_0.i.i, align 8
  %9 = load ptr, ptr @"test/testcases/malgo/Show.mlg.showInt32", align 8
  %showDictA_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %9, ptr %fun_capture_0.i.i.i, align 8
  %showDictA_0.i.i.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i.i.i, i64 0, i32 1
  store ptr %showDictA_0.i.i, ptr %showDictA_0.i.i.i, align 8
  store ptr %fun_capture_0.i.i.i, ptr %10, align 8
  %fun_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#fun_closure_299", ptr %fun_func_0.i.i.i, align 8
  %11 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %11, ptr noundef nonnull @str300, ptr noundef nonnull %10)
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %11, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %12, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_331", ptr %let_func_0.i2.i, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { i32 } }, ptr %13, i64 0, i32 1, i32 0
  store i32 1, ptr %14, align 4
  %15 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i32 } }, ptr %15, i64 0, i32 1, i32 0
  store i32 2, ptr %16, align 4
  %17 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %17, align 1
  %18 = getelementptr { i8, { ptr, ptr } }, ptr %17, i64 0, i32 1, i32 0
  store ptr %13, ptr %18, align 8
  %19 = getelementptr { i8, { ptr, ptr } }, ptr %17, i64 0, i32 1, i32 1
  store ptr %15, ptr %19, align 8
  %20 = load ptr, ptr %12, align 8
  %21 = load ptr, ptr %let_func_0.i2.i, align 8
  %22 = tail call ptr %21(ptr %20, ptr nonnull %17)
  ret i32 0
}

define internal noundef ptr @"test/testcases/malgo/Show.mlg.#fun_closure_482"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %1) {
  %3 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %3, align 4
  %4 = tail call ptr @malgo_int32_t_to_string(i32 %.val)
  %5 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %4, ptr %6, align 8
  ret ptr %5
}
