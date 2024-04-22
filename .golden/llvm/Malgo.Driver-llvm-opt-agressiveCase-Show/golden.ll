; ModuleID = './test/tmp/malgo_test/aggressive/Show.ll'
source_filename = "./test/testcases/malgo/Show.mlg"

@Show.showInt32 = local_unnamed_addr global ptr undef
@str3849 = unnamed_addr constant [2 x i8] c"(\00"
@str3850 = unnamed_addr constant [5 x i8] c"show\00"
@str3851 = unnamed_addr constant [3 x i8] c", \00"
@str3852 = unnamed_addr constant [2 x i8] c")\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_string_append(ptr, ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

define internal noundef ptr @"Show.#let_closure_3853"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 8 %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %d_0, i64 8
  %d_0.val = load ptr, ptr %3, align 8
  %4 = getelementptr i8, ptr %1, i64 8
  %.val = load ptr, ptr %4, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %d_0.val, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Show.#let_closure_3864", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %6 = tail call ptr @malgo_string_append(ptr %x_0.i.i, ptr %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %6, ptr %8, align 8
  ret ptr %7
}

define internal noundef ptr @"Show.#let_closure_3854"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 8 %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %d_0, i64 8
  %d_0.val = load ptr, ptr %3, align 8
  %4 = getelementptr i8, ptr %1, i64 8
  %.val = load ptr, ptr %4, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %d_0.val, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Show.#let_closure_3864", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %6 = tail call ptr @malgo_string_append(ptr %x_0.i.i, ptr %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %6, ptr %8, align 8
  ret ptr %7
}

define internal noundef ptr @"Show.#let_closure_3855"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 8 %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %d_0, i64 8
  %d_0.val = load ptr, ptr %3, align 8
  %4 = getelementptr i8, ptr %1, i64 8
  %.val = load ptr, ptr %4, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %d_0.val, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Show.#let_closure_3864", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %6 = tail call ptr @malgo_string_append(ptr %x_0.i.i, ptr %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %6, ptr %8, align 8
  ret ptr %7
}

define internal noundef ptr @"Show.#let_closure_3856"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 8 %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %d_0, i64 8
  %d_0.val = load ptr, ptr %3, align 8
  %4 = getelementptr i8, ptr %1, i64 8
  %.val = load ptr, ptr %4, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %d_0.val, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Show.#let_closure_3864", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %6 = tail call ptr @malgo_string_append(ptr %x_0.i.i, ptr %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %6, ptr %8, align 8
  ret ptr %7
}

define internal ptr @"Show.#let_closure_3860"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_string_append(ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Show.#let_closure_3862"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %cast_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_hash_table_get(ptr %cast_0, ptr noundef nonnull @str3850)
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr %1)
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call ptr @malgo_print_string(ptr %9)
  ret ptr %10
}

define internal noundef ptr @"Show.#fun_closure_3863"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readonly align 8 %1) {
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
  store ptr @str3849, ptr %6, align 8
  %7 = tail call ptr @malgo_hash_table_get(ptr %showDictA_0, ptr noundef nonnull @str3850)
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, ptr %.val)
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr @str3851, ptr %13, align 8
  %14 = tail call ptr @malgo_hash_table_get(ptr %showDictB_0, ptr noundef nonnull @str3850)
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = tail call ptr %17(ptr %15, ptr %.val1)
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { ptr } }, ptr %19, i64 0, i32 1, i32 0
  store ptr @str3852, ptr %20, align 8
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %18, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %21, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %21, i64 0, i32 1
  store ptr @"Show.#let_closure_3853", ptr %let_func_0.i, align 8
  %22 = getelementptr i8, ptr %18, i64 8
  %.val6.i = load ptr, ptr %22, align 8
  %.val7.i = load ptr, ptr %20, align 8
  %23 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %.val6.i, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %23, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %23, i64 0, i32 1
  store ptr @"Show.#let_closure_3860", ptr %let_func_0.i.i, align 8
  %24 = tail call ptr @malgo_string_append(ptr %.val6.i, ptr %.val7.i)
  %25 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %25, align 1
  %26 = getelementptr { i8, { ptr } }, ptr %25, i64 0, i32 1, i32 0
  store ptr %24, ptr %26, align 8
  %27 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %12, ptr %let_capture_2.i, align 8
  store ptr %let_capture_2.i, ptr %27, align 8
  %let_func_1.i = getelementptr { ptr, ptr }, ptr %27, i64 0, i32 1
  store ptr @"Show.#let_closure_3854", ptr %let_func_1.i, align 8
  %.val4.i = load ptr, ptr %13, align 8
  %.val5.i = load ptr, ptr %26, align 8
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i8.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %.val4.i, ptr %let_capture_0.i8.i, align 8
  store ptr %let_capture_0.i8.i, ptr %28, align 8
  %let_func_0.i9.i = getelementptr { ptr, ptr }, ptr %28, i64 0, i32 1
  store ptr @"Show.#let_closure_3860", ptr %let_func_0.i9.i, align 8
  %29 = tail call ptr @malgo_string_append(ptr %.val4.i, ptr %.val5.i)
  %30 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %30, align 1
  %31 = getelementptr { i8, { ptr } }, ptr %30, i64 0, i32 1, i32 0
  store ptr %29, ptr %31, align 8
  %32 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_4.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %11, ptr %let_capture_4.i, align 8
  store ptr %let_capture_4.i, ptr %32, align 8
  %let_func_2.i = getelementptr { ptr, ptr }, ptr %32, i64 0, i32 1
  store ptr @"Show.#let_closure_3855", ptr %let_func_2.i, align 8
  %33 = getelementptr i8, ptr %11, i64 8
  %.val2.i = load ptr, ptr %33, align 8
  %.val3.i = load ptr, ptr %31, align 8
  %34 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i10.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %.val2.i, ptr %let_capture_0.i10.i, align 8
  store ptr %let_capture_0.i10.i, ptr %34, align 8
  %let_func_0.i11.i = getelementptr { ptr, ptr }, ptr %34, i64 0, i32 1
  store ptr @"Show.#let_closure_3860", ptr %let_func_0.i11.i, align 8
  %35 = tail call ptr @malgo_string_append(ptr %.val2.i, ptr %.val3.i)
  %36 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %36, align 1
  %37 = getelementptr { i8, { ptr } }, ptr %36, i64 0, i32 1, i32 0
  store ptr %35, ptr %37, align 8
  %38 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_6.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_6.i, align 8
  store ptr %let_capture_6.i, ptr %38, align 8
  %let_func_3.i = getelementptr { ptr, ptr }, ptr %38, i64 0, i32 1
  store ptr @"Show.#let_closure_3856", ptr %let_func_3.i, align 8
  %.val.i = load ptr, ptr %6, align 8
  %.val1.i = load ptr, ptr %37, align 8
  %39 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i12.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %.val.i, ptr %let_capture_0.i12.i, align 8
  store ptr %let_capture_0.i12.i, ptr %39, align 8
  %let_func_0.i13.i = getelementptr { ptr, ptr }, ptr %39, i64 0, i32 1
  store ptr @"Show.#let_closure_3860", ptr %let_func_0.i13.i, align 8
  %40 = tail call ptr @malgo_string_append(ptr %.val.i, ptr %.val1.i)
  %41 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %41, align 1
  %42 = getelementptr { i8, { ptr } }, ptr %41, i64 0, i32 1, i32 0
  store ptr %40, ptr %42, align 8
  ret ptr %41
}

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr

define internal ptr @"Show.#let_closure_3864"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_string_append(ptr %x_0, ptr %1)
  ret ptr %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %2, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Show.#fun_closure_3869", ptr %fun_func_0.i, align 8
  %3 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %3, ptr noundef nonnull @str3850, ptr noundef nonnull %2)
  store ptr %3, ptr @Show.showInt32, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = load ptr, ptr @Show.showInt32, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %5, ptr %fun_capture_0.i.i, align 8
  %showDictA_0.i.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i.i, i64 0, i32 1
  store ptr %5, ptr %showDictA_0.i.i, align 8
  store ptr %fun_capture_0.i.i, ptr %6, align 8
  %fun_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Show.#fun_closure_3863", ptr %fun_func_0.i.i, align 8
  %7 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %7, ptr noundef nonnull @str3850, ptr noundef nonnull %6)
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %7, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %8, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"Show.#let_closure_3862", ptr %let_func_0.i, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { i32 } }, ptr %9, i64 0, i32 1, i32 0
  store i32 1, ptr %10, align 4
  %11 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { i32 } }, ptr %11, i64 0, i32 1, i32 0
  store i32 2, ptr %12, align 4
  %13 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { ptr, ptr } }, ptr %13, i64 0, i32 1, i32 0
  store ptr %9, ptr %14, align 8
  %15 = getelementptr { i8, { ptr, ptr } }, ptr %13, i64 0, i32 1, i32 1
  store ptr %11, ptr %15, align 8
  %16 = load ptr, ptr %8, align 8
  %17 = load ptr, ptr %let_func_0.i, align 8
  %18 = tail call ptr %17(ptr %16, ptr nonnull %13)
  ret i32 0
}

define internal noundef ptr @"Show.#fun_closure_3869"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %1) {
"switch_branch_Builtin.Int32#_0":
  %2 = getelementptr { i8, { i32 } }, ptr %1, i64 0, i32 1
  %3 = load i32, ptr %2, align 4
  %4 = tail call ptr @malgo_int32_t_to_string(i32 %3)
  %5 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %4, ptr %6, align 8
  ret ptr %5
}
