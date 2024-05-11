; ModuleID = '.malgo-work/test/testcases/malgo/Show.ll'
source_filename = "test/testcases/malgo/Show.mlg"

@_M9showInt3241test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External = local_unnamed_addr global ptr undef
@str3966 = unnamed_addr constant [5 x i8] c"show\00"
@str3968 = unnamed_addr constant [2 x i8] c"(\00"
@str3969 = unnamed_addr constant [3 x i8] c", \00"
@str3970 = unnamed_addr constant [2 x i8] c")\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_string_append(ptr, ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3963(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %cast_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_hash_table_get(ptr %cast_0, ptr noundef nonnull @str3966)
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr %1)
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call ptr @malgo_print_string(ptr %9)
  ret ptr %10
}

define internal ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3964(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %showDictA_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %showDictA_0, ptr %fun_capture_0.i, align 8
  %showDictB_0.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %1, ptr %showDictB_0.i, align 8
  store ptr %fun_capture_0.i, ptr %3, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @_M14fun_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3967, ptr %fun_func_0.i, align 8
  %4 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %4, ptr noundef nonnull @str3966, ptr noundef nonnull %3)
  ret ptr %4
}

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

define internal noundef ptr @_M14fun_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3967(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readonly align 8 %1) {
  %showDictA_0 = load ptr, ptr %0, align 8
  %showDictB_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %showDictB_0 = load ptr, ptr %showDictB_addr_0, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val = load ptr, ptr %3, align 8
  %4 = getelementptr i8, ptr %1, i64 16
  %.val1 = load ptr, ptr %4, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr @str3968, ptr %6, align 8
  %7 = tail call ptr @malgo_hash_table_get(ptr %showDictA_0, ptr noundef nonnull @str3966)
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, ptr %.val)
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr @str3969, ptr %13, align 8
  %14 = tail call ptr @malgo_hash_table_get(ptr %showDictB_0, ptr noundef nonnull @str3966)
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = tail call ptr %17(ptr %15, ptr %.val1)
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { ptr } }, ptr %19, i64 0, i32 1, i32 0
  store ptr @str3970, ptr %20, align 8
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %18, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %21, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %21, i64 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3972, ptr %let_func_0.i.i, align 8
  %"string#_0.i.i" = load ptr, ptr %let_capture_0.i.i, align 8
  %22 = getelementptr i8, ptr %"string#_0.i.i", i64 8
  %"string#_0.val.i.i" = load ptr, ptr %22, align 8
  %.val.i.i = load ptr, ptr %20, align 8
  %23 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"string#_0.val.i.i", ptr %let_capture_0.i.i.i, align 8
  store ptr %let_capture_0.i.i.i, ptr %23, align 8
  %let_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %23, i64 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3973, ptr %let_func_0.i.i.i, align 8
  %24 = tail call ptr @malgo_string_append(ptr %"string#_0.val.i.i", ptr %.val.i.i)
  %25 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %25, align 1
  %26 = getelementptr { i8, { ptr } }, ptr %25, i64 0, i32 1, i32 0
  store ptr %24, ptr %26, align 8
  %27 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %12, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %27, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %27, i64 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3972, ptr %let_func_0.i2.i, align 8
  %"string#_0.i1.i" = load ptr, ptr %let_capture_0.i1.i, align 8
  %28 = getelementptr i8, ptr %"string#_0.i1.i", i64 8
  %"string#_0.val.i2.i" = load ptr, ptr %28, align 8
  %.val.i3.i = load ptr, ptr %26, align 8
  %29 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i4.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"string#_0.val.i2.i", ptr %let_capture_0.i.i4.i, align 8
  store ptr %let_capture_0.i.i4.i, ptr %29, align 8
  %let_func_0.i.i5.i = getelementptr { ptr, ptr }, ptr %29, i64 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3973, ptr %let_func_0.i.i5.i, align 8
  %30 = tail call ptr @malgo_string_append(ptr %"string#_0.val.i2.i", ptr %.val.i3.i)
  %31 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %31, align 1
  %32 = getelementptr { i8, { ptr } }, ptr %31, i64 0, i32 1, i32 0
  store ptr %30, ptr %32, align 8
  %33 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %11, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %33, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %33, i64 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3972, ptr %let_func_0.i4.i, align 8
  %"string#_0.i6.i" = load ptr, ptr %let_capture_0.i3.i, align 8
  %34 = getelementptr i8, ptr %"string#_0.i6.i", i64 8
  %"string#_0.val.i7.i" = load ptr, ptr %34, align 8
  %.val.i8.i = load ptr, ptr %32, align 8
  %35 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i9.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"string#_0.val.i7.i", ptr %let_capture_0.i.i9.i, align 8
  store ptr %let_capture_0.i.i9.i, ptr %35, align 8
  %let_func_0.i.i10.i = getelementptr { ptr, ptr }, ptr %35, i64 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3973, ptr %let_func_0.i.i10.i, align 8
  %36 = tail call ptr @malgo_string_append(ptr %"string#_0.val.i7.i", ptr %.val.i8.i)
  %37 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %37, align 1
  %38 = getelementptr { i8, { ptr } }, ptr %37, i64 0, i32 1, i32 0
  store ptr %36, ptr %38, align 8
  %39 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i5.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_0.i5.i, align 8
  store ptr %let_capture_0.i5.i, ptr %39, align 8
  %let_func_0.i6.i = getelementptr { ptr, ptr }, ptr %39, i64 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3972, ptr %let_func_0.i6.i, align 8
  %"string#_0.i11.i" = load ptr, ptr %let_capture_0.i5.i, align 8
  %40 = getelementptr i8, ptr %"string#_0.i11.i", i64 8
  %"string#_0.val.i12.i" = load ptr, ptr %40, align 8
  %.val.i13.i = load ptr, ptr %38, align 8
  %41 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i14.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"string#_0.val.i12.i", ptr %let_capture_0.i.i14.i, align 8
  store ptr %let_capture_0.i.i14.i, ptr %41, align 8
  %let_func_0.i.i15.i = getelementptr { ptr, ptr }, ptr %41, i64 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3973, ptr %let_func_0.i.i15.i, align 8
  %42 = tail call ptr @malgo_string_append(ptr %"string#_0.val.i12.i", ptr %.val.i13.i)
  %43 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %43, align 1
  %44 = getelementptr { i8, { ptr } }, ptr %43, i64 0, i32 1, i32 0
  store ptr %42, ptr %44, align 8
  ret ptr %43
}

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr

define internal noundef ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3972(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 8 %1) {
  %"string#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"string#_0", i64 8
  %"string#_0.val" = load ptr, ptr %3, align 8
  %4 = getelementptr i8, ptr %1, i64 8
  %.val = load ptr, ptr %4, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"string#_0.val", ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3973, ptr %let_func_0.i, align 8
  %6 = tail call ptr @malgo_string_append(ptr %"string#_0.val", ptr %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %6, ptr %8, align 8
  ret ptr %7
}

define internal ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3973(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_string_append(ptr %p_0, ptr %1)
  ret ptr %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %2, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @_M14fun_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3978, ptr %fun_func_0.i, align 8
  %3 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %3, ptr noundef nonnull @str3966, ptr noundef nonnull %2)
  store ptr %3, ptr @_M9showInt3241test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = load ptr, ptr @_M9showInt3241test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %6, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3964, ptr %let_func_0.i.i, align 8
  %7 = load ptr, ptr @_M9showInt3241test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External, align 8
  %showDictA_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %showDictA_0.i.i, ptr %fun_capture_0.i.i.i, align 8
  %showDictB_0.i.i.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i.i.i, i64 0, i32 1
  store ptr %7, ptr %showDictB_0.i.i.i, align 8
  store ptr %fun_capture_0.i.i.i, ptr %8, align 8
  %fun_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @_M14fun_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3967, ptr %fun_func_0.i.i.i, align 8
  %9 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %9, ptr noundef nonnull @str3966, ptr noundef nonnull %8)
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %9, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %10, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3963, ptr %let_func_0.i, align 8
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

define internal noundef ptr @_M14fun_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3978(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %1) {
"switch_branch_Int32#_0":
  %2 = getelementptr { i8, { i32 } }, ptr %1, i64 0, i32 1
  %3 = load i32, ptr %2, align 4
  %4 = tail call ptr @malgo_int32_t_to_string(i32 %3)
  %5 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %4, ptr %6, align 8
  ret ptr %5
}
