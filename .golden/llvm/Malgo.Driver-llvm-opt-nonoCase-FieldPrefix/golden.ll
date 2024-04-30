; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/FieldPrefix.ll'
source_filename = "test/testcases/malgo/FieldPrefix.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef
@FieldPrefix.zero3D = local_unnamed_addr global ptr undef
@FieldPrefix.zero2D = local_unnamed_addr global ptr undef
@str90 = unnamed_addr constant [1 x i8] zeroinitializer
@str110 = unnamed_addr constant [10 x i8] c"no branch\00"
@str287 = unnamed_addr constant [2 x i8] c"x\00"
@str288 = unnamed_addr constant [2 x i8] c"y\00"
@str289 = unnamed_addr constant [3 x i8] c", \00"
@str294 = unnamed_addr constant [2 x i8] c"z\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"Builtin.Int32#"(ptr nocapture nofree readnone %0, i32 %"Builtin.$p_1792_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %2, i64 0, i32 1, i32 0
  store i32 %"Builtin.$p_1792_0", ptr %3, align 4
  ret ptr %2
}

define internal noundef ptr @"Builtin.Float#"(ptr nocapture nofree readnone %0, float %"Builtin.$p_1796_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { float } }, ptr %2, i64 0, i32 1, i32 0
  store float %"Builtin.$p_1796_0", ptr %3, align 4
  ret ptr %2
}

define internal noundef ptr @"Builtin.String#"(ptr nocapture nofree readnone %0, ptr nofree %"Builtin.$p_1802_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"Builtin.$p_1802_0", ptr %3, align 8
  ret ptr %2
}

define internal ptr @Builtin.malgo_unsafe_cast(ptr nocapture nofree readnone %0, ptr %"Builtin.$p_1807_0") {
  %2 = tail call ptr @malgo_unsafe_cast(ptr %"Builtin.$p_1807_0")
  ret ptr %2
}

define internal ptr @Builtin.malgo_int32_t_to_string(ptr nocapture nofree readnone %0, i32 %"Builtin.$p_2155_0") {
  %2 = tail call ptr @malgo_int32_t_to_string(i32 %"Builtin.$p_2155_0")
  ret ptr %2
}

define internal ptr @Builtin.malgo_print_string(ptr nocapture nofree readnone %0, ptr %"Builtin.$p_2163_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"Builtin.$p_2163_0")
  ret ptr %2
}

define internal ptr @"Builtin.toStringInt32#"(ptr nocapture nofree readnone %0, i32 %"Builtin.$x_2179_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_int32_t_to_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @Builtin.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_0, align 8
  %3 = tail call ptr @malgo_int32_t_to_string(i32 %"Builtin.$x_2179_0")
  ret ptr %3
}

define internal noundef ptr @Builtin.toStringInt32(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %"Builtin.$int32#_2181_0") {
"switch_branch_Builtin.Int32#_0":
  %1 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_2181_0", i64 0, i32 1
  %2 = load i32, ptr %1, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"toStringInt32#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Builtin.toStringInt32#", ptr %"toStringInt32#_func_0", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_int32_t_to_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @Builtin.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_0.i, align 8
  %5 = tail call ptr @malgo_int32_t_to_string(i32 %2)
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_0", align 8
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %5, ptr %8, align 8
  ret ptr %7
}

define internal ptr @"Builtin.printString#"(ptr nocapture nofree readnone %0, ptr %"Builtin.$x_2399_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_print_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0, align 8
  %3 = tail call ptr @malgo_print_string(ptr %"Builtin.$x_2399_0")
  ret ptr %3
}

define internal ptr @Builtin.printString(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"Builtin.$string#_2401_0") {
"switch_branch_Builtin.String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_2401_0", i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"printString#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_0", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_print_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0.i, align 8
  %5 = tail call ptr @malgo_print_string(ptr %2)
  ret ptr %5
}

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

define internal ptr @FieldPrefix.print2D(ptr nocapture nofree readnone %0, ptr %"FieldPrefix.$record_67_0") {
  %2 = tail call ptr @malgo_hash_table_get(ptr %"FieldPrefix.$record_67_0", ptr noundef nonnull @str287)
  %3 = tail call ptr @malgo_hash_table_get(ptr %"FieldPrefix.$record_67_0", ptr noundef nonnull @str288)
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %toStringInt32_func_0 = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @Builtin.toStringInt32, ptr %toStringInt32_func_0, align 8
  %5 = getelementptr { i8, { i32 } }, ptr %2, i64 0, i32 1
  %6 = load i32, ptr %5, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %"toStringInt32#_func_0.i" = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"Builtin.toStringInt32#", ptr %"toStringInt32#_func_0.i", align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %malgo_int32_t_to_string_func_0.i.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @Builtin.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_0.i.i, align 8
  %9 = tail call ptr @malgo_int32_t_to_string(i32 %6)
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %"String#_func_0.i" = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_0.i", align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr %9, ptr %12, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %printString_func_0 = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @Builtin.printString, ptr %printString_func_0, align 8
  %14 = load ptr, ptr %12, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %"printString#_func_0.i" = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_0.i", align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %16, align 8
  %malgo_print_string_func_0.i.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0.i.i, align 8
  %17 = tail call ptr @malgo_print_string(ptr %14)
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %18, align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_0", align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { ptr } }, ptr %19, i64 0, i32 1, i32 0
  store ptr @str289, ptr %20, align 8
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %21, align 8
  %printString_func_1 = getelementptr { ptr, ptr }, ptr %21, i64 0, i32 1
  store ptr @Builtin.printString, ptr %printString_func_1, align 8
  %22 = load ptr, ptr %20, align 8
  %23 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %23, align 8
  %"printString#_func_0.i1" = getelementptr { ptr, ptr }, ptr %23, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_0.i1", align 8
  %24 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %24, align 8
  %malgo_print_string_func_0.i.i2 = getelementptr { ptr, ptr }, ptr %24, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0.i.i2, align 8
  %25 = tail call ptr @malgo_print_string(ptr %22)
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %26, align 8
  %toStringInt32_func_1 = getelementptr { ptr, ptr }, ptr %26, i64 0, i32 1
  store ptr @Builtin.toStringInt32, ptr %toStringInt32_func_1, align 8
  %27 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1
  %28 = load i32, ptr %27, align 4
  %29 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %29, align 8
  %"toStringInt32#_func_0.i3" = getelementptr { ptr, ptr }, ptr %29, i64 0, i32 1
  store ptr @"Builtin.toStringInt32#", ptr %"toStringInt32#_func_0.i3", align 8
  %30 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %30, align 8
  %malgo_int32_t_to_string_func_0.i.i4 = getelementptr { ptr, ptr }, ptr %30, i64 0, i32 1
  store ptr @Builtin.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_0.i.i4, align 8
  %31 = tail call ptr @malgo_int32_t_to_string(i32 %28)
  %32 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %32, align 8
  %"String#_func_0.i5" = getelementptr { ptr, ptr }, ptr %32, i64 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_0.i5", align 8
  %33 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %33, align 1
  %34 = getelementptr { i8, { ptr } }, ptr %33, i64 0, i32 1, i32 0
  store ptr %31, ptr %34, align 8
  %35 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %35, align 8
  %printString_func_2 = getelementptr { ptr, ptr }, ptr %35, i64 0, i32 1
  store ptr @Builtin.printString, ptr %printString_func_2, align 8
  %36 = load ptr, ptr %34, align 8
  %37 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %37, align 8
  %"printString#_func_0.i6" = getelementptr { ptr, ptr }, ptr %37, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_0.i6", align 8
  %38 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %38, align 8
  %malgo_print_string_func_0.i.i7 = getelementptr { ptr, ptr }, ptr %38, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0.i.i7, align 8
  %39 = tail call ptr @malgo_print_string(ptr %36)
  ret ptr %39
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_unsafe_cast_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Builtin.malgo_unsafe_cast, ptr %malgo_unsafe_cast_func_0.i, align 8
  %4 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %4, ptr @Builtin.undefined, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"Float#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Builtin.Float#", ptr %"Float#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { float } }, ptr %6, i64 0, i32 1, i32 0
  store float 0.000000e+00, ptr %7, align 4
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %"Float#_func_1.i" = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"Builtin.Float#", ptr %"Float#_func_1.i", align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { float } }, ptr %9, i64 0, i32 1, i32 0
  store float 0.000000e+00, ptr %10, align 4
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %"Float#_func_2.i" = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"Builtin.Float#", ptr %"Float#_func_2.i", align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { float } }, ptr %12, i64 0, i32 1, i32 0
  store float 0.000000e+00, ptr %13, align 4
  %14 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %14, ptr noundef nonnull @str287, ptr noundef nonnull %6)
  tail call void @malgo_hash_table_insert(ptr %14, ptr noundef nonnull @str288, ptr noundef nonnull %9)
  tail call void @malgo_hash_table_insert(ptr %14, ptr noundef nonnull @str294, ptr noundef nonnull %12)
  store ptr %14, ptr @FieldPrefix.zero3D, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %"Int32#_func_0.i" = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"Builtin.Int32#", ptr %"Int32#_func_0.i", align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %16, align 1
  %17 = getelementptr { i8, { i32 } }, ptr %16, i64 0, i32 1, i32 0
  store i32 0, ptr %17, align 4
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %18, align 8
  %"Int32#_func_1.i" = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"Builtin.Int32#", ptr %"Int32#_func_1.i", align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { i32 } }, ptr %19, i64 0, i32 1, i32 0
  store i32 0, ptr %20, align 4
  %21 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %21, ptr noundef nonnull @str287, ptr noundef nonnull %16)
  tail call void @malgo_hash_table_insert(ptr %21, ptr noundef nonnull @str288, ptr noundef nonnull %19)
  store ptr %21, ptr @FieldPrefix.zero2D, align 8
  %22 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %22, align 1
  %23 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %23, align 8
  %print2D_func_0.i = getelementptr { ptr, ptr }, ptr %23, i64 0, i32 1
  store ptr @FieldPrefix.print2D, ptr %print2D_func_0.i, align 8
  %24 = load ptr, ptr @FieldPrefix.zero2D, align 8
  %25 = tail call ptr @FieldPrefix.print2D(ptr poison, ptr %24)
  ret i32 0
}

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr
