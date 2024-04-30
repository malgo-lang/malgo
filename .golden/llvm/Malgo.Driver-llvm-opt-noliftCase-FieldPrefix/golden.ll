; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/FieldPrefix.ll'
source_filename = "test/testcases/malgo/FieldPrefix.mlg"

@FieldPrefix.zero3D = local_unnamed_addr global ptr undef
@FieldPrefix.zero2D = local_unnamed_addr global ptr undef
@str2924 = unnamed_addr constant [2 x i8] c"x\00"
@str2925 = unnamed_addr constant [2 x i8] c"y\00"
@str2926 = unnamed_addr constant [3 x i8] c", \00"
@str2931 = unnamed_addr constant [2 x i8] c"z\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @FieldPrefix.print2D(ptr nocapture nofree readnone %0, ptr %"FieldPrefix.$record_67_0") {
"switch_branch_Builtin.Int32#_0":
  %1 = tail call ptr @malgo_hash_table_get(ptr %"FieldPrefix.$record_67_0", ptr noundef nonnull @str2924)
  %2 = tail call ptr @malgo_hash_table_get(ptr %"FieldPrefix.$record_67_0", ptr noundef nonnull @str2925)
  %3 = getelementptr { i8, { i32 } }, ptr %1, i64 0, i32 1
  %4 = load i32, ptr %3, align 4
  %5 = tail call ptr @malgo_int32_t_to_string(i32 %4)
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %6, i64 0, i32 1, i32 0
  store ptr %5, ptr %7, align 8
  %8 = tail call ptr @malgo_print_string(ptr %5)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr @str2926, ptr %10, align 8
  %11 = tail call ptr @malgo_print_string(ptr noundef nonnull @str2926)
  %12 = getelementptr { i8, { i32 } }, ptr %2, i64 0, i32 1
  %13 = load i32, ptr %12, align 4
  %14 = tail call ptr @malgo_int32_t_to_string(i32 %13)
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %15, i64 0, i32 1, i32 0
  store ptr %14, ptr %16, align 8
  %17 = tail call ptr @malgo_print_string(ptr %14)
  ret ptr %17
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { float } }, ptr %2, i64 0, i32 1, i32 0
  store float 0.000000e+00, ptr %3, align 4
  %4 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { float } }, ptr %4, i64 0, i32 1, i32 0
  store float 0.000000e+00, ptr %5, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { float } }, ptr %6, i64 0, i32 1, i32 0
  store float 0.000000e+00, ptr %7, align 4
  %8 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %8, ptr noundef nonnull @str2924, ptr noundef nonnull %2)
  tail call void @malgo_hash_table_insert(ptr %8, ptr noundef nonnull @str2925, ptr noundef nonnull %4)
  tail call void @malgo_hash_table_insert(ptr %8, ptr noundef nonnull @str2931, ptr noundef nonnull %6)
  store ptr %8, ptr @FieldPrefix.zero3D, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { i32 } }, ptr %9, i64 0, i32 1, i32 0
  store i32 0, ptr %10, align 4
  %11 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { i32 } }, ptr %11, i64 0, i32 1, i32 0
  store i32 0, ptr %12, align 4
  %13 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %13, ptr noundef nonnull @str2924, ptr noundef nonnull %9)
  tail call void @malgo_hash_table_insert(ptr %13, ptr noundef nonnull @str2925, ptr noundef nonnull %11)
  store ptr %13, ptr @FieldPrefix.zero2D, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %14, align 1
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %print2D_func_0.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @FieldPrefix.print2D, ptr %print2D_func_0.i, align 8
  %16 = load ptr, ptr @FieldPrefix.zero2D, align 8
  %17 = tail call ptr @malgo_hash_table_get(ptr %16, ptr noundef nonnull @str2924)
  %18 = tail call ptr @malgo_hash_table_get(ptr %16, ptr noundef nonnull @str2925)
  %19 = getelementptr { i8, { i32 } }, ptr %17, i64 0, i32 1
  %20 = load i32, ptr %19, align 4
  %21 = tail call ptr @malgo_int32_t_to_string(i32 %20)
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %22, align 1
  %23 = getelementptr { i8, { ptr } }, ptr %22, i64 0, i32 1, i32 0
  store ptr %21, ptr %23, align 8
  %24 = tail call ptr @malgo_print_string(ptr %21)
  %25 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %25, align 1
  %26 = getelementptr { i8, { ptr } }, ptr %25, i64 0, i32 1, i32 0
  store ptr @str2926, ptr %26, align 8
  %27 = tail call ptr @malgo_print_string(ptr noundef nonnull @str2926)
  %28 = getelementptr { i8, { i32 } }, ptr %18, i64 0, i32 1
  %29 = load i32, ptr %28, align 4
  %30 = tail call ptr @malgo_int32_t_to_string(i32 %29)
  %31 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %31, align 1
  %32 = getelementptr { i8, { ptr } }, ptr %31, i64 0, i32 1, i32 0
  store ptr %30, ptr %32, align 8
  %33 = tail call ptr @malgo_print_string(ptr %30)
  ret i32 0
}

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr
