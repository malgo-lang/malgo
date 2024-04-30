; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/FieldPrefix.ll'
source_filename = "test/testcases/malgo/FieldPrefix.mlg"

@FieldPrefix.zero3D = local_unnamed_addr global ptr undef
@FieldPrefix.zero2D = local_unnamed_addr global ptr undef
@str3626 = unnamed_addr constant [2 x i8] c"x\00"
@str3627 = unnamed_addr constant [2 x i8] c"y\00"
@str3628 = unnamed_addr constant [3 x i8] c", \00"
@str3633 = unnamed_addr constant [2 x i8] c"z\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

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
  tail call void @malgo_hash_table_insert(ptr %8, ptr noundef nonnull @str3626, ptr noundef nonnull %2)
  tail call void @malgo_hash_table_insert(ptr %8, ptr noundef nonnull @str3627, ptr noundef nonnull %4)
  tail call void @malgo_hash_table_insert(ptr %8, ptr noundef nonnull @str3633, ptr noundef nonnull %6)
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
  tail call void @malgo_hash_table_insert(ptr %13, ptr noundef nonnull @str3626, ptr noundef nonnull %9)
  tail call void @malgo_hash_table_insert(ptr %13, ptr noundef nonnull @str3627, ptr noundef nonnull %11)
  store ptr %13, ptr @FieldPrefix.zero2D, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %14, align 1
  %15 = load ptr, ptr @FieldPrefix.zero2D, align 8
  %16 = tail call ptr @malgo_hash_table_get(ptr %15, ptr noundef nonnull @str3626)
  %17 = tail call ptr @malgo_hash_table_get(ptr %15, ptr noundef nonnull @str3627)
  %18 = getelementptr { i8, { i32 } }, ptr %16, i64 0, i32 1
  %19 = load i32, ptr %18, align 4
  %20 = tail call ptr @malgo_int32_t_to_string(i32 %19)
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %21, align 1
  %22 = getelementptr { i8, { ptr } }, ptr %21, i64 0, i32 1, i32 0
  store ptr %20, ptr %22, align 8
  %23 = tail call ptr @malgo_print_string(ptr %20)
  %24 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %24, align 1
  %25 = getelementptr { i8, { ptr } }, ptr %24, i64 0, i32 1, i32 0
  store ptr @str3628, ptr %25, align 8
  %26 = tail call ptr @malgo_print_string(ptr noundef nonnull @str3628)
  %27 = getelementptr { i8, { i32 } }, ptr %17, i64 0, i32 1
  %28 = load i32, ptr %27, align 4
  %29 = tail call ptr @malgo_int32_t_to_string(i32 %28)
  %30 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %30, align 1
  %31 = getelementptr { i8, { ptr } }, ptr %30, i64 0, i32 1, i32 0
  store ptr %29, ptr %31, align 8
  %32 = tail call ptr @malgo_print_string(ptr %29)
  ret i32 0
}

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr
