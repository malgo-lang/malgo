; ModuleID = '.malgo-work/test/testcases/malgo/FieldPrefix.ll'
source_filename = "test/testcases/malgo/FieldPrefix.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@"test/testcases/malgo/FieldPrefix.mlg.zero3D" = local_unnamed_addr global ptr undef
@"test/testcases/malgo/FieldPrefix.mlg.zero2D" = local_unnamed_addr global ptr undef
@str240 = unnamed_addr constant [1 x i8] zeroinitializer
@str241 = unnamed_addr constant [2 x i8] c"x\00"
@str242 = unnamed_addr constant [2 x i8] c"y\00"
@str243 = unnamed_addr constant [3 x i8] c", \00"
@str307 = unnamed_addr constant [10 x i8] c"no branch\00"
@str320 = unnamed_addr constant [2 x i8] c"z\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @"runtime/malgo/Builtin.mlg.undefined", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { float } }, ptr %4, i64 0, i32 1, i32 0
  store float 0.000000e+00, ptr %5, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { float } }, ptr %6, i64 0, i32 1, i32 0
  store float 0.000000e+00, ptr %7, align 4
  %8 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { float } }, ptr %8, i64 0, i32 1, i32 0
  store float 0.000000e+00, ptr %9, align 4
  %10 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %10, ptr noundef nonnull @str241, ptr noundef nonnull %4)
  tail call void @malgo_hash_table_insert(ptr %10, ptr noundef nonnull @str242, ptr noundef nonnull %6)
  tail call void @malgo_hash_table_insert(ptr %10, ptr noundef nonnull @str320, ptr noundef nonnull %8)
  store ptr %10, ptr @"test/testcases/malgo/FieldPrefix.mlg.zero3D", align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { i32 } }, ptr %11, i64 0, i32 1, i32 0
  store i32 0, ptr %12, align 4
  %13 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { i32 } }, ptr %13, i64 0, i32 1, i32 0
  store i32 0, ptr %14, align 4
  %15 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %15, ptr noundef nonnull @str241, ptr noundef nonnull %11)
  tail call void @malgo_hash_table_insert(ptr %15, ptr noundef nonnull @str242, ptr noundef nonnull %13)
  store ptr %15, ptr @"test/testcases/malgo/FieldPrefix.mlg.zero2D", align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %16, align 1
  %17 = load ptr, ptr @"test/testcases/malgo/FieldPrefix.mlg.zero2D", align 8
  %18 = tail call ptr @malgo_hash_table_get(ptr %17, ptr noundef nonnull @str241)
  %19 = tail call ptr @malgo_hash_table_get(ptr %17, ptr noundef nonnull @str242)
  %20 = getelementptr i8, ptr %18, i64 4
  %.val1.i.i = load i32, ptr %20, align 4
  %21 = tail call ptr @malgo_int32_t_to_string(i32 %.val1.i.i)
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %22, align 1
  %23 = getelementptr { i8, { ptr } }, ptr %22, i64 0, i32 1, i32 0
  store ptr %21, ptr %23, align 8
  %24 = tail call ptr @malgo_print_string(ptr %21)
  %25 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %25, align 1
  %26 = getelementptr { i8, { ptr } }, ptr %25, i64 0, i32 1, i32 0
  store ptr @str243, ptr %26, align 8
  %27 = tail call ptr @malgo_print_string(ptr noundef nonnull @str243)
  %28 = getelementptr i8, ptr %19, i64 4
  %.val.i.i = load i32, ptr %28, align 4
  %29 = tail call ptr @malgo_int32_t_to_string(i32 %.val.i.i)
  %30 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %30, align 1
  %31 = getelementptr { i8, { ptr } }, ptr %30, i64 0, i32 1, i32 0
  store ptr %29, ptr %31, align 8
  %32 = tail call ptr @malgo_print_string(ptr %29)
  ret i32 0
}

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr
