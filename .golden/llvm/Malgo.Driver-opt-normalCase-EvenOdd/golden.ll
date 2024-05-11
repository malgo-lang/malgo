; ModuleID = '.malgo-work/test/testcases/malgo/EvenOdd.ll'
source_filename = "test/testcases/malgo/EvenOdd.mlg"

@str3678 = unnamed_addr constant [6 x i8] c"False\00"
@str3679 = unnamed_addr constant [5 x i8] c"True\00"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_sub_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @_M14let_x5Fclosure44test_x2Ftestcases_x2Fmalgo_x2FEvenOdd_x2Emlg12Internal3677(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int32#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int32#_0", i64 4
  %"int32#_0.val" = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val", ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M14let_x5Fclosure44test_x2Ftestcases_x2Fmalgo_x2FEvenOdd_x2Emlg12Internal3680, ptr %let_func_0.i, align 8
  %6 = tail call i32 @malgo_sub_int32_t(i32 %"int32#_0.val", i32 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 %6, ptr %8, align 4
  ret ptr %7
}

define internal i32 @_M14let_x5Fclosure44test_x2Ftestcases_x2Fmalgo_x2FEvenOdd_x2Emlg12Internal3680(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_sub_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 10, ptr %4, align 4
  br label %switch-unboxed_default_0.i.i

switch-unboxed_default_0.i.i:                     ; preds = %switch-unboxed_default_0.i.i.i, %1
  %_M9int32_x2344test_x2Ftestcases_x2Fmalgo_x2FEvenOdd_x2Emlg10Temporal51_0.tr4.i.i = phi ptr [ %18, %switch-unboxed_default_0.i.i.i ], [ %3, %1 ]
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %_M9int32_x2344test_x2Ftestcases_x2Fmalgo_x2FEvenOdd_x2Emlg10Temporal51_0.tr4.i.i, ptr %let_capture_0.i.i.i, align 8
  store ptr %let_capture_0.i.i.i, ptr %5, align 8
  %let_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M14let_x5Fclosure44test_x2Ftestcases_x2Fmalgo_x2FEvenOdd_x2Emlg12Internal3677, ptr %let_func_0.i.i.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { i32 } }, ptr %6, i64 0, i32 1, i32 0
  store i32 1, ptr %7, align 4
  %8 = load ptr, ptr %5, align 8
  %9 = load ptr, ptr %let_func_0.i.i.i, align 8
  %10 = tail call ptr %9(ptr %8, ptr nonnull %6)
  %11 = getelementptr { i8, { i32 } }, ptr %10, i64 0, i32 1
  %12 = load i32, ptr %11, align 4
  %cond.i.i.i = icmp eq i32 %12, 0
  br i1 %cond.i.i.i, label %_M4main44test_x2Ftestcases_x2Fmalgo_x2FEvenOdd_x2Emlg8External.exit, label %switch-unboxed_default_0.i.i.i

switch-unboxed_default_0.i.i.i:                   ; preds = %switch-unboxed_default_0.i.i
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %10, ptr %let_capture_0.i1.i.i, align 8
  store ptr %let_capture_0.i1.i.i, ptr %13, align 8
  %let_func_0.i2.i.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @_M14let_x5Fclosure44test_x2Ftestcases_x2Fmalgo_x2FEvenOdd_x2Emlg12Internal3677, ptr %let_func_0.i2.i.i, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %14, align 1
  %15 = getelementptr { i8, { i32 } }, ptr %14, i64 0, i32 1, i32 0
  store i32 1, ptr %15, align 4
  %16 = load ptr, ptr %13, align 8
  %17 = load ptr, ptr %let_func_0.i2.i.i, align 8
  %18 = tail call ptr %17(ptr %16, ptr nonnull %14)
  %19 = getelementptr { i8, { i32 } }, ptr %18, i64 0, i32 1
  %20 = load i32, ptr %19, align 4
  %cond.i.i = icmp eq i32 %20, 0
  br i1 %cond.i.i, label %_M4main44test_x2Ftestcases_x2Fmalgo_x2FEvenOdd_x2Emlg8External.exit, label %switch-unboxed_default_0.i.i

_M4main44test_x2Ftestcases_x2Fmalgo_x2FEvenOdd_x2Emlg8External.exit: ; preds = %switch-unboxed_default_0.i.i, %switch-unboxed_default_0.i.i.i
  %.sink.i = phi i8 [ 1, %switch-unboxed_default_0.i.i.i ], [ 0, %switch-unboxed_default_0.i.i ]
  %spec.select.i.i = phi ptr [ @str3679, %switch-unboxed_default_0.i.i.i ], [ @str3678, %switch-unboxed_default_0.i.i ]
  %21 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 %.sink.i, ptr %21, align 1
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %22, align 1
  %23 = getelementptr { i8, { ptr } }, ptr %22, i64 0, i32 1, i32 0
  store ptr %spec.select.i.i, ptr %23, align 8
  %24 = tail call ptr @malgo_print_string(ptr noundef nonnull %spec.select.i.i)
  %25 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %25, align 1
  %26 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %26, align 1
  %27 = tail call ptr @malgo_newline(ptr noundef nonnull %26)
  ret i32 0
}
