; ModuleID = '.malgo-work/test/testcases/malgo/Test3.ll'
source_filename = "test/testcases/malgo/Test3.mlg"

@str151 = unnamed_addr constant [6 x i8] c"WRONG\00"
@str152 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @_M32let_x2455_x2486_x5Fclosure_x249342test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"d$60_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"d$60_0")
  ret ptr %6
}

define internal ptr @_M26fun_x2472_x5Fclosure_x249442test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %_M22raw_x5Ffun_x2472_x249242test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal.exit, label %switch_branch_Cons_0.i

switch_branch_Cons_0.i:                           ; preds = %2
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = load i8, ptr %5, align 1
  %switch1.i = icmp eq i8 %8, 0
  br i1 %switch1.i, label %switch_branch_A_0.i, label %_M22raw_x5Ffun_x2472_x249242test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal.exit

switch_branch_A_0.i:                              ; preds = %switch_branch_Cons_0.i
  %9 = load i8, ptr %7, align 1
  %switch2.i = icmp eq i8 %9, 0
  br i1 %switch2.i, label %_M22raw_x5Ffun_x2472_x249242test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal.exit, label %switch_branch_Cons_1.i

switch_branch_Cons_1.i:                           ; preds = %switch_branch_A_0.i
  %10 = getelementptr { i8, { ptr, ptr } }, ptr %7, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = load i8, ptr %11, align 1
  %switch3.i = icmp eq i8 %12, 0
  br i1 %switch3.i, label %_M22raw_x5Ffun_x2472_x249242test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal.exit, label %switch_branch_B_0.i

switch_branch_B_0.i:                              ; preds = %switch_branch_Cons_1.i
  %13 = getelementptr { i8, { ptr, ptr } }, ptr %7, i64 0, i32 1, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = load i8, ptr %14, align 1
  %switch4.i = icmp eq i8 %15, 0
  %str152.str151.i = select i1 %switch4.i, ptr @str152, ptr @str151
  br label %_M22raw_x5Ffun_x2472_x249242test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal.exit

_M22raw_x5Ffun_x2472_x249242test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal.exit: ; preds = %2, %switch_branch_Cons_0.i, %switch_branch_A_0.i, %switch_branch_Cons_1.i, %switch_branch_B_0.i
  %str151.sink.i = phi ptr [ @str151, %2 ], [ @str151, %switch_branch_A_0.i ], [ @str151, %switch_branch_Cons_1.i ], [ %str152.str151.i, %switch_branch_B_0.i ], [ @str151, %switch_branch_Cons_0.i ]
  %16 = tail call ptr @malgo_print_string(ptr noundef nonnull %str151.sink.i)
  ret ptr %16
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %6, align 1
  %7 = getelementptr { i8, { ptr, ptr } }, ptr %6, i64 0, i32 1, i32 0
  store ptr %4, ptr %7, align 8
  %8 = getelementptr { i8, { ptr, ptr } }, ptr %6, i64 0, i32 1, i32 1
  store ptr %5, ptr %8, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %9, align 1
  %10 = getelementptr { i8, { ptr, ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %3, ptr %10, align 8
  %11 = getelementptr { i8, { ptr, ptr } }, ptr %9, i64 0, i32 1, i32 1
  store ptr %6, ptr %11, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$55$86_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %9, ptr %"let$55$86_capture_0.i", align 8
  store ptr %"let$55$86_capture_0.i", ptr %12, align 8
  %"let$55$86_func_0.i" = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @_M32let_x2455_x2486_x5Fclosure_x249342test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Internal, ptr %"let$55$86_func_0.i", align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"fun$72_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %"fun$72_capture_0.i", ptr %13, align 8
  %"fun$72_func_0.i" = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @_M26fun_x2472_x5Fclosure_x249442test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Internal, ptr %"fun$72_func_0.i", align 8
  %14 = load ptr, ptr %12, align 8
  %15 = load ptr, ptr %"let$55$86_func_0.i", align 8
  %16 = tail call ptr %15(ptr %14, ptr nonnull %13)
  ret i32 0
}
