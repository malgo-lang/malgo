; ModuleID = '.malgo-work/test/testcases/malgo/TestEither.ll'
source_filename = "test/testcases/malgo/TestEither.mlg"

@str4533 = unnamed_addr constant [6 x i8] c"error\00"
@str4563 = unnamed_addr constant [12 x i8] c"unreachable\00"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @_M36let_x242c2_x24f67_x5Fclosure_x2411af47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"d$10f_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"d$10f_0")
  ret ptr %6
}

define internal ptr @_M29fun_x2411c_x5Fclosure_x2411b047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Left_0.i, label %switch_branch_Right_0.i

switch_branch_Left_0.i:                           ; preds = %2
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %4, i64 0, i32 1, i32 0
  store ptr @str4563, ptr %5, align 8
  %6 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4563)
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %7, align 1
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = tail call ptr @malgo_newline(ptr noundef nonnull %8)
  br label %_M25raw_x5Ffun_x2411c_x24115c47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal.exit

switch_branch_Right_0.i:                          ; preds = %2
  %10 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = getelementptr { i8, { i32 } }, ptr %11, i64 0, i32 1
  %13 = load i32, ptr %12, align 4
  %14 = tail call ptr @malgo_int32_t_to_string(i32 %13)
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %15, i64 0, i32 1, i32 0
  store ptr %14, ptr %16, align 8
  %17 = tail call ptr @malgo_print_string(ptr %14)
  br label %_M25raw_x5Ffun_x2411c_x24115c47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal.exit

_M25raw_x5Ffun_x2411c_x24115c47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal.exit: ; preds = %switch_branch_Left_0.i, %switch_branch_Right_0.i
  %common.ret.op.i = phi ptr [ %9, %switch_branch_Left_0.i ], [ %17, %switch_branch_Right_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411ca47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"d$164_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"d$164_0")
  ret ptr %6
}

define internal ptr @_M29fun_x24173_x5Fclosure_x2411cb47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %1) {
  %.val = load i8, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %switch.i = icmp eq i8 %.val, 0
  br i1 %switch.i, label %_M25raw_x5Ffun_x24173_x24118147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal.exit, label %switch_branch_Right_0.i

switch_branch_Right_0.i:                          ; preds = %2
  %4 = getelementptr { i8, { i32 } }, ptr %.val1, i64 0, i32 1
  %5 = load i32, ptr %4, align 4
  %6 = tail call ptr @malgo_int32_t_to_string(i32 %5)
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %6, ptr %8, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$2c2$fe4_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %7, ptr %"let$2c2$fe4_capture_0.i", align 8
  store ptr %"let$2c2$fe4_capture_0.i", ptr %9, align 8
  %"let$2c2$fe4_func_0.i" = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0.i", align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0.i, align 8
  %"d$889$1060_0.i.i" = load ptr, ptr %"let$2c2$fe4_capture_0.i", align 8
  br label %_M25raw_x5Ffun_x24173_x24118147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal.exit

_M25raw_x5Ffun_x24173_x24118147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal.exit: ; preds = %2, %switch_branch_Right_0.i
  %"d$889$1060_0.i.sink.i" = phi ptr [ %"d$889$1060_0.i.i", %switch_branch_Right_0.i ], [ %.val1, %2 ]
  %11 = getelementptr { i8, { ptr } }, ptr %"d$889$1060_0.i.sink.i", i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call ptr @malgo_print_string(ptr %12)
  %14 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %14, align 1
  %15 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %15, align 1
  %16 = tail call ptr @malgo_newline(ptr noundef nonnull %15)
  ret ptr %16
}

define internal ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411ce47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"d$164_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"d$164_0")
  ret ptr %6
}

define internal ptr @_M29fun_x24173_x5Fclosure_x2411cf47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %1) {
  %.val = load i8, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %switch.i = icmp eq i8 %.val, 0
  br i1 %switch.i, label %_M25raw_x5Ffun_x24173_x24118447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal.exit, label %switch_branch_Right_0.i

switch_branch_Right_0.i:                          ; preds = %2
  %4 = getelementptr { i8, { i32 } }, ptr %.val1, i64 0, i32 1
  %5 = load i32, ptr %4, align 4
  %6 = tail call ptr @malgo_int32_t_to_string(i32 %5)
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %6, ptr %8, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$2c2$fe4_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %7, ptr %"let$2c2$fe4_capture_0.i", align 8
  store ptr %"let$2c2$fe4_capture_0.i", ptr %9, align 8
  %"let$2c2$fe4_func_0.i" = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411fb47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0.i", align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0.i, align 8
  %"d$889$1060_0.i.i" = load ptr, ptr %"let$2c2$fe4_capture_0.i", align 8
  br label %_M25raw_x5Ffun_x24173_x24118447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal.exit

_M25raw_x5Ffun_x24173_x24118447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal.exit: ; preds = %2, %switch_branch_Right_0.i
  %"d$889$1060_0.i.sink.i" = phi ptr [ %"d$889$1060_0.i.i", %switch_branch_Right_0.i ], [ %.val1, %2 ]
  %11 = getelementptr { i8, { ptr } }, ptr %"d$889$1060_0.i.sink.i", i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call ptr @malgo_print_string(ptr %12)
  %14 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %14, align 1
  %15 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %15, align 1
  %16 = tail call ptr @malgo_newline(ptr noundef nonnull %15)
  ret ptr %16
}

define internal noundef ptr @_M29let_x24fda_x5Fclosure_x2411d247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int32#$fc7_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int32#$fc7_0", i64 4
  %"int32#$fc7_0.val" = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$715$dcf_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#$fc7_0.val", ptr %"let$715$dcf_capture_0.i", align 4
  store ptr %"let$715$dcf_capture_0.i", ptr %5, align 8
  %"let$715$dcf_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M36let_x24715_x24dcf_x5Fclosure_x24122747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$715$dcf_func_0.i", align 8
  %6 = tail call i32 @malgo_add_int32_t(i32 %"int32#$fc7_0.val", i32 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 %6, ptr %8, align 4
  ret ptr %7
}

define internal ptr @_M6putStr34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %_M16str_x245a_x242d334runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) {
"switch_branch_String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %_M16str_x245a_x242d334runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_print_string(ptr %2)
  ret ptr %3
}

define internal ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %_M16str_x245b_x242cc34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) {
"switch_branch_String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %_M16str_x245b_x242cc34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_print_string(ptr %2)
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_newline(ptr noundef nonnull %5)
  ret ptr %6
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"d$889$1060_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"d$889$1060_0")
  ret ptr %6
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411fb47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"d$889$1060_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"d$889$1060_0")
  ret ptr %6
}

define internal noundef ptr @_M5Right47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8External(ptr nocapture nofree readnone %0, ptr nofree %_M7p_x24fb47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %_M7p_x24fb47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, ptr %3, align 8
  ret ptr %2
}

define internal ptr @_M36let_x242c2_x24f99_x5Fclosure_x24122547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"d$889$103c_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"d$889$103c_0")
  ret ptr %6
}

define internal ptr @_M36let_x242c2_x24f9f_x5Fclosure_x24122647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"cast$13f_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"cast$13f_0")
  ret ptr %6
}

define internal i32 @_M36let_x24715_x24dcf_x5Fclosure_x24122747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %"p$fc9$fd3$db2_0" = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %"p$fc9$fd3$db2_0", i32 %1)
  ret i32 %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %3, ptr %6, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$2c2$f67_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %"let$2c2$f67_capture_0.i", align 8
  store ptr %"let$2c2$f67_capture_0.i", ptr %7, align 8
  %"let$2c2$f67_func_0.i" = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @_M36let_x242c2_x24f67_x5Fclosure_x2411af47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$f67_func_0.i", align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"fun$11c_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %"fun$11c_capture_0.i", ptr %8, align 8
  %"fun$11c_func_0.i" = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @_M29fun_x2411c_x5Fclosure_x2411b047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"fun$11c_func_0.i", align 8
  %9 = load ptr, ptr %7, align 8
  %10 = load ptr, ptr %"let$2c2$f67_func_0.i", align 8
  %11 = tail call ptr %10(ptr %9, ptr nonnull %8)
  %12 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { i32 } }, ptr %12, i64 0, i32 1, i32 0
  store i32 1, ptr %13, align 4
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %14, align 1
  %15 = getelementptr { i8, { ptr } }, ptr %14, i64 0, i32 1, i32 0
  store ptr %12, ptr %15, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$fda_capture_0.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %12, ptr %"let$fda_capture_0.i.i", align 8
  store ptr %"let$fda_capture_0.i.i", ptr %16, align 8
  %"let$fda_func_0.i.i" = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @_M29let_x24fda_x5Fclosure_x2411d247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$fda_func_0.i.i", align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %17, align 1
  %18 = getelementptr { i8, { i32 } }, ptr %17, i64 0, i32 1, i32 0
  store i32 1, ptr %18, align 4
  %19 = load ptr, ptr %16, align 8
  %20 = load ptr, ptr %"let$fda_func_0.i.i", align 8
  %21 = tail call ptr %20(ptr %19, ptr nonnull %17)
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %22, align 1
  %23 = getelementptr { i8, { ptr } }, ptr %22, i64 0, i32 1, i32 0
  store ptr %21, ptr %23, align 8
  %24 = getelementptr i8, ptr %21, i64 4
  %.val.i = load i32, ptr %24, align 4
  %25 = tail call ptr @malgo_int32_t_to_string(i32 %.val.i)
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %26, align 1
  %27 = getelementptr { i8, { ptr } }, ptr %26, i64 0, i32 1, i32 0
  store ptr %25, ptr %27, align 8
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$2c2$f99_capture_0.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %26, ptr %"let$2c2$f99_capture_0.i.i", align 8
  store ptr %"let$2c2$f99_capture_0.i.i", ptr %28, align 8
  %"let$2c2$f99_func_0.i.i" = getelementptr { ptr, ptr }, ptr %28, i64 0, i32 1
  store ptr @_M36let_x242c2_x24f99_x5Fclosure_x24122547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$f99_func_0.i.i", align 8
  %29 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %29, align 8
  %putStr_func_0.i.i = getelementptr { ptr, ptr }, ptr %29, i64 0, i32 1
  store ptr @_M6putStr34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStr_func_0.i.i, align 8
  %"d$889$103c_0.i.i.i" = load ptr, ptr %"let$2c2$f99_capture_0.i.i", align 8
  %30 = getelementptr { i8, { ptr } }, ptr %"d$889$103c_0.i.i.i", i64 0, i32 1
  %31 = load ptr, ptr %30, align 8
  %32 = tail call ptr @malgo_print_string(ptr %31)
  %33 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$2c2$f9f_capture_0.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %32, ptr %"let$2c2$f9f_capture_0.i.i", align 8
  store ptr %"let$2c2$f9f_capture_0.i.i", ptr %33, align 8
  %"let$2c2$f9f_func_0.i.i" = getelementptr { ptr, ptr }, ptr %33, i64 0, i32 1
  store ptr @_M36let_x242c2_x24f9f_x5Fclosure_x24122647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$f9f_func_0.i.i", align 8
  %34 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %34, align 8
  %Right_func_0.i.i = getelementptr { ptr, ptr }, ptr %34, i64 0, i32 1
  store ptr @_M5Right47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8External, ptr %Right_func_0.i.i, align 8
  %35 = load ptr, ptr %33, align 8
  %36 = load ptr, ptr %"let$2c2$f9f_func_0.i.i", align 8
  %37 = tail call ptr %36(ptr %35, ptr nonnull %34)
  %38 = load i8, ptr %14, align 1
  %switch.i = icmp eq i8 %38, 0
  br i1 %switch.i, label %switch_branch_Left_12.i, label %switch_branch_Right_13.i

switch_branch_Left_12.i:                          ; preds = %1
  %39 = load ptr, ptr %15, align 8
  %40 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %40, align 1
  %41 = getelementptr { i8, { ptr } }, ptr %40, i64 0, i32 1, i32 0
  store ptr %39, ptr %41, align 8
  %42 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %42, align 1
  %43 = getelementptr { i8, { ptr } }, ptr %42, i64 0, i32 1, i32 0
  store ptr %39, ptr %43, align 8
  %44 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$2c2$fd3_capture_24.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %42, ptr %"let$2c2$fd3_capture_24.i", align 8
  store ptr %"let$2c2$fd3_capture_24.i", ptr %44, align 8
  %"let$2c2$fd3_func_12.i" = getelementptr { ptr, ptr }, ptr %44, i64 0, i32 1
  store ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411ca47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fd3_func_12.i", align 8
  br label %_M4main47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8External.exit

switch_branch_Right_13.i:                         ; preds = %1
  %45 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %45, align 1
  %46 = getelementptr { i8, { ptr } }, ptr %45, i64 0, i32 1, i32 0
  store ptr @str4533, ptr %46, align 8
  %47 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %47, align 1
  %48 = getelementptr { i8, { ptr } }, ptr %47, i64 0, i32 1, i32 0
  store ptr %45, ptr %48, align 8
  %49 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %49, align 1
  %50 = getelementptr { i8, { ptr } }, ptr %49, i64 0, i32 1, i32 0
  store ptr %45, ptr %50, align 8
  %51 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$2c2$fd3_capture_28.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %49, ptr %"let$2c2$fd3_capture_28.i", align 8
  store ptr %"let$2c2$fd3_capture_28.i", ptr %51, align 8
  %"let$2c2$fd3_func_14.i" = getelementptr { ptr, ptr }, ptr %51, i64 0, i32 1
  store ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411ce47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fd3_func_14.i", align 8
  br label %_M4main47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8External.exit

_M4main47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8External.exit: ; preds = %switch_branch_Left_12.i, %switch_branch_Right_13.i
  %_M29fun_x24173_x5Fclosure_x2411cf47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal.sink.i = phi ptr [ @_M29fun_x24173_x5Fclosure_x2411cf47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, %switch_branch_Right_13.i ], [ @_M29fun_x24173_x5Fclosure_x2411cb47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, %switch_branch_Left_12.i ]
  %.sink3.i = phi ptr [ %51, %switch_branch_Right_13.i ], [ %44, %switch_branch_Left_12.i ]
  %"let$2c2$fd3_func_14.sink.i" = phi ptr [ %"let$2c2$fd3_func_14.i", %switch_branch_Right_13.i ], [ %"let$2c2$fd3_func_12.i", %switch_branch_Left_12.i ]
  %52 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"fun$173_capture_28.i" = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %"fun$173_capture_28.i", ptr %52, align 8
  %"fun$173_func_14.i" = getelementptr { ptr, ptr }, ptr %52, i64 0, i32 1
  store ptr %_M29fun_x24173_x5Fclosure_x2411cf47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal.sink.i, ptr %"fun$173_func_14.i", align 8
  %53 = load ptr, ptr %.sink3.i, align 8
  %54 = load ptr, ptr %"let$2c2$fd3_func_14.sink.i", align 8
  %55 = tail call ptr %54(ptr %53, ptr nonnull %52)
  ret i32 0
}
