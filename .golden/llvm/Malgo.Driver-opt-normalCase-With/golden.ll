; ModuleID = '.malgo-work/test/testcases/malgo/With.ll'
source_filename = "test/testcases/malgo/With.mlg"

@str3825 = unnamed_addr constant [4 x i8] c"end\00"
@str3826 = unnamed_addr constant [4 x i8] c"foo\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @_M27fun_x2478_x5Fclosure_x24eee41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr @str3825, ptr %4, align 8
  %5 = tail call ptr @malgo_print_string(ptr noundef nonnull @str3825)
  ret ptr %5
}

define internal ptr @_M34let_x2472_x24e85_x5Fclosure_x24eef41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %"cast$7a_0" = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = load ptr, ptr %1, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %3)
  %8 = load ptr, ptr %"cast$7a_0", align 8
  %9 = getelementptr { ptr, ptr }, ptr %"cast$7a_0", i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, ptr %7)
  ret ptr %11
}

define internal ptr @_M27fun_x248c_x5Fclosure_x24ef041test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"fun$88_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %"fun$88_capture_0.i", ptr %3, align 8
  %"fun$88_func_0.i" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @_M27fun_x2488_x5Fclosure_x24ef541test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal, ptr %"fun$88_func_0.i", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = load ptr, ptr %3, align 8
  %6 = load ptr, ptr %"fun$88_func_0.i", align 8
  %7 = tail call ptr %6(ptr %5, ptr nonnull %4)
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = load ptr, ptr %3, align 8
  %10 = load ptr, ptr %"fun$88_func_0.i", align 8
  %11 = tail call ptr %10(ptr %9, ptr nonnull %8)
  ret ptr %11
}

define internal ptr @_M34let_x2462_x24e8f_x5Fclosure_x24ef341test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %"d$7f_0" = load ptr, ptr %0, align 8
  %3 = getelementptr { i8, { ptr } }, ptr %"d$7f_0", i64 0, i32 1
  %4 = load ptr, ptr %3, align 8
  %5 = tail call ptr @malgo_print_string(ptr %4)
  %6 = load ptr, ptr %1, align 8
  %7 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, ptr %"d$7f_0")
  ret ptr %9
}

define internal ptr @_M27fun_x2485_x5Fclosure_x24ef441test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %1) {
  %3 = getelementptr i8, ptr %1, i64 8
  %.val = load ptr, ptr %3, align 8
  %4 = tail call ptr @malgo_print_string(ptr %.val)
  ret ptr %4
}

define internal ptr @_M27fun_x2488_x5Fclosure_x24ef541test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr @str3826, ptr %4, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$62$e8f_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %"let$62$e8f_capture_0.i", align 8
  store ptr %"let$62$e8f_capture_0.i", ptr %5, align 8
  %"let$62$e8f_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M34let_x2462_x24e8f_x5Fclosure_x24ef341test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal, ptr %"let$62$e8f_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"fun$85_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %"fun$85_capture_0.i", ptr %6, align 8
  %"fun$85_func_0.i" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @_M27fun_x2485_x5Fclosure_x24ef441test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal, ptr %"fun$85_func_0.i", align 8
  %7 = load ptr, ptr %5, align 8
  %8 = load ptr, ptr %"let$62$e8f_func_0.i", align 8
  %9 = tail call ptr %8(ptr %7, ptr nonnull %6)
  ret ptr %9
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"fun$78_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %"fun$78_capture_0.i", ptr %3, align 8
  %"fun$78_func_0.i" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @_M27fun_x2478_x5Fclosure_x24eee41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal, ptr %"fun$78_func_0.i", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$72$e85_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %"let$72$e85_capture_0.i", align 8
  store ptr %"let$72$e85_capture_0.i", ptr %4, align 8
  %"let$72$e85_func_0.i" = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @_M34let_x2472_x24e85_x5Fclosure_x24eef41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal, ptr %"let$72$e85_func_0.i", align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"fun$8c_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %"fun$8c_capture_0.i", ptr %5, align 8
  %"fun$8c_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M27fun_x248c_x5Fclosure_x24ef041test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal, ptr %"fun$8c_func_0.i", align 8
  %6 = load ptr, ptr %4, align 8
  %7 = load ptr, ptr %"let$72$e85_func_0.i", align 8
  %8 = tail call ptr %7(ptr %6, ptr nonnull %5)
  ret i32 0
}
