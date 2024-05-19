; ModuleID = '.malgo-work/test/testcases/malgo/HelloImport.ll'
source_filename = "test/testcases/malgo/HelloImport.mlg"

@str3605 = unnamed_addr constant [7 x i8] c" world\00"
@str3606 = unnamed_addr constant [6 x i8] c"hello\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @_M35let_x242c2_x24e07_x5Fclosure_x24e1748test_x2Ftestcases_x2Fmalgo_x2FHelloImport_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"cast$17_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"cast$17_0")
  ret ptr %6
}

define internal ptr @_M27fun_x241e_x5Fclosure_x24e1848test_x2Ftestcases_x2Fmalgo_x2FHelloImport_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr @str3605, ptr %4, align 8
  %5 = load ptr, ptr %1, align 8
  %6 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = tail call ptr %7(ptr %5, ptr nonnull %3)
  ret ptr %8
}

define internal ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %_M16str_x245b_x242cd34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) {
"switch_branch_String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %_M16str_x245b_x242cd34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_print_string(ptr %2)
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_newline(ptr noundef nonnull %5)
  ret ptr %6
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr @str3606, ptr %4, align 8
  %5 = tail call ptr @malgo_print_string(ptr noundef nonnull @str3606)
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0.i, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$2c2$e07_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %6, ptr %"let$2c2$e07_capture_0.i", align 8
  store ptr %"let$2c2$e07_capture_0.i", ptr %7, align 8
  %"let$2c2$e07_func_0.i" = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @_M35let_x242c2_x24e07_x5Fclosure_x24e1748test_x2Ftestcases_x2Fmalgo_x2FHelloImport_x2Emlg8Internal, ptr %"let$2c2$e07_func_0.i", align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"fun$1e_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %"fun$1e_capture_0.i", ptr %8, align 8
  %"fun$1e_func_0.i" = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @_M27fun_x241e_x5Fclosure_x24e1848test_x2Ftestcases_x2Fmalgo_x2FHelloImport_x2Emlg8Internal, ptr %"fun$1e_func_0.i", align 8
  %9 = load ptr, ptr %7, align 8
  %10 = load ptr, ptr %"let$2c2$e07_func_0.i", align 8
  %11 = tail call ptr %10(ptr %9, ptr nonnull %8)
  ret i32 0
}
