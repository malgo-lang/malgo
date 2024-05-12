; ModuleID = '.malgo-work/test/testcases/malgo/Factorial.ll'
source_filename = "test/testcases/malgo/Factorial.mlg"

declare void @GC_init() local_unnamed_addr

declare i64 @malgo_sub_int64_t(i64, i64) local_unnamed_addr

declare i64 @malgo_mul_int64_t(i64, i64) local_unnamed_addr

declare i32 @malgo_eq_int64_t(i64, i64) local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @_M35let_x242c2_x24f27_x5Fclosure_x24f4746test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"d$882$f34_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"d$882$f34_0")
  ret ptr %6
}

define internal ptr @_M28let_x24345_x5Fclosure_x24f4846test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %"true$331_0" = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$344_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %1, ptr %"let$344_capture_0.i", align 8
  %"true$331_0.i" = getelementptr { ptr, ptr }, ptr %"let$344_capture_0.i", i64 0, i32 1
  store ptr %"true$331_0", ptr %"true$331_0.i", align 8
  store ptr %"let$344_capture_0.i", ptr %3, align 8
  %"let$344_func_0.i" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @_M28let_x24344_x5Fclosure_x24f5046test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$344_func_0.i", align 8
  ret ptr %3
}

define internal noundef ptr @_M28let_x24e36_x5Fclosure_x24f4c46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int64#$e23_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int64#$e23_0", i64 4
  %"int64#$e23_0.val" = load i64, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i64, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$79b$a48_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"int64#$e23_0.val", ptr %"let$79b$a48_capture_0.i", align 4
  store ptr %"let$79b$a48_capture_0.i", ptr %5, align 8
  %"let$79b$a48_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M35let_x2479b_x24a48_x5Fclosure_x24f5546test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$79b$a48_func_0.i", align 8
  %6 = tail call i32 @malgo_eq_int64_t(i64 %"int64#$e23_0.val", i64 %.val)
  %cond.i = icmp eq i32 %6, 1
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  %spec.select.i = zext i1 %cond.i to i8
  store i8 %spec.select.i, ptr %7, align 1
  ret ptr %7
}

define internal ptr @_M27let_x24c7_x5Fclosure_x24f4d46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %"n$7$86_0" = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %3, i64 0, i32 1, i32 0
  store i64 0, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$e36_capture_0.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"n$7$86_0", ptr %"let$e36_capture_0.i.i", align 8
  store ptr %"let$e36_capture_0.i.i", ptr %5, align 8
  %"let$e36_func_0.i.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M28let_x24e36_x5Fclosure_x24f4c46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$e36_func_0.i.i", align 8
  %"int64#$e23_0.i.i" = load ptr, ptr %"let$e36_capture_0.i.i", align 8
  %6 = getelementptr i8, ptr %"int64#$e23_0.i.i", i64 4
  %"int64#$e23_0.val.i.i" = load i64, ptr %6, align 4
  %.val.i.i = load i64, ptr %4, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$79b$a48_capture_0.i.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"int64#$e23_0.val.i.i", ptr %"let$79b$a48_capture_0.i.i.i", align 4
  store ptr %"let$79b$a48_capture_0.i.i.i", ptr %7, align 8
  %"let$79b$a48_func_0.i.i.i" = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @_M35let_x2479b_x24a48_x5Fclosure_x24f5546test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$79b$a48_func_0.i.i.i", align 8
  %8 = tail call i32 @malgo_eq_int64_t(i64 %"int64#$e23_0.val.i.i", i64 %.val.i.i)
  %cond.i.i.i = icmp eq i32 %8, 1
  %9 = tail call ptr @malgo_malloc(i64 noundef 1)
  %spec.select.i.i.i = zext i1 %cond.i.i.i to i8
  store i8 %spec.select.i.i.i, ptr %9, align 1
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$345_capture_0.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %9, ptr %"let$345_capture_0.i.i", align 8
  store ptr %"let$345_capture_0.i.i", ptr %10, align 8
  %"let$345_func_0.i.i" = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @_M28let_x24345_x5Fclosure_x24f4846test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$345_func_0.i.i", align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"fun$91$b1$ef8_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %1, ptr %"fun$91$b1$ef8_capture_0.i", align 8
  store ptr %"fun$91$b1$ef8_capture_0.i", ptr %11, align 8
  %"fun$91$b1$ef8_func_0.i" = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @_M40fun_x2491_x24b1_x24ef8_x5Fclosure_x24f5346test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"fun$91$b1$ef8_func_0.i", align 8
  %12 = load ptr, ptr %10, align 8
  %13 = load ptr, ptr %"let$345_func_0.i.i", align 8
  %14 = tail call ptr %13(ptr %12, ptr nonnull %11)
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"fun$a3$b6$efc_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %1, ptr %"fun$a3$b6$efc_capture_0.i", align 8
  %"n$7$86_0.i" = getelementptr { ptr, ptr }, ptr %"fun$a3$b6$efc_capture_0.i", i64 0, i32 1
  store ptr %"n$7$86_0", ptr %"n$7$86_0.i", align 8
  store ptr %"fun$a3$b6$efc_capture_0.i", ptr %15, align 8
  %"fun$a3$b6$efc_func_0.i" = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @_M40fun_x24a3_x24b6_x24efc_x5Fclosure_x24f5446test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"fun$a3$b6$efc_func_0.i", align 8
  %16 = load ptr, ptr %14, align 8
  %17 = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  %18 = load ptr, ptr %17, align 8
  %19 = tail call ptr %18(ptr %16, ptr nonnull %15)
  ret ptr %19
}

define internal noundef ptr @_M28let_x249df_x5Fclosure_x24f4e46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int64#$9cc_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int64#$9cc_0", i64 4
  %"int64#$9cc_0.val" = load i64, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i64, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$739$2b6_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"int64#$9cc_0.val", ptr %"let$739$2b6_capture_0.i", align 4
  store ptr %"let$739$2b6_capture_0.i", ptr %5, align 8
  %"let$739$2b6_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M35let_x24739_x242b6_x5Fclosure_x24f5246test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$739$2b6_func_0.i", align 8
  %6 = tail call i64 @malgo_mul_int64_t(i64 %"int64#$9cc_0.val", i64 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i64 } }, ptr %7, i64 0, i32 1, i32 0
  store i64 %6, ptr %8, align 4
  ret ptr %7
}

define internal noundef ptr @_M28let_x248ee_x5Fclosure_x24f4f46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int64#$8db_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int64#$8db_0", i64 4
  %"int64#$8db_0.val" = load i64, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i64, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$733$179_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"int64#$8db_0.val", ptr %"let$733$179_capture_0.i", align 4
  store ptr %"let$733$179_capture_0.i", ptr %5, align 8
  %"let$733$179_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M35let_x24733_x24179_x5Fclosure_x24f5146test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$733$179_func_0.i", align 8
  %6 = tail call i64 @malgo_sub_int64_t(i64 %"int64#$8db_0.val", i64 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i64 } }, ptr %7, i64 0, i32 1, i32 0
  store i64 %6, ptr %8, align 4
  ret ptr %7
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

define internal ptr @_M28let_x24344_x5Fclosure_x24f5046test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readonly %1) {
  %"t$4d$332_0" = load ptr, ptr %0, align 8
  %"true$331_addr_0" = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %"true$331_0" = load ptr, ptr %"true$331_addr_0", align 8
  %"true$331_0.val" = load i8, ptr %"true$331_0", align 1
  %switch.i = icmp eq i8 %"true$331_0.val", 0
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %spec.select.i = select i1 %switch.i, ptr %1, ptr %"t$4d$332_0"
  %4 = load ptr, ptr %spec.select.i, align 8
  %5 = getelementptr { ptr, ptr }, ptr %spec.select.i, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %3)
  ret ptr %7
}

define internal i64 @_M35let_x24733_x24179_x5Fclosure_x24f5146test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %"p$8dd$8e7$15c_0" = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_sub_int64_t(i64 %"p$8dd$8e7$15c_0", i64 %1)
  ret i64 %3
}

define internal i64 @_M35let_x24739_x242b6_x5Fclosure_x24f5246test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %"p$9ce$9d8$299_0" = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_mul_int64_t(i64 %"p$9ce$9d8$299_0", i64 %1)
  ret i64 %3
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define internal ptr @_M40fun_x2491_x24b1_x24ef8_x5Fclosure_x24f5346test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) #0 {
  %"acc$8$87_0" = load ptr, ptr %0, align 8
  ret ptr %"acc$8$87_0"
}

define internal ptr @_M40fun_x24a3_x24b6_x24efc_x5Fclosure_x24f5446test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readnone %1) {
  %"acc$8$87_0" = load ptr, ptr %0, align 8
  %"n$7$86_addr_0" = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %"n$7$86_0" = load ptr, ptr %"n$7$86_addr_0", align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %3, i64 0, i32 1, i32 0
  store i64 1, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$8ee_capture_0.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"n$7$86_0", ptr %"let$8ee_capture_0.i.i", align 8
  store ptr %"let$8ee_capture_0.i.i", ptr %5, align 8
  %"let$8ee_func_0.i.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M28let_x248ee_x5Fclosure_x24f4f46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$8ee_func_0.i.i", align 8
  %"int64#$8db_0.i.i" = load ptr, ptr %"let$8ee_capture_0.i.i", align 8
  %6 = getelementptr i8, ptr %"int64#$8db_0.i.i", i64 4
  %"int64#$8db_0.val.i.i" = load i64, ptr %6, align 4
  %.val.i.i = load i64, ptr %4, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$733$179_capture_0.i.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"int64#$8db_0.val.i.i", ptr %"let$733$179_capture_0.i.i.i", align 4
  store ptr %"let$733$179_capture_0.i.i.i", ptr %7, align 8
  %"let$733$179_func_0.i.i.i" = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @_M35let_x24733_x24179_x5Fclosure_x24f5146test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$733$179_func_0.i.i.i", align 8
  %8 = tail call i64 @malgo_sub_int64_t(i64 %"int64#$8db_0.val.i.i", i64 %.val.i.i)
  %9 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { i64 } }, ptr %9, i64 0, i32 1, i32 0
  store i64 %8, ptr %10, align 4
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$c7_capture_0.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %9, ptr %"let$c7_capture_0.i.i", align 8
  store ptr %"let$c7_capture_0.i.i", ptr %11, align 8
  %"let$c7_func_0.i.i" = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @_M27let_x24c7_x5Fclosure_x24f4d46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$c7_func_0.i.i", align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$9df_capture_0.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"n$7$86_0", ptr %"let$9df_capture_0.i.i", align 8
  store ptr %"let$9df_capture_0.i.i", ptr %12, align 8
  %"let$9df_func_0.i.i" = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @_M28let_x249df_x5Fclosure_x24f4e46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$9df_func_0.i.i", align 8
  %"int64#$9cc_0.i.i" = load ptr, ptr %"let$9df_capture_0.i.i", align 8
  %13 = getelementptr i8, ptr %"int64#$9cc_0.i.i", i64 4
  %"int64#$9cc_0.val.i.i" = load i64, ptr %13, align 4
  %14 = getelementptr i8, ptr %"acc$8$87_0", i64 4
  %.val.i1.i = load i64, ptr %14, align 4
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$739$2b6_capture_0.i.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"int64#$9cc_0.val.i.i", ptr %"let$739$2b6_capture_0.i.i.i", align 4
  store ptr %"let$739$2b6_capture_0.i.i.i", ptr %15, align 8
  %"let$739$2b6_func_0.i.i.i" = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @_M35let_x24739_x242b6_x5Fclosure_x24f5246test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$739$2b6_func_0.i.i.i", align 8
  %16 = tail call i64 @malgo_mul_int64_t(i64 %"int64#$9cc_0.val.i.i", i64 %.val.i1.i)
  %17 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %17, align 1
  %18 = getelementptr { i8, { i64 } }, ptr %17, i64 0, i32 1, i32 0
  store i64 %16, ptr %18, align 4
  %19 = load ptr, ptr %11, align 8
  %20 = load ptr, ptr %"let$c7_func_0.i.i", align 8
  %21 = tail call ptr %20(ptr %19, ptr nonnull %17)
  ret ptr %21
}

define internal i32 @_M35let_x2479b_x24a48_x5Fclosure_x24f5546test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %"p$e25$e2f$a2a_0" = load i64, ptr %0, align 4
  %3 = tail call i32 @malgo_eq_int64_t(i64 %"p$e25$e2f$a2a_0", i64 %1)
  ret i32 %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %3, i64 0, i32 1, i32 0
  store i64 5, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$c7_capture_0.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %"let$c7_capture_0.i.i", align 8
  store ptr %"let$c7_capture_0.i.i", ptr %5, align 8
  %"let$c7_func_0.i.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M27let_x24c7_x5Fclosure_x24f4d46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$c7_func_0.i.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { i64 } }, ptr %6, i64 0, i32 1, i32 0
  store i64 1, ptr %7, align 4
  %8 = load ptr, ptr %5, align 8
  %9 = load ptr, ptr %"let$c7_func_0.i.i", align 8
  %10 = tail call ptr %9(ptr %8, ptr nonnull %6)
  %11 = getelementptr { i8, { i64 } }, ptr %10, i64 0, i32 1
  %12 = load i64, ptr %11, align 4
  %13 = tail call ptr @malgo_int64_t_to_string(i64 %12)
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %14, align 1
  %15 = getelementptr { i8, { ptr } }, ptr %14, i64 0, i32 1, i32 0
  store ptr %13, ptr %15, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$2c2$f27_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %14, ptr %"let$2c2$f27_capture_0.i", align 8
  store ptr %"let$2c2$f27_capture_0.i", ptr %16, align 8
  %"let$2c2$f27_func_0.i" = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @_M35let_x242c2_x24f27_x5Fclosure_x24f4746test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$2c2$f27_func_0.i", align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %17, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0.i, align 8
  %"d$882$f34_0.i.i" = load ptr, ptr %"let$2c2$f27_capture_0.i", align 8
  %18 = getelementptr { i8, { ptr } }, ptr %"d$882$f34_0.i.i", i64 0, i32 1
  %19 = load ptr, ptr %18, align 8
  %20 = tail call ptr @malgo_print_string(ptr %19)
  %21 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %21, align 1
  %22 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %22, align 1
  %23 = tail call ptr @malgo_newline(ptr noundef nonnull %22)
  ret i32 0
}

attributes #0 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn }
