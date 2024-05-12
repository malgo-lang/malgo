; ModuleID = '.malgo-work/test/testcases/malgo/Punctuate.ll'
source_filename = "test/testcases/malgo/Punctuate.mlg"

@str3841 = unnamed_addr constant [2 x i8] c"x\00"
@str3842 = unnamed_addr constant [2 x i8] c"y\00"
@str3843 = unnamed_addr constant [2 x i8] c"z\00"
@str3846 = unnamed_addr constant [1 x i8] zeroinitializer
@str3847 = unnamed_addr constant [8 x i8] c"Symbol \00"
@str3848 = unnamed_addr constant [6 x i8] c"SInt \00"
@str3849 = unnamed_addr constant [8 x i8] c"SList [\00"
@str3850 = unnamed_addr constant [3 x i8] c", \00"
@str3851 = unnamed_addr constant [2 x i8] c"]\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_string_append(ptr, ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @_M28let_x24324_x5Fclosure_x24f0046test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal(ptr nocapture nofree readonly %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Nil_0.i, label %switch_branch_Cons_0.i

switch_branch_Nil_0.i:                            ; preds = %2
  %4 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  br label %common.ret1

common.ret1:                                      ; preds = %switch_branch_Cons_0.i, %switch_branch_Nil_0.i
  %common.ret1.op = phi ptr [ %4, %switch_branch_Nil_0.i ], [ %15, %switch_branch_Cons_0.i ]
  ret ptr %common.ret1.op

switch_branch_Cons_0.i:                           ; preds = %2
  %"_$26$309_0" = load ptr, ptr %0, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = load ptr, ptr %"_$26$309_0", align 8
  %10 = getelementptr { ptr, ptr }, ptr %"_$26$309_0", i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = tail call ptr %11(ptr %9, ptr %6)
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$324_capture_0.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"_$26$309_0", ptr %"let$324_capture_0.i.i", align 8
  store ptr %"let$324_capture_0.i.i", ptr %13, align 8
  %"let$324_func_0.i.i" = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @_M28let_x24324_x5Fclosure_x24f0046test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal, ptr %"let$324_func_0.i.i", align 8
  %14 = tail call ptr @_M28let_x24324_x5Fclosure_x24f0046test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %"let$324_capture_0.i.i", ptr nocapture nofree readonly %8)
  %15 = tail call noundef ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %15, align 1
  %16 = getelementptr { i8, { ptr, ptr } }, ptr %15, i64 0, i32 1, i32 0
  store ptr %12, ptr %16, align 8
  %17 = getelementptr { i8, { ptr, ptr } }, ptr %15, i64 0, i32 1, i32 1
  store ptr %14, ptr %17, align 8
  br label %common.ret1
}

define internal noundef ptr @_M28let_x24f9a_x5Fclosure_x24f0446test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 8 %1) {
  %"string#$f87_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"string#$f87_0", i64 8
  %"string#$f87_0.val" = load ptr, ptr %3, align 8
  %4 = getelementptr i8, ptr %1, i64 8
  %.val = load ptr, ptr %4, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$861$cb1_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"string#$f87_0.val", ptr %"let$861$cb1_capture_0.i", align 8
  store ptr %"let$861$cb1_capture_0.i", ptr %5, align 8
  %"let$861$cb1_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M35let_x24861_x24cb1_x5Fclosure_x24f0c46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal, ptr %"let$861$cb1_func_0.i", align 8
  %6 = tail call ptr @malgo_string_append(ptr %"string#$f87_0.val", ptr %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %6, ptr %8, align 8
  ret ptr %7
}

define internal noundef ptr @_M28let_x24300_x5Fclosure_x24f0546test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %"_$37$2d5_0" = load ptr, ptr %0, align 8
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Nil_0.i, label %switch_branch_Cons_0.i

switch_branch_Nil_0.i:                            ; preds = %2
  %4 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  br label %common.ret1

switch_branch_Cons_0.i:                           ; preds = %2
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = load i8, ptr %8, align 1
  %switch1.i = icmp eq i8 %9, 0
  br i1 %switch1.i, label %switch_branch_Nil_1.i, label %switch_branch_Cons_1.i

switch_branch_Nil_1.i:                            ; preds = %switch_branch_Cons_0.i
  %10 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %10, align 1
  %11 = tail call noundef ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %11, align 1
  %12 = getelementptr { i8, { ptr, ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr %6, ptr %12, align 8
  %13 = getelementptr { i8, { ptr, ptr } }, ptr %11, i64 0, i32 1, i32 1
  store ptr %10, ptr %13, align 8
  br label %common.ret1

common.ret1:                                      ; preds = %switch_branch_Nil_0.i, %switch_branch_Nil_1.i, %switch_branch_Cons_1.i
  %common.ret1.op = phi ptr [ %19, %switch_branch_Cons_1.i ], [ %4, %switch_branch_Nil_0.i ], [ %11, %switch_branch_Nil_1.i ]
  ret ptr %common.ret1.op

switch_branch_Cons_1.i:                           ; preds = %switch_branch_Cons_0.i
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$300_capture_0.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"_$37$2d5_0", ptr %"let$300_capture_0.i.i", align 8
  store ptr %"let$300_capture_0.i.i", ptr %14, align 8
  %"let$300_func_0.i.i" = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @_M28let_x24300_x5Fclosure_x24f0546test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal, ptr %"let$300_func_0.i.i", align 8
  %15 = tail call ptr @_M28let_x24300_x5Fclosure_x24f0546test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %"let$300_capture_0.i.i", ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %8)
  %16 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %16, align 1
  %17 = getelementptr { i8, { ptr, ptr } }, ptr %16, i64 0, i32 1, i32 0
  store ptr %"_$37$2d5_0", ptr %17, align 8
  %18 = getelementptr { i8, { ptr, ptr } }, ptr %16, i64 0, i32 1, i32 1
  store ptr %15, ptr %18, align 8
  %19 = tail call noundef ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %19, align 1
  %20 = getelementptr { i8, { ptr, ptr } }, ptr %19, i64 0, i32 1, i32 0
  store ptr %6, ptr %20, align 8
  %21 = getelementptr { i8, { ptr, ptr } }, ptr %19, i64 0, i32 1, i32 1
  store ptr %16, ptr %21, align 8
  br label %common.ret1
}

define internal fastcc ptr @_M12concatString34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %_M10nil_x243ce34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) unnamed_addr {
  %1 = load i8, ptr %_M10nil_x243ce34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, align 1
  %switch = icmp eq i8 %1, 0
  br i1 %switch, label %switch_branch_Nil_0, label %switch_branch_Cons_0

common.ret:                                       ; preds = %switch_branch_Cons_0, %switch_branch_Nil_0
  %common.ret.op = phi ptr [ %2, %switch_branch_Nil_0 ], [ %12, %switch_branch_Cons_0 ]
  ret ptr %common.ret.op

switch_branch_Nil_0:                              ; preds = %0
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr @str3846, ptr %3, align 8
  br label %common.ret

switch_branch_Cons_0:                             ; preds = %0
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %_M10nil_x243ce34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %_M10nil_x243ce34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i64 0, i32 1, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$f9a_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %"let$f9a_capture_0.i", align 8
  store ptr %"let$f9a_capture_0.i", ptr %8, align 8
  %"let$f9a_func_0.i" = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @_M28let_x24f9a_x5Fclosure_x24f0446test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal, ptr %"let$f9a_func_0.i", align 8
  %9 = tail call fastcc ptr @_M12concatString34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr nocapture nofree readonly %7)
  %10 = load ptr, ptr %8, align 8
  %11 = load ptr, ptr %"let$f9a_func_0.i", align 8
  %12 = tail call ptr %11(ptr %10, ptr %9)
  br label %common.ret
}

define internal ptr @_M4show46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8External(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %_M12symbol_x247846test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal_0) {
  %2 = load i8, ptr %_M12symbol_x247846test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal_0, align 1
  switch i8 %2, label %switch_default_1 [
    i8 0, label %switch_branch_Symbol_0
    i8 1, label %switch_branch_SInt_0
    i8 2, label %switch_branch_SList_0
  ]

common.ret:                                       ; preds = %switch_branch_SList_0, %switch_branch_SInt_0, %switch_branch_Symbol_0
  %common.ret.op = phi ptr [ %12, %switch_branch_Symbol_0 ], [ %26, %switch_branch_SInt_0 ], [ %50, %switch_branch_SList_0 ]
  ret ptr %common.ret.op

switch_branch_Symbol_0:                           ; preds = %1
  %3 = getelementptr { i8, { ptr } }, ptr %_M12symbol_x247846test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal_0, i64 0, i32 1
  %4 = load ptr, ptr %3, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr @str3847, ptr %6, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$f9a_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %"let$f9a_capture_0.i", align 8
  store ptr %"let$f9a_capture_0.i", ptr %7, align 8
  %"let$f9a_func_0.i" = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @_M28let_x24f9a_x5Fclosure_x24f0446test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal, ptr %"let$f9a_func_0.i", align 8
  %"string#$f87_0.i" = load ptr, ptr %"let$f9a_capture_0.i", align 8
  %8 = getelementptr i8, ptr %"string#$f87_0.i", i64 8
  %"string#$f87_0.val.i" = load ptr, ptr %8, align 8
  %9 = getelementptr i8, ptr %4, i64 8
  %.val.i = load ptr, ptr %9, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$861$cb1_capture_0.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"string#$f87_0.val.i", ptr %"let$861$cb1_capture_0.i.i", align 8
  store ptr %"let$861$cb1_capture_0.i.i", ptr %10, align 8
  %"let$861$cb1_func_0.i.i" = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @_M35let_x24861_x24cb1_x5Fclosure_x24f0c46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal, ptr %"let$861$cb1_func_0.i.i", align 8
  %11 = tail call ptr @malgo_string_append(ptr %"string#$f87_0.val.i", ptr %.val.i)
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr %11, ptr %13, align 8
  br label %common.ret

switch_branch_SInt_0:                             ; preds = %1
  %14 = getelementptr { i8, { ptr } }, ptr %_M12symbol_x247846test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal_0, i64 0, i32 1
  %15 = load ptr, ptr %14, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %16, align 1
  %17 = getelementptr { i8, { ptr } }, ptr %16, i64 0, i32 1, i32 0
  store ptr @str3848, ptr %17, align 8
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$f9a_capture_0.i1" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %16, ptr %"let$f9a_capture_0.i1", align 8
  store ptr %"let$f9a_capture_0.i1", ptr %18, align 8
  %"let$f9a_func_0.i2" = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @_M28let_x24f9a_x5Fclosure_x24f0446test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal, ptr %"let$f9a_func_0.i2", align 8
  %19 = getelementptr { i8, { i32 } }, ptr %15, i64 0, i32 1
  %20 = load i32, ptr %19, align 4
  %21 = tail call ptr @malgo_int32_t_to_string(i32 %20)
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %22, align 1
  %23 = getelementptr { i8, { ptr } }, ptr %22, i64 0, i32 1, i32 0
  store ptr %21, ptr %23, align 8
  %24 = load ptr, ptr %18, align 8
  %25 = load ptr, ptr %"let$f9a_func_0.i2", align 8
  %26 = tail call ptr %25(ptr %24, ptr nonnull %22)
  br label %common.ret

switch_branch_SList_0:                            ; preds = %1
  %27 = getelementptr { i8, { ptr } }, ptr %_M12symbol_x247846test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal_0, i64 0, i32 1
  %28 = load ptr, ptr %27, align 8
  %29 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %29, align 1
  %30 = getelementptr { i8, { ptr } }, ptr %29, i64 0, i32 1, i32 0
  store ptr @str3849, ptr %30, align 8
  %31 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$f9a_capture_0.i3" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %29, ptr %"let$f9a_capture_0.i3", align 8
  store ptr %"let$f9a_capture_0.i3", ptr %31, align 8
  %"let$f9a_func_0.i4" = getelementptr { ptr, ptr }, ptr %31, i64 0, i32 1
  store ptr @_M28let_x24f9a_x5Fclosure_x24f0446test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal, ptr %"let$f9a_func_0.i4", align 8
  %32 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %32, align 1
  %33 = getelementptr { i8, { ptr } }, ptr %32, i64 0, i32 1, i32 0
  store ptr @str3850, ptr %33, align 8
  %34 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$300_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %32, ptr %"let$300_capture_0.i", align 8
  store ptr %"let$300_capture_0.i", ptr %34, align 8
  %"let$300_func_0.i" = getelementptr { ptr, ptr }, ptr %34, i64 0, i32 1
  store ptr @_M28let_x24300_x5Fclosure_x24f0546test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal, ptr %"let$300_func_0.i", align 8
  %35 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %35, align 8
  %show_func_0 = getelementptr { ptr, ptr }, ptr %35, i64 0, i32 1
  store ptr @_M4show46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8External, ptr %show_func_0, align 8
  %36 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$324_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %35, ptr %"let$324_capture_0.i", align 8
  store ptr %"let$324_capture_0.i", ptr %36, align 8
  %"let$324_func_0.i" = getelementptr { ptr, ptr }, ptr %36, i64 0, i32 1
  store ptr @_M28let_x24324_x5Fclosure_x24f0046test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal, ptr %"let$324_func_0.i", align 8
  %37 = tail call ptr @_M28let_x24324_x5Fclosure_x24f0046test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %"let$324_capture_0.i", ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %28)
  %38 = load ptr, ptr %34, align 8
  %39 = load ptr, ptr %"let$300_func_0.i", align 8
  %40 = tail call ptr %39(ptr %38, ptr %37)
  %41 = tail call fastcc ptr @_M12concatString34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %40)
  %42 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$f9a_capture_0.i5" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %41, ptr %"let$f9a_capture_0.i5", align 8
  store ptr %"let$f9a_capture_0.i5", ptr %42, align 8
  %"let$f9a_func_0.i6" = getelementptr { ptr, ptr }, ptr %42, i64 0, i32 1
  store ptr @_M28let_x24f9a_x5Fclosure_x24f0446test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal, ptr %"let$f9a_func_0.i6", align 8
  %43 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %43, align 1
  %44 = getelementptr { i8, { ptr } }, ptr %43, i64 0, i32 1, i32 0
  store ptr @str3851, ptr %44, align 8
  %45 = load ptr, ptr %42, align 8
  %46 = load ptr, ptr %"let$f9a_func_0.i6", align 8
  %47 = tail call ptr %46(ptr %45, ptr nonnull %43)
  %48 = load ptr, ptr %31, align 8
  %49 = load ptr, ptr %"let$f9a_func_0.i4", align 8
  %50 = tail call ptr %49(ptr %48, ptr %47)
  br label %common.ret

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M35let_x24861_x24cb1_x5Fclosure_x24f0c46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %"p$f89$f93$c94_0" = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_string_append(ptr %"p$f89$f93$c94_0", ptr %1)
  ret ptr %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr @str3841, ptr %4, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %3, ptr %6, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr @str3842, ptr %8, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %7, ptr %10, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr @str3843, ptr %12, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { ptr } }, ptr %13, i64 0, i32 1, i32 0
  store ptr %11, ptr %14, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %15, align 1
  %16 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %16, align 1
  %17 = getelementptr { i8, { ptr, ptr } }, ptr %16, i64 0, i32 1, i32 0
  store ptr %13, ptr %17, align 8
  %18 = getelementptr { i8, { ptr, ptr } }, ptr %16, i64 0, i32 1, i32 1
  store ptr %15, ptr %18, align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 2, ptr %19, align 1
  %20 = getelementptr { i8, { ptr } }, ptr %19, i64 0, i32 1, i32 0
  store ptr %16, ptr %20, align 8
  %21 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %21, align 1
  %22 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %22, align 1
  %23 = getelementptr { i8, { ptr, ptr } }, ptr %22, i64 0, i32 1, i32 0
  store ptr %19, ptr %23, align 8
  %24 = getelementptr { i8, { ptr, ptr } }, ptr %22, i64 0, i32 1, i32 1
  store ptr %21, ptr %24, align 8
  %25 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %25, align 1
  %26 = getelementptr { i8, { ptr, ptr } }, ptr %25, i64 0, i32 1, i32 0
  store ptr %9, ptr %26, align 8
  %27 = getelementptr { i8, { ptr, ptr } }, ptr %25, i64 0, i32 1, i32 1
  store ptr %22, ptr %27, align 8
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 2, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %28, i64 0, i32 1, i32 0
  store ptr %25, ptr %29, align 8
  %30 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %30, align 1
  %31 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %31, align 1
  %32 = getelementptr { i8, { ptr, ptr } }, ptr %31, i64 0, i32 1, i32 0
  store ptr %28, ptr %32, align 8
  %33 = getelementptr { i8, { ptr, ptr } }, ptr %31, i64 0, i32 1, i32 1
  store ptr %30, ptr %33, align 8
  %34 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %34, align 1
  %35 = getelementptr { i8, { ptr, ptr } }, ptr %34, i64 0, i32 1, i32 0
  store ptr %5, ptr %35, align 8
  %36 = getelementptr { i8, { ptr, ptr } }, ptr %34, i64 0, i32 1, i32 1
  store ptr %31, ptr %36, align 8
  %37 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 2, ptr %37, align 8
  %38 = getelementptr { i8, { ptr } }, ptr %37, i64 0, i32 1, i32 0
  store ptr %34, ptr %38, align 8
  %39 = tail call ptr @_M4show46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8External(ptr poison, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %37)
  %40 = getelementptr { i8, { ptr } }, ptr %39, i64 0, i32 1
  %41 = load ptr, ptr %40, align 8
  %42 = tail call ptr @malgo_print_string(ptr %41)
  %43 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %43, align 1
  %44 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %44, align 1
  %45 = tail call ptr @malgo_newline(ptr noundef nonnull %44)
  ret i32 0
}
