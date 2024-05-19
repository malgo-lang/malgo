; ModuleID = '.malgo-work/test/testcases/malgo/ToplevelVariable.ll'
source_filename = "test/testcases/malgo/ToplevelVariable.mlg"

@_M3one53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External = local_unnamed_addr global ptr undef
@_M4comp53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External = local_unnamed_addr global ptr undef
@str3779 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define internal ptr @_M35let_x243cf_x24e84_x5Fclosure_x24ec453test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) #0 {
  %"cast$54$e78_0" = load ptr, ptr %0, align 8
  ret ptr %"cast$54$e78_0"
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define internal ptr @_M35let_x243cf_x24e91_x5Fclosure_x24ec553test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) #0 {
  %"cast$54$e78_0" = load ptr, ptr %0, align 8
  ret ptr %"cast$54$e78_0"
}

define internal noundef ptr @_M29let_x2410c5_x5Fclosure_x24ec653test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int32#$10ae_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int32#$10ae_0", i64 4
  %"int32#$10ae_0.val" = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$715$cda_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#$10ae_0.val", ptr %"let$715$cda_capture_0.i", align 4
  store ptr %"let$715$cda_capture_0.i", ptr %5, align 8
  %"let$715$cda_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M35let_x24715_x24cda_x5Fclosure_x24ec953test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8Internal, ptr %"let$715$cda_func_0.i", align 8
  %6 = tail call i32 @malgo_add_int32_t(i32 %"int32#$10ae_0.val", i32 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 %6, ptr %8, align 4
  ret ptr %7
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define internal ptr @_M28let_x243cf_x5Fclosure_x24ec753test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) #0 {
  %"a$4$3ca_0" = load ptr, ptr %0, align 8
  ret ptr %"a$4$3ca_0"
}

define internal ptr @_M5const34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr nocapture nofree readnone %0, ptr nofree %_M13a_x244_x243ca34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$3cf_capture_0" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %_M13a_x244_x243ca34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, ptr %"let$3cf_capture_0", align 8
  store ptr %"let$3cf_capture_0", ptr %2, align 8
  %"let$3cf_func_0" = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @_M28let_x243cf_x5Fclosure_x24ec753test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8Internal, ptr %"let$3cf_func_0", align 8
  ret ptr %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define internal ptr @_M8identity34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr nocapture nofree readnone %0, ptr nofree readnone returned %_M13x_x241_x2438534runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) #1 {
  ret ptr %_M13x_x241_x2438534runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0
}

define internal i32 @_M35let_x24715_x24cda_x5Fclosure_x24ec953test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %"p$10b0$10bd$cbd_0" = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %"p$10b0$10bd$cbd_0", i32 %1)
  ret i32 %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %2, i64 0, i32 1, i32 0
  store i32 1, ptr %3, align 4
  store ptr %2, ptr @_M3one53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %4, ptr %6, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %7, align 1
  store ptr %7, ptr @_M4comp53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = load ptr, ptr @_M4comp53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External, align 8
  %10 = load i8, ptr %9, align 1
  %switch.i = icmp eq i8 %10, 0
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  br i1 %switch.i, label %switch_branch_Nothing_0.i, label %switch_branch_Just_0.i

switch_branch_Nothing_0.i:                        ; preds = %1
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr @str3779, ptr %12, align 8
  %13 = tail call ptr @malgo_print_string(ptr noundef nonnull @str3779)
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %const_func_0.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @_M5const34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %const_func_0.i, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %identity_func_0.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @_M8identity34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %identity_func_0.i, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$3cf$e84_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %15, ptr %"let$3cf$e84_capture_0.i", align 8
  store ptr %"let$3cf$e84_capture_0.i", ptr %16, align 8
  %"let$3cf$e84_func_0.i" = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @_M35let_x243cf_x24e84_x5Fclosure_x24ec453test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8Internal, ptr %"let$3cf$e84_func_0.i", align 8
  %"cast$54$e78_0.i.i" = load ptr, ptr %"let$3cf$e84_capture_0.i", align 8
  %17 = load ptr, ptr @_M3one53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External, align 8
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$10c5_capture_0.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %17, ptr %"let$10c5_capture_0.i.i", align 8
  store ptr %"let$10c5_capture_0.i.i", ptr %18, align 8
  br label %_M4main53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External.exit

switch_branch_Just_0.i:                           ; preds = %1
  store ptr null, ptr %11, align 8
  %const_func_1.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @_M5const34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %const_func_1.i, align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %19, align 8
  %identity_func_1.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @_M8identity34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %identity_func_1.i, align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$3cf$e91_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %19, ptr %"let$3cf$e91_capture_0.i", align 8
  store ptr %"let$3cf$e91_capture_0.i", ptr %20, align 8
  %"let$3cf$e91_func_0.i" = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @_M35let_x243cf_x24e91_x5Fclosure_x24ec553test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8Internal, ptr %"let$3cf$e91_func_0.i", align 8
  %"cast$54$e78_0.i1.i" = load ptr, ptr %"let$3cf$e91_capture_0.i", align 8
  %21 = load ptr, ptr @_M3one53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External, align 8
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$10c5_capture_0.i2.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %21, ptr %"let$10c5_capture_0.i2.i", align 8
  store ptr %"let$10c5_capture_0.i2.i", ptr %22, align 8
  br label %_M4main53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External.exit

_M4main53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External.exit: ; preds = %switch_branch_Nothing_0.i, %switch_branch_Just_0.i
  %.sink.i = phi ptr [ %22, %switch_branch_Just_0.i ], [ %18, %switch_branch_Nothing_0.i ]
  %"let$10c5_capture_0.i2.sink.i" = phi ptr [ %"let$10c5_capture_0.i2.i", %switch_branch_Just_0.i ], [ %"let$10c5_capture_0.i.i", %switch_branch_Nothing_0.i ]
  %"cast$54$e78_0.i1.sink12.i" = phi ptr [ %"cast$54$e78_0.i1.i", %switch_branch_Just_0.i ], [ %"cast$54$e78_0.i.i", %switch_branch_Nothing_0.i ]
  %"let$10c5_func_0.i3.i" = getelementptr { ptr, ptr }, ptr %.sink.i, i64 0, i32 1
  store ptr @_M29let_x2410c5_x5Fclosure_x24ec653test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8Internal, ptr %"let$10c5_func_0.i3.i", align 8
  %23 = load ptr, ptr @_M3one53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External, align 8
  %"int32#$10ae_0.i.i" = load ptr, ptr %"let$10c5_capture_0.i2.sink.i", align 8
  %24 = getelementptr i8, ptr %"int32#$10ae_0.i.i", i64 4
  %"int32#$10ae_0.val.i.i" = load i32, ptr %24, align 4
  %25 = getelementptr i8, ptr %23, i64 4
  %.val.i.i = load i32, ptr %25, align 4
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$715$cda_capture_0.i.i.i" = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#$10ae_0.val.i.i", ptr %"let$715$cda_capture_0.i.i.i", align 4
  store ptr %"let$715$cda_capture_0.i.i.i", ptr %26, align 8
  %"let$715$cda_func_0.i.i.i" = getelementptr { ptr, ptr }, ptr %26, i64 0, i32 1
  store ptr @_M35let_x24715_x24cda_x5Fclosure_x24ec953test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8Internal, ptr %"let$715$cda_func_0.i.i.i", align 8
  %27 = tail call i32 @malgo_add_int32_t(i32 %"int32#$10ae_0.val.i.i", i32 %.val.i.i)
  %28 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { i32 } }, ptr %28, i64 0, i32 1, i32 0
  store i32 %27, ptr %29, align 4
  %30 = load ptr, ptr %"cast$54$e78_0.i1.sink12.i", align 8
  %31 = getelementptr { ptr, ptr }, ptr %"cast$54$e78_0.i1.sink12.i", i64 0, i32 1
  %32 = load ptr, ptr %31, align 8
  %33 = tail call ptr %32(ptr %30, ptr nonnull %28)
  %34 = getelementptr { i8, { i32 } }, ptr %33, i64 0, i32 1
  %35 = load i32, ptr %34, align 4
  %36 = tail call ptr @malgo_int32_t_to_string(i32 %35)
  %37 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %37, align 1
  %38 = getelementptr { i8, { ptr } }, ptr %37, i64 0, i32 1, i32 0
  store ptr %36, ptr %38, align 8
  %39 = tail call ptr @malgo_print_string(ptr %36)
  ret i32 0
}

attributes #0 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn }
attributes #1 = { mustprogress nofree norecurse nosync nounwind readnone willreturn }
