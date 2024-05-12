; ModuleID = 'test/testcases/malgo/DataDef.mlg'
source_filename = "test/testcases/malgo/DataDef.mlg"

declare void @GC_init()

declare ptr @malgo_int64_t_to_string(i64)

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_malloc(i64)

define internal ptr @_M3Nil44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8External(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @_M35malgo_x5Fint64_x5Ft_x5Fto_x5Fstring44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8External(ptr %0, i64 %_M7p_x241d44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_int64_t_to_string(i64 %_M7p_x241d44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8Temporal_0)
  ret ptr %2
}

define internal ptr @_M7Int_x2344test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8External(ptr %0, i64 %_M7p_x241244test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 %_M7p_x241244test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8Temporal_0, ptr %4, align 4
  ret ptr %2
}

define internal ptr @_M24malgo_x5Fprint_x5Fstring44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8External(ptr %0, ptr %_M7p_x241e44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_print_string(ptr %_M7p_x241e44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8Temporal_0)
  ret ptr %2
}

define internal ptr @_M4main44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8External(ptr %0, ptr %_M20_x24_x5F_x2411_x241f44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 1, ptr %4, align 4
  %5 = getelementptr { i8, <8 x i8> }, ptr %2, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  switch i8 %6, label %switch_default_0 [
    i8 0, label %"switch_branch_Int#_0"
  ]

"switch_branch_Int#_0":                           ; preds = %1
  %7 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1
  %8 = getelementptr { i64 }, ptr %7, i32 0, i32 0
  %9 = load i64, ptr %8, align 4
  %10 = call ptr @malgo_int64_t_to_string(i64 %9)
  %11 = call ptr @malgo_print_string(ptr %10)
  ret ptr %11

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M26let_x241c_x5Fclosure_x243344test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8Internal(ptr %0, ptr %1) {
  %"p$15_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"p$15_0" = load ptr, ptr %"p$15_addr_0", align 8
  %3 = call ptr @_M22raw_x5Flet_x241c_x243244test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8Temporal(ptr null, ptr %"p$15_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M4Cons44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8External(ptr %0, ptr %_M7p_x241544test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$1c_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"p$15_0" = getelementptr { ptr }, ptr %"let$1c_capture_0", i32 0, i32 0
  store ptr %_M7p_x241544test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8Temporal_0, ptr %"p$15_0", align 8
  %"let$1c_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$1c_capture_0", ptr %"let$1c_capture_1", align 8
  %"let$1c_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M26let_x241c_x5Fclosure_x243344test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8Internal, ptr %"let$1c_func_0", align 8
  ret ptr %2
}

define internal ptr @_M22raw_x5Flet_x241c_x243244test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8Temporal(ptr %0, ptr %_M7p_x241544test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8Temporal_0, ptr %_M7p_x241644test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %_M7p_x241544test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8Temporal_0, ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 1
  store ptr %_M7p_x241644test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8Temporal_0, ptr %5, align 8
  ret ptr %2
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/DataDef.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/DataDef.mlg"() {
  ret void
}
