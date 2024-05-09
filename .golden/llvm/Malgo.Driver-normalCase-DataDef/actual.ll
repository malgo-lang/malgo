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

define internal ptr @_M35malgo_x5Fint64_x5Ft_x5Fto_x5Fstring44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8External(ptr %0, i64 %_M1p44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg10Temporal29_0) {
  %2 = call ptr @malgo_int64_t_to_string(i64 %_M1p44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg10Temporal29_0)
  ret ptr %2
}

define internal ptr @_M7Int_x2344test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8External(ptr %0, i64 %_M1p44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg10Temporal18_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 %_M1p44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg10Temporal18_0, ptr %4, align 4
  ret ptr %2
}

define internal ptr @_M24malgo_x5Fprint_x5Fstring44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8External(ptr %0, ptr %_M1p44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg10Temporal30_0) {
  %2 = call ptr @malgo_print_string(ptr %_M1p44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg10Temporal30_0)
  ret ptr %2
}

define internal ptr @_M4main44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8External(ptr %0, ptr %_M8_x24_x5F44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg10Temporal31_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 1, ptr %4, align 4
  %5 = getelementptr { i8, <8 x i8> }, ptr %2, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  switch i8 %6, label %switch_default_0 [
    i8 0, label %switch_branch__M7Int_x2344test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8External_0
  ]

switch_branch__M7Int_x2344test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8External_0: ; preds = %1
  %7 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1
  %8 = getelementptr { i64 }, ptr %7, i32 0, i32 0
  %9 = load i64, ptr %8, align 4
  %10 = call ptr @malgo_int64_t_to_string(i64 %9)
  %11 = call ptr @malgo_print_string(ptr %10)
  ret ptr %11

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M14let_x5Fclosure44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg10Internal51(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg10Temporal50(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M4Cons44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg8External(ptr %0, ptr %_M1p44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg10Temporal21_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1p44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg10Temporal21_0, ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg10Internal51, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M10raw_x5Flet44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg10Temporal50(ptr %0, ptr %_M1p44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg10Temporal21_0, ptr %_M1p44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg10Temporal22_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %_M1p44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg10Temporal21_0, ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 1
  store ptr %_M1p44test_x2Ftestcases_x2Fmalgo_x2FDataDef_x2Emlg10Temporal22_0, ptr %5, align 8
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
