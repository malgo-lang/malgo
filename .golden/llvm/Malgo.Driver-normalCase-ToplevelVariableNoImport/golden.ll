; ModuleID = 'test/testcases/malgo/ToplevelVariableNoImport.mlg'
source_filename = "test/testcases/malgo/ToplevelVariableNoImport.mlg"

@_M3one61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External = global ptr undef
@_M4comp61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External = global ptr undef
@str456 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare i32 @malgo_add_int32_t(i32, i32)

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_int32_t_to_string(i32)

declare ptr @malgo_malloc(i64)

define internal ptr @_M7Nothing61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal i32 @_M27let_x24ea_x5Fclosure_x241c661test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Internal(ptr %0, i32 %1) {
  %"x$0$df_addr_0" = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %"x$0$df_0" = load i32, ptr %"x$0$df_addr_0", align 4
  %3 = call i32 @malgo_add_int32_t(i32 %"x$0$df_0", i32 %1)
  ret i32 %3
}

define internal ptr @_M12addInt32_x2361test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr %0, i32 %_M12x_x240_x24df61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$ea_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %"x$0$df_0" = getelementptr { i32 }, ptr %"let$ea_capture_0", i32 0, i32 0
  store i32 %_M12x_x240_x24df61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, ptr %"x$0$df_0", align 4
  %"let$ea_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$ea_capture_0", ptr %"let$ea_capture_1", align 8
  %"let$ea_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x24ea_x5Fclosure_x241c661test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Internal, ptr %"let$ea_func_0", align 8
  ret ptr %2
}

define internal i32 @_M27let_x24b3_x5Fclosure_x241c761test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Internal(ptr %0, i32 %1) {
  %"p$ae_addr_0" = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %"p$ae_0" = load i32, ptr %"p$ae_addr_0", align 4
  %3 = call i32 @malgo_add_int32_t(i32 %"p$ae_0", i32 %1)
  ret i32 %3
}

define internal ptr @_M26malgo_x5Fadd_x5Fint32_x5Ft61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr %0, i32 %_M7p_x24ae61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$b3_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %"p$ae_0" = getelementptr { i32 }, ptr %"let$b3_capture_0", i32 0, i32 0
  store i32 %_M7p_x24ae61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, ptr %"p$ae_0", align 4
  %"let$b3_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$b3_capture_0", ptr %"let$b3_capture_1", align 8
  %"let$b3_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x24b3_x5Fclosure_x241c761test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Internal, ptr %"let$b3_func_0", align 8
  ret ptr %2
}

define internal ptr @_M35malgo_x5Fint32_x5Ft_x5Fto_x5Fstring61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr %0, i32 %_M7p_x24b561test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_int32_t_to_string(i32 %_M7p_x24b561test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0)
  ret ptr %2
}

define internal ptr @_M17toStringInt32_x2361test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr %0, i32 %_M12x_x24e_x24b661test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_int32_t_to_string(i32 %_M12x_x24e_x24b661test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0)
  ret ptr %2
}

define internal ptr @_M9Int32_x2361test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr %0, i32 %_M7p_x24a761test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 %_M7p_x24a761test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, ptr %4, align 4
  ret ptr %2
}

define internal ptr @_M24malgo_x5Fprint_x5Fstring61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr %0, ptr %_M7p_x24b461test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_print_string(ptr %_M7p_x24b461test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0)
  ret ptr %2
}

define internal ptr @_M15printString_x2361test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr %0, ptr %_M12x_x244_x24bd61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_print_string(ptr %_M12x_x244_x24bd61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0)
  ret ptr %2
}

define internal ptr @_M10String_x2361test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr %0, ptr %_M7p_x24a961test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %_M7p_x24a961test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, ptr %4, align 8
  ret ptr %2
}

define internal ptr @_M34let_x24ce_x24187_x5Fclosure_x241c961test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Internal(ptr %0, ptr %1) {
  %"cast$cf$17b_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"cast$cf$17b_0" = load ptr, ptr %"cast$cf$17b_addr_0", align 8
  %3 = call ptr @_M30raw_x5Flet_x24ce_x24187_x241c261test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal(ptr null, ptr %"cast$cf$17b_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M34let_x24ce_x24194_x5Fclosure_x241ca61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Internal(ptr %0, ptr %1) {
  %"cast$cf$17b_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"cast$cf$17b_0" = load ptr, ptr %"cast$cf$17b_addr_0", align 8
  %3 = call ptr @_M30raw_x5Flet_x24ce_x24194_x241c461test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal(ptr null, ptr %"cast$cf$17b_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M4main61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr %0, ptr %_M21_x24_x5F_x24a6_x2410361test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  %2 = load ptr, ptr @_M4comp61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %2, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_5 [
    i8 0, label %switch_branch_Nothing_0
    i8 1, label %switch_branch_Just_0
  ]

switch_branch_Nothing_0:                          ; preds = %1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 1, i32 0
  store ptr @str456, ptr %7, align 8
  %8 = getelementptr { i8, <8 x i8> }, ptr %5, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_2 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Nothing_0
  %10 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_print_string(ptr %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %const_capture_0 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 0
  store ptr null, ptr %const_capture_0, align 8
  %const_func_0 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 1
  store ptr @_M5const61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External, ptr %const_func_0, align 8
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %identity_capture_0 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  store ptr null, ptr %identity_capture_0, align 8
  %identity_func_0 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  store ptr @_M8identity61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External, ptr %identity_func_0, align 8
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$ce$187_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"cast$cf$17b_0" = getelementptr { ptr }, ptr %"let$ce$187_capture_0", i32 0, i32 0
  store ptr %15, ptr %"cast$cf$17b_0", align 8
  %"let$ce$187_capture_1" = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  store ptr %"let$ce$187_capture_0", ptr %"let$ce$187_capture_1", align 8
  %"let$ce$187_func_0" = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  store ptr @_M34let_x24ce_x24187_x5Fclosure_x241c961test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Internal, ptr %"let$ce$187_func_0", align 8
  %17 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  %18 = load ptr, ptr %17, align 8
  %19 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  %20 = load ptr, ptr %19, align 8
  %21 = call ptr %20(ptr %18, ptr %14)
  %22 = load ptr, ptr @_M3one61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External, align 8
  %23 = call ptr @_M8addInt3261test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr null, ptr %22)
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = load ptr, ptr @_M3one61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External, align 8
  %29 = call ptr %27(ptr %25, ptr %28)
  %30 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 0
  %31 = load ptr, ptr %30, align 8
  %32 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 1
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr %33(ptr %31, ptr %29)
  %35 = getelementptr { i8, <4 x i8> }, ptr %34, i32 0, i32 0
  %36 = load i8, ptr %35, align 1
  switch i8 %36, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %"switch_branch_String#_0"
  %37 = getelementptr { i8, { i32 } }, ptr %34, i32 0, i32 1
  %38 = getelementptr { i32 }, ptr %37, i32 0, i32 0
  %39 = load i32, ptr %38, align 4
  %40 = call ptr @malgo_int32_t_to_string(i32 %39)
  %41 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %42 = getelementptr { i8, { ptr } }, ptr %41, i32 0, i32 0
  store i8 0, ptr %42, align 1
  %43 = getelementptr { i8, { ptr } }, ptr %41, i32 0, i32 1, i32 0
  store ptr %40, ptr %43, align 8
  %44 = getelementptr { i8, <8 x i8> }, ptr %41, i32 0, i32 0
  %45 = load i8, ptr %44, align 1
  switch i8 %45, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_1"
  ]

"switch_branch_String#_1":                        ; preds = %"switch_branch_Int32#_0"
  %46 = getelementptr { i8, { ptr } }, ptr %41, i32 0, i32 1
  %47 = getelementptr { ptr }, ptr %46, i32 0, i32 0
  %48 = load ptr, ptr %47, align 8
  %49 = call ptr @malgo_print_string(ptr %48)
  ret ptr %49

switch_default_0:                                 ; preds = %"switch_branch_Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %"switch_branch_String#_0"
  unreachable

switch_default_2:                                 ; preds = %switch_branch_Nothing_0
  unreachable

switch_branch_Just_0:                             ; preds = %1
  %50 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1
  %51 = getelementptr { ptr }, ptr %50, i32 0, i32 0
  %52 = load ptr, ptr %51, align 8
  %53 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %const_capture_1 = getelementptr { ptr, ptr }, ptr %53, i32 0, i32 0
  store ptr null, ptr %const_capture_1, align 8
  %const_func_1 = getelementptr { ptr, ptr }, ptr %53, i32 0, i32 1
  store ptr @_M5const61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External, ptr %const_func_1, align 8
  %54 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %identity_capture_1 = getelementptr { ptr, ptr }, ptr %54, i32 0, i32 0
  store ptr null, ptr %identity_capture_1, align 8
  %identity_func_1 = getelementptr { ptr, ptr }, ptr %54, i32 0, i32 1
  store ptr @_M8identity61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External, ptr %identity_func_1, align 8
  %55 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$ce$194_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"cast$cf$17b_1" = getelementptr { ptr }, ptr %"let$ce$194_capture_0", i32 0, i32 0
  store ptr %54, ptr %"cast$cf$17b_1", align 8
  %"let$ce$194_capture_1" = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 0
  store ptr %"let$ce$194_capture_0", ptr %"let$ce$194_capture_1", align 8
  %"let$ce$194_func_0" = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 1
  store ptr @_M34let_x24ce_x24194_x5Fclosure_x241ca61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Internal, ptr %"let$ce$194_func_0", align 8
  %56 = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 0
  %57 = load ptr, ptr %56, align 8
  %58 = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 1
  %59 = load ptr, ptr %58, align 8
  %60 = call ptr %59(ptr %57, ptr %53)
  %61 = load ptr, ptr @_M3one61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External, align 8
  %62 = call ptr @_M8addInt3261test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr null, ptr %61)
  %63 = getelementptr { ptr, ptr }, ptr %62, i32 0, i32 0
  %64 = load ptr, ptr %63, align 8
  %65 = getelementptr { ptr, ptr }, ptr %62, i32 0, i32 1
  %66 = load ptr, ptr %65, align 8
  %67 = load ptr, ptr @_M3one61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External, align 8
  %68 = call ptr %66(ptr %64, ptr %67)
  %69 = getelementptr { ptr, ptr }, ptr %60, i32 0, i32 0
  %70 = load ptr, ptr %69, align 8
  %71 = getelementptr { ptr, ptr }, ptr %60, i32 0, i32 1
  %72 = load ptr, ptr %71, align 8
  %73 = call ptr %72(ptr %70, ptr %68)
  %74 = getelementptr { i8, <4 x i8> }, ptr %73, i32 0, i32 0
  %75 = load i8, ptr %74, align 1
  switch i8 %75, label %switch_default_4 [
    i8 0, label %"switch_branch_Int32#_1"
  ]

"switch_branch_Int32#_1":                         ; preds = %switch_branch_Just_0
  %76 = getelementptr { i8, { i32 } }, ptr %73, i32 0, i32 1
  %77 = getelementptr { i32 }, ptr %76, i32 0, i32 0
  %78 = load i32, ptr %77, align 4
  %79 = call ptr @malgo_int32_t_to_string(i32 %78)
  %80 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %81 = getelementptr { i8, { ptr } }, ptr %80, i32 0, i32 0
  store i8 0, ptr %81, align 1
  %82 = getelementptr { i8, { ptr } }, ptr %80, i32 0, i32 1, i32 0
  store ptr %79, ptr %82, align 8
  %83 = getelementptr { i8, <8 x i8> }, ptr %80, i32 0, i32 0
  %84 = load i8, ptr %83, align 1
  switch i8 %84, label %switch_default_3 [
    i8 0, label %"switch_branch_String#_2"
  ]

"switch_branch_String#_2":                        ; preds = %"switch_branch_Int32#_1"
  %85 = getelementptr { i8, { ptr } }, ptr %80, i32 0, i32 1
  %86 = getelementptr { ptr }, ptr %85, i32 0, i32 0
  %87 = load ptr, ptr %86, align 8
  %88 = call ptr @malgo_print_string(ptr %87)
  ret ptr %88

switch_default_3:                                 ; preds = %"switch_branch_Int32#_1"
  unreachable

switch_default_4:                                 ; preds = %switch_branch_Just_0
  unreachable

switch_default_5:                                 ; preds = %1
  unreachable
}

define internal ptr @_M27let_x24fe_x5Fclosure_x241cb61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Internal(ptr %0, ptr %1) {
  %"int32#$eb_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#$eb_0" = load ptr, ptr %"int32#$eb_addr_0", align 8
  %3 = call ptr @_M23raw_x5Flet_x24fe_x241c161test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal(ptr null, ptr %"int32#$eb_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M8addInt3261test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr %0, ptr %_M15int32_x23_x24eb61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$fe_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#$eb_0" = getelementptr { ptr }, ptr %"let$fe_capture_0", i32 0, i32 0
  store ptr %_M15int32_x23_x24eb61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, ptr %"int32#$eb_0", align 8
  %"let$fe_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$fe_capture_0", ptr %"let$fe_capture_1", align 8
  %"let$fe_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x24fe_x5Fclosure_x241cb61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Internal, ptr %"let$fe_func_0", align 8
  ret ptr %2
}

define internal ptr @_M27let_x24ce_x5Fclosure_x241cc61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Internal(ptr %0, ptr %1) {
  %"a$a$c9_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"a$a$c9_0" = load ptr, ptr %"a$a$c9_addr_0", align 8
  %3 = call ptr @_M23raw_x5Flet_x24ce_x241bf61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal(ptr null, ptr %"a$a$c9_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M5const61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr %0, ptr %_M12a_x24a_x24c961test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$ce_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"a$a$c9_0" = getelementptr { ptr }, ptr %"let$ce_capture_0", i32 0, i32 0
  store ptr %_M12a_x24a_x24c961test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, ptr %"a$a$c9_0", align 8
  %"let$ce_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$ce_capture_0", ptr %"let$ce_capture_1", align 8
  %"let$ce_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x24ce_x5Fclosure_x241cc61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Internal, ptr %"let$ce_func_0", align 8
  ret ptr %2
}

define internal ptr @_M34let_x24ce_x24133_x5Fclosure_x241cd61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Internal(ptr %0, ptr %1) {
  %"cast$cf_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"cast$cf_0" = load ptr, ptr %"cast$cf_addr_0", align 8
  %3 = call ptr @_M30raw_x5Flet_x24ce_x24133_x241c061test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal(ptr null, ptr %"cast$cf_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M7constId61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr %0, ptr %_M9eta_x24d261test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %identity_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %identity_capture_0, align 8
  %identity_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M8identity61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External, ptr %identity_func_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$ce$133_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"cast$cf_0" = getelementptr { ptr }, ptr %"let$ce$133_capture_0", i32 0, i32 0
  store ptr %2, ptr %"cast$cf_0", align 8
  %"let$ce$133_capture_1" = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %"let$ce$133_capture_0", ptr %"let$ce$133_capture_1", align 8
  %"let$ce$133_func_0" = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @_M34let_x24ce_x24133_x5Fclosure_x241cd61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Internal, ptr %"let$ce$133_func_0", align 8
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %_M9eta_x24d261test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0)
  ret ptr %8
}

define internal ptr @_M10printInt3261test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr %0, ptr %_M12i_x24d_x24c261test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M12i_x24d_x24c261test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M12i_x24d_x24c261test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = call ptr @malgo_int32_t_to_string(i32 %6)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1, i32 0
  store ptr %7, ptr %10, align 8
  %11 = getelementptr { i8, <8 x i8> }, ptr %8, i32 0, i32 0
  %12 = load i8, ptr %11, align 1
  switch i8 %12, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %"switch_branch_Int32#_0"
  %13 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1
  %14 = getelementptr { ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr @malgo_print_string(ptr %15)
  ret ptr %16

switch_default_0:                                 ; preds = %"switch_branch_Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M11printString61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr %0, ptr %_M16string_x23_x24bf61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M16string_x23_x24bf61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M16string_x23_x24bf61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_print_string(ptr %6)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M4Just61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr %0, ptr %_M7p_x24ac61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %_M7p_x24ac61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, ptr %4, align 8
  ret ptr %2
}

define internal ptr @_M6addOne61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr %0, ptr %_M10eta_x2410161test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  %2 = load ptr, ptr @_M3one61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External, align 8
  %3 = call ptr @_M8addInt3261test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr null, ptr %2)
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %_M10eta_x2410161test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0)
  ret ptr %8
}

define internal ptr @_M8identity61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr %0, ptr %_M12x_x247_x24c861test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  ret ptr %_M12x_x247_x24c861test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0
}

define internal ptr @_M13toStringInt3261test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr %0, ptr %_M15int32_x23_x24b861test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M15int32_x23_x24b861test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M15int32_x23_x24b861test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = call ptr @malgo_int32_t_to_string(i32 %6)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1, i32 0
  store ptr %7, ptr %10, align 8
  ret ptr %8

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M30raw_x5Flet_x24ce_x24133_x241c061test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal(ptr %0, ptr %_M10cast_x24cf61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, ptr %_M22_x5F_x24b_x24ca_x2413461test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  ret ptr %_M10cast_x24cf61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0
}

define internal ptr @_M30raw_x5Flet_x24ce_x24187_x241c261test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal(ptr %0, ptr %_M17cast_x24cf_x2417b61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, ptr %_M22_x5F_x24b_x24ca_x2418861test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  ret ptr %_M17cast_x24cf_x2417b61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0
}

define internal ptr @_M30raw_x5Flet_x24ce_x24187_x241c361test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal(ptr %0, ptr %_M17cast_x24cf_x2417b61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, ptr %_M22_x5F_x24b_x24ca_x2418861test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  ret ptr %_M17cast_x24cf_x2417b61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0
}

define internal ptr @_M30raw_x5Flet_x24ce_x24194_x241c461test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal(ptr %0, ptr %_M17cast_x24cf_x2417b61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, ptr %_M22_x5F_x24b_x24ca_x2419561test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  ret ptr %_M17cast_x24cf_x2417b61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0
}

define internal ptr @_M30raw_x5Flet_x24ce_x241a1_x241c561test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal(ptr %0, ptr %_M17cast_x24cf_x2417b61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, ptr %_M22_x5F_x24b_x24ca_x241a261test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  ret ptr %_M17cast_x24cf_x2417b61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0
}

define internal ptr @_M23raw_x5Flet_x24ce_x241bf61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal(ptr %0, ptr %_M12a_x24a_x24c961test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, ptr %_M15_x5F_x24b_x24ca61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  ret ptr %_M12a_x24a_x24c961test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0
}

define internal i32 @_M34let_x24b3_x2416c_x5Fclosure_x241ce61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Internal(ptr %0, i32 %1) {
  %"p$ed$f7$14f_addr_0" = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %"p$ed$f7$14f_0" = load i32, ptr %"p$ed$f7$14f_addr_0", align 4
  %3 = call i32 @malgo_add_int32_t(i32 %"p$ed$f7$14f_0", i32 %1)
  ret i32 %3
}

define internal ptr @_M23raw_x5Flet_x24fe_x241c161test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal(ptr %0, ptr %_M15int32_x23_x24eb61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, ptr %_M15int32_x23_x24ec61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M15int32_x23_x24eb61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M15int32_x23_x24eb61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %_M15int32_x23_x24ec61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Int32#_1"
  ]

"switch_branch_Int32#_1":                         ; preds = %"switch_branch_Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %_M15int32_x23_x24ec61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Temporal_0, i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$b3$16c_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %"p$ed$f7$14f_0" = getelementptr { i32 }, ptr %"let$b3$16c_capture_0", i32 0, i32 0
  store i32 %6, ptr %"p$ed$f7$14f_0", align 4
  %"let$b3$16c_capture_1" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %"let$b3$16c_capture_0", ptr %"let$b3$16c_capture_1", align 8
  %"let$b3$16c_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M34let_x24b3_x2416c_x5Fclosure_x241ce61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8Internal, ptr %"let$b3$16c_func_0", align 8
  %13 = call i32 @malgo_add_int32_t(i32 %6, i32 %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 1, i32 0
  store i32 %13, ptr %16, align 4
  ret ptr %14

switch_default_0:                                 ; preds = %"switch_branch_Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/ToplevelVariableNoImport.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/ToplevelVariableNoImport.mlg"() {
  %1 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %2 = getelementptr { i8, { i32 } }, ptr %1, i32 0, i32 0
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %1, i32 0, i32 1, i32 0
  store i32 1, ptr %3, align 4
  store ptr %1, ptr @_M3one61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External, align 8
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %7 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 0
  store i8 1, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1, i32 0
  store ptr %4, ptr %8, align 8
  %9 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %10 = load i8, ptr %9, align 1
  switch i8 %10, label %switch_default_0 [
    i8 0, label %switch_branch_Nothing_0
    i8 1, label %switch_branch_Just_0
  ]

switch_branch_Nothing_0:                          ; preds = %0
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, {} }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  store ptr %11, ptr @_M4comp61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External, align 8
  ret void

switch_branch_Just_0:                             ; preds = %0
  %13 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %14 = getelementptr { ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  store ptr %16, ptr @_M4comp61test_x2Ftestcases_x2Fmalgo_x2FToplevelVariableNoImport_x2Emlg8External, align 8
  ret void

switch_default_0:                                 ; preds = %0
  unreachable
}
