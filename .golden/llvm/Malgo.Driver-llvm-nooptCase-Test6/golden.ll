; ModuleID = './test/testcases/malgo/Test6.mlg'
source_filename = "./test/testcases/malgo/Test6.mlg"

@str51 = unnamed_addr constant [6 x i8] c"WRONG\00"
@str52 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare ptr @malgo_exit_failure(ptr)

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_malloc(i64)

define internal ptr @Test6.B(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 2, ptr %3, align 1
  ret ptr %2
}

define internal ptr @Test6.main(ptr %0, ptr %"Test6.$$__39_0") {
  %2 = call ptr @Test6.R(ptr null)
  %3 = call ptr @Test6.rtob(ptr null, ptr %2)
  %4 = call ptr @"Test6.$fun_43"(ptr null, ptr %3)
  ret ptr %4
}

define internal ptr @Test6.malgo_exit_failure(ptr %0, ptr %"Test6.$p_33_0") {
  %2 = call ptr @malgo_exit_failure(ptr %"Test6.$p_33_0")
  ret ptr %2
}

define internal ptr @"Test6.$fun_43"(ptr %0, ptr %"Test6.$b_40_0") {
  %2 = getelementptr { i8, {} }, ptr %"Test6.$b_40_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_Test6.R_0
    i8 1, label %switch_branch_Test6.G_0
    i8 2, label %switch_branch_Test6.B_0
  ]

switch_branch_Test6.R_0:                          ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"Test6.$b_40_0", i32 0, i32 1
  %5 = call ptr @Test6.malgo_print_string(ptr null, ptr @str51)
  ret ptr %5

switch_branch_Test6.G_0:                          ; preds = %1
  %6 = getelementptr { i8, {} }, ptr %"Test6.$b_40_0", i32 0, i32 1
  %7 = call ptr @Test6.malgo_print_string(ptr null, ptr @str51)
  ret ptr %7

switch_branch_Test6.B_0:                          ; preds = %1
  %8 = getelementptr { i8, {} }, ptr %"Test6.$b_40_0", i32 0, i32 1
  %9 = call ptr @Test6.malgo_print_string(ptr null, ptr @str52)
  ret ptr %9

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @Test6.G(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @Test6.R(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @Test6.rtob(ptr %0, ptr %"Test6.$r_35_0") {
  %2 = getelementptr { i8, {} }, ptr %"Test6.$r_35_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_Test6.R_0
    i8 1, label %switch_branch_Test6.G_0
    i8 2, label %switch_branch_Test6.B_0
  ]

switch_branch_Test6.R_0:                          ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"Test6.$r_35_0", i32 0, i32 1
  %5 = call ptr @Test6.B(ptr null)
  ret ptr %5

switch_branch_Test6.G_0:                          ; preds = %1
  %6 = getelementptr { i8, {} }, ptr %"Test6.$r_35_0", i32 0, i32 1
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %8 = getelementptr { i8, {} }, ptr %7, i32 0, i32 0
  store i8 0, ptr %8, align 1
  %9 = call ptr @Test6.malgo_exit_failure(ptr null, ptr %7)
  ret ptr %9

switch_branch_Test6.B_0:                          ; preds = %1
  %10 = getelementptr { i8, {} }, ptr %"Test6.$r_35_0", i32 0, i32 1
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, {} }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  %13 = call ptr @Test6.malgo_exit_failure(ptr null, ptr %11)
  ret ptr %13

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @Test6.malgo_print_string(ptr %0, ptr %"Test6.$p_34_0") {
  %2 = call ptr @malgo_print_string(ptr %"Test6.$p_34_0")
  ret ptr %2
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @koriel_load_Test6()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @Test6.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @koriel_load_Test6() {
  ret void
}
