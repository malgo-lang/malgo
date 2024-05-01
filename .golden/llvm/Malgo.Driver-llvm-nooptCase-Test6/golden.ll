; ModuleID = 'test/testcases/malgo/Test6.mlg'
source_filename = "test/testcases/malgo/Test6.mlg"

@str51 = unnamed_addr constant [6 x i8] c"WRONG\00"
@str52 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare ptr @malgo_exit_failure(ptr)

declare ptr @malgo_print_string(ptr)

define internal ptr @"test/testcases/malgo/Test6.mlg.$fun_43"(ptr %0, ptr %"test/testcases/malgo/Test6.mlg.$b_40_0") {
  %2 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test6.mlg.$b_40_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/Test6.mlg.R_0"
    i8 1, label %"switch_branch_test/testcases/malgo/Test6.mlg.G_0"
    i8 2, label %"switch_branch_test/testcases/malgo/Test6.mlg.B_0"
  ]

"switch_branch_test/testcases/malgo/Test6.mlg.R_0": ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test6.mlg.$b_40_0", i32 0, i32 1
  %5 = call ptr @"test/testcases/malgo/Test6.mlg.malgo_print_string"(ptr null, ptr @str51)
  ret ptr %5

"switch_branch_test/testcases/malgo/Test6.mlg.G_0": ; preds = %1
  %6 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test6.mlg.$b_40_0", i32 0, i32 1
  %7 = call ptr @"test/testcases/malgo/Test6.mlg.malgo_print_string"(ptr null, ptr @str51)
  ret ptr %7

"switch_branch_test/testcases/malgo/Test6.mlg.B_0": ; preds = %1
  %8 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test6.mlg.$b_40_0", i32 0, i32 1
  %9 = call ptr @"test/testcases/malgo/Test6.mlg.malgo_print_string"(ptr null, ptr @str52)
  ret ptr %9

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/Test6.mlg.malgo_exit_failure"(ptr %0, ptr %"test/testcases/malgo/Test6.mlg.$p_33_0") {
  %2 = call ptr @malgo_exit_failure(ptr %"test/testcases/malgo/Test6.mlg.$p_33_0")
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test6.mlg.main"(ptr %0, ptr %"test/testcases/malgo/Test6.mlg.$$__39_0") {
  %2 = call ptr @"test/testcases/malgo/Test6.mlg.R"(ptr null)
  %3 = call ptr @"test/testcases/malgo/Test6.mlg.rtob"(ptr null, ptr %2)
  %4 = call ptr @"test/testcases/malgo/Test6.mlg.$fun_43"(ptr null, ptr %3)
  ret ptr %4
}

declare ptr @malgo_malloc(i64)

define internal ptr @"test/testcases/malgo/Test6.mlg.B"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 2, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test6.mlg.malgo_print_string"(ptr %0, ptr %"test/testcases/malgo/Test6.mlg.$p_34_0") {
  %2 = call ptr @malgo_print_string(ptr %"test/testcases/malgo/Test6.mlg.$p_34_0")
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test6.mlg.R"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test6.mlg.G"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test6.mlg.rtob"(ptr %0, ptr %"test/testcases/malgo/Test6.mlg.$r_35_0") {
  %2 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test6.mlg.$r_35_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/Test6.mlg.R_0"
    i8 1, label %"switch_branch_test/testcases/malgo/Test6.mlg.G_0"
    i8 2, label %"switch_branch_test/testcases/malgo/Test6.mlg.B_0"
  ]

"switch_branch_test/testcases/malgo/Test6.mlg.R_0": ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test6.mlg.$r_35_0", i32 0, i32 1
  %5 = call ptr @"test/testcases/malgo/Test6.mlg.B"(ptr null)
  ret ptr %5

"switch_branch_test/testcases/malgo/Test6.mlg.G_0": ; preds = %1
  %6 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test6.mlg.$r_35_0", i32 0, i32 1
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %8 = getelementptr { i8, {} }, ptr %7, i32 0, i32 0
  store i8 0, ptr %8, align 1
  %9 = call ptr @"test/testcases/malgo/Test6.mlg.malgo_exit_failure"(ptr null, ptr %7)
  ret ptr %9

"switch_branch_test/testcases/malgo/Test6.mlg.B_0": ; preds = %1
  %10 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test6.mlg.$r_35_0", i32 0, i32 1
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, {} }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  %13 = call ptr @"test/testcases/malgo/Test6.mlg.malgo_exit_failure"(ptr null, ptr %11)
  ret ptr %13

switch_default_0:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/Test6.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/Test6.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/Test6.mlg"() {
  ret void
}
