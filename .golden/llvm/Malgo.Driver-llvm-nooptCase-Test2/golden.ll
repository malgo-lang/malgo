; ModuleID = 'test/testcases/malgo/Test2.mlg'
source_filename = "test/testcases/malgo/Test2.mlg"

@str40 = unnamed_addr constant [6 x i8] c"WRONG\00"
@str41 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_malloc(i64)

define internal ptr @"test/testcases/malgo/Test2.mlg.B"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 2, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test2.mlg.G"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test2.mlg.R"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test2.mlg.malgo_print_string"(ptr %0, ptr %"test/testcases/malgo/Test2.mlg.$p_26_0") {
  %2 = call ptr @malgo_print_string(ptr %"test/testcases/malgo/Test2.mlg.$p_26_0")
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test2.mlg.main"(ptr %0, ptr %"test/testcases/malgo/Test2.mlg.$$__28_0") {
  %2 = call ptr @"test/testcases/malgo/Test2.mlg.R"(ptr null)
  %3 = call ptr @"test/testcases/malgo/Test2.mlg.rtob"(ptr null, ptr %2)
  %4 = call ptr @"test/testcases/malgo/Test2.mlg.$fun_32"(ptr null, ptr %3)
  ret ptr %4
}

define internal ptr @"test/testcases/malgo/Test2.mlg.$fun_32"(ptr %0, ptr %"test/testcases/malgo/Test2.mlg.$b_29_0") {
  %2 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test2.mlg.$b_29_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/Test2.mlg.R_0"
    i8 1, label %"switch_branch_test/testcases/malgo/Test2.mlg.G_0"
    i8 2, label %"switch_branch_test/testcases/malgo/Test2.mlg.B_0"
  ]

"switch_branch_test/testcases/malgo/Test2.mlg.R_0": ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test2.mlg.$b_29_0", i32 0, i32 1
  %5 = call ptr @"test/testcases/malgo/Test2.mlg.malgo_print_string"(ptr null, ptr @str40)
  ret ptr %5

"switch_branch_test/testcases/malgo/Test2.mlg.G_0": ; preds = %1
  %6 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test2.mlg.$b_29_0", i32 0, i32 1
  %7 = call ptr @"test/testcases/malgo/Test2.mlg.malgo_print_string"(ptr null, ptr @str40)
  ret ptr %7

"switch_branch_test/testcases/malgo/Test2.mlg.B_0": ; preds = %1
  %8 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test2.mlg.$b_29_0", i32 0, i32 1
  %9 = call ptr @"test/testcases/malgo/Test2.mlg.malgo_print_string"(ptr null, ptr @str41)
  ret ptr %9

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/Test2.mlg.rtob"(ptr %0, ptr %"test/testcases/malgo/Test2.mlg.$r_27_0") {
  %2 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test2.mlg.$r_27_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/Test2.mlg.R_0"
    i8 1, label %"switch_branch_test/testcases/malgo/Test2.mlg.G_0"
    i8 2, label %"switch_branch_test/testcases/malgo/Test2.mlg.B_0"
  ]

"switch_branch_test/testcases/malgo/Test2.mlg.R_0": ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test2.mlg.$r_27_0", i32 0, i32 1
  %5 = call ptr @"test/testcases/malgo/Test2.mlg.B"(ptr null)
  ret ptr %5

"switch_branch_test/testcases/malgo/Test2.mlg.G_0": ; preds = %1
  %6 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test2.mlg.$r_27_0", i32 0, i32 1
  ret ptr %"test/testcases/malgo/Test2.mlg.$r_27_0"

"switch_branch_test/testcases/malgo/Test2.mlg.B_0": ; preds = %1
  %7 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test2.mlg.$r_27_0", i32 0, i32 1
  ret ptr %"test/testcases/malgo/Test2.mlg.$r_27_0"

switch_default_0:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/Test2.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/Test2.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/Test2.mlg"() {
  ret void
}
