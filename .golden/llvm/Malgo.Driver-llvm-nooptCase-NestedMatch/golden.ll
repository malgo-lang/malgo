; ModuleID = './test/testcases/malgo/NestedMatch.mlg'
source_filename = "./test/testcases/malgo/NestedMatch.mlg"

@str61 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_exit_failure(ptr)

define internal ptr @NestedMatch.malgo_exit_failure(ptr %0, ptr %"NestedMatch.$p_37_0") {
  %2 = call ptr @malgo_exit_failure(ptr %"NestedMatch.$p_37_0")
  ret ptr %2
}

declare ptr @malgo_malloc(i64)

define internal ptr @NestedMatch.True2(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @NestedMatch.False2(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"NestedMatch.$fun_44"(ptr %0, ptr %"NestedMatch.$true_39_0") {
  %2 = getelementptr { i8, {} }, ptr %"NestedMatch.$true_39_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_NestedMatch.False_0
    i8 1, label %switch_branch_NestedMatch.True_0
  ]

switch_branch_NestedMatch.False_0:                ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"NestedMatch.$true_39_0", i32 0, i32 1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, {} }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = call ptr @NestedMatch.malgo_exit_failure(ptr null, ptr %5)
  ret ptr %7

switch_branch_NestedMatch.True_0:                 ; preds = %1
  %8 = getelementptr { i8, {} }, ptr %"NestedMatch.$true_39_0", i32 0, i32 1
  %9 = call ptr @NestedMatch.malgo_print_string(ptr null, ptr @str61)
  ret ptr %9

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @NestedMatch.main(ptr %0, ptr %"NestedMatch.$$__38_0") {
  %2 = call ptr @NestedMatch.True2(ptr null)
  %3 = call ptr @"NestedMatch.$fun_50"(ptr null, ptr %2)
  %4 = call ptr @"NestedMatch.$fun_47"(ptr null, ptr %3)
  %5 = call ptr @"NestedMatch.$fun_44"(ptr null, ptr %4)
  ret ptr %5
}

define internal ptr @NestedMatch.malgo_print_string(ptr %0, ptr %"NestedMatch.$p_36_0") {
  %2 = call ptr @malgo_print_string(ptr %"NestedMatch.$p_36_0")
  ret ptr %2
}

define internal ptr @"NestedMatch.$fun_47"(ptr %0, ptr %"NestedMatch.$true1_46_0") {
  %2 = getelementptr { i8, {} }, ptr %"NestedMatch.$true1_46_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_NestedMatch.False1_0
    i8 1, label %switch_branch_NestedMatch.True1_0
  ]

switch_branch_NestedMatch.False1_0:               ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"NestedMatch.$true1_46_0", i32 0, i32 1
  %5 = call ptr @NestedMatch.True(ptr null)
  ret ptr %5

switch_branch_NestedMatch.True1_0:                ; preds = %1
  %6 = getelementptr { i8, {} }, ptr %"NestedMatch.$true1_46_0", i32 0, i32 1
  %7 = call ptr @NestedMatch.False(ptr null)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @NestedMatch.True(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"NestedMatch.$fun_50"(ptr %0, ptr %"NestedMatch.$true2_49_0") {
  %2 = getelementptr { i8, {} }, ptr %"NestedMatch.$true2_49_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_NestedMatch.False2_0
    i8 1, label %switch_branch_NestedMatch.True2_0
  ]

switch_branch_NestedMatch.False2_0:               ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"NestedMatch.$true2_49_0", i32 0, i32 1
  %5 = call ptr @NestedMatch.True1(ptr null)
  ret ptr %5

switch_branch_NestedMatch.True2_0:                ; preds = %1
  %6 = getelementptr { i8, {} }, ptr %"NestedMatch.$true2_49_0", i32 0, i32 1
  %7 = call ptr @NestedMatch.False1(ptr null)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @NestedMatch.False1(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @NestedMatch.True1(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @NestedMatch.False(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @malgo_load_NestedMatch()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @NestedMatch.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @malgo_load_NestedMatch() {
  ret void
}
