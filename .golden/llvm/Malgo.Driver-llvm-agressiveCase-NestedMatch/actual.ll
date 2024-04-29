; ModuleID = './test/testcases/malgo/NestedMatch.mlg'
source_filename = "./test/testcases/malgo/NestedMatch.mlg"

@str80 = unnamed_addr constant [3 x i8] c"OK\00"

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

define internal ptr @NestedMatch.main(ptr %0, ptr %"NestedMatch.$$__38_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  %5 = load i8, ptr %4, align 1
  switch i8 %5, label %switch_default_6 [
    i8 0, label %switch_branch_NestedMatch.False2_0
    i8 1, label %switch_branch_NestedMatch.True2_0
  ]

switch_branch_NestedMatch.False2_0:               ; preds = %1
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %7 = getelementptr { i8, {} }, ptr %6, i32 0, i32 0
  store i8 1, ptr %7, align 1
  %8 = getelementptr { i8, {} }, ptr %6, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_2 [
    i8 0, label %switch_branch_NestedMatch.False1_0
    i8 1, label %switch_branch_NestedMatch.True1_0
  ]

switch_branch_NestedMatch.False1_0:               ; preds = %switch_branch_NestedMatch.False2_0
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %11 = getelementptr { i8, {} }, ptr %10, i32 0, i32 0
  store i8 1, ptr %11, align 1
  %12 = getelementptr { i8, {} }, ptr %10, i32 0, i32 0
  %13 = load i8, ptr %12, align 1
  switch i8 %13, label %switch_default_0 [
    i8 0, label %switch_branch_NestedMatch.False_0
    i8 1, label %switch_branch_NestedMatch.True_0
  ]

switch_branch_NestedMatch.False_0:                ; preds = %switch_branch_NestedMatch.False1_0
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, {} }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = call ptr @malgo_exit_failure(ptr %14)
  ret ptr %16

switch_branch_NestedMatch.True_0:                 ; preds = %switch_branch_NestedMatch.False1_0
  %17 = call ptr @malgo_print_string(ptr @str80)
  ret ptr %17

switch_default_0:                                 ; preds = %switch_branch_NestedMatch.False1_0
  unreachable

switch_branch_NestedMatch.True1_0:                ; preds = %switch_branch_NestedMatch.False2_0
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, {} }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, {} }, ptr %18, i32 0, i32 0
  %21 = load i8, ptr %20, align 1
  switch i8 %21, label %switch_default_1 [
    i8 0, label %switch_branch_NestedMatch.False_1
    i8 1, label %switch_branch_NestedMatch.True_1
  ]

switch_branch_NestedMatch.False_1:                ; preds = %switch_branch_NestedMatch.True1_0
  %22 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %23 = getelementptr { i8, {} }, ptr %22, i32 0, i32 0
  store i8 0, ptr %23, align 1
  %24 = call ptr @malgo_exit_failure(ptr %22)
  ret ptr %24

switch_branch_NestedMatch.True_1:                 ; preds = %switch_branch_NestedMatch.True1_0
  %25 = call ptr @malgo_print_string(ptr @str80)
  ret ptr %25

switch_default_1:                                 ; preds = %switch_branch_NestedMatch.True1_0
  unreachable

switch_default_2:                                 ; preds = %switch_branch_NestedMatch.False2_0
  unreachable

switch_branch_NestedMatch.True2_0:                ; preds = %1
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, {} }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, {} }, ptr %26, i32 0, i32 0
  %29 = load i8, ptr %28, align 1
  switch i8 %29, label %switch_default_5 [
    i8 0, label %switch_branch_NestedMatch.False1_1
    i8 1, label %switch_branch_NestedMatch.True1_1
  ]

switch_branch_NestedMatch.False1_1:               ; preds = %switch_branch_NestedMatch.True2_0
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %31 = getelementptr { i8, {} }, ptr %30, i32 0, i32 0
  store i8 1, ptr %31, align 1
  %32 = getelementptr { i8, {} }, ptr %30, i32 0, i32 0
  %33 = load i8, ptr %32, align 1
  switch i8 %33, label %switch_default_3 [
    i8 0, label %switch_branch_NestedMatch.False_2
    i8 1, label %switch_branch_NestedMatch.True_2
  ]

switch_branch_NestedMatch.False_2:                ; preds = %switch_branch_NestedMatch.False1_1
  %34 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %35 = getelementptr { i8, {} }, ptr %34, i32 0, i32 0
  store i8 0, ptr %35, align 1
  %36 = call ptr @malgo_exit_failure(ptr %34)
  ret ptr %36

switch_branch_NestedMatch.True_2:                 ; preds = %switch_branch_NestedMatch.False1_1
  %37 = call ptr @malgo_print_string(ptr @str80)
  ret ptr %37

switch_default_3:                                 ; preds = %switch_branch_NestedMatch.False1_1
  unreachable

switch_branch_NestedMatch.True1_1:                ; preds = %switch_branch_NestedMatch.True2_0
  %38 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %39 = getelementptr { i8, {} }, ptr %38, i32 0, i32 0
  store i8 0, ptr %39, align 1
  %40 = getelementptr { i8, {} }, ptr %38, i32 0, i32 0
  %41 = load i8, ptr %40, align 1
  switch i8 %41, label %switch_default_4 [
    i8 0, label %switch_branch_NestedMatch.False_3
    i8 1, label %switch_branch_NestedMatch.True_3
  ]

switch_branch_NestedMatch.False_3:                ; preds = %switch_branch_NestedMatch.True1_1
  %42 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %43 = getelementptr { i8, {} }, ptr %42, i32 0, i32 0
  store i8 0, ptr %43, align 1
  %44 = call ptr @malgo_exit_failure(ptr %42)
  ret ptr %44

switch_branch_NestedMatch.True_3:                 ; preds = %switch_branch_NestedMatch.True1_1
  %45 = call ptr @malgo_print_string(ptr @str80)
  ret ptr %45

switch_default_4:                                 ; preds = %switch_branch_NestedMatch.True1_1
  unreachable

switch_default_5:                                 ; preds = %switch_branch_NestedMatch.True2_0
  unreachable

switch_default_6:                                 ; preds = %1
  unreachable
}

define internal ptr @NestedMatch.malgo_print_string(ptr %0, ptr %"NestedMatch.$p_36_0") {
  %2 = call ptr @malgo_print_string(ptr %"NestedMatch.$p_36_0")
  ret ptr %2
}

define internal ptr @NestedMatch.True(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
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
