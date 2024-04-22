; ModuleID = './test/testcases/malgo/Test2.mlg'
source_filename = "./test/testcases/malgo/Test2.mlg"

@str48 = unnamed_addr constant [6 x i8] c"WRONG\00"
@str49 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_malloc(i64)

define internal ptr @Test2.R(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @Test2.G(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @Test2.B(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 2, ptr %3, align 1
  ret ptr %2
}

define internal ptr @Test2.malgo_print_string(ptr %0, ptr %"Test2.$p_26_0") {
  %2 = call ptr @malgo_print_string(ptr %"Test2.$p_26_0")
  ret ptr %2
}

define internal ptr @Test2.rtob(ptr %0, ptr %"Test2.$r_27_0") {
  %2 = getelementptr { i8, {} }, ptr %"Test2.$r_27_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_Test2.R_0
    i8 1, label %switch_branch_Test2.G_0
    i8 2, label %switch_branch_Test2.B_0
  ]

switch_branch_Test2.R_0:                          ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 2, ptr %5, align 1
  ret ptr %4

switch_branch_Test2.G_0:                          ; preds = %1
  ret ptr %"Test2.$r_27_0"

switch_branch_Test2.B_0:                          ; preds = %1
  ret ptr %"Test2.$r_27_0"

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @Test2.main(ptr %0, ptr %"Test2.$$__28_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  %5 = load i8, ptr %4, align 1
  switch i8 %5, label %switch_default_3 [
    i8 0, label %switch_branch_Test2.R_0
    i8 1, label %switch_branch_Test2.G_1
    i8 2, label %switch_branch_Test2.B_2
  ]

switch_branch_Test2.R_0:                          ; preds = %1
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %7 = getelementptr { i8, {} }, ptr %6, i32 0, i32 0
  store i8 2, ptr %7, align 1
  %8 = getelementptr { i8, {} }, ptr %6, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %switch_branch_Test2.R_1
    i8 1, label %switch_branch_Test2.G_0
    i8 2, label %switch_branch_Test2.B_0
  ]

switch_branch_Test2.R_1:                          ; preds = %switch_branch_Test2.R_0
  %10 = call ptr @malgo_print_string(ptr @str48)
  ret ptr %10

switch_branch_Test2.G_0:                          ; preds = %switch_branch_Test2.R_0
  %11 = call ptr @malgo_print_string(ptr @str48)
  ret ptr %11

switch_branch_Test2.B_0:                          ; preds = %switch_branch_Test2.R_0
  %12 = call ptr @malgo_print_string(ptr @str49)
  ret ptr %12

switch_default_0:                                 ; preds = %switch_branch_Test2.R_0
  unreachable

switch_branch_Test2.G_1:                          ; preds = %1
  %13 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  %14 = load i8, ptr %13, align 1
  switch i8 %14, label %switch_default_1 [
    i8 0, label %switch_branch_Test2.R_2
    i8 1, label %switch_branch_Test2.G_2
    i8 2, label %switch_branch_Test2.B_1
  ]

switch_branch_Test2.R_2:                          ; preds = %switch_branch_Test2.G_1
  %15 = call ptr @malgo_print_string(ptr @str48)
  ret ptr %15

switch_branch_Test2.G_2:                          ; preds = %switch_branch_Test2.G_1
  %16 = call ptr @malgo_print_string(ptr @str48)
  ret ptr %16

switch_branch_Test2.B_1:                          ; preds = %switch_branch_Test2.G_1
  %17 = call ptr @malgo_print_string(ptr @str49)
  ret ptr %17

switch_default_1:                                 ; preds = %switch_branch_Test2.G_1
  unreachable

switch_branch_Test2.B_2:                          ; preds = %1
  %18 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  %19 = load i8, ptr %18, align 1
  switch i8 %19, label %switch_default_2 [
    i8 0, label %switch_branch_Test2.R_3
    i8 1, label %switch_branch_Test2.G_3
    i8 2, label %switch_branch_Test2.B_3
  ]

switch_branch_Test2.R_3:                          ; preds = %switch_branch_Test2.B_2
  %20 = call ptr @malgo_print_string(ptr @str48)
  ret ptr %20

switch_branch_Test2.G_3:                          ; preds = %switch_branch_Test2.B_2
  %21 = call ptr @malgo_print_string(ptr @str48)
  ret ptr %21

switch_branch_Test2.B_3:                          ; preds = %switch_branch_Test2.B_2
  %22 = call ptr @malgo_print_string(ptr @str49)
  ret ptr %22

switch_default_2:                                 ; preds = %switch_branch_Test2.B_2
  unreachable

switch_default_3:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @koriel_load_Test2()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @Test2.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @koriel_load_Test2() {
  ret void
}
