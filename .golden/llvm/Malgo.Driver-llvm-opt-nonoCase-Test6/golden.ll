; ModuleID = '.malgo-work/test/testcases/malgo/Test6.ll'
source_filename = "test/testcases/malgo/Test6.mlg"

@str52 = unnamed_addr constant [6 x i8] c"WRONG\00"
@str53 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_exit_failure(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"test/testcases/malgo/Test6.mlg.malgo_exit_failure"(ptr nocapture nofree readnone %0, ptr %"test/testcases/malgo/Test6.mlg.$p_33_0") {
  %2 = tail call ptr @malgo_exit_failure(ptr %"test/testcases/malgo/Test6.mlg.$p_33_0")
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test6.mlg.malgo_print_string"(ptr nocapture nofree readnone %0, ptr %"test/testcases/malgo/Test6.mlg.$p_34_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"test/testcases/malgo/Test6.mlg.$p_34_0")
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test6.mlg.rtob"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %"test/testcases/malgo/Test6.mlg.$r_35_0") {
  %2 = load i8, ptr %"test/testcases/malgo/Test6.mlg.$r_35_0", align 1
  switch i8 %2, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/Test6.mlg.R_0"
    i8 1, label %"switch_branch_test/testcases/malgo/Test6.mlg.G_0"
    i8 2, label %"switch_branch_test/testcases/malgo/Test6.mlg.B_0"
  ]

common.ret:                                       ; preds = %"switch_branch_test/testcases/malgo/Test6.mlg.B_0", %"switch_branch_test/testcases/malgo/Test6.mlg.G_0", %"switch_branch_test/testcases/malgo/Test6.mlg.R_0"
  %common.ret.op = phi ptr [ %3, %"switch_branch_test/testcases/malgo/Test6.mlg.R_0" ], [ %6, %"switch_branch_test/testcases/malgo/Test6.mlg.G_0" ], [ %9, %"switch_branch_test/testcases/malgo/Test6.mlg.B_0" ]
  ret ptr %common.ret.op

"switch_branch_test/testcases/malgo/Test6.mlg.R_0": ; preds = %1
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 2, ptr %3, align 1
  br label %common.ret

"switch_branch_test/testcases/malgo/Test6.mlg.G_0": ; preds = %1
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %malgo_exit_failure_func_0 = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/Test6.mlg.malgo_exit_failure", ptr %malgo_exit_failure_func_0, align 8
  %6 = tail call ptr @malgo_exit_failure(ptr noundef nonnull %4)
  br label %common.ret

"switch_branch_test/testcases/malgo/Test6.mlg.B_0": ; preds = %1
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %7, align 1
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %malgo_exit_failure_func_1 = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/Test6.mlg.malgo_exit_failure", ptr %malgo_exit_failure_func_1, align 8
  %9 = tail call ptr @malgo_exit_failure(ptr noundef nonnull %7)
  br label %common.ret

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/Test6.mlg.#fun_closure_51"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
common.ret:
  %malgo_print_string_0 = load ptr, ptr %0, align 8
  %2 = load i8, ptr %1, align 1
  %switch = icmp ult i8 %2, 2
  %spec.select = select i1 %switch, ptr @str52, ptr @str53
  %3 = load ptr, ptr %malgo_print_string_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr nonnull %spec.select)
  ret ptr %6
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_print_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/Test6.mlg.malgo_print_string", ptr %malgo_print_string_func_0.i, align 8
  store ptr %4, ptr %fun_capture_0.i, align 8
  store ptr %fun_capture_0.i, ptr %3, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/Test6.mlg.#fun_closure_51", ptr %fun_func_0.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %rtob_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/Test6.mlg.rtob", ptr %rtob_func_0.i, align 8
  %7 = tail call ptr @"test/testcases/malgo/Test6.mlg.rtob"(ptr poison, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %5)
  %8 = load ptr, ptr %3, align 8
  %9 = load ptr, ptr %fun_func_0.i, align 8
  %10 = tail call ptr %9(ptr %8, ptr %7)
  ret i32 0
}
