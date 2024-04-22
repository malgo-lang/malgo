; ModuleID = './test/tmp/malgo_test/nolift/Test3.ll'
source_filename = "./test/testcases/malgo/Test3.mlg"

@str137 = unnamed_addr constant [6 x i8] c"WRONG\00"
@str138 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"Test3.#let_closure_135"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"Test3.#fun_closure_136"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch = icmp eq i8 %3, 0
  br i1 %switch, label %common.ret, label %switch_branch_Test3.Cons_0

common.ret:                                       ; preds = %switch_branch_Test3.Cons_0, %switch_branch_Test3.B_0, %switch_branch_Test3.Cons_1, %switch_branch_Test3.A_0, %2
  %str137.sink = phi ptr [ @str137, %2 ], [ @str137, %switch_branch_Test3.A_0 ], [ @str137, %switch_branch_Test3.Cons_1 ], [ %str138.str137, %switch_branch_Test3.B_0 ], [ @str137, %switch_branch_Test3.Cons_0 ]
  %4 = tail call ptr @malgo_print_string(ptr noundef nonnull %str137.sink)
  ret ptr %4

switch_branch_Test3.Cons_0:                       ; preds = %2
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = load i8, ptr %6, align 1
  %switch1 = icmp eq i8 %9, 0
  br i1 %switch1, label %switch_branch_Test3.A_0, label %common.ret

switch_branch_Test3.A_0:                          ; preds = %switch_branch_Test3.Cons_0
  %10 = load i8, ptr %8, align 1
  %switch2 = icmp eq i8 %10, 0
  br i1 %switch2, label %common.ret, label %switch_branch_Test3.Cons_1

switch_branch_Test3.Cons_1:                       ; preds = %switch_branch_Test3.A_0
  %11 = getelementptr { i8, { ptr, ptr } }, ptr %8, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = load i8, ptr %12, align 1
  %switch3 = icmp eq i8 %13, 0
  br i1 %switch3, label %common.ret, label %switch_branch_Test3.B_0

switch_branch_Test3.B_0:                          ; preds = %switch_branch_Test3.Cons_1
  %14 = getelementptr { i8, { ptr, ptr } }, ptr %8, i64 0, i32 1, i32 1
  %15 = load ptr, ptr %14, align 8
  %16 = load i8, ptr %15, align 1
  %switch4 = icmp eq i8 %16, 0
  %str138.str137 = select i1 %switch4, ptr @str138, ptr @str137
  br label %common.ret
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %6, align 1
  %7 = getelementptr { i8, { ptr, ptr } }, ptr %6, i64 0, i32 1, i32 0
  store ptr %4, ptr %7, align 8
  %8 = getelementptr { i8, { ptr, ptr } }, ptr %6, i64 0, i32 1, i32 1
  store ptr %5, ptr %8, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %9, align 1
  %10 = getelementptr { i8, { ptr, ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %3, ptr %10, align 8
  %11 = getelementptr { i8, { ptr, ptr } }, ptr %9, i64 0, i32 1, i32 1
  store ptr %6, ptr %11, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %9, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %12, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"Test3.#let_closure_135", ptr %let_func_0.i, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %13, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"Test3.#fun_closure_136", ptr %fun_func_0.i, align 8
  %14 = load ptr, ptr %12, align 8
  %15 = load ptr, ptr %let_func_0.i, align 8
  %16 = tail call ptr %15(ptr %14, ptr nonnull %13)
  ret i32 0
}
