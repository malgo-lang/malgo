; ModuleID = './test/tmp/malgo_test/normal/Test3.ll'
source_filename = "./test/testcases/malgo/Test3.mlg"

@str139 = unnamed_addr constant [6 x i8] c"WRONG\00"
@str140 = unnamed_addr constant [3 x i8] c"OK\00"

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
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %"Test3.$raw_fun_134.exit", label %switch_branch_Test3.Cons_0.i

switch_branch_Test3.Cons_0.i:                     ; preds = %2
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = load i8, ptr %5, align 1
  %switch1.i = icmp eq i8 %8, 0
  br i1 %switch1.i, label %switch_branch_Test3.A_0.i, label %"Test3.$raw_fun_134.exit"

switch_branch_Test3.A_0.i:                        ; preds = %switch_branch_Test3.Cons_0.i
  %9 = load i8, ptr %7, align 1
  %switch2.i = icmp eq i8 %9, 0
  br i1 %switch2.i, label %"Test3.$raw_fun_134.exit", label %switch_branch_Test3.Cons_1.i

switch_branch_Test3.Cons_1.i:                     ; preds = %switch_branch_Test3.A_0.i
  %10 = getelementptr { i8, { ptr, ptr } }, ptr %7, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = load i8, ptr %11, align 1
  %switch3.i = icmp eq i8 %12, 0
  br i1 %switch3.i, label %"Test3.$raw_fun_134.exit", label %switch_branch_Test3.B_0.i

switch_branch_Test3.B_0.i:                        ; preds = %switch_branch_Test3.Cons_1.i
  %13 = getelementptr { i8, { ptr, ptr } }, ptr %7, i64 0, i32 1, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = load i8, ptr %14, align 1
  %switch4.i = icmp eq i8 %15, 0
  %str140.str139.i = select i1 %switch4.i, ptr @str140, ptr @str139
  br label %"Test3.$raw_fun_134.exit"

"Test3.$raw_fun_134.exit":                        ; preds = %2, %switch_branch_Test3.Cons_0.i, %switch_branch_Test3.A_0.i, %switch_branch_Test3.Cons_1.i, %switch_branch_Test3.B_0.i
  %str139.sink.i = phi ptr [ @str139, %2 ], [ @str139, %switch_branch_Test3.A_0.i ], [ @str139, %switch_branch_Test3.Cons_1.i ], [ %str140.str139.i, %switch_branch_Test3.B_0.i ], [ @str139, %switch_branch_Test3.Cons_0.i ]
  %16 = tail call ptr @malgo_print_string(ptr noundef nonnull %str139.sink.i)
  ret ptr %16
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
