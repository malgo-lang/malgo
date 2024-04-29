; ModuleID = '/workspaces/malgo/.malgo-work/Test3.ll'
source_filename = "./test/testcases/malgo/Test3.mlg"

@str119 = unnamed_addr constant [6 x i8] c"WRONG\00"
@str120 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"Test3.#fun_closure_118"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %"Test3.$raw_fun_117.exit", label %switch_branch_Test3.Cons_0.i

switch_branch_Test3.Cons_0.i:                     ; preds = %2
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = load i8, ptr %5, align 1
  %switch1.i = icmp eq i8 %8, 0
  br i1 %switch1.i, label %switch_branch_Test3.A_0.i, label %"Test3.$raw_fun_117.exit"

switch_branch_Test3.A_0.i:                        ; preds = %switch_branch_Test3.Cons_0.i
  %9 = load i8, ptr %7, align 1
  %switch2.i = icmp eq i8 %9, 0
  br i1 %switch2.i, label %"Test3.$raw_fun_117.exit", label %switch_branch_Test3.Cons_1.i

switch_branch_Test3.Cons_1.i:                     ; preds = %switch_branch_Test3.A_0.i
  %10 = getelementptr { i8, { ptr, ptr } }, ptr %7, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = load i8, ptr %11, align 1
  %switch3.i = icmp eq i8 %12, 0
  br i1 %switch3.i, label %"Test3.$raw_fun_117.exit", label %switch_branch_Test3.B_0.i

switch_branch_Test3.B_0.i:                        ; preds = %switch_branch_Test3.Cons_1.i
  %13 = getelementptr { i8, { ptr, ptr } }, ptr %7, i64 0, i32 1, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = load i8, ptr %14, align 1
  %switch4.i = icmp eq i8 %15, 0
  %str120.str119.i = select i1 %switch4.i, ptr @str120, ptr @str119
  br label %"Test3.$raw_fun_117.exit"

"Test3.$raw_fun_117.exit":                        ; preds = %2, %switch_branch_Test3.Cons_0.i, %switch_branch_Test3.A_0.i, %switch_branch_Test3.Cons_1.i, %switch_branch_Test3.B_0.i
  %str119.sink.i = phi ptr [ @str119, %2 ], [ @str119, %switch_branch_Test3.A_0.i ], [ @str119, %switch_branch_Test3.Cons_1.i ], [ %str120.str119.i, %switch_branch_Test3.B_0.i ], [ @str119, %switch_branch_Test3.Cons_0.i ]
  %16 = tail call ptr @malgo_print_string(ptr noundef nonnull %str119.sink.i)
  ret ptr %16
}

define internal noundef ptr @"Test3.#let_closure_121"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr %p_0, ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 1
  store ptr %1, ptr %5, align 8
  ret ptr %3
}

define internal ptr @"Test3.#let_closure_122"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %x_0 = load ptr, ptr %0, align 8
  %.val = load ptr, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %.val1(ptr %.val, ptr %x_0)
  ret ptr %4
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"Test3.#let_closure_121", ptr %let_func_0.i.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %5, align 1
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %6, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Test3.#let_closure_121", ptr %let_func_0.i2.i, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %7, align 1
  %8 = load ptr, ptr %6, align 8
  %9 = load ptr, ptr %let_func_0.i2.i, align 8
  %10 = tail call ptr %9(ptr %8, ptr nonnull %7)
  %11 = load ptr, ptr %4, align 8
  %12 = load ptr, ptr %let_func_0.i.i, align 8
  %13 = tail call ptr %12(ptr %11, ptr %10)
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %13, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %14, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"Test3.#let_closure_122", ptr %let_func_0.i4.i, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %15, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"Test3.#fun_closure_118", ptr %fun_func_0.i, align 8
  %16 = load ptr, ptr %14, align 8
  %17 = load ptr, ptr %let_func_0.i4.i, align 8
  %18 = tail call ptr %17(ptr %16, ptr nonnull %15)
  ret i32 0
}
