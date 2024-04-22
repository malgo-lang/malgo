; ModuleID = './test/tmp/malgo_test/nono/Test3.ll'
source_filename = "./test/testcases/malgo/Test3.mlg"

@str120 = unnamed_addr constant [6 x i8] c"WRONG\00"
@str121 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"Test3.#let_closure_117"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr %p_0, ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 1
  store ptr %1, ptr %5, align 8
  ret ptr %3
}

define internal ptr @Test3.Cons(ptr nocapture nofree readnone %0, ptr nofree %"Test3.$p_69_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Test3.$p_69_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Test3.#let_closure_117", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Test3.malgo_print_string(ptr nocapture nofree readnone %0, ptr %"Test3.$p_77_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"Test3.$p_77_0")
  ret ptr %2
}

define internal ptr @"Test3.#let_closure_118"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %x_0 = load ptr, ptr %0, align 8
  %.val = load ptr, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %.val1(ptr %.val, ptr %x_0)
  ret ptr %4
}

define internal ptr @"Test3.|>"(ptr nocapture nofree readnone %0, ptr nofree %"Test3.$x_78_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Test3.$x_78_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Test3.#let_closure_118", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Test3.#fun_closure_119"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %malgo_print_string_0 = load ptr, ptr %0, align 8
  %3 = load i8, ptr %1, align 1
  %switch = icmp eq i8 %3, 0
  br i1 %switch, label %common.ret, label %switch_branch_Test3.Cons_0

common.ret:                                       ; preds = %switch_branch_Test3.Cons_0, %switch_branch_Test3.B_0, %switch_branch_Test3.Cons_1, %switch_branch_Test3.A_0, %2
  %str120.sink = phi ptr [ @str120, %2 ], [ @str120, %switch_branch_Test3.A_0 ], [ @str120, %switch_branch_Test3.Cons_1 ], [ %str121.str120, %switch_branch_Test3.B_0 ], [ @str120, %switch_branch_Test3.Cons_0 ]
  %4 = load ptr, ptr %malgo_print_string_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %str120.sink)
  ret ptr %7

switch_branch_Test3.Cons_0:                       ; preds = %2
  %8 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = load i8, ptr %9, align 1
  %switch1 = icmp eq i8 %12, 0
  br i1 %switch1, label %switch_branch_Test3.A_0, label %common.ret

switch_branch_Test3.A_0:                          ; preds = %switch_branch_Test3.Cons_0
  %13 = load i8, ptr %11, align 1
  %switch2 = icmp eq i8 %13, 0
  br i1 %switch2, label %common.ret, label %switch_branch_Test3.Cons_1

switch_branch_Test3.Cons_1:                       ; preds = %switch_branch_Test3.A_0
  %14 = getelementptr { i8, { ptr, ptr } }, ptr %11, i64 0, i32 1
  %15 = load ptr, ptr %14, align 8
  %16 = load i8, ptr %15, align 1
  %switch3 = icmp eq i8 %16, 0
  br i1 %switch3, label %common.ret, label %switch_branch_Test3.B_0

switch_branch_Test3.B_0:                          ; preds = %switch_branch_Test3.Cons_1
  %17 = getelementptr { i8, { ptr, ptr } }, ptr %11, i64 0, i32 1, i32 1
  %18 = load ptr, ptr %17, align 8
  %19 = load i8, ptr %18, align 1
  %switch4 = icmp eq i8 %19, 0
  %str121.str120 = select i1 %switch4, ptr @str121, ptr @str120
  br label %common.ret
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %Cons_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @Test3.Cons, ptr %Cons_func_0.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Test3.#let_closure_117", ptr %let_func_0.i.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %6, align 1
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %Cons_func_1.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @Test3.Cons, ptr %Cons_func_1.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %6, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %8, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"Test3.#let_closure_117", ptr %let_func_0.i2.i, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %9, align 1
  %10 = load ptr, ptr %8, align 8
  %11 = load ptr, ptr %let_func_0.i2.i, align 8
  %12 = tail call ptr %11(ptr %10, ptr nonnull %9)
  %13 = load ptr, ptr %5, align 8
  %14 = load ptr, ptr %let_func_0.i.i, align 8
  %15 = tail call ptr %14(ptr %13, ptr %12)
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %16, align 8
  %"|>_func_0.i" = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"Test3.|>", ptr %"|>_func_0.i", align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %15, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %17, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"Test3.#let_closure_118", ptr %let_func_0.i4.i, align 8
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %19, align 8
  %malgo_print_string_func_0.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @Test3.malgo_print_string, ptr %malgo_print_string_func_0.i, align 8
  store ptr %19, ptr %fun_capture_0.i, align 8
  store ptr %fun_capture_0.i, ptr %18, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"Test3.#fun_closure_119", ptr %fun_func_0.i, align 8
  %20 = load ptr, ptr %17, align 8
  %21 = load ptr, ptr %let_func_0.i4.i, align 8
  %22 = tail call ptr %21(ptr %20, ptr nonnull %18)
  ret i32 0
}
