; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/Eventually.ll'
source_filename = "test/testcases/malgo/Eventually.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@"test/testcases/malgo/Eventually.mlg.eventually" = local_unnamed_addr global ptr undef
@"test/testcases/malgo/Eventually.mlg.comp" = local_unnamed_addr global ptr undef
@str440 = unnamed_addr constant [1 x i8] zeroinitializer
@str467 = unnamed_addr constant [7 x i8] c"return\00"
@str468 = unnamed_addr constant [5 x i8] c"bind\00"
@str513 = unnamed_addr constant [8 x i8] c"not yet\00"
@str605 = unnamed_addr constant [10 x i8] c"no branch\00"
@str631 = unnamed_addr constant [2 x i8] c"1\00"
@str634 = unnamed_addr constant [2 x i8] c"2\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"test/testcases/malgo/Eventually.mlg.step"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %"test/testcases/malgo/Eventually.mlg.$done_262_0") {
  %2 = load i8, ptr %"test/testcases/malgo/Eventually.mlg.$done_262_0", align 8
  %switch = icmp eq i8 %2, 0
  %3 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/Eventually.mlg.$done_262_0", i64 0, i32 1
  %4 = load ptr, ptr %3, align 8
  br i1 %switch, label %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0", label %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0"

common.ret:                                       ; preds = %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0", %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0"
  %common.ret.op = phi ptr [ %5, %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0" ], [ %11, %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0" ]
  ret ptr %common.ret.op

"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0": ; preds = %1
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %4, ptr %6, align 8
  br label %common.ret

"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0": ; preds = %1
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %7, align 1
  %8 = load ptr, ptr %4, align 8
  %9 = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, ptr nonnull %7)
  br label %common.ret
}

define internal noundef ptr @"test/testcases/malgo/Eventually.mlg.Done"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/Eventually.mlg.$p_258_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/Eventually.mlg.$p_258_0", ptr %3, align 8
  ret ptr %2
}

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

define internal ptr @"test/testcases/malgo/Eventually.mlg.#let_closure_511"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %x_0 = load ptr, ptr %0, align 8
  %.val = load ptr, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %.val1(ptr %.val, ptr %x_0)
  ret ptr %4
}

define internal ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_528"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readnone %1) {
  %p_0 = load ptr, ptr %0, align 8
  %k_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %k_0 = load ptr, ptr %k_addr_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = load ptr, ptr %p_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %p_0, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %3)
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %7, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %8, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#let_closure_620", ptr %let_func_0.i.i, align 8
  %done_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %9 = load i8, ptr %done_0.i.i, align 8
  %switch.i.i.i = icmp eq i8 %9, 0
  %10 = getelementptr { i8, { ptr } }, ptr %done_0.i.i, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  br i1 %switch.i.i.i, label %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i.i.i", label %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i.i.i"

"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i.i.i": ; preds = %2
  %12 = load ptr, ptr %k_0, align 8
  %13 = getelementptr { ptr, ptr }, ptr %k_0, i64 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = tail call ptr %14(ptr %12, ptr %11)
  br label %"test/testcases/malgo/Eventually.mlg.$raw_fun_417.exit"

"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i.i.i": ; preds = %2
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %11, ptr %fun_capture_0.i.i.i, align 8
  %k_0.i.i.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i.i.i, i64 0, i32 1
  store ptr %k_0, ptr %k_0.i.i.i, align 8
  store ptr %fun_capture_0.i.i.i, ptr %16, align 8
  %fun_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_528", ptr %fun_func_0.i.i.i, align 8
  %17 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %17, align 1
  %18 = getelementptr { i8, { ptr } }, ptr %17, i64 0, i32 1, i32 0
  store ptr %16, ptr %18, align 8
  br label %"test/testcases/malgo/Eventually.mlg.$raw_fun_417.exit"

"test/testcases/malgo/Eventually.mlg.$raw_fun_417.exit": ; preds = %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i.i.i", %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i.i.i"
  %common.ret.op.i.i.i = phi ptr [ %15, %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i.i.i" ], [ %17, %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i.i.i" ]
  ret ptr %common.ret.op.i.i.i
}

define internal ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_577"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i", label %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i"

"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i": ; preds = %2
  %4 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr i8, ptr %5, i64 4
  %.val2.i = load i32, ptr %6, align 4
  %7 = tail call ptr @malgo_int32_t_to_string(i32 %.val2.i)
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %8, i64 0, i32 1, i32 0
  store ptr %7, ptr %9, align 8
  %10 = tail call ptr @malgo_print_string(ptr %7)
  br label %"test/testcases/malgo/Eventually.mlg.$raw_fun_418.exit"

"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i": ; preds = %2
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr @str513, ptr %12, align 8
  %13 = tail call ptr @malgo_print_string(ptr noundef nonnull @str513)
  br label %"test/testcases/malgo/Eventually.mlg.$raw_fun_418.exit"

"test/testcases/malgo/Eventually.mlg.$raw_fun_418.exit": ; preds = %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i", %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i"
  %common.ret.op.i = phi ptr [ %10, %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i" ], [ %13, %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i" ]
  ret ptr %common.ret.op.i
}

define internal ptr @"test/testcases/malgo/Eventually.mlg.#let_closure_620"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %done_0 = load ptr, ptr %0, align 8
  %3 = load i8, ptr %done_0, align 8
  %switch.i = icmp eq i8 %3, 0
  %4 = getelementptr { i8, { ptr } }, ptr %done_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  br i1 %switch.i, label %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i", label %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i"

"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i": ; preds = %2
  %6 = load ptr, ptr %1, align 8
  %7 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, ptr %5)
  br label %"test/testcases/malgo/Eventually.mlg.$eventuallyBind_curry_288.exit"

"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i": ; preds = %2
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %5, ptr %fun_capture_0.i, align 8
  %k_0.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %1, ptr %k_0.i, align 8
  store ptr %fun_capture_0.i, ptr %10, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_528", ptr %fun_func_0.i, align 8
  %11 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr %10, ptr %12, align 8
  br label %"test/testcases/malgo/Eventually.mlg.$eventuallyBind_curry_288.exit"

"test/testcases/malgo/Eventually.mlg.$eventuallyBind_curry_288.exit": ; preds = %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i", %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i"
  %common.ret.op.i = phi ptr [ %9, %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i" ], [ %11, %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i" ]
  ret ptr %common.ret.op.i
}

define internal ptr @"test/testcases/malgo/Eventually.mlg.eventuallyBind"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/Eventually.mlg.$done_272_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/Eventually.mlg.$done_272_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#let_closure_620", ptr %let_func_0, align 8
  ret ptr %2
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @"runtime/malgo/Builtin.mlg.undefined", align 8
  %4 = tail call ptr @malgo_hash_table_new()
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %Done_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.Done", ptr %Done_func_0.i, align 8
  tail call void @malgo_hash_table_insert(ptr %4, ptr noundef nonnull @str467, ptr noundef nonnull %5)
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %eventuallyBind_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.eventuallyBind", ptr %eventuallyBind_func_0.i, align 8
  tail call void @malgo_hash_table_insert(ptr %4, ptr noundef nonnull @str468, ptr noundef nonnull %6)
  store ptr %4, ptr @"test/testcases/malgo/Eventually.mlg.eventually", align 8
  %7 = tail call ptr @malgo_hash_table_get(ptr %4, ptr noundef nonnull @str467)
  %8 = tail call ptr @malgo_hash_table_get(ptr %4, ptr noundef nonnull @str468)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %9, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_630", ptr %fun_func_0.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %10, align 1
  %11 = getelementptr { i8, { ptr } }, ptr %10, i64 0, i32 1, i32 0
  store ptr %9, ptr %11, align 8
  %12 = load ptr, ptr %8, align 8
  %13 = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = tail call ptr %14(ptr %12, ptr nonnull %10)
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_2.i, ptr %16, align 8
  %fun_func_1.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_632", ptr %fun_func_1.i, align 8
  %17 = load ptr, ptr %15, align 8
  %18 = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  %19 = load ptr, ptr %18, align 8
  %20 = tail call ptr %19(ptr %17, ptr nonnull %16)
  store ptr %20, ptr @"test/testcases/malgo/Eventually.mlg.comp", align 8
  %21 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %21, align 1
  %22 = load ptr, ptr @"test/testcases/malgo/Eventually.mlg.comp", align 8
  %23 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %22, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %23, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %23, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#let_closure_511", ptr %let_func_0.i.i, align 8
  %24 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %24, align 8
  %step_func_0.i = getelementptr { ptr, ptr }, ptr %24, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.step", ptr %step_func_0.i, align 8
  %x_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %25 = load i8, ptr %x_0.i.i, align 8
  %switch.i.i = icmp eq i8 %25, 0
  %26 = getelementptr { i8, { ptr } }, ptr %x_0.i.i, i64 0, i32 1
  %27 = load ptr, ptr %26, align 8
  br i1 %switch.i.i, label %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i.i", label %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i.i"

"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i.i": ; preds = %1
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %28, i64 0, i32 1, i32 0
  store ptr %27, ptr %29, align 8
  br label %"test/testcases/malgo/Eventually.mlg.step.exit.i"

"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i.i": ; preds = %1
  %30 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %30, align 1
  %31 = load ptr, ptr %27, align 8
  %32 = getelementptr { ptr, ptr }, ptr %27, i64 0, i32 1
  %33 = load ptr, ptr %32, align 8
  %34 = tail call ptr %33(ptr %31, ptr nonnull %30)
  br label %"test/testcases/malgo/Eventually.mlg.step.exit.i"

"test/testcases/malgo/Eventually.mlg.step.exit.i": ; preds = %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i.i", %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i.i"
  %35 = load ptr, ptr @"test/testcases/malgo/Eventually.mlg.comp", align 8
  %36 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %35, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %36, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %36, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#let_closure_511", ptr %let_func_0.i2.i, align 8
  %37 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %37, align 8
  %step_func_1.i = getelementptr { ptr, ptr }, ptr %37, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.step", ptr %step_func_1.i, align 8
  %x_0.i9.i = load ptr, ptr %let_capture_0.i1.i, align 8
  %38 = load i8, ptr %x_0.i9.i, align 8
  %switch.i18.i = icmp eq i8 %38, 0
  %39 = getelementptr { i8, { ptr } }, ptr %x_0.i9.i, i64 0, i32 1
  %40 = load ptr, ptr %39, align 8
  br i1 %switch.i18.i, label %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i20.i", label %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i21.i"

"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i20.i": ; preds = %"test/testcases/malgo/Eventually.mlg.step.exit.i"
  %41 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %41, align 1
  %42 = getelementptr { i8, { ptr } }, ptr %41, i64 0, i32 1, i32 0
  store ptr %40, ptr %42, align 8
  br label %"test/testcases/malgo/Eventually.mlg.step.exit22.i"

"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i21.i": ; preds = %"test/testcases/malgo/Eventually.mlg.step.exit.i"
  %43 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %43, align 1
  %44 = load ptr, ptr %40, align 8
  %45 = getelementptr { ptr, ptr }, ptr %40, i64 0, i32 1
  %46 = load ptr, ptr %45, align 8
  %47 = tail call ptr %46(ptr %44, ptr nonnull %43)
  br label %"test/testcases/malgo/Eventually.mlg.step.exit22.i"

"test/testcases/malgo/Eventually.mlg.step.exit22.i": ; preds = %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i21.i", %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i20.i"
  %common.ret.op.i19.i = phi ptr [ %41, %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i20.i" ], [ %47, %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i21.i" ]
  %48 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %common.ret.op.i19.i, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %48, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %48, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#let_closure_511", ptr %let_func_0.i4.i, align 8
  %49 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %49, align 8
  %step_func_2.i = getelementptr { ptr, ptr }, ptr %49, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.step", ptr %step_func_2.i, align 8
  %x_0.i12.i = load ptr, ptr %let_capture_0.i3.i, align 8
  %50 = load i8, ptr %x_0.i12.i, align 8
  %switch.i23.i = icmp eq i8 %50, 0
  %51 = getelementptr { i8, { ptr } }, ptr %x_0.i12.i, i64 0, i32 1
  %52 = load ptr, ptr %51, align 8
  br i1 %switch.i23.i, label %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i25.i", label %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i26.i"

"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i25.i": ; preds = %"test/testcases/malgo/Eventually.mlg.step.exit22.i"
  %53 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %53, align 1
  %54 = getelementptr { i8, { ptr } }, ptr %53, i64 0, i32 1, i32 0
  store ptr %52, ptr %54, align 8
  br label %"test/testcases/malgo/Eventually.mlg.step.exit27.i"

"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i26.i": ; preds = %"test/testcases/malgo/Eventually.mlg.step.exit22.i"
  %55 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %55, align 1
  %56 = load ptr, ptr %52, align 8
  %57 = getelementptr { ptr, ptr }, ptr %52, i64 0, i32 1
  %58 = load ptr, ptr %57, align 8
  %59 = tail call ptr %58(ptr %56, ptr nonnull %55)
  br label %"test/testcases/malgo/Eventually.mlg.step.exit27.i"

"test/testcases/malgo/Eventually.mlg.step.exit27.i": ; preds = %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i26.i", %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i25.i"
  %common.ret.op.i24.i = phi ptr [ %53, %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i25.i" ], [ %59, %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i26.i" ]
  %60 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i5.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %common.ret.op.i24.i, ptr %let_capture_0.i5.i, align 8
  store ptr %let_capture_0.i5.i, ptr %60, align 8
  %let_func_0.i6.i = getelementptr { ptr, ptr }, ptr %60, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#let_closure_511", ptr %let_func_0.i6.i, align 8
  %61 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %61, align 8
  %step_func_3.i = getelementptr { ptr, ptr }, ptr %61, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.step", ptr %step_func_3.i, align 8
  %x_0.i15.i = load ptr, ptr %let_capture_0.i5.i, align 8
  %62 = load i8, ptr %x_0.i15.i, align 8
  %switch.i28.i = icmp eq i8 %62, 0
  %63 = getelementptr { i8, { ptr } }, ptr %x_0.i15.i, i64 0, i32 1
  %64 = load ptr, ptr %63, align 8
  br i1 %switch.i28.i, label %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i30.i", label %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i31.i"

"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i30.i": ; preds = %"test/testcases/malgo/Eventually.mlg.step.exit27.i"
  %65 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %65, align 1
  %66 = getelementptr { i8, { ptr } }, ptr %65, i64 0, i32 1, i32 0
  store ptr %64, ptr %66, align 8
  br label %"test/testcases/malgo/Eventually.mlg.main.exit"

"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i31.i": ; preds = %"test/testcases/malgo/Eventually.mlg.step.exit27.i"
  %67 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %67, align 1
  %68 = load ptr, ptr %64, align 8
  %69 = getelementptr { ptr, ptr }, ptr %64, i64 0, i32 1
  %70 = load ptr, ptr %69, align 8
  %71 = tail call ptr %70(ptr %68, ptr nonnull %67)
  br label %"test/testcases/malgo/Eventually.mlg.main.exit"

"test/testcases/malgo/Eventually.mlg.main.exit":  ; preds = %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i30.i", %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i31.i"
  %common.ret.op.i29.i = phi ptr [ %65, %"switch_branch_test/testcases/malgo/Eventually.mlg.Done_0.i30.i" ], [ %71, %"switch_branch_test/testcases/malgo/Eventually.mlg.NotYetDone_0.i31.i" ]
  %72 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i7.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %common.ret.op.i29.i, ptr %let_capture_0.i7.i, align 8
  store ptr %let_capture_0.i7.i, ptr %72, align 8
  %let_func_0.i8.i = getelementptr { ptr, ptr }, ptr %72, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#let_closure_511", ptr %let_func_0.i8.i, align 8
  %73 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i1 = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i1, ptr %73, align 8
  %fun_func_0.i2 = getelementptr { ptr, ptr }, ptr %73, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_577", ptr %fun_func_0.i2, align 8
  %74 = load ptr, ptr %72, align 8
  %75 = load ptr, ptr %let_func_0.i8.i, align 8
  %76 = tail call ptr %75(ptr %74, ptr nonnull %73)
  ret i32 0
}

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr

define internal noundef ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_630"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr @str631, ptr %4, align 8
  %5 = tail call ptr @malgo_print_string(ptr noundef nonnull @str631)
  %6 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %6, i64 0, i32 1, i32 0
  store ptr %5, ptr %7, align 8
  ret ptr %6
}

define internal noundef ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_633"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr @str634, ptr %4, align 8
  %5 = tail call ptr @malgo_print_string(ptr noundef nonnull @str634)
  %6 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %6, i64 0, i32 1, i32 0
  store ptr %5, ptr %7, align 8
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_635"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = load ptr, ptr @"test/testcases/malgo/Eventually.mlg.eventually", align 8
  %4 = tail call ptr @malgo_hash_table_get(ptr %3, ptr noundef nonnull @str467)
  %5 = tail call ptr @malgo_hash_table_get(ptr %3, ptr noundef nonnull @str468)
  %6 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { i32 } }, ptr %6, i64 0, i32 1, i32 0
  store i32 3, ptr %7, align 4
  %8 = load ptr, ptr %4, align 8
  %9 = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, ptr nonnull %6)
  ret ptr %11
}

define internal ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_632"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = load ptr, ptr @"test/testcases/malgo/Eventually.mlg.eventually", align 8
  %4 = tail call ptr @malgo_hash_table_get(ptr %3, ptr noundef nonnull @str467)
  %5 = tail call ptr @malgo_hash_table_get(ptr %3, ptr noundef nonnull @str468)
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0 = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0, ptr %6, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_633", ptr %fun_func_0, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %6, ptr %8, align 8
  %9 = load ptr, ptr %5, align 8
  %10 = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = tail call ptr %11(ptr %9, ptr nonnull %7)
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2 = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_2, ptr %13, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"test/testcases/malgo/Eventually.mlg.#fun_closure_635", ptr %fun_func_1, align 8
  %14 = load ptr, ptr %12, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr %16(ptr %14, ptr nonnull %13)
  ret ptr %17
}
