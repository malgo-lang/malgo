; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/EvenOdd.ll'
source_filename = "test/testcases/malgo/EvenOdd.mlg"

@str2949 = unnamed_addr constant [6 x i8] c"False\00"
@str2950 = unnamed_addr constant [5 x i8] c"True\00"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_sub_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal i32 @"EvenOdd.#let_closure_2945"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_sub_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_sub_int32_t(ptr nocapture nofree readnone %0, i32 %"Builtin.$p_1814_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"Builtin.$p_1814_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"EvenOdd.#let_closure_2945", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"EvenOdd.#let_closure_2946"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 8
  %malgo_sub_int32_t_addr_0 = getelementptr { i32, ptr }, ptr %0, i64 0, i32 1
  %malgo_sub_int32_t_0 = load ptr, ptr %malgo_sub_int32_t_addr_0, align 8
  %3 = load ptr, ptr %malgo_sub_int32_t_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %malgo_sub_int32_t_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, i32 %p_0)
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call i32 %9(ptr %7, i32 %1)
  ret i32 %10
}

define internal noundef ptr @EvenOdd.even(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %"EvenOdd.$int32#_51_0") {
"switch_branch_Builtin.Int32#_0":
  %1 = getelementptr { i8, { i32 } }, ptr %"EvenOdd.$int32#_51_0", i64 0, i32 1
  %2 = load i32, ptr %1, align 4
  %cond = icmp eq i32 %2, 0
  br i1 %cond, label %switch-unboxed_branch_0_i32_0, label %switch-unboxed_default_0

common.ret:                                       ; preds = %switch-unboxed_default_0, %switch-unboxed_branch_0_i32_0
  %common.ret.op = phi ptr [ %3, %switch-unboxed_branch_0_i32_0 ], [ %13, %switch-unboxed_default_0 ]
  ret ptr %common.ret.op

switch-unboxed_branch_0_i32_0:                    ; preds = %"switch_branch_Builtin.Int32#_0"
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %3, align 1
  br label %common.ret

switch-unboxed_default_0:                         ; preds = %"switch_branch_Builtin.Int32#_0"
  %4 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i32 } }, ptr %4, i64 0, i32 1, i32 0
  store i32 1, ptr %5, align 4
  %6 = load i32, ptr %1, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i32 %6, ptr %let_capture_0, align 4
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %malgo_sub_int32_t_func_0 = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @Builtin.malgo_sub_int32_t, ptr %malgo_sub_int32_t_func_0, align 8
  %malgo_sub_int32_t_0 = getelementptr { i32, ptr }, ptr %let_capture_0, i64 0, i32 1
  store ptr %8, ptr %malgo_sub_int32_t_0, align 8
  store ptr %let_capture_0, ptr %7, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"EvenOdd.#let_closure_2946", ptr %let_func_0, align 8
  %9 = tail call i32 @malgo_sub_int32_t(i32 %6, i32 noundef 1)
  %10 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %10, align 4
  %11 = getelementptr { i8, { i32 } }, ptr %10, i64 0, i32 1, i32 0
  store i32 %9, ptr %11, align 4
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %odd_func_0 = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @EvenOdd.odd, ptr %odd_func_0, align 8
  %13 = tail call noundef ptr @EvenOdd.odd(ptr poison, ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(1) %10)
  br label %common.ret
}

define internal i32 @"EvenOdd.#let_closure_2947"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 8
  %malgo_sub_int32_t_addr_0 = getelementptr { i32, ptr }, ptr %0, i64 0, i32 1
  %malgo_sub_int32_t_0 = load ptr, ptr %malgo_sub_int32_t_addr_0, align 8
  %3 = load ptr, ptr %malgo_sub_int32_t_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %malgo_sub_int32_t_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, i32 %p_0)
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call i32 %9(ptr %7, i32 %1)
  ret i32 %10
}

define internal noundef ptr @EvenOdd.odd(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %"EvenOdd.$int32#_61_0") {
"switch_branch_Builtin.Int32#_0":
  %1 = getelementptr { i8, { i32 } }, ptr %"EvenOdd.$int32#_61_0", i64 0, i32 1
  %2 = load i32, ptr %1, align 4
  %cond = icmp eq i32 %2, 0
  br i1 %cond, label %switch-unboxed_branch_0_i32_0, label %switch-unboxed_default_0

common.ret:                                       ; preds = %switch-unboxed_default_0, %switch-unboxed_branch_0_i32_0
  %common.ret.op = phi ptr [ %3, %switch-unboxed_branch_0_i32_0 ], [ %13, %switch-unboxed_default_0 ]
  ret ptr %common.ret.op

switch-unboxed_branch_0_i32_0:                    ; preds = %"switch_branch_Builtin.Int32#_0"
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  br label %common.ret

switch-unboxed_default_0:                         ; preds = %"switch_branch_Builtin.Int32#_0"
  %4 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i32 } }, ptr %4, i64 0, i32 1, i32 0
  store i32 1, ptr %5, align 4
  %6 = load i32, ptr %1, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i32 %6, ptr %let_capture_0, align 4
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %malgo_sub_int32_t_func_0 = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @Builtin.malgo_sub_int32_t, ptr %malgo_sub_int32_t_func_0, align 8
  %malgo_sub_int32_t_0 = getelementptr { i32, ptr }, ptr %let_capture_0, i64 0, i32 1
  store ptr %8, ptr %malgo_sub_int32_t_0, align 8
  store ptr %let_capture_0, ptr %7, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"EvenOdd.#let_closure_2947", ptr %let_func_0, align 8
  %9 = tail call i32 @malgo_sub_int32_t(i32 %6, i32 noundef 1)
  %10 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %10, align 4
  %11 = getelementptr { i8, { i32 } }, ptr %10, i64 0, i32 1, i32 0
  store i32 %9, ptr %11, align 4
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %even_func_0 = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @EvenOdd.even, ptr %even_func_0, align 8
  %13 = tail call noundef ptr @EvenOdd.even(ptr poison, ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(1) %10)
  br label %common.ret
}

define internal ptr @"EvenOdd.#fun_closure_2948"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
common.ret:
  %2 = load i8, ptr %1, align 1
  %switch = icmp eq i8 %2, 0
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %3, i64 0, i32 1, i32 0
  %spec.select = select i1 %switch, ptr @str2949, ptr @str2950
  %spec.select3 = select i1 %switch, ptr @str2949, ptr @str2950
  store ptr %spec.select, ptr %4, align 8
  %5 = tail call ptr @malgo_print_string(ptr noundef nonnull %spec.select3)
  %6 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %6, align 1
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %7, align 1
  %8 = tail call ptr @malgo_newline(ptr noundef nonnull %7)
  ret ptr %8
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %3, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"EvenOdd.#fun_closure_2948", ptr %fun_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %4, align 4
  %5 = getelementptr { i8, { i32 } }, ptr %4, i64 0, i32 1, i32 0
  store i32 10, ptr %5, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %even_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @EvenOdd.even, ptr %even_func_0.i, align 8
  %7 = tail call ptr @EvenOdd.even(ptr poison, ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(1) %4)
  %8 = load ptr, ptr %3, align 8
  %9 = load ptr, ptr %fun_func_0.i, align 8
  %10 = tail call ptr %9(ptr %8, ptr %7)
  ret i32 0
}
