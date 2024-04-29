; ModuleID = '/workspaces/malgo/.malgo-work/Eventually.ll'
source_filename = "./test/testcases/malgo/Eventually.mlg"

@Eventually.eventually = local_unnamed_addr global ptr undef
@Eventually.comp = local_unnamed_addr global ptr undef
@str4108 = unnamed_addr constant [8 x i8] c"not yet\00"
@str4109 = unnamed_addr constant [7 x i8] c"return\00"
@str4110 = unnamed_addr constant [5 x i8] c"bind\00"
@str4156 = unnamed_addr constant [2 x i8] c"1\00"
@str4159 = unnamed_addr constant [2 x i8] c"2\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

define internal ptr @"Eventually.#fun_closure_4111"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readnone %1) {
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
  store ptr @"Eventually.#let_closure_4113", ptr %let_func_0.i.i, align 8
  %done_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %9 = load i8, ptr %done_0.i.i, align 8
  %switch.i.i.i = icmp eq i8 %9, 0
  %10 = getelementptr { i8, { ptr } }, ptr %done_0.i.i, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  br i1 %switch.i.i.i, label %switch_branch_Eventually.Done_0.i.i.i, label %switch_branch_Eventually.NotYetDone_0.i.i.i

switch_branch_Eventually.Done_0.i.i.i:            ; preds = %2
  %12 = load ptr, ptr %k_0, align 8
  %13 = getelementptr { ptr, ptr }, ptr %k_0, i64 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = tail call ptr %14(ptr %12, ptr %11)
  br label %"Eventually.$raw_fun_4016.exit"

switch_branch_Eventually.NotYetDone_0.i.i.i:      ; preds = %2
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %11, ptr %fun_capture_0.i.i.i, align 8
  %k_0.i.i.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i.i.i, i64 0, i32 1
  store ptr %k_0, ptr %k_0.i.i.i, align 8
  store ptr %fun_capture_0.i.i.i, ptr %16, align 8
  %fun_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"Eventually.#fun_closure_4114", ptr %fun_func_0.i.i.i, align 8
  %17 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %17, align 1
  %18 = getelementptr { i8, { ptr } }, ptr %17, i64 0, i32 1, i32 0
  store ptr %16, ptr %18, align 8
  br label %"Eventually.$raw_fun_4016.exit"

"Eventually.$raw_fun_4016.exit":                  ; preds = %switch_branch_Eventually.Done_0.i.i.i, %switch_branch_Eventually.NotYetDone_0.i.i.i
  %common.ret.op.i.i.i = phi ptr [ %15, %switch_branch_Eventually.Done_0.i.i.i ], [ %17, %switch_branch_Eventually.NotYetDone_0.i.i.i ]
  ret ptr %common.ret.op.i.i.i
}

define internal noundef ptr @Eventually.Done(ptr nocapture nofree readnone %0, ptr nofree %"Eventually.$p_258_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"Eventually.$p_258_0", ptr %3, align 8
  ret ptr %2
}

define internal ptr @"Eventually.#let_closure_4112"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load i8, ptr %d_0, align 8
  %switch.i = icmp eq i8 %3, 0
  %4 = getelementptr { i8, { ptr } }, ptr %d_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  br i1 %switch.i, label %switch_branch_Eventually.Done_0.i, label %switch_branch_Eventually.NotYetDone_0.i

switch_branch_Eventually.Done_0.i:                ; preds = %2
  %6 = load ptr, ptr %1, align 8
  %7 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, ptr %5)
  br label %"Eventually.$raw_let_4017.exit"

switch_branch_Eventually.NotYetDone_0.i:          ; preds = %2
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %5, ptr %fun_capture_0.i, align 8
  %k_0.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %1, ptr %k_0.i, align 8
  store ptr %fun_capture_0.i, ptr %10, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"Eventually.#fun_closure_4111", ptr %fun_func_0.i, align 8
  %11 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr %10, ptr %12, align 8
  br label %"Eventually.$raw_let_4017.exit"

"Eventually.$raw_let_4017.exit":                  ; preds = %switch_branch_Eventually.Done_0.i, %switch_branch_Eventually.NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %9, %switch_branch_Eventually.Done_0.i ], [ %11, %switch_branch_Eventually.NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @"Eventually.#let_closure_4113"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %done_0 = load ptr, ptr %0, align 8
  %3 = load i8, ptr %done_0, align 8
  %switch.i = icmp eq i8 %3, 0
  %4 = getelementptr { i8, { ptr } }, ptr %done_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  br i1 %switch.i, label %switch_branch_Eventually.Done_0.i, label %switch_branch_Eventually.NotYetDone_0.i

switch_branch_Eventually.Done_0.i:                ; preds = %2
  %6 = load ptr, ptr %1, align 8
  %7 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, ptr %5)
  br label %"Eventually.$raw_let_4026.exit"

switch_branch_Eventually.NotYetDone_0.i:          ; preds = %2
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %5, ptr %fun_capture_0.i, align 8
  %k_0.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %1, ptr %k_0.i, align 8
  store ptr %fun_capture_0.i, ptr %10, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"Eventually.#fun_closure_4114", ptr %fun_func_0.i, align 8
  %11 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr %10, ptr %12, align 8
  br label %"Eventually.$raw_let_4026.exit"

"Eventually.$raw_let_4026.exit":                  ; preds = %switch_branch_Eventually.Done_0.i, %switch_branch_Eventually.NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %9, %switch_branch_Eventually.Done_0.i ], [ %11, %switch_branch_Eventually.NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @Eventually.eventuallyBind(ptr nocapture nofree readnone %0, ptr nofree %"Eventually.$done_272_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Eventually.$done_272_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Eventually.#let_closure_4113", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Eventually.#fun_closure_4114"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readnone %1) {
  %p_0 = load ptr, ptr %0, align 8
  %k_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %k_0 = load ptr, ptr %k_addr_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = load ptr, ptr %p_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %p_0, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %3)
  %8 = load i8, ptr %7, align 1
  %switch.i = icmp eq i8 %8, 0
  %9 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  br i1 %switch.i, label %switch_branch_Eventually.Done_0.i, label %switch_branch_Eventually.NotYetDone_0.i

switch_branch_Eventually.Done_0.i:                ; preds = %2
  %11 = load ptr, ptr %k_0, align 8
  %12 = getelementptr { ptr, ptr }, ptr %k_0, i64 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = tail call ptr %13(ptr %11, ptr %10)
  br label %"Eventually.$raw_fun_4025.exit"

switch_branch_Eventually.NotYetDone_0.i:          ; preds = %2
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %10, ptr %fun_capture_0.i, align 8
  %k_0.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %k_0, ptr %k_0.i, align 8
  store ptr %fun_capture_0.i, ptr %15, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"Eventually.#fun_closure_4116", ptr %fun_func_0.i, align 8
  %16 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %16, align 1
  %17 = getelementptr { i8, { ptr } }, ptr %16, i64 0, i32 1, i32 0
  store ptr %15, ptr %17, align 8
  br label %"Eventually.$raw_fun_4025.exit"

"Eventually.$raw_fun_4025.exit":                  ; preds = %switch_branch_Eventually.Done_0.i, %switch_branch_Eventually.NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %14, %switch_branch_Eventually.Done_0.i ], [ %16, %switch_branch_Eventually.NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @"Eventually.#let_closure_4115"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load i8, ptr %d_0, align 8
  %switch.i = icmp eq i8 %3, 0
  %4 = getelementptr { i8, { ptr } }, ptr %d_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  br i1 %switch.i, label %switch_branch_Eventually.Done_0.i, label %switch_branch_Eventually.NotYetDone_0.i

switch_branch_Eventually.Done_0.i:                ; preds = %2
  %6 = load ptr, ptr %1, align 8
  %7 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, ptr %5)
  br label %"Eventually.$eventuallyBind_curry_288.exit"

switch_branch_Eventually.NotYetDone_0.i:          ; preds = %2
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %5, ptr %fun_capture_0.i, align 8
  %k_0.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %1, ptr %k_0.i, align 8
  store ptr %fun_capture_0.i, ptr %10, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"Eventually.#fun_closure_4149", ptr %fun_func_0.i, align 8
  %11 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr %10, ptr %12, align 8
  br label %"Eventually.$eventuallyBind_curry_288.exit"

"Eventually.$eventuallyBind_curry_288.exit":      ; preds = %switch_branch_Eventually.Done_0.i, %switch_branch_Eventually.NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %9, %switch_branch_Eventually.Done_0.i ], [ %11, %switch_branch_Eventually.NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @"Eventually.#fun_closure_4116"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readnone %1) {
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
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %7, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %8, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"Eventually.#let_closure_4115", ptr %let_func_0.i, align 8
  %d_0.i.i = load ptr, ptr %let_capture_0.i, align 8
  %9 = load i8, ptr %d_0.i.i, align 8
  %switch.i.i.i = icmp eq i8 %9, 0
  %10 = getelementptr { i8, { ptr } }, ptr %d_0.i.i, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  br i1 %switch.i.i.i, label %switch_branch_Eventually.Done_0.i.i.i, label %switch_branch_Eventually.NotYetDone_0.i.i.i

switch_branch_Eventually.Done_0.i.i.i:            ; preds = %2
  %12 = load ptr, ptr %k_0, align 8
  %13 = getelementptr { ptr, ptr }, ptr %k_0, i64 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = tail call ptr %14(ptr %12, ptr %11)
  br label %"Eventually.$raw_fun_4024.exit"

switch_branch_Eventually.NotYetDone_0.i.i.i:      ; preds = %2
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %11, ptr %fun_capture_0.i.i.i, align 8
  %k_0.i.i.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i.i.i, i64 0, i32 1
  store ptr %k_0, ptr %k_0.i.i.i, align 8
  store ptr %fun_capture_0.i.i.i, ptr %16, align 8
  %fun_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"Eventually.#fun_closure_4149", ptr %fun_func_0.i.i.i, align 8
  %17 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %17, align 1
  %18 = getelementptr { i8, { ptr } }, ptr %17, i64 0, i32 1, i32 0
  store ptr %16, ptr %18, align 8
  br label %"Eventually.$raw_fun_4024.exit"

"Eventually.$raw_fun_4024.exit":                  ; preds = %switch_branch_Eventually.Done_0.i.i.i, %switch_branch_Eventually.NotYetDone_0.i.i.i
  %common.ret.op.i.i.i = phi ptr [ %15, %switch_branch_Eventually.Done_0.i.i.i ], [ %17, %switch_branch_Eventually.NotYetDone_0.i.i.i ]
  ret ptr %common.ret.op.i.i.i
}

define internal ptr @"Eventually.#let_closure_4117"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"Eventually.#fun_closure_4118"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Eventually.Done_0.i, label %switch_branch_Eventually.NotYetDone_0.i

switch_branch_Eventually.Done_0.i:                ; preds = %2
  %4 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { i8, { i32 } }, ptr %5, i64 0, i32 1
  %7 = load i32, ptr %6, align 4
  %8 = tail call ptr @malgo_int32_t_to_string(i32 %7)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %8, ptr %10, align 8
  %11 = tail call ptr @malgo_print_string(ptr %8)
  br label %"Eventually.$raw_fun_4027.exit"

switch_branch_Eventually.NotYetDone_0.i:          ; preds = %2
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr @str4108, ptr %13, align 8
  %14 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4108)
  br label %"Eventually.$raw_fun_4027.exit"

"Eventually.$raw_fun_4027.exit":                  ; preds = %switch_branch_Eventually.Done_0.i, %switch_branch_Eventually.NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %11, %switch_branch_Eventually.Done_0.i ], [ %14, %switch_branch_Eventually.NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @"Eventually.#let_closure_4125"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"Eventually.#fun_closure_4126"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Eventually.Done_0.i, label %switch_branch_Eventually.NotYetDone_0.i

switch_branch_Eventually.Done_0.i:                ; preds = %2
  %4 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { i8, { i32 } }, ptr %5, i64 0, i32 1
  %7 = load i32, ptr %6, align 4
  %8 = tail call ptr @malgo_int32_t_to_string(i32 %7)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %8, ptr %10, align 8
  %11 = tail call ptr @malgo_print_string(ptr %8)
  br label %"Eventually.$raw_fun_4036.exit"

switch_branch_Eventually.NotYetDone_0.i:          ; preds = %2
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr @str4108, ptr %13, align 8
  %14 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4108)
  br label %"Eventually.$raw_fun_4036.exit"

"Eventually.$raw_fun_4036.exit":                  ; preds = %switch_branch_Eventually.Done_0.i, %switch_branch_Eventually.NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %11, %switch_branch_Eventually.Done_0.i ], [ %14, %switch_branch_Eventually.NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @"Eventually.#let_closure_4129"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"Eventually.#fun_closure_4130"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Eventually.Done_0.i, label %switch_branch_Eventually.NotYetDone_0.i

switch_branch_Eventually.Done_0.i:                ; preds = %2
  %4 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { i8, { i32 } }, ptr %5, i64 0, i32 1
  %7 = load i32, ptr %6, align 4
  %8 = tail call ptr @malgo_int32_t_to_string(i32 %7)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %8, ptr %10, align 8
  %11 = tail call ptr @malgo_print_string(ptr %8)
  br label %"Eventually.$raw_fun_4039.exit"

switch_branch_Eventually.NotYetDone_0.i:          ; preds = %2
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr @str4108, ptr %13, align 8
  %14 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4108)
  br label %"Eventually.$raw_fun_4039.exit"

"Eventually.$raw_fun_4039.exit":                  ; preds = %switch_branch_Eventually.Done_0.i, %switch_branch_Eventually.NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %11, %switch_branch_Eventually.Done_0.i ], [ %14, %switch_branch_Eventually.NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @"Eventually.#let_closure_4131"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"Eventually.#fun_closure_4132"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Eventually.Done_0.i, label %switch_branch_Eventually.NotYetDone_0.i

switch_branch_Eventually.Done_0.i:                ; preds = %2
  %4 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { i8, { i32 } }, ptr %5, i64 0, i32 1
  %7 = load i32, ptr %6, align 4
  %8 = tail call ptr @malgo_int32_t_to_string(i32 %7)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %8, ptr %10, align 8
  %11 = tail call ptr @malgo_print_string(ptr %8)
  br label %"Eventually.$raw_fun_4040.exit"

switch_branch_Eventually.NotYetDone_0.i:          ; preds = %2
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr @str4108, ptr %13, align 8
  %14 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4108)
  br label %"Eventually.$raw_fun_4040.exit"

"Eventually.$raw_fun_4040.exit":                  ; preds = %switch_branch_Eventually.Done_0.i, %switch_branch_Eventually.NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %11, %switch_branch_Eventually.Done_0.i ], [ %14, %switch_branch_Eventually.NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @"Eventually.#let_closure_4133"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"Eventually.#fun_closure_4134"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Eventually.Done_0.i, label %switch_branch_Eventually.NotYetDone_0.i

switch_branch_Eventually.Done_0.i:                ; preds = %2
  %4 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { i8, { i32 } }, ptr %5, i64 0, i32 1
  %7 = load i32, ptr %6, align 4
  %8 = tail call ptr @malgo_int32_t_to_string(i32 %7)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %8, ptr %10, align 8
  %11 = tail call ptr @malgo_print_string(ptr %8)
  br label %"Eventually.$raw_fun_4054.exit"

switch_branch_Eventually.NotYetDone_0.i:          ; preds = %2
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr @str4108, ptr %13, align 8
  %14 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4108)
  br label %"Eventually.$raw_fun_4054.exit"

"Eventually.$raw_fun_4054.exit":                  ; preds = %switch_branch_Eventually.Done_0.i, %switch_branch_Eventually.NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %11, %switch_branch_Eventually.Done_0.i ], [ %14, %switch_branch_Eventually.NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @"Eventually.#let_closure_4141"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"Eventually.#fun_closure_4142"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Eventually.Done_0.i, label %switch_branch_Eventually.NotYetDone_0.i

switch_branch_Eventually.Done_0.i:                ; preds = %2
  %4 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { i8, { i32 } }, ptr %5, i64 0, i32 1
  %7 = load i32, ptr %6, align 4
  %8 = tail call ptr @malgo_int32_t_to_string(i32 %7)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %8, ptr %10, align 8
  %11 = tail call ptr @malgo_print_string(ptr %8)
  br label %"Eventually.$raw_fun_4063.exit"

switch_branch_Eventually.NotYetDone_0.i:          ; preds = %2
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr @str4108, ptr %13, align 8
  %14 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4108)
  br label %"Eventually.$raw_fun_4063.exit"

"Eventually.$raw_fun_4063.exit":                  ; preds = %switch_branch_Eventually.Done_0.i, %switch_branch_Eventually.NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %11, %switch_branch_Eventually.Done_0.i ], [ %14, %switch_branch_Eventually.NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @"Eventually.#let_closure_4145"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"Eventually.#fun_closure_4146"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Eventually.Done_0.i, label %switch_branch_Eventually.NotYetDone_0.i

switch_branch_Eventually.Done_0.i:                ; preds = %2
  %4 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { i8, { i32 } }, ptr %5, i64 0, i32 1
  %7 = load i32, ptr %6, align 4
  %8 = tail call ptr @malgo_int32_t_to_string(i32 %7)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %8, ptr %10, align 8
  %11 = tail call ptr @malgo_print_string(ptr %8)
  br label %"Eventually.$raw_fun_4066.exit"

switch_branch_Eventually.NotYetDone_0.i:          ; preds = %2
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr @str4108, ptr %13, align 8
  %14 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4108)
  br label %"Eventually.$raw_fun_4066.exit"

"Eventually.$raw_fun_4066.exit":                  ; preds = %switch_branch_Eventually.Done_0.i, %switch_branch_Eventually.NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %11, %switch_branch_Eventually.Done_0.i ], [ %14, %switch_branch_Eventually.NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @"Eventually.#let_closure_4147"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"Eventually.#fun_closure_4148"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Eventually.Done_0.i, label %switch_branch_Eventually.NotYetDone_0.i

switch_branch_Eventually.Done_0.i:                ; preds = %2
  %4 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { i8, { i32 } }, ptr %5, i64 0, i32 1
  %7 = load i32, ptr %6, align 4
  %8 = tail call ptr @malgo_int32_t_to_string(i32 %7)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %8, ptr %10, align 8
  %11 = tail call ptr @malgo_print_string(ptr %8)
  br label %"Eventually.$raw_fun_4067.exit"

switch_branch_Eventually.NotYetDone_0.i:          ; preds = %2
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr @str4108, ptr %13, align 8
  %14 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4108)
  br label %"Eventually.$raw_fun_4067.exit"

"Eventually.$raw_fun_4067.exit":                  ; preds = %switch_branch_Eventually.Done_0.i, %switch_branch_Eventually.NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %11, %switch_branch_Eventually.Done_0.i ], [ %14, %switch_branch_Eventually.NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @"Eventually.#fun_closure_4149"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readnone %1) {
  %p_0 = load ptr, ptr %0, align 8
  %k_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %k_0 = load ptr, ptr %k_addr_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = load ptr, ptr %p_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %p_0, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %3)
  %8 = load i8, ptr %7, align 1
  %switch.i = icmp eq i8 %8, 0
  %9 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  br i1 %switch.i, label %switch_branch_Eventually.Done_0.i, label %switch_branch_Eventually.NotYetDone_0.i

switch_branch_Eventually.Done_0.i:                ; preds = %2
  %11 = load ptr, ptr %k_0, align 8
  %12 = getelementptr { ptr, ptr }, ptr %k_0, i64 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = tail call ptr %13(ptr %11, ptr %10)
  br label %"Eventually.$raw_fun_4019.exit"

switch_branch_Eventually.NotYetDone_0.i:          ; preds = %2
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %k_0, ptr %fun_capture_0.i, align 8
  %p_0.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %10, ptr %p_0.i, align 8
  store ptr %fun_capture_0.i, ptr %15, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"Eventually.#fun_closure_4150", ptr %fun_func_0.i, align 8
  %16 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %16, align 1
  %17 = getelementptr { i8, { ptr } }, ptr %16, i64 0, i32 1, i32 0
  store ptr %15, ptr %17, align 8
  br label %"Eventually.$raw_fun_4019.exit"

"Eventually.$raw_fun_4019.exit":                  ; preds = %switch_branch_Eventually.Done_0.i, %switch_branch_Eventually.NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %14, %switch_branch_Eventually.Done_0.i ], [ %16, %switch_branch_Eventually.NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @"Eventually.#fun_closure_4150"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readnone %1) {
  %k_0 = load ptr, ptr %0, align 8
  %p_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = load ptr, ptr %p_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %p_0, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %3)
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %7, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %8, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"Eventually.#let_closure_4112", ptr %let_func_0.i, align 8
  %9 = load i8, ptr %7, align 1
  %switch.i.i = icmp eq i8 %9, 0
  %10 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  br i1 %switch.i.i, label %switch_branch_Eventually.Done_0.i.i, label %switch_branch_Eventually.NotYetDone_0.i.i

switch_branch_Eventually.Done_0.i.i:              ; preds = %2
  %12 = load ptr, ptr %k_0, align 8
  %13 = getelementptr { ptr, ptr }, ptr %k_0, i64 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = tail call ptr %14(ptr %12, ptr %11)
  br label %"Eventually.$raw_fun_4018.exit"

switch_branch_Eventually.NotYetDone_0.i.i:        ; preds = %2
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %11, ptr %fun_capture_0.i.i, align 8
  %k_0.i.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i.i, i64 0, i32 1
  store ptr %k_0, ptr %k_0.i.i, align 8
  store ptr %fun_capture_0.i.i, ptr %16, align 8
  %fun_func_0.i.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"Eventually.#fun_closure_4149", ptr %fun_func_0.i.i, align 8
  %17 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %17, align 1
  %18 = getelementptr { i8, { ptr } }, ptr %17, i64 0, i32 1, i32 0
  store ptr %16, ptr %18, align 8
  br label %"Eventually.$raw_fun_4018.exit"

"Eventually.$raw_fun_4018.exit":                  ; preds = %switch_branch_Eventually.Done_0.i.i, %switch_branch_Eventually.NotYetDone_0.i.i
  %common.ret.op.i.i = phi ptr [ %15, %switch_branch_Eventually.Done_0.i.i ], [ %17, %switch_branch_Eventually.NotYetDone_0.i.i ]
  ret ptr %common.ret.op.i.i
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_hash_table_new()
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %Done_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Eventually.Done, ptr %Done_func_0.i, align 8
  tail call void @malgo_hash_table_insert(ptr %2, ptr noundef nonnull @str4109, ptr noundef nonnull %3)
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %eventuallyBind_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @Eventually.eventuallyBind, ptr %eventuallyBind_func_0.i, align 8
  tail call void @malgo_hash_table_insert(ptr %2, ptr noundef nonnull @str4110, ptr noundef nonnull %4)
  store ptr %2, ptr @Eventually.eventually, align 8
  %5 = tail call ptr @malgo_hash_table_get(ptr %2, ptr noundef nonnull @str4109)
  %6 = tail call ptr @malgo_hash_table_get(ptr %2, ptr noundef nonnull @str4110)
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %7, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"Eventually.#fun_closure_4155", ptr %fun_func_0.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %8, i64 0, i32 1, i32 0
  store ptr %7, ptr %9, align 8
  %10 = load ptr, ptr %6, align 8
  %11 = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call ptr %12(ptr %10, ptr nonnull %8)
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_2.i, ptr %14, align 8
  %fun_func_1.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"Eventually.#fun_closure_4157", ptr %fun_func_1.i, align 8
  %15 = load ptr, ptr %13, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = tail call ptr %17(ptr %15, ptr nonnull %14)
  store ptr %18, ptr @Eventually.comp, align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %19, align 1
  %20 = load ptr, ptr @Eventually.comp, align 8
  %21 = load i8, ptr %20, align 8
  %switch.i = icmp eq i8 %21, 0
  %22 = getelementptr { i8, { ptr } }, ptr %20, i64 0, i32 1
  %23 = load ptr, ptr %22, align 8
  br i1 %switch.i, label %switch_branch_Eventually.Done_0.i, label %switch_branch_Eventually.NotYetDone_7.i

switch_branch_Eventually.Done_0.i:                ; preds = %1
  %24 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %24, align 1
  %25 = getelementptr { i8, { ptr } }, ptr %24, i64 0, i32 1, i32 0
  store ptr %23, ptr %25, align 8
  %26 = load ptr, ptr @Eventually.comp, align 8
  %27 = load i8, ptr %26, align 8
  %switch1.i = icmp eq i8 %27, 0
  %28 = getelementptr { i8, { ptr } }, ptr %26, i64 0, i32 1
  %29 = load ptr, ptr %28, align 8
  br i1 %switch1.i, label %switch_branch_Eventually.Done_1.i, label %switch_branch_Eventually.NotYetDone_3.i

switch_branch_Eventually.Done_1.i:                ; preds = %switch_branch_Eventually.Done_0.i
  %30 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %30, align 1
  %31 = getelementptr { i8, { ptr } }, ptr %30, i64 0, i32 1, i32 0
  store ptr %29, ptr %31, align 8
  %32 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %32, align 1
  %33 = getelementptr { i8, { ptr } }, ptr %32, i64 0, i32 1, i32 0
  store ptr %29, ptr %33, align 8
  %34 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %34, align 1
  %35 = getelementptr { i8, { ptr } }, ptr %34, i64 0, i32 1, i32 0
  store ptr %29, ptr %35, align 8
  %36 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %34, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %36, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %36, i64 0, i32 1
  store ptr @"Eventually.#let_closure_4117", ptr %let_func_0.i, align 8
  br label %Eventually.main.exit

switch_branch_Eventually.NotYetDone_3.i:          ; preds = %switch_branch_Eventually.Done_0.i
  %37 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %37, align 1
  %38 = load ptr, ptr %29, align 8
  %39 = getelementptr { ptr, ptr }, ptr %29, i64 0, i32 1
  %40 = load ptr, ptr %39, align 8
  %41 = tail call ptr %40(ptr %38, ptr nonnull %37)
  %42 = load i8, ptr %41, align 1
  %switch2.i = icmp eq i8 %42, 0
  %43 = getelementptr { i8, { ptr } }, ptr %41, i64 0, i32 1
  %44 = load ptr, ptr %43, align 8
  br i1 %switch2.i, label %switch_branch_Eventually.Done_5.i, label %switch_branch_Eventually.NotYetDone_5.i

switch_branch_Eventually.Done_5.i:                ; preds = %switch_branch_Eventually.NotYetDone_3.i
  %45 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %45, align 1
  %46 = getelementptr { i8, { ptr } }, ptr %45, i64 0, i32 1, i32 0
  store ptr %44, ptr %46, align 8
  %47 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %47, align 1
  %48 = getelementptr { i8, { ptr } }, ptr %47, i64 0, i32 1, i32 0
  store ptr %44, ptr %48, align 8
  %49 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_8.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %47, ptr %let_capture_8.i, align 8
  store ptr %let_capture_8.i, ptr %49, align 8
  %let_func_4.i = getelementptr { ptr, ptr }, ptr %49, i64 0, i32 1
  store ptr @"Eventually.#let_closure_4125", ptr %let_func_4.i, align 8
  br label %Eventually.main.exit

switch_branch_Eventually.NotYetDone_5.i:          ; preds = %switch_branch_Eventually.NotYetDone_3.i
  %50 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %50, align 1
  %51 = load ptr, ptr %44, align 8
  %52 = getelementptr { ptr, ptr }, ptr %44, i64 0, i32 1
  %53 = load ptr, ptr %52, align 8
  %54 = tail call ptr %53(ptr %51, ptr nonnull %50)
  %55 = load i8, ptr %54, align 1
  %switch3.i = icmp eq i8 %55, 0
  %56 = getelementptr { i8, { ptr } }, ptr %54, i64 0, i32 1
  %57 = load ptr, ptr %56, align 8
  br i1 %switch3.i, label %switch_branch_Eventually.Done_7.i, label %switch_branch_Eventually.NotYetDone_6.i

switch_branch_Eventually.Done_7.i:                ; preds = %switch_branch_Eventually.NotYetDone_5.i
  %58 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %58, align 1
  %59 = getelementptr { i8, { ptr } }, ptr %58, i64 0, i32 1, i32 0
  store ptr %57, ptr %59, align 8
  %60 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_12.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %58, ptr %let_capture_12.i, align 8
  store ptr %let_capture_12.i, ptr %60, align 8
  %let_func_6.i = getelementptr { ptr, ptr }, ptr %60, i64 0, i32 1
  store ptr @"Eventually.#let_closure_4129", ptr %let_func_6.i, align 8
  br label %Eventually.main.exit

switch_branch_Eventually.NotYetDone_6.i:          ; preds = %switch_branch_Eventually.NotYetDone_5.i
  %61 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %61, align 1
  %62 = load ptr, ptr %57, align 8
  %63 = getelementptr { ptr, ptr }, ptr %57, i64 0, i32 1
  %64 = load ptr, ptr %63, align 8
  %65 = tail call ptr %64(ptr %62, ptr nonnull %61)
  %66 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_14.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %65, ptr %let_capture_14.i, align 8
  store ptr %let_capture_14.i, ptr %66, align 8
  %let_func_7.i = getelementptr { ptr, ptr }, ptr %66, i64 0, i32 1
  store ptr @"Eventually.#let_closure_4131", ptr %let_func_7.i, align 8
  br label %Eventually.main.exit

switch_branch_Eventually.NotYetDone_7.i:          ; preds = %1
  %67 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %67, align 1
  %68 = load ptr, ptr %23, align 8
  %69 = getelementptr { ptr, ptr }, ptr %23, i64 0, i32 1
  %70 = load ptr, ptr %69, align 8
  %71 = tail call ptr %70(ptr %68, ptr nonnull %67)
  %72 = load ptr, ptr @Eventually.comp, align 8
  %73 = load i8, ptr %72, align 8
  %switch4.i = icmp eq i8 %73, 0
  %74 = getelementptr { i8, { ptr } }, ptr %72, i64 0, i32 1
  %75 = load ptr, ptr %74, align 8
  br i1 %switch4.i, label %switch_branch_Eventually.Done_8.i, label %switch_branch_Eventually.NotYetDone_11.i

switch_branch_Eventually.Done_8.i:                ; preds = %switch_branch_Eventually.NotYetDone_7.i
  %76 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %76, align 1
  %77 = getelementptr { i8, { ptr } }, ptr %76, i64 0, i32 1, i32 0
  store ptr %75, ptr %77, align 8
  %78 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %78, align 1
  %79 = getelementptr { i8, { ptr } }, ptr %78, i64 0, i32 1, i32 0
  store ptr %75, ptr %79, align 8
  %80 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %80, align 1
  %81 = getelementptr { i8, { ptr } }, ptr %80, i64 0, i32 1, i32 0
  store ptr %75, ptr %81, align 8
  %82 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_16.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %80, ptr %let_capture_16.i, align 8
  store ptr %let_capture_16.i, ptr %82, align 8
  %let_func_8.i = getelementptr { ptr, ptr }, ptr %82, i64 0, i32 1
  store ptr @"Eventually.#let_closure_4133", ptr %let_func_8.i, align 8
  br label %Eventually.main.exit

switch_branch_Eventually.NotYetDone_11.i:         ; preds = %switch_branch_Eventually.NotYetDone_7.i
  %83 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %83, align 1
  %84 = load ptr, ptr %75, align 8
  %85 = getelementptr { ptr, ptr }, ptr %75, i64 0, i32 1
  %86 = load ptr, ptr %85, align 8
  %87 = tail call ptr %86(ptr %84, ptr nonnull %83)
  %88 = load i8, ptr %87, align 1
  %switch5.i = icmp eq i8 %88, 0
  %89 = getelementptr { i8, { ptr } }, ptr %87, i64 0, i32 1
  %90 = load ptr, ptr %89, align 8
  br i1 %switch5.i, label %switch_branch_Eventually.Done_12.i, label %switch_branch_Eventually.NotYetDone_13.i

switch_branch_Eventually.Done_12.i:               ; preds = %switch_branch_Eventually.NotYetDone_11.i
  %91 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %91, align 1
  %92 = getelementptr { i8, { ptr } }, ptr %91, i64 0, i32 1, i32 0
  store ptr %90, ptr %92, align 8
  %93 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %93, align 1
  %94 = getelementptr { i8, { ptr } }, ptr %93, i64 0, i32 1, i32 0
  store ptr %90, ptr %94, align 8
  %95 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_24.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %93, ptr %let_capture_24.i, align 8
  store ptr %let_capture_24.i, ptr %95, align 8
  %let_func_12.i = getelementptr { ptr, ptr }, ptr %95, i64 0, i32 1
  store ptr @"Eventually.#let_closure_4141", ptr %let_func_12.i, align 8
  br label %Eventually.main.exit

switch_branch_Eventually.NotYetDone_13.i:         ; preds = %switch_branch_Eventually.NotYetDone_11.i
  %96 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %96, align 1
  %97 = load ptr, ptr %90, align 8
  %98 = getelementptr { ptr, ptr }, ptr %90, i64 0, i32 1
  %99 = load ptr, ptr %98, align 8
  %100 = tail call ptr %99(ptr %97, ptr nonnull %96)
  %101 = load i8, ptr %100, align 1
  %switch6.i = icmp eq i8 %101, 0
  %102 = getelementptr { i8, { ptr } }, ptr %100, i64 0, i32 1
  %103 = load ptr, ptr %102, align 8
  br i1 %switch6.i, label %switch_branch_Eventually.Done_14.i, label %switch_branch_Eventually.NotYetDone_14.i

switch_branch_Eventually.Done_14.i:               ; preds = %switch_branch_Eventually.NotYetDone_13.i
  %104 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %104, align 1
  %105 = getelementptr { i8, { ptr } }, ptr %104, i64 0, i32 1, i32 0
  store ptr %103, ptr %105, align 8
  %106 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_28.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %104, ptr %let_capture_28.i, align 8
  store ptr %let_capture_28.i, ptr %106, align 8
  %let_func_14.i = getelementptr { ptr, ptr }, ptr %106, i64 0, i32 1
  store ptr @"Eventually.#let_closure_4145", ptr %let_func_14.i, align 8
  br label %Eventually.main.exit

switch_branch_Eventually.NotYetDone_14.i:         ; preds = %switch_branch_Eventually.NotYetDone_13.i
  %107 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %107, align 1
  %108 = load ptr, ptr %103, align 8
  %109 = getelementptr { ptr, ptr }, ptr %103, i64 0, i32 1
  %110 = load ptr, ptr %109, align 8
  %111 = tail call ptr %110(ptr %108, ptr nonnull %107)
  %112 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_30.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %111, ptr %let_capture_30.i, align 8
  store ptr %let_capture_30.i, ptr %112, align 8
  %let_func_15.i = getelementptr { ptr, ptr }, ptr %112, i64 0, i32 1
  store ptr @"Eventually.#let_closure_4147", ptr %let_func_15.i, align 8
  br label %Eventually.main.exit

Eventually.main.exit:                             ; preds = %switch_branch_Eventually.Done_1.i, %switch_branch_Eventually.Done_5.i, %switch_branch_Eventually.Done_7.i, %switch_branch_Eventually.NotYetDone_6.i, %switch_branch_Eventually.Done_8.i, %switch_branch_Eventually.Done_12.i, %switch_branch_Eventually.Done_14.i, %switch_branch_Eventually.NotYetDone_14.i
  %"Eventually.#fun_closure_4148.sink.i" = phi ptr [ @"Eventually.#fun_closure_4148", %switch_branch_Eventually.NotYetDone_14.i ], [ @"Eventually.#fun_closure_4146", %switch_branch_Eventually.Done_14.i ], [ @"Eventually.#fun_closure_4142", %switch_branch_Eventually.Done_12.i ], [ @"Eventually.#fun_closure_4134", %switch_branch_Eventually.Done_8.i ], [ @"Eventually.#fun_closure_4132", %switch_branch_Eventually.NotYetDone_6.i ], [ @"Eventually.#fun_closure_4130", %switch_branch_Eventually.Done_7.i ], [ @"Eventually.#fun_closure_4126", %switch_branch_Eventually.Done_5.i ], [ @"Eventually.#fun_closure_4118", %switch_branch_Eventually.Done_1.i ]
  %.sink9.i = phi ptr [ %112, %switch_branch_Eventually.NotYetDone_14.i ], [ %106, %switch_branch_Eventually.Done_14.i ], [ %95, %switch_branch_Eventually.Done_12.i ], [ %82, %switch_branch_Eventually.Done_8.i ], [ %66, %switch_branch_Eventually.NotYetDone_6.i ], [ %60, %switch_branch_Eventually.Done_7.i ], [ %49, %switch_branch_Eventually.Done_5.i ], [ %36, %switch_branch_Eventually.Done_1.i ]
  %let_func_15.sink.i = phi ptr [ %let_func_15.i, %switch_branch_Eventually.NotYetDone_14.i ], [ %let_func_14.i, %switch_branch_Eventually.Done_14.i ], [ %let_func_12.i, %switch_branch_Eventually.Done_12.i ], [ %let_func_8.i, %switch_branch_Eventually.Done_8.i ], [ %let_func_7.i, %switch_branch_Eventually.NotYetDone_6.i ], [ %let_func_6.i, %switch_branch_Eventually.Done_7.i ], [ %let_func_4.i, %switch_branch_Eventually.Done_5.i ], [ %let_func_0.i, %switch_branch_Eventually.Done_1.i ]
  %113 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_30.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_30.i, ptr %113, align 8
  %fun_func_15.i = getelementptr { ptr, ptr }, ptr %113, i64 0, i32 1
  store ptr %"Eventually.#fun_closure_4148.sink.i", ptr %fun_func_15.i, align 8
  %114 = load ptr, ptr %.sink9.i, align 8
  %115 = load ptr, ptr %let_func_15.sink.i, align 8
  %116 = tail call ptr %115(ptr %114, ptr nonnull %113)
  ret i32 0
}

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr

define internal noundef ptr @"Eventually.#fun_closure_4155"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
"switch_branch_Builtin.String#_0":
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr @str4156, ptr %3, align 8
  %4 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4156)
  %5 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %4, ptr %6, align 8
  ret ptr %5
}

define internal noundef ptr @"Eventually.#fun_closure_4158"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
"switch_branch_Builtin.String#_0":
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr @str4159, ptr %3, align 8
  %4 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4159)
  %5 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %4, ptr %6, align 8
  ret ptr %5
}

define internal ptr @"Eventually.#fun_closure_4160"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = load ptr, ptr @Eventually.eventually, align 8
  %4 = tail call ptr @malgo_hash_table_get(ptr %3, ptr noundef nonnull @str4109)
  %5 = tail call ptr @malgo_hash_table_get(ptr %3, ptr noundef nonnull @str4110)
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

define internal ptr @"Eventually.#fun_closure_4157"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = load ptr, ptr @Eventually.eventually, align 8
  %4 = tail call ptr @malgo_hash_table_get(ptr %3, ptr noundef nonnull @str4109)
  %5 = tail call ptr @malgo_hash_table_get(ptr %3, ptr noundef nonnull @str4110)
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0 = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0, ptr %6, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Eventually.#fun_closure_4158", ptr %fun_func_0, align 8
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
  store ptr @"Eventually.#fun_closure_4160", ptr %fun_func_1, align 8
  %14 = load ptr, ptr %12, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr %16(ptr %14, ptr nonnull %13)
  ret ptr %17
}
