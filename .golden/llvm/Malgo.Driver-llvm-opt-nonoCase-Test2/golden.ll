; ModuleID = '/workspaces/malgo/.malgo-work/Test2.ll'
source_filename = "./test/testcases/malgo/Test2.mlg"

@str41 = unnamed_addr constant [6 x i8] c"WRONG\00"
@str42 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @Test2.malgo_print_string(ptr nocapture nofree readnone %0, ptr %"Test2.$p_26_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"Test2.$p_26_0")
  ret ptr %2
}

define internal noundef ptr @Test2.rtob(ptr nocapture nofree readnone %0, ptr nofree noundef nonnull readonly dereferenceable(1) %"Test2.$r_27_0") {
  %2 = load i8, ptr %"Test2.$r_27_0", align 1
  %switch = icmp eq i8 %2, 0
  br i1 %switch, label %switch_branch_Test2.R_0, label %common.ret

common.ret:                                       ; preds = %1, %switch_branch_Test2.R_0
  %common.ret.op = phi ptr [ %3, %switch_branch_Test2.R_0 ], [ %"Test2.$r_27_0", %1 ]
  ret ptr %common.ret.op

switch_branch_Test2.R_0:                          ; preds = %1
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 2, ptr %3, align 1
  br label %common.ret
}

define internal ptr @"Test2.#fun_closure_40"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
common.ret:
  %malgo_print_string_0 = load ptr, ptr %0, align 8
  %2 = load i8, ptr %1, align 1
  %switch = icmp ult i8 %2, 2
  %spec.select = select i1 %switch, ptr @str41, ptr @str42
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
  store ptr @Test2.malgo_print_string, ptr %malgo_print_string_func_0.i, align 8
  store ptr %4, ptr %fun_capture_0.i, align 8
  store ptr %fun_capture_0.i, ptr %3, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Test2.#fun_closure_40", ptr %fun_func_0.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %rtob_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @Test2.rtob, ptr %rtob_func_0.i, align 8
  %7 = load i8, ptr %5, align 1
  %switch.i.i = icmp eq i8 %7, 0
  br i1 %switch.i.i, label %switch_branch_Test2.R_0.i.i, label %Test2.main.exit

switch_branch_Test2.R_0.i.i:                      ; preds = %1
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 2, ptr %8, align 1
  br label %Test2.main.exit

Test2.main.exit:                                  ; preds = %1, %switch_branch_Test2.R_0.i.i
  %common.ret.op.i.i = phi ptr [ %8, %switch_branch_Test2.R_0.i.i ], [ %5, %1 ]
  %9 = load ptr, ptr %3, align 8
  %10 = load ptr, ptr %fun_func_0.i, align 8
  %11 = tail call ptr %10(ptr %9, ptr nonnull %common.ret.op.i.i)
  ret i32 0
}
