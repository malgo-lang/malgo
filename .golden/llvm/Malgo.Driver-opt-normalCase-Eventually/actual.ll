; ModuleID = '.malgo-work/test/testcases/malgo/Eventually.ll'
source_filename = "test/testcases/malgo/Eventually.mlg"

@_M10eventually47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External = local_unnamed_addr global ptr undef
@_M4comp47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External = local_unnamed_addr global ptr undef
@str4216 = unnamed_addr constant [5 x i8] c"bind\00"
@str4217 = unnamed_addr constant [7 x i8] c"return\00"
@str4219 = unnamed_addr constant [8 x i8] c"not yet\00"
@str4226 = unnamed_addr constant [2 x i8] c"1\00"
@str4229 = unnamed_addr constant [2 x i8] c"2\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @_M36let_x242c2_x24fa8_x5Fclosure_x24105847test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"d$108$fd8_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"d$108$fd8_0")
  ret ptr %6
}

define internal ptr @_M29fun_x2417f_x5Fclosure_x24105947test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Done_0.i, label %switch_branch_NotYetDone_0.i

switch_branch_Done_0.i:                           ; preds = %2
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
  br label %_M25raw_x5Ffun_x2417f_x24100747test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

switch_branch_NotYetDone_0.i:                     ; preds = %2
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr @str4219, ptr %13, align 8
  %14 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4219)
  br label %_M25raw_x5Ffun_x2417f_x24100747test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

_M25raw_x5Ffun_x2417f_x24100747test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit: ; preds = %switch_branch_Done_0.i, %switch_branch_NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %11, %switch_branch_Done_0.i ], [ %14, %switch_branch_NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @_M36let_x242c2_x24fa8_x5Fclosure_x24106047test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"d$108$fd8_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"d$108$fd8_0")
  ret ptr %6
}

define internal ptr @_M29fun_x2417f_x5Fclosure_x24106147test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Done_0.i, label %switch_branch_NotYetDone_0.i

switch_branch_Done_0.i:                           ; preds = %2
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
  br label %_M25raw_x5Ffun_x2417f_x24101047test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

switch_branch_NotYetDone_0.i:                     ; preds = %2
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr @str4219, ptr %13, align 8
  %14 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4219)
  br label %_M25raw_x5Ffun_x2417f_x24101047test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

_M25raw_x5Ffun_x2417f_x24101047test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit: ; preds = %switch_branch_Done_0.i, %switch_branch_NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %11, %switch_branch_Done_0.i ], [ %14, %switch_branch_NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @_M36let_x242c2_x24fa8_x5Fclosure_x24106447test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"d$108$fd8_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"d$108$fd8_0")
  ret ptr %6
}

define internal ptr @_M29fun_x2417f_x5Fclosure_x24106547test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Done_0.i, label %switch_branch_NotYetDone_0.i

switch_branch_Done_0.i:                           ; preds = %2
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
  br label %_M25raw_x5Ffun_x2417f_x24101347test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

switch_branch_NotYetDone_0.i:                     ; preds = %2
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr @str4219, ptr %13, align 8
  %14 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4219)
  br label %_M25raw_x5Ffun_x2417f_x24101347test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

_M25raw_x5Ffun_x2417f_x24101347test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit: ; preds = %switch_branch_Done_0.i, %switch_branch_NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %11, %switch_branch_Done_0.i ], [ %14, %switch_branch_NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @_M36let_x242c2_x24fa8_x5Fclosure_x24106647test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"d$10c$fdb_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"d$10c$fdb_0")
  ret ptr %6
}

define internal ptr @_M29fun_x2417f_x5Fclosure_x24106747test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Done_0.i, label %switch_branch_NotYetDone_0.i

switch_branch_Done_0.i:                           ; preds = %2
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
  br label %_M25raw_x5Ffun_x2417f_x24101447test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

switch_branch_NotYetDone_0.i:                     ; preds = %2
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr @str4219, ptr %13, align 8
  %14 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4219)
  br label %_M25raw_x5Ffun_x2417f_x24101447test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

_M25raw_x5Ffun_x2417f_x24101447test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit: ; preds = %switch_branch_Done_0.i, %switch_branch_NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %11, %switch_branch_Done_0.i ], [ %14, %switch_branch_NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @_M36let_x242c2_x24fa8_x5Fclosure_x24106847test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"d$108$fd8_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"d$108$fd8_0")
  ret ptr %6
}

define internal ptr @_M29fun_x2417f_x5Fclosure_x24106947test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Done_0.i, label %switch_branch_NotYetDone_0.i

switch_branch_Done_0.i:                           ; preds = %2
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
  br label %_M25raw_x5Ffun_x2417f_x24102247test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

switch_branch_NotYetDone_0.i:                     ; preds = %2
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr @str4219, ptr %13, align 8
  %14 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4219)
  br label %_M25raw_x5Ffun_x2417f_x24102247test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

_M25raw_x5Ffun_x2417f_x24102247test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit: ; preds = %switch_branch_Done_0.i, %switch_branch_NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %11, %switch_branch_Done_0.i ], [ %14, %switch_branch_NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @_M36let_x242c2_x24fa8_x5Fclosure_x24107047test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"d$108$fd8_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"d$108$fd8_0")
  ret ptr %6
}

define internal ptr @_M29fun_x2417f_x5Fclosure_x24107147test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Done_0.i, label %switch_branch_NotYetDone_0.i

switch_branch_Done_0.i:                           ; preds = %2
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
  br label %_M25raw_x5Ffun_x2417f_x24102b47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

switch_branch_NotYetDone_0.i:                     ; preds = %2
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr @str4219, ptr %13, align 8
  %14 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4219)
  br label %_M25raw_x5Ffun_x2417f_x24102b47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

_M25raw_x5Ffun_x2417f_x24102b47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit: ; preds = %switch_branch_Done_0.i, %switch_branch_NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %11, %switch_branch_Done_0.i ], [ %14, %switch_branch_NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @_M36let_x242c2_x24fa8_x5Fclosure_x24107447test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"d$108$fd8_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"d$108$fd8_0")
  ret ptr %6
}

define internal ptr @_M29fun_x2417f_x5Fclosure_x24107547test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Done_0.i, label %switch_branch_NotYetDone_0.i

switch_branch_Done_0.i:                           ; preds = %2
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
  br label %_M25raw_x5Ffun_x2417f_x24102e47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

switch_branch_NotYetDone_0.i:                     ; preds = %2
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr @str4219, ptr %13, align 8
  %14 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4219)
  br label %_M25raw_x5Ffun_x2417f_x24102e47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

_M25raw_x5Ffun_x2417f_x24102e47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit: ; preds = %switch_branch_Done_0.i, %switch_branch_NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %11, %switch_branch_Done_0.i ], [ %14, %switch_branch_NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @_M36let_x242c2_x24fa8_x5Fclosure_x24107647test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"d$10c$fdb_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"d$10c$fdb_0")
  ret ptr %6
}

define internal ptr @_M29fun_x2417f_x5Fclosure_x24107747test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Done_0.i, label %switch_branch_NotYetDone_0.i

switch_branch_Done_0.i:                           ; preds = %2
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
  br label %_M25raw_x5Ffun_x2417f_x24102f47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

switch_branch_NotYetDone_0.i:                     ; preds = %2
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr @str4219, ptr %13, align 8
  %14 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4219)
  br label %_M25raw_x5Ffun_x2417f_x24102f47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

_M25raw_x5Ffun_x2417f_x24102f47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit: ; preds = %switch_branch_Done_0.i, %switch_branch_NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %11, %switch_branch_Done_0.i ], [ %14, %switch_branch_NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

define internal ptr @_M29let_x24131_x5Fclosure_x24107a47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %"done$110_0" = load ptr, ptr %0, align 8
  %3 = load i8, ptr %"done$110_0", align 8
  %switch.i = icmp eq i8 %3, 0
  %4 = getelementptr { i8, { ptr } }, ptr %"done$110_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  br i1 %switch.i, label %switch_branch_Done_0.i, label %switch_branch_NotYetDone_0.i

switch_branch_Done_0.i:                           ; preds = %2
  %6 = load ptr, ptr %1, align 8
  %7 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, ptr %5)
  br label %_M25raw_x5Flet_x24131_x24100647test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

switch_branch_NotYetDone_0.i:                     ; preds = %2
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"fun$11d$126$f86_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %5, ptr %"fun$11d$126$f86_capture_0.i", align 8
  %"k$8$111_0.i" = getelementptr { ptr, ptr }, ptr %"fun$11d$126$f86_capture_0.i", i64 0, i32 1
  store ptr %1, ptr %"k$8$111_0.i", align 8
  store ptr %"fun$11d$126$f86_capture_0.i", ptr %10, align 8
  %"fun$11d$126$f86_func_0.i" = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @_M43fun_x2411d_x24126_x24f86_x5Fclosure_x24107c47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, ptr %"fun$11d$126$f86_func_0.i", align 8
  %11 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr %10, ptr %12, align 8
  br label %_M25raw_x5Flet_x24131_x24100647test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

_M25raw_x5Flet_x24131_x24100647test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit: ; preds = %switch_branch_Done_0.i, %switch_branch_NotYetDone_0.i
  %common.ret.op.i = phi ptr [ %9, %switch_branch_Done_0.i ], [ %11, %switch_branch_NotYetDone_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @_M14eventuallyBind47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External(ptr nocapture nofree readnone %0, ptr nofree %_M11done_x2411047test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal_0) {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$131_capture_0" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %_M11done_x2411047test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal_0, ptr %"let$131_capture_0", align 8
  store ptr %"let$131_capture_0", ptr %2, align 8
  %"let$131_func_0" = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @_M29let_x24131_x5Fclosure_x24107a47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, ptr %"let$131_func_0", align 8
  ret ptr %2
}

define internal noundef ptr @_M4Done47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External(ptr nocapture nofree readnone %0, ptr nofree %_M8p_x2410247test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal_0) {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %_M8p_x2410247test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal_0, ptr %3, align 8
  ret ptr %2
}

define internal ptr @_M43fun_x2411d_x24126_x24f86_x5Fclosure_x24107c47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readnone %1) {
  %"p$114$125$f85_0" = load ptr, ptr %0, align 8
  %"k$8$111_addr_0" = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %"k$8$111_0" = load ptr, ptr %"k$8$111_addr_0", align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = load ptr, ptr %"p$114$125$f85_0", align 8
  %5 = getelementptr { ptr, ptr }, ptr %"p$114$125$f85_0", i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %3)
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$131_capture_0.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %7, ptr %"let$131_capture_0.i.i", align 8
  store ptr %"let$131_capture_0.i.i", ptr %8, align 8
  %"let$131_func_0.i.i" = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @_M29let_x24131_x5Fclosure_x24107a47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, ptr %"let$131_func_0.i.i", align 8
  %"done$110_0.i.i" = load ptr, ptr %"let$131_capture_0.i.i", align 8
  %9 = load i8, ptr %"done$110_0.i.i", align 8
  %switch.i.i.i = icmp eq i8 %9, 0
  %10 = getelementptr { i8, { ptr } }, ptr %"done$110_0.i.i", i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  br i1 %switch.i.i.i, label %switch_branch_Done_0.i.i.i, label %switch_branch_NotYetDone_0.i.i.i

switch_branch_Done_0.i.i.i:                       ; preds = %2
  %12 = load ptr, ptr %"k$8$111_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %"k$8$111_0", i64 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = tail call ptr %14(ptr %12, ptr %11)
  br label %_M39raw_x5Ffun_x2411d_x24126_x24f86_x24100547test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

switch_branch_NotYetDone_0.i.i.i:                 ; preds = %2
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"fun$11d$126$f86_capture_0.i.i.i" = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %11, ptr %"fun$11d$126$f86_capture_0.i.i.i", align 8
  %"k$8$111_0.i.i.i" = getelementptr { ptr, ptr }, ptr %"fun$11d$126$f86_capture_0.i.i.i", i64 0, i32 1
  store ptr %"k$8$111_0", ptr %"k$8$111_0.i.i.i", align 8
  store ptr %"fun$11d$126$f86_capture_0.i.i.i", ptr %16, align 8
  %"fun$11d$126$f86_func_0.i.i.i" = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @_M43fun_x2411d_x24126_x24f86_x5Fclosure_x24107c47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, ptr %"fun$11d$126$f86_func_0.i.i.i", align 8
  %17 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %17, align 1
  %18 = getelementptr { i8, { ptr } }, ptr %17, i64 0, i32 1, i32 0
  store ptr %16, ptr %18, align 8
  br label %_M39raw_x5Ffun_x2411d_x24126_x24f86_x24100547test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit

_M39raw_x5Ffun_x2411d_x24126_x24f86_x24100547test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Temporal.exit: ; preds = %switch_branch_Done_0.i.i.i, %switch_branch_NotYetDone_0.i.i.i
  %common.ret.op.i.i.i = phi ptr [ %15, %switch_branch_Done_0.i.i.i ], [ %17, %switch_branch_NotYetDone_0.i.i.i ]
  ret ptr %common.ret.op.i.i.i
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_hash_table_new()
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %eventuallyBind_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @_M14eventuallyBind47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External, ptr %eventuallyBind_func_0.i, align 8
  tail call void @malgo_hash_table_insert(ptr %2, ptr noundef nonnull @str4216, ptr noundef nonnull %3)
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %Done_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @_M4Done47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External, ptr %Done_func_0.i, align 8
  tail call void @malgo_hash_table_insert(ptr %2, ptr noundef nonnull @str4217, ptr noundef nonnull %4)
  store ptr %2, ptr @_M10eventually47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External, align 8
  %5 = tail call ptr @malgo_hash_table_get(ptr %2, ptr noundef nonnull @str4216)
  %6 = tail call ptr @malgo_hash_table_get(ptr %2, ptr noundef nonnull @str4217)
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"fun$140_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %"fun$140_capture_0.i", ptr %7, align 8
  %"fun$140_func_0.i" = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @_M29fun_x24140_x5Fclosure_x24108147test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, ptr %"fun$140_func_0.i", align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %8, i64 0, i32 1, i32 0
  store ptr %7, ptr %9, align 8
  %10 = load ptr, ptr %5, align 8
  %11 = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call ptr %12(ptr %10, ptr nonnull %8)
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"fun$15f_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %"fun$15f_capture_0.i", ptr %14, align 8
  %"fun$15f_func_0.i" = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @_M29fun_x2415f_x5Fclosure_x24108347test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, ptr %"fun$15f_func_0.i", align 8
  %15 = load ptr, ptr %13, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = tail call ptr %17(ptr %15, ptr nonnull %14)
  store ptr %18, ptr @_M4comp47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External, align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %19, align 1
  %20 = load ptr, ptr @_M4comp47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External, align 8
  %21 = load i8, ptr %20, align 8
  %switch.i = icmp eq i8 %21, 0
  %22 = getelementptr { i8, { ptr } }, ptr %20, i64 0, i32 1
  %23 = load ptr, ptr %22, align 8
  br i1 %switch.i, label %switch_branch_Done_0.i, label %switch_branch_NotYetDone_7.i

switch_branch_Done_0.i:                           ; preds = %1
  %24 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %24, align 1
  %25 = getelementptr { i8, { ptr } }, ptr %24, i64 0, i32 1, i32 0
  store ptr %23, ptr %25, align 8
  %26 = load ptr, ptr @_M4comp47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External, align 8
  %27 = load i8, ptr %26, align 8
  %switch1.i = icmp eq i8 %27, 0
  %28 = getelementptr { i8, { ptr } }, ptr %26, i64 0, i32 1
  %29 = load ptr, ptr %28, align 8
  br i1 %switch1.i, label %switch_branch_Done_1.i, label %switch_branch_NotYetDone_3.i

switch_branch_Done_1.i:                           ; preds = %switch_branch_Done_0.i
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
  %"let$2c2$fa8_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %34, ptr %"let$2c2$fa8_capture_0.i", align 8
  store ptr %"let$2c2$fa8_capture_0.i", ptr %36, align 8
  %"let$2c2$fa8_func_0.i" = getelementptr { ptr, ptr }, ptr %36, i64 0, i32 1
  store ptr @_M36let_x242c2_x24fa8_x5Fclosure_x24105847test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, ptr %"let$2c2$fa8_func_0.i", align 8
  br label %_M4main47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External.exit

switch_branch_NotYetDone_3.i:                     ; preds = %switch_branch_Done_0.i
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
  br i1 %switch2.i, label %switch_branch_Done_5.i, label %switch_branch_NotYetDone_5.i

switch_branch_Done_5.i:                           ; preds = %switch_branch_NotYetDone_3.i
  %45 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %45, align 1
  %46 = getelementptr { i8, { ptr } }, ptr %45, i64 0, i32 1, i32 0
  store ptr %44, ptr %46, align 8
  %47 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %47, align 1
  %48 = getelementptr { i8, { ptr } }, ptr %47, i64 0, i32 1, i32 0
  store ptr %44, ptr %48, align 8
  %49 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$2c2$fa8_capture_8.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %47, ptr %"let$2c2$fa8_capture_8.i", align 8
  store ptr %"let$2c2$fa8_capture_8.i", ptr %49, align 8
  %"let$2c2$fa8_func_4.i" = getelementptr { ptr, ptr }, ptr %49, i64 0, i32 1
  store ptr @_M36let_x242c2_x24fa8_x5Fclosure_x24106047test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, ptr %"let$2c2$fa8_func_4.i", align 8
  br label %_M4main47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External.exit

switch_branch_NotYetDone_5.i:                     ; preds = %switch_branch_NotYetDone_3.i
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
  br i1 %switch3.i, label %switch_branch_Done_7.i, label %switch_branch_NotYetDone_6.i

switch_branch_Done_7.i:                           ; preds = %switch_branch_NotYetDone_5.i
  %58 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %58, align 1
  %59 = getelementptr { i8, { ptr } }, ptr %58, i64 0, i32 1, i32 0
  store ptr %57, ptr %59, align 8
  %60 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$2c2$fa8_capture_12.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %58, ptr %"let$2c2$fa8_capture_12.i", align 8
  store ptr %"let$2c2$fa8_capture_12.i", ptr %60, align 8
  %"let$2c2$fa8_func_6.i" = getelementptr { ptr, ptr }, ptr %60, i64 0, i32 1
  store ptr @_M36let_x242c2_x24fa8_x5Fclosure_x24106447test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, ptr %"let$2c2$fa8_func_6.i", align 8
  br label %_M4main47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External.exit

switch_branch_NotYetDone_6.i:                     ; preds = %switch_branch_NotYetDone_5.i
  %61 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %61, align 1
  %62 = load ptr, ptr %57, align 8
  %63 = getelementptr { ptr, ptr }, ptr %57, i64 0, i32 1
  %64 = load ptr, ptr %63, align 8
  %65 = tail call ptr %64(ptr %62, ptr nonnull %61)
  %66 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$2c2$fa8_capture_14.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %65, ptr %"let$2c2$fa8_capture_14.i", align 8
  store ptr %"let$2c2$fa8_capture_14.i", ptr %66, align 8
  %"let$2c2$fa8_func_7.i" = getelementptr { ptr, ptr }, ptr %66, i64 0, i32 1
  store ptr @_M36let_x242c2_x24fa8_x5Fclosure_x24106647test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, ptr %"let$2c2$fa8_func_7.i", align 8
  br label %_M4main47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External.exit

switch_branch_NotYetDone_7.i:                     ; preds = %1
  %67 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %67, align 1
  %68 = load ptr, ptr %23, align 8
  %69 = getelementptr { ptr, ptr }, ptr %23, i64 0, i32 1
  %70 = load ptr, ptr %69, align 8
  %71 = tail call ptr %70(ptr %68, ptr nonnull %67)
  %72 = load ptr, ptr @_M4comp47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External, align 8
  %73 = load i8, ptr %72, align 8
  %switch4.i = icmp eq i8 %73, 0
  %74 = getelementptr { i8, { ptr } }, ptr %72, i64 0, i32 1
  %75 = load ptr, ptr %74, align 8
  br i1 %switch4.i, label %switch_branch_Done_8.i, label %switch_branch_NotYetDone_11.i

switch_branch_Done_8.i:                           ; preds = %switch_branch_NotYetDone_7.i
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
  %"let$2c2$fa8_capture_16.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %80, ptr %"let$2c2$fa8_capture_16.i", align 8
  store ptr %"let$2c2$fa8_capture_16.i", ptr %82, align 8
  %"let$2c2$fa8_func_8.i" = getelementptr { ptr, ptr }, ptr %82, i64 0, i32 1
  store ptr @_M36let_x242c2_x24fa8_x5Fclosure_x24106847test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, ptr %"let$2c2$fa8_func_8.i", align 8
  br label %_M4main47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External.exit

switch_branch_NotYetDone_11.i:                    ; preds = %switch_branch_NotYetDone_7.i
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
  br i1 %switch5.i, label %switch_branch_Done_12.i, label %switch_branch_NotYetDone_13.i

switch_branch_Done_12.i:                          ; preds = %switch_branch_NotYetDone_11.i
  %91 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %91, align 1
  %92 = getelementptr { i8, { ptr } }, ptr %91, i64 0, i32 1, i32 0
  store ptr %90, ptr %92, align 8
  %93 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %93, align 1
  %94 = getelementptr { i8, { ptr } }, ptr %93, i64 0, i32 1, i32 0
  store ptr %90, ptr %94, align 8
  %95 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$2c2$fa8_capture_24.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %93, ptr %"let$2c2$fa8_capture_24.i", align 8
  store ptr %"let$2c2$fa8_capture_24.i", ptr %95, align 8
  %"let$2c2$fa8_func_12.i" = getelementptr { ptr, ptr }, ptr %95, i64 0, i32 1
  store ptr @_M36let_x242c2_x24fa8_x5Fclosure_x24107047test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, ptr %"let$2c2$fa8_func_12.i", align 8
  br label %_M4main47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External.exit

switch_branch_NotYetDone_13.i:                    ; preds = %switch_branch_NotYetDone_11.i
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
  br i1 %switch6.i, label %switch_branch_Done_14.i, label %switch_branch_NotYetDone_14.i

switch_branch_Done_14.i:                          ; preds = %switch_branch_NotYetDone_13.i
  %104 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %104, align 1
  %105 = getelementptr { i8, { ptr } }, ptr %104, i64 0, i32 1, i32 0
  store ptr %103, ptr %105, align 8
  %106 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$2c2$fa8_capture_28.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %104, ptr %"let$2c2$fa8_capture_28.i", align 8
  store ptr %"let$2c2$fa8_capture_28.i", ptr %106, align 8
  %"let$2c2$fa8_func_14.i" = getelementptr { ptr, ptr }, ptr %106, i64 0, i32 1
  store ptr @_M36let_x242c2_x24fa8_x5Fclosure_x24107447test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, ptr %"let$2c2$fa8_func_14.i", align 8
  br label %_M4main47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External.exit

switch_branch_NotYetDone_14.i:                    ; preds = %switch_branch_NotYetDone_13.i
  %107 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %107, align 1
  %108 = load ptr, ptr %103, align 8
  %109 = getelementptr { ptr, ptr }, ptr %103, i64 0, i32 1
  %110 = load ptr, ptr %109, align 8
  %111 = tail call ptr %110(ptr %108, ptr nonnull %107)
  %112 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$2c2$fa8_capture_30.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %111, ptr %"let$2c2$fa8_capture_30.i", align 8
  store ptr %"let$2c2$fa8_capture_30.i", ptr %112, align 8
  %"let$2c2$fa8_func_15.i" = getelementptr { ptr, ptr }, ptr %112, i64 0, i32 1
  store ptr @_M36let_x242c2_x24fa8_x5Fclosure_x24107647test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, ptr %"let$2c2$fa8_func_15.i", align 8
  br label %_M4main47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External.exit

_M4main47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External.exit: ; preds = %switch_branch_Done_1.i, %switch_branch_Done_5.i, %switch_branch_Done_7.i, %switch_branch_NotYetDone_6.i, %switch_branch_Done_8.i, %switch_branch_Done_12.i, %switch_branch_Done_14.i, %switch_branch_NotYetDone_14.i
  %_M29fun_x2417f_x5Fclosure_x24107747test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal.sink.i = phi ptr [ @_M29fun_x2417f_x5Fclosure_x24107747test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, %switch_branch_NotYetDone_14.i ], [ @_M29fun_x2417f_x5Fclosure_x24107547test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, %switch_branch_Done_14.i ], [ @_M29fun_x2417f_x5Fclosure_x24107147test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, %switch_branch_Done_12.i ], [ @_M29fun_x2417f_x5Fclosure_x24106947test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, %switch_branch_Done_8.i ], [ @_M29fun_x2417f_x5Fclosure_x24106747test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, %switch_branch_NotYetDone_6.i ], [ @_M29fun_x2417f_x5Fclosure_x24106547test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, %switch_branch_Done_7.i ], [ @_M29fun_x2417f_x5Fclosure_x24106147test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, %switch_branch_Done_5.i ], [ @_M29fun_x2417f_x5Fclosure_x24105947test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, %switch_branch_Done_1.i ]
  %.sink9.i = phi ptr [ %112, %switch_branch_NotYetDone_14.i ], [ %106, %switch_branch_Done_14.i ], [ %95, %switch_branch_Done_12.i ], [ %82, %switch_branch_Done_8.i ], [ %66, %switch_branch_NotYetDone_6.i ], [ %60, %switch_branch_Done_7.i ], [ %49, %switch_branch_Done_5.i ], [ %36, %switch_branch_Done_1.i ]
  %"let$2c2$fa8_func_15.sink.i" = phi ptr [ %"let$2c2$fa8_func_15.i", %switch_branch_NotYetDone_14.i ], [ %"let$2c2$fa8_func_14.i", %switch_branch_Done_14.i ], [ %"let$2c2$fa8_func_12.i", %switch_branch_Done_12.i ], [ %"let$2c2$fa8_func_8.i", %switch_branch_Done_8.i ], [ %"let$2c2$fa8_func_7.i", %switch_branch_NotYetDone_6.i ], [ %"let$2c2$fa8_func_6.i", %switch_branch_Done_7.i ], [ %"let$2c2$fa8_func_4.i", %switch_branch_Done_5.i ], [ %"let$2c2$fa8_func_0.i", %switch_branch_Done_1.i ]
  %113 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"fun$17f_capture_30.i" = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %"fun$17f_capture_30.i", ptr %113, align 8
  %"fun$17f_func_15.i" = getelementptr { ptr, ptr }, ptr %113, i64 0, i32 1
  store ptr %_M29fun_x2417f_x5Fclosure_x24107747test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal.sink.i, ptr %"fun$17f_func_15.i", align 8
  %114 = load ptr, ptr %.sink9.i, align 8
  %115 = load ptr, ptr %"let$2c2$fa8_func_15.sink.i", align 8
  %116 = tail call ptr %115(ptr %114, ptr nonnull %113)
  ret i32 0
}

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr

define internal noundef ptr @_M29fun_x24140_x5Fclosure_x24108147test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
"switch_branch_String#_0":
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr @str4226, ptr %3, align 8
  %4 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4226)
  %5 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %4, ptr %6, align 8
  ret ptr %5
}

define internal noundef ptr @_M29fun_x24150_x5Fclosure_x24108447test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
"switch_branch_String#_0":
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr @str4229, ptr %3, align 8
  %4 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4229)
  %5 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %4, ptr %6, align 8
  ret ptr %5
}

define internal ptr @_M29fun_x2415c_x5Fclosure_x24108647test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = load ptr, ptr @_M10eventually47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External, align 8
  %4 = tail call ptr @malgo_hash_table_get(ptr %3, ptr noundef nonnull @str4216)
  %5 = tail call ptr @malgo_hash_table_get(ptr %3, ptr noundef nonnull @str4217)
  %6 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { i32 } }, ptr %6, i64 0, i32 1, i32 0
  store i32 3, ptr %7, align 4
  %8 = load ptr, ptr %5, align 8
  %9 = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, ptr nonnull %6)
  ret ptr %11
}

define internal ptr @_M29fun_x2415f_x5Fclosure_x24108347test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = load ptr, ptr @_M10eventually47test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8External, align 8
  %4 = tail call ptr @malgo_hash_table_get(ptr %3, ptr noundef nonnull @str4216)
  %5 = tail call ptr @malgo_hash_table_get(ptr %3, ptr noundef nonnull @str4217)
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"fun$150_capture_0" = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %"fun$150_capture_0", ptr %6, align 8
  %"fun$150_func_0" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @_M29fun_x24150_x5Fclosure_x24108447test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, ptr %"fun$150_func_0", align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %6, ptr %8, align 8
  %9 = load ptr, ptr %4, align 8
  %10 = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = tail call ptr %11(ptr %9, ptr nonnull %7)
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"fun$15c_capture_0" = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %"fun$15c_capture_0", ptr %13, align 8
  %"fun$15c_func_0" = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @_M29fun_x2415c_x5Fclosure_x24108647test_x2Ftestcases_x2Fmalgo_x2FEventually_x2Emlg8Internal, ptr %"fun$15c_func_0", align 8
  %14 = load ptr, ptr %12, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr %16(ptr %14, ptr nonnull %13)
  ret ptr %17
}
