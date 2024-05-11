; ModuleID = '.malgo-work/test/testcases/malgo/TestLet.ll'
source_filename = "test/testcases/malgo/TestLet.mlg"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call i32 @malgo_add_int32_t(i32 noundef 1, i32 noundef 2)
  %4 = tail call ptr @malgo_int32_t_to_string(i32 %3)
  %5 = tail call ptr @malgo_print_string(ptr %4)
  %6 = tail call i32 @malgo_add_int32_t(i32 noundef 1, i32 noundef 2)
  %7 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 %6, ptr %8, align 4
  %9 = tail call ptr @malgo_int32_t_to_string(i32 %6)
  %10 = tail call ptr @malgo_print_string(ptr %9)
  ret i32 0
}
