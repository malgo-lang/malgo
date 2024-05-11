; ModuleID = '.malgo-work/test/testcases/malgo/Primitive.ll'
source_filename = "test/testcases/malgo/Primitive.mlg"

declare void @GC_init() local_unnamed_addr

declare i64 @malgo_add_int64_t(i64, i64) local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call i64 @malgo_add_int64_t(i64 noundef 40, i64 noundef 2)
  %4 = tail call ptr @malgo_int64_t_to_string(i64 %3)
  %5 = tail call ptr @malgo_print_string(ptr %4)
  ret i32 0
}
