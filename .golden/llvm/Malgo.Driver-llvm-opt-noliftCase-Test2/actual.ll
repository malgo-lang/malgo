; ModuleID = '.malgo-work/test/testcases/malgo/Test2.ll'
source_filename = "test/testcases/malgo/Test2.mlg"

@str48 = unnamed_addr constant [6 x i8] c"WRONG\00"
@str49 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 2, ptr %4, align 1
  %5 = tail call ptr @malgo_print_string(ptr noundef nonnull @str49)
  ret i32 0
}
