; ModuleID = './test/tmp/malgo_test/normal/HelloBoxed.ll'
source_filename = "./test/testcases/malgo/HelloBoxed.mlg"

@str47 = unnamed_addr constant [13 x i8] c"Hello, world\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr @str47, ptr %4, align 8
  %5 = tail call ptr @malgo_print_string(ptr noundef nonnull @str47)
  %6 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %6, align 1
  %7 = tail call ptr @malgo_newline(ptr noundef nonnull %6)
  ret i32 0
}
