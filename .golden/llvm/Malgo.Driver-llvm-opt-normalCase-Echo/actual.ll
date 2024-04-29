; ModuleID = '/workspaces/malgo/.malgo-work/Echo.ll'
source_filename = "./test/testcases/malgo/Echo.mlg"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_get_contents(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_get_contents(ptr noundef nonnull %4)
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %6, i64 0, i32 1, i32 0
  store ptr %5, ptr %7, align 8
  %8 = tail call ptr @malgo_print_string(ptr %5)
  ret i32 0
}
