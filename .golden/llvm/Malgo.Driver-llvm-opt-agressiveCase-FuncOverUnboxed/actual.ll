; ModuleID = '/workspaces/malgo/.malgo-work/FuncOverUnboxed.ll'
source_filename = "./test/testcases/malgo/FuncOverUnboxed.mlg"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_int64_t_to_string(i64 noundef 1)
  %4 = tail call ptr @malgo_print_string(ptr %3)
  ret i32 0
}
