; ModuleID = './test/tmp/malgo_test/nolift/FuncOverUnboxed.ll'
source_filename = "./test/testcases/malgo/FuncOverUnboxed.mlg"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

define internal ptr @FuncOverUnboxed.f(ptr nocapture nofree readnone %0, i64 %"FuncOverUnboxed.$unboxed_22_0") {
common.ret:
  %switch = icmp ult i64 %"FuncOverUnboxed.$unboxed_22_0", 2
  %spec.select = zext i1 %switch to i64
  %1 = tail call ptr @malgo_int64_t_to_string(i64 %spec.select)
  %2 = tail call ptr @malgo_print_string(ptr %1)
  ret ptr %2
}

declare ptr @malgo_malloc(i64) local_unnamed_addr

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %f_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @FuncOverUnboxed.f, ptr %f_func_0.i, align 8
  %4 = tail call ptr @malgo_int64_t_to_string(i64 noundef 1)
  %5 = tail call ptr @malgo_print_string(ptr %4)
  ret i32 0
}
