; ModuleID = './examples/gen/array.mlg.ll'
source_filename = "./examples/array.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i32 @main27() local_unnamed_addr {
end_0:
  %0 = tail call i8* @GC_malloc(i64 80)
  call void @llvm.memset.p0i8.i64(i8* align 8 %0, i8 0, i64 80, i1 false)
  %1 = getelementptr i8, i8* %0, i64 16
  %2 = bitcast i8* %1 to i64*
  store i64 42, i64* %2, align 8
  %3 = tail call i8* @GC_malloc(i64 0)
  %4 = load i64, i64* %2, align 8
  %5 = tail call {}* @print_int(i64 0)
  %6 = tail call {}* @newline()
  %7 = tail call {}* @print_int(i64 %4)
  %8 = tail call {}* @newline()
  ret i32 0
}

declare {}* @newline() local_unnamed_addr

define {}* @newline28() local_unnamed_addr {
  %1 = tail call {}* @newline()
  ret {}* %1
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int29(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 80)
  tail call void @llvm.memset.p0i8.i64(i8* align 8 %1, i8 0, i64 80, i1 false)
  %2 = getelementptr i8, i8* %1, i64 16
  %3 = bitcast i8* %2 to i64*
  store i64 42, i64* %3, align 8
  %4 = tail call i8* @GC_malloc(i64 0)
  %5 = load i64, i64* %3, align 8
  %6 = tail call {}* @print_int(i64 0)
  %7 = tail call {}* @newline()
  %8 = tail call {}* @print_int(i64 %5)
  %9 = tail call {}* @newline()
  ret i32 0
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1) #0

attributes #0 = { argmemonly nounwind }
