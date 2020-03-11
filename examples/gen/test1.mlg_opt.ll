; ModuleID = './examples/gen/test1.mlg.ll'
source_filename = "./examples/test1.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

; Function Attrs: norecurse nounwind readnone
define i64 @answer0() local_unnamed_addr #0 {
  ret i64 42
}

declare void @GC_init() local_unnamed_addr

declare i8* @GC_malloc(i64) local_unnamed_addr

define i32 @main() local_unnamed_addr {
  tail call void @GC_init()
  %1 = tail call i8* @GC_malloc(i64 0)
  ret i32 0
}

attributes #0 = { norecurse nounwind readnone }
