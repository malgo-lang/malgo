; ModuleID = './examples/gen/test.mlg.ll'
source_filename = "./examples/test.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i32 @main4() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 0)
  ret i32 0
}

; Function Attrs: norecurse nounwind readnone
define i64 @answer5() local_unnamed_addr #0 {
  ret i64 42
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 0)
  ret i32 0
}

attributes #0 = { norecurse nounwind readnone }
