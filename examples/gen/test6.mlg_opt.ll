; ModuleID = './examples/gen/test6.mlg.ll'
source_filename = "./examples/test6.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int.13(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

; Function Attrs: norecurse nounwind readnone
define i64 @id.12(i64 returned) local_unnamed_addr #0 {
  ret i64 %0
}

define i32 @main() local_unnamed_addr {
  %1 = tail call {}* @print_int(i64 126)
  ret i32 0
}

attributes #0 = { norecurse nounwind readnone }
