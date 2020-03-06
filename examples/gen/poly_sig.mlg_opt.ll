; ModuleID = './examples/gen/poly_sig.mlg.ll'
source_filename = "./examples/poly_sig.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

; Function Attrs: norecurse nounwind readnone
define i8* @id0(i8* readnone returned) local_unnamed_addr #0 {
  ret i8* %0
}

; Function Attrs: norecurse nounwind readnone
define i8* @const3(i8* readnone returned, i8* nocapture readnone) local_unnamed_addr #0 {
  ret i8* %0
}

declare void @GC_init() local_unnamed_addr

define i32 @main() local_unnamed_addr {
  tail call void @GC_init()
  ret i32 0
}

attributes #0 = { norecurse nounwind readnone }
