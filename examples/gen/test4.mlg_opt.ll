; ModuleID = './examples/gen/test4.mlg.ll'
source_filename = "./examples/test4.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

@0 = internal global [6 x i8] c"hello\00"

; Function Attrs: norecurse nounwind readnone
define i8* @f.7(i64, i8) local_unnamed_addr #0 {
  ret i8* getelementptr inbounds ([6 x i8], [6 x i8]* @0, i64 0, i64 0)
}

; Function Attrs: norecurse nounwind readnone
define i32 @main() local_unnamed_addr #0 {
  ret i32 0
}

attributes #0 = { norecurse nounwind readnone }
