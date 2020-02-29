; ModuleID = './examples/gen/test11.mlg.ll'
source_filename = "./examples/test11.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare void @GC_init() local_unnamed_addr

define i32 @main() local_unnamed_addr {
then_0:
  tail call void @GC_init()
  ret i32 0
}
