; ModuleID = './examples/gen/test14.mlg.ll'
source_filename = "./examples/test14.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define {}* @void0(i1) local_unnamed_addr {
  %2 = tail call i8* @GC_malloc(i64 0)
  %3 = bitcast i8* %2 to {}*
  ret {}* %3
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 0)
  %2 = tail call i8* @GC_malloc(i64 0)
  %3 = tail call i8* @GC_malloc(i64 0)
  %4 = tail call i8* @GC_malloc(i64 0)
  ret i32 0
}
