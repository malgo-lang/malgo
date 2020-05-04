; ModuleID = './examples/gen/hello.mlg.ll'
source_filename = "./examples/hello.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

@"$globle_str_8" = unnamed_addr constant [13 x i8] c"Hello, world\00"

declare {}* @println(i8*) local_unnamed_addr

define {}* @println0(i8*) local_unnamed_addr {
  %2 = tail call {}* @println(i8* %0)
  ret {}* %2
}

declare void @GC_init() local_unnamed_addr

define i32 @main() local_unnamed_addr {
  tail call void @GC_init()
  %1 = tail call {}* @println(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @"$globle_str_8", i64 0, i64 0))
  ret i32 0
}
