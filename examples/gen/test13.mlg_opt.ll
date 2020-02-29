; ModuleID = './examples/gen/test13.mlg.ll'
source_filename = "./examples/test13.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define {}* @f0(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @f0, {}* (i8*, i64)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = icmp slt i64 %1, 0
  %8 = tail call i8* @GC_malloc(i64 0)
  %9 = bitcast i8* %8 to {}*
  br i1 %7, label %end_0, label %else_0

else_0:                                           ; preds = %2
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to i8**
  store i8* %3, i8** %11, align 8
  %12 = getelementptr i8, i8* %10, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %8, i8** %13, align 8
  br label %end_0

end_0:                                            ; preds = %2, %else_0
  ret {}* %9
}

declare void @GC_init() local_unnamed_addr

define i32 @main() local_unnamed_addr {
  tail call void @GC_init()
  %1 = tail call i8* @GC_malloc(i64 0)
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @f0, {}* (i8*, i64)** %3, align 8
  %4 = getelementptr i8, i8* %2, i64 8
  %5 = bitcast i8* %4 to i8**
  store i8* %1, i8** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 16)
  %7 = bitcast i8* %6 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @f0, {}* (i8*, i64)** %7, align 8
  %8 = getelementptr i8, i8* %6, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %1, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to i8**
  store i8* %6, i8** %12, align 8
  %13 = getelementptr i8, i8* %11, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %10, i8** %14, align 8
  ret i32 0
}
