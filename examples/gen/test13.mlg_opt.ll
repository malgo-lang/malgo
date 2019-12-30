; ModuleID = './examples/gen/test13.mlg.ll'
source_filename = "./examples/test13.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define {}* @f15(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @f15, {}* (i8*, i64)** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = getelementptr i8, i8* %3, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 0)
  %9 = icmp slt i64 %1, 0
  %10 = tail call i8* @GC_malloc(i64 0)
  br i1 %9, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %11 = bitcast i8* %10 to {}*
  br label %end_0

else_0:                                           ; preds = %2
  %12 = tail call i8* @GC_malloc(i64 16)
  %13 = bitcast i8* %12 to i8**
  store i8* %3, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 0)
  %15 = getelementptr i8, i8* %12, i64 8
  %16 = bitcast i8* %15 to {}**
  %17 = bitcast i8* %15 to i8**
  store i8* %10, i8** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 0)
  %19 = load {}*, {}** %16, align 8
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %.0 = phi {}* [ %11, %then_0 ], [ %19, %else_0 ]
  ret {}* %.0
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 0)
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @f15, {}* (i8*, i64)** %3, align 8
  %4 = tail call i8* @GC_malloc(i64 0)
  %5 = getelementptr i8, i8* %2, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %1, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = load {}* (i8*, i64)*, {}* (i8*, i64)** %3, align 8
  %9 = load i8*, i8** %6, align 8
  %10 = tail call {}* %8(i8* %9, i64 5)
  ret i32 0
}
