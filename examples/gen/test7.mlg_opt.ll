; ModuleID = './examples/gen/test7.mlg.ll'
source_filename = "./examples/test7.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i64 @f.11(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  br label %tailrecurse

tailrecurse:                                      ; preds = %tailrecurse, %2
  %.tr2 = phi i64 [ %1, %2 ], [ %10, %tailrecurse ]
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @f.11, i64 (i8*, i64)** %6, align 8
  %7 = getelementptr i8, i8* %5, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %0, i8** %8, align 8
  %9 = icmp slt i64 %.tr2, 2
  %10 = add i64 %.tr2, -1
  br i1 %9, label %endif_0, label %tailrecurse

endif_0:                                          ; preds = %tailrecurse
  ret i64 %4
}

define i32 @main() local_unnamed_addr {
tailrecurse.i:
  %0 = tail call i8* @GC_malloc(i64 8)
  %1 = bitcast i8* %0 to i64*
  store i64 42, i64* %1, align 8
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @f.11, i64 (i8*, i64)** %3, align 8
  %4 = getelementptr i8, i8* %2, i64 8
  %5 = bitcast i8* %4 to i8**
  store i8* %0, i8** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 16)
  %7 = bitcast i8* %6 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @f.11, i64 (i8*, i64)** %7, align 8
  %8 = getelementptr i8, i8* %6, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %0, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @f.11, i64 (i8*, i64)** %11, align 8
  %12 = getelementptr i8, i8* %10, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %0, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @f.11, i64 (i8*, i64)** %15, align 8
  %16 = getelementptr i8, i8* %14, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %0, i8** %17, align 8
  ret i32 0
}
