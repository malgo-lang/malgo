; ModuleID = './examples/gen/test7.mlg.ll'
source_filename = "./examples/test7.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i32 @main18() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 8)
  %2 = bitcast i8* %1 to i64*
  store i64 42, i64* %2, align 8
  %3 = tail call i8* @GC_malloc(i64 0)
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @f19, i64 (i8*, i64)** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 0)
  %7 = getelementptr i8, i8* %4, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %1, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 0)
  %10 = load i64 (i8*, i64)*, i64 (i8*, i64)** %5, align 8
  %11 = load i8*, i8** %8, align 8
  %12 = tail call i64 %10(i8* %11, i64 3)
  ret i32 0
}

define i64 @f19(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  br label %tailrecurse

tailrecurse:                                      ; preds = %tailrecurse, %2
  %.tr2 = phi i64 [ %1, %2 ], [ %12, %tailrecurse ]
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @f19, i64 (i8*, i64)** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = getelementptr i8, i8* %5, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %0, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = icmp slt i64 %.tr2, 2
  %12 = add i64 %.tr2, -1
  br i1 %11, label %end_0, label %tailrecurse

end_0:                                            ; preds = %tailrecurse
  ret i64 %4
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 8)
  %2 = bitcast i8* %1 to i64*
  store i64 42, i64* %2, align 8
  %3 = tail call i8* @GC_malloc(i64 0)
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @f19, i64 (i8*, i64)** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 0)
  %7 = getelementptr i8, i8* %4, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %1, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 0)
  %10 = load i64 (i8*, i64)*, i64 (i8*, i64)** %5, align 8
  %11 = load i8*, i8** %8, align 8
  %12 = tail call i64 %10(i8* %11, i64 3)
  ret i32 0
}
