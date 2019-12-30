; ModuleID = './examples/gen/test12.mlg.ll'
source_filename = "./examples/test12.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i1 @even37(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i1 (i8*, i64)**
  store i1 (i8*, i64)* @even37, i1 (i8*, i64)** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = getelementptr i8, i8* %5, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %0, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to i1 (i8*, i64)**
  store i1 (i8*, i64)* @odd36, i1 (i8*, i64)** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = getelementptr i8, i8* %11, i64 8
  %15 = bitcast i8* %14 to i8**
  store i8* %0, i8** %15, align 8
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = icmp eq i64 %4, %1
  br i1 %17, label %end_0, label %else_0

else_0:                                           ; preds = %2
  %18 = add i64 %1, -1
  %19 = tail call i1 @odd36(i8* nonnull %0, i64 %18)
  br label %end_0

end_0:                                            ; preds = %2, %else_0
  %.0 = phi i1 [ %19, %else_0 ], [ true, %2 ]
  ret i1 %.0
}

define i1 @odd36(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i1 (i8*, i64)**
  store i1 (i8*, i64)* @even37, i1 (i8*, i64)** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = getelementptr i8, i8* %5, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %0, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to i1 (i8*, i64)**
  store i1 (i8*, i64)* @odd36, i1 (i8*, i64)** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = getelementptr i8, i8* %11, i64 8
  %15 = bitcast i8* %14 to i8**
  store i8* %0, i8** %15, align 8
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = icmp eq i64 %4, %1
  br i1 %17, label %end_0, label %else_0

else_0:                                           ; preds = %2
  %18 = add i64 %1, -1
  %19 = tail call i1 @even37(i8* nonnull %0, i64 %18)
  br label %end_0

end_0:                                            ; preds = %2, %else_0
  %.0 = phi i1 [ %19, %else_0 ], [ false, %2 ]
  ret i1 %.0
}

declare {}* @print_bool(i1) local_unnamed_addr

define {}* @print_bool35(i1) local_unnamed_addr {
  %2 = tail call {}* @print_bool(i1 %0)
  ret {}* %2
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 8)
  %2 = bitcast i8* %1 to i64*
  store i64 0, i64* %2, align 8
  %3 = tail call i8* @GC_malloc(i64 0)
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to i1 (i8*, i64)**
  store i1 (i8*, i64)* @even37, i1 (i8*, i64)** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 0)
  %7 = getelementptr i8, i8* %4, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %1, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 0)
  %10 = tail call i8* @GC_malloc(i64 8)
  %11 = bitcast i8* %10 to i64*
  store i64 0, i64* %11, align 8
  %12 = tail call i8* @GC_malloc(i64 0)
  %13 = tail call i8* @GC_malloc(i64 16)
  %14 = bitcast i8* %13 to i1 (i8*, i64)**
  store i1 (i8*, i64)* @odd36, i1 (i8*, i64)** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 0)
  %16 = getelementptr i8, i8* %13, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %10, i8** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 0)
  %19 = load i1 (i8*, i64)*, i1 (i8*, i64)** %14, align 8
  %20 = load i8*, i8** %17, align 8
  %21 = tail call i1 %19(i8* %20, i64 34)
  %22 = tail call {}* @print_bool(i1 %21)
  ret i32 0
}
