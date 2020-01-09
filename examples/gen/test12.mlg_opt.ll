; ModuleID = './examples/gen/test12.mlg.ll'
source_filename = "./examples/test12.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i1 @even1(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i1 (i8*, i64)**
  store i1 (i8*, i64)* @even1, i1 (i8*, i64)** %6, align 8
  %7 = getelementptr i8, i8* %5, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %0, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to i1 (i8*, i64)**
  store i1 (i8*, i64)* @odd2, i1 (i8*, i64)** %10, align 8
  %11 = getelementptr i8, i8* %9, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %0, i8** %12, align 8
  %13 = icmp eq i64 %4, %1
  br i1 %13, label %end_0, label %else_0

tailrecurse:                                      ; preds = %else_0
  %14 = load i64, i64* %3, align 8
  %15 = tail call i8* @GC_malloc(i64 16)
  %16 = bitcast i8* %15 to i1 (i8*, i64)**
  store i1 (i8*, i64)* @even1, i1 (i8*, i64)** %16, align 8
  %17 = getelementptr i8, i8* %15, i64 8
  %18 = bitcast i8* %17 to i8**
  store i8* %0, i8** %18, align 8
  %19 = tail call i8* @GC_malloc(i64 16)
  %20 = bitcast i8* %19 to i1 (i8*, i64)**
  store i1 (i8*, i64)* @odd2, i1 (i8*, i64)** %20, align 8
  %21 = getelementptr i8, i8* %19, i64 8
  %22 = bitcast i8* %21 to i8**
  store i8* %0, i8** %22, align 8
  %23 = icmp eq i64 %14, %35
  br i1 %23, label %end_0, label %else_0

else_0:                                           ; preds = %2, %tailrecurse
  %.tr23 = phi i64 [ %35, %tailrecurse ], [ %1, %2 ]
  %24 = add i64 %.tr23, -1
  %25 = load i64, i64* %3, align 8
  %26 = tail call i8* @GC_malloc(i64 16)
  %27 = bitcast i8* %26 to i1 (i8*, i64)**
  store i1 (i8*, i64)* @even1, i1 (i8*, i64)** %27, align 8
  %28 = getelementptr i8, i8* %26, i64 8
  %29 = bitcast i8* %28 to i8**
  store i8* %0, i8** %29, align 8
  %30 = tail call i8* @GC_malloc(i64 16)
  %31 = bitcast i8* %30 to i1 (i8*, i64)**
  store i1 (i8*, i64)* @odd2, i1 (i8*, i64)** %31, align 8
  %32 = getelementptr i8, i8* %30, i64 8
  %33 = bitcast i8* %32 to i8**
  store i8* %0, i8** %33, align 8
  %34 = icmp eq i64 %25, %24
  %35 = add i64 %.tr23, -2
  br i1 %34, label %end_0, label %tailrecurse

end_0:                                            ; preds = %tailrecurse, %else_0, %2
  %.0 = phi i1 [ true, %2 ], [ false, %else_0 ], [ true, %tailrecurse ]
  ret i1 %.0
}

define i1 @odd2(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i1 (i8*, i64)**
  store i1 (i8*, i64)* @even1, i1 (i8*, i64)** %6, align 8
  %7 = getelementptr i8, i8* %5, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %0, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to i1 (i8*, i64)**
  store i1 (i8*, i64)* @odd2, i1 (i8*, i64)** %10, align 8
  %11 = getelementptr i8, i8* %9, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %0, i8** %12, align 8
  %13 = icmp eq i64 %4, %1
  br i1 %13, label %end_0, label %else_0

else_0:                                           ; preds = %2
  %14 = add i64 %1, -1
  %15 = tail call i1 @even1(i8* nonnull %0, i64 %14)
  br label %end_0

end_0:                                            ; preds = %2, %else_0
  %.0 = phi i1 [ %15, %else_0 ], [ false, %2 ]
  ret i1 %.0
}

declare {}* @print_bool(i1) local_unnamed_addr

define {}* @print_bool5(i1) local_unnamed_addr {
  %2 = tail call {}* @print_bool(i1 %0)
  ret {}* %2
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 8)
  %2 = bitcast i8* %1 to i64*
  store i64 0, i64* %2, align 8
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i1 (i8*, i64)**
  store i1 (i8*, i64)* @even1, i1 (i8*, i64)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %1, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 8)
  %8 = bitcast i8* %7 to i64*
  store i64 0, i64* %8, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to i1 (i8*, i64)**
  store i1 (i8*, i64)* @odd2, i1 (i8*, i64)** %10, align 8
  %11 = getelementptr i8, i8* %9, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %7, i8** %12, align 8
  %13 = load i64, i64* %8, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to i1 (i8*, i64)**
  store i1 (i8*, i64)* @even1, i1 (i8*, i64)** %15, align 8
  %16 = getelementptr i8, i8* %14, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %7, i8** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 16)
  %19 = bitcast i8* %18 to i1 (i8*, i64)**
  store i1 (i8*, i64)* @odd2, i1 (i8*, i64)** %19, align 8
  %20 = getelementptr i8, i8* %18, i64 8
  %21 = bitcast i8* %20 to i8**
  store i8* %7, i8** %21, align 8
  %22 = icmp eq i64 %13, 34
  br i1 %22, label %odd2.exit, label %else_0.i

else_0.i:                                         ; preds = %0
  %23 = tail call i1 @even1(i8* nonnull %7, i64 33)
  br label %odd2.exit

odd2.exit:                                        ; preds = %0, %else_0.i
  %.0.i = phi i1 [ %23, %else_0.i ], [ false, %0 ]
  %24 = tail call {}* @print_bool(i1 %.0.i)
  ret i32 0
}
