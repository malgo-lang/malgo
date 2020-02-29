; ModuleID = './examples/gen/test15.mlg.ll'
source_filename = "./examples/test15.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define {}* @g1(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @f0, {}* (i8*, i64)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 16)
  %8 = bitcast i8* %7 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @g1, {}* (i8*, i64)** %8, align 8
  %9 = getelementptr i8, i8* %7, i64 8
  %10 = bitcast i8* %9 to i8**
  store i8* %0, i8** %10, align 8
  %11 = icmp slt i64 %1, 0
  %12 = tail call i8* @GC_malloc(i64 0)
  br i1 %11, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %13 = bitcast i8* %12 to {}*
  br label %end_0

else_0:                                           ; preds = %2
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to i8**
  store i8* %3, i8** %15, align 8
  %16 = getelementptr i8, i8* %14, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %12, i8** %17, align 8
  %18 = add i64 %1, -1
  %19 = load {}* (i8*, i64)*, {}* (i8*, i64)** %4, align 8
  %20 = load i8*, i8** %6, align 8
  %21 = tail call {}* %19(i8* %20, i64 %18)
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %.0 = phi {}* [ %13, %then_0 ], [ %21, %else_0 ]
  ret {}* %.0
}

define {}* @f0(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @f0, {}* (i8*, i64)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 16)
  %8 = bitcast i8* %7 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @g1, {}* (i8*, i64)** %8, align 8
  %9 = getelementptr i8, i8* %7, i64 8
  %10 = bitcast i8* %9 to i8**
  store i8* %0, i8** %10, align 8
  %11 = icmp slt i64 %1, 0
  %12 = tail call i8* @GC_malloc(i64 0)
  br i1 %11, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %13 = bitcast i8* %12 to {}*
  br label %end_0

else_0:                                           ; preds = %2
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to i8**
  store i8* %7, i8** %15, align 8
  %16 = getelementptr i8, i8* %14, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %12, i8** %17, align 8
  %18 = add i64 %1, -1
  %19 = load {}* (i8*, i64)*, {}* (i8*, i64)** %8, align 8
  %20 = load i8*, i8** %10, align 8
  %21 = tail call {}* %19(i8* %20, i64 %18)
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %.0 = phi {}* [ %13, %then_0 ], [ %21, %else_0 ]
  ret {}* %.0
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
  %6 = tail call i8* @GC_malloc(i64 0)
  %7 = tail call i8* @GC_malloc(i64 16)
  %8 = bitcast i8* %7 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @g1, {}* (i8*, i64)** %8, align 8
  %9 = getelementptr i8, i8* %7, i64 8
  %10 = bitcast i8* %9 to i8**
  store i8* %6, i8** %10, align 8
  %11 = load {}* (i8*, i64)*, {}* (i8*, i64)** %3, align 8
  %12 = load i8*, i8** %5, align 8
  %13 = tail call {}* %11(i8* %12, i64 5)
  ret i32 0
}
