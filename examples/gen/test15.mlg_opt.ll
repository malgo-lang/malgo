; ModuleID = './examples/gen/test15.mlg.ll'
source_filename = "./examples/test15.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define {}* @f39(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @f39, {}* (i8*, i64)** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = getelementptr i8, i8* %3, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 0)
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @g38, {}* (i8*, i64)** %10, align 8
  %11 = tail call i8* @GC_malloc(i64 0)
  %12 = getelementptr i8, i8* %9, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %0, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 0)
  %15 = icmp slt i64 %1, 0
  %16 = tail call i8* @GC_malloc(i64 0)
  br i1 %15, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %17 = bitcast i8* %16 to {}*
  br label %end_0

else_0:                                           ; preds = %2
  %18 = tail call i8* @GC_malloc(i64 16)
  %19 = bitcast i8* %18 to { {}* (i8*, i64)*, i8* }**
  %20 = bitcast i8* %18 to i8**
  store i8* %9, i8** %20, align 8
  %21 = tail call i8* @GC_malloc(i64 0)
  %22 = getelementptr i8, i8* %18, i64 8
  %23 = bitcast i8* %22 to i8**
  store i8* %16, i8** %23, align 8
  %24 = tail call i8* @GC_malloc(i64 0)
  %25 = load { {}* (i8*, i64)*, i8* }*, { {}* (i8*, i64)*, i8* }** %19, align 8
  %26 = add i64 %1, -1
  %27 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %25, i64 0, i32 0
  %28 = load {}* (i8*, i64)*, {}* (i8*, i64)** %27, align 8
  %29 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %25, i64 0, i32 1
  %30 = load i8*, i8** %29, align 8
  %31 = tail call {}* %28(i8* %30, i64 %26)
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %.0 = phi {}* [ %17, %then_0 ], [ %31, %else_0 ]
  ret {}* %.0
}

define {}* @g38(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @f39, {}* (i8*, i64)** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = getelementptr i8, i8* %3, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 0)
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @g38, {}* (i8*, i64)** %10, align 8
  %11 = tail call i8* @GC_malloc(i64 0)
  %12 = getelementptr i8, i8* %9, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %0, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 0)
  %15 = icmp slt i64 %1, 0
  %16 = tail call i8* @GC_malloc(i64 0)
  br i1 %15, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %17 = bitcast i8* %16 to {}*
  br label %end_0

else_0:                                           ; preds = %2
  %18 = tail call i8* @GC_malloc(i64 16)
  %19 = bitcast i8* %18 to { {}* (i8*, i64)*, i8* }**
  %20 = bitcast i8* %18 to i8**
  store i8* %3, i8** %20, align 8
  %21 = tail call i8* @GC_malloc(i64 0)
  %22 = getelementptr i8, i8* %18, i64 8
  %23 = bitcast i8* %22 to i8**
  store i8* %16, i8** %23, align 8
  %24 = tail call i8* @GC_malloc(i64 0)
  %25 = load { {}* (i8*, i64)*, i8* }*, { {}* (i8*, i64)*, i8* }** %19, align 8
  %26 = add i64 %1, -1
  %27 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %25, i64 0, i32 0
  %28 = load {}* (i8*, i64)*, {}* (i8*, i64)** %27, align 8
  %29 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %25, i64 0, i32 1
  %30 = load i8*, i8** %29, align 8
  %31 = tail call {}* %28(i8* %30, i64 %26)
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %.0 = phi {}* [ %17, %then_0 ], [ %31, %else_0 ]
  ret {}* %.0
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 0)
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @f39, {}* (i8*, i64)** %3, align 8
  %4 = tail call i8* @GC_malloc(i64 0)
  %5 = getelementptr i8, i8* %2, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %1, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = tail call i8* @GC_malloc(i64 0)
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @g38, {}* (i8*, i64)** %10, align 8
  %11 = tail call i8* @GC_malloc(i64 0)
  %12 = getelementptr i8, i8* %9, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %8, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 0)
  %15 = load {}* (i8*, i64)*, {}* (i8*, i64)** %3, align 8
  %16 = load i8*, i8** %6, align 8
  %17 = tail call {}* %15(i8* %16, i64 5)
  ret i32 0
}
