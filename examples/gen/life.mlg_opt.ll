; ModuleID = './examples/gen/life.mlg.ll'
source_filename = "./examples/life.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define {}* @loop.224(i8*, i64) {
  %3 = bitcast i8* %0 to i1**
  %4 = load i1*, i1** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to { {}* (i8*, i1*)*, i8* }**
  %7 = load { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }** %6, align 8
  %8 = getelementptr i8, i8* %0, i64 16
  %9 = bitcast i8* %8 to { {}* (i8*, i1*)*, i8* }**
  %10 = load { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }** %9, align 8
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop.224, {}* (i8*, i64)** %12, align 8
  %13 = getelementptr i8, i8* %11, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %0, i8** %14, align 8
  %15 = icmp sgt i64 %1, 0
  br i1 %15, label %then_0, label %else_0

then_0:                                           ; preds = %2, %then_0
  %16 = phi { {}* (i8*, i1*)*, i8* }* [ %34, %then_0 ], [ %10, %2 ]
  %17 = phi { {}* (i8*, i1*)*, i8* }* [ %33, %then_0 ], [ %7, %2 ]
  %18 = phi i1* [ %32, %then_0 ], [ %4, %2 ]
  %.tr23 = phi i64 [ %31, %then_0 ], [ %1, %2 ]
  %19 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %17, i64 0, i32 0
  %20 = load {}* (i8*, i1*)*, {}* (i8*, i1*)** %19, align 8
  %21 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %17, i64 0, i32 1
  %22 = load i8*, i8** %21, align 8
  %23 = tail call {}* %20(i8* %22, i1* %18)
  %24 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %16, i64 0, i32 0
  %25 = load {}* (i8*, i1*)*, {}* (i8*, i1*)** %24, align 8
  %26 = getelementptr { {}* (i8*, i1*)*, i8* }, { {}* (i8*, i1*)*, i8* }* %16, i64 0, i32 1
  %27 = load i8*, i8** %26, align 8
  %28 = tail call {}* %25(i8* %27, i1* %18)
  %29 = tail call {}* @newline()
  %30 = tail call {}* @malgo_sleep(i64 1)
  %31 = add nsw i64 %.tr23, -1
  %32 = load i1*, i1** %3, align 8
  %33 = load { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }** %6, align 8
  %34 = load { {}* (i8*, i1*)*, i8* }*, { {}* (i8*, i1*)*, i8* }** %9, align 8
  %35 = tail call i8* @GC_malloc(i64 16)
  %36 = bitcast i8* %35 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop.224, {}* (i8*, i64)** %36, align 8
  %37 = getelementptr i8, i8* %35, i64 8
  %38 = bitcast i8* %37 to i8**
  store i8* %0, i8** %38, align 8
  %39 = icmp sgt i64 %31, 0
  br i1 %39, label %then_0, label %else_0

else_0:                                           ; preds = %then_0, %2
  %40 = tail call i8* @GC_malloc(i64 0)
  %41 = bitcast i8* %40 to {}*
  ret {}* %41
}

define {}* @update_cells.210(i8*, i1*) {
  %3 = bitcast i8* %0 to <2 x i64>*
  %4 = load <2 x i64>, <2 x i64>* %3, align 8
  %5 = getelementptr i8, i8* %0, i64 16
  %6 = bitcast i8* %5 to i64*
  %7 = load i64, i64* %6, align 8
  %8 = getelementptr i8, i8* %0, i64 24
  %9 = bitcast i8* %8 to i64*
  %10 = load i64, i64* %9, align 8
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to {}* (i8*, i1*)**
  store {}* (i8*, i1*)* @update_cells.210, {}* (i8*, i1*)** %12, align 8
  %13 = getelementptr i8, i8* %11, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %0, i8** %14, align 8
  %15 = mul i64 %10, %7
  %16 = tail call i1* @copy_bool_array(i1* %1, i64 %15)
  %17 = tail call i8* @GC_malloc(i64 48)
  %18 = bitcast i8* %17 to i1**
  store i1* %1, i1** %18, align 8
  %19 = getelementptr i8, i8* %17, i64 8
  %20 = bitcast i8* %19 to i1**
  store i1* %16, i1** %20, align 8
  %21 = getelementptr i8, i8* %17, i64 16
  %22 = bitcast i8* %21 to <2 x i64>*
  store <2 x i64> %4, <2 x i64>* %22, align 8
  %23 = getelementptr i8, i8* %17, i64 32
  %24 = bitcast i8* %23 to i64*
  store i64 %7, i64* %24, align 8
  %25 = getelementptr i8, i8* %17, i64 40
  %26 = bitcast i8* %25 to i64*
  store i64 %10, i64* %26, align 8
  %27 = tail call i8* @GC_malloc(i64 16)
  %28 = bitcast i8* %27 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_y.208, {}* (i8*, i64)** %28, align 8
  %29 = getelementptr i8, i8* %27, i64 8
  %30 = bitcast i8* %29 to i8**
  store i8* %17, i8** %30, align 8
  %31 = tail call {}* @go_y.208(i8* %17, i64 0)
  ret {}* %31
}

define {}* @go_y.208(i8*, i64) {
  %3 = getelementptr i8, i8* %0, i64 16
  %4 = getelementptr i8, i8* %0, i64 32
  %5 = bitcast i8* %4 to i64*
  %6 = getelementptr i8, i8* %0, i64 40
  %7 = bitcast i8* %6 to i64*
  %8 = bitcast i8* %0 to <2 x i64>*
  %9 = bitcast i8* %3 to <2 x i64>*
  br label %tailrecurse

tailrecurse:                                      ; preds = %else_0, %2
  %.tr2 = phi i64 [ %1, %2 ], [ %34, %else_0 ]
  %10 = load <2 x i64>, <2 x i64>* %8, align 8
  %11 = load <2 x i64>, <2 x i64>* %9, align 8
  %12 = load i64, i64* %5, align 8
  %13 = load i64, i64* %7, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_y.208, {}* (i8*, i64)** %15, align 8
  %16 = getelementptr i8, i8* %14, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %0, i8** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 48)
  %19 = bitcast i8* %18 to <2 x i64>*
  store <2 x i64> %10, <2 x i64>* %19, align 8
  %20 = getelementptr i8, i8* %18, i64 16
  %21 = bitcast i8* %20 to <2 x i64>*
  store <2 x i64> %11, <2 x i64>* %21, align 8
  %22 = getelementptr i8, i8* %18, i64 32
  %23 = bitcast i8* %22 to i64*
  store i64 %12, i64* %23, align 8
  %24 = getelementptr i8, i8* %18, i64 40
  %25 = bitcast i8* %24 to i64*
  store i64 %.tr2, i64* %25, align 8
  %26 = tail call i8* @GC_malloc(i64 16)
  %27 = bitcast i8* %26 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x.202, {}* (i8*, i64)** %27, align 8
  %28 = getelementptr i8, i8* %26, i64 8
  %29 = bitcast i8* %28 to i8**
  store i8* %18, i8** %29, align 8
  %30 = icmp sgt i64 %13, %.tr2
  br i1 %30, label %else_0, label %then_0

then_0:                                           ; preds = %tailrecurse
  %31 = tail call i8* @GC_malloc(i64 0)
  %32 = bitcast i8* %31 to {}*
  ret {}* %32

else_0:                                           ; preds = %tailrecurse
  %33 = tail call {}* @go_x.202(i8* %18, i64 0)
  %34 = add i64 %.tr2, 1
  br label %tailrecurse
}

define {}* @go_x.202(i8*, i64) {
  %3 = bitcast i8* %0 to i1**
  %4 = load i1*, i1** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i1**
  %7 = load i1*, i1** %6, align 8
  %8 = getelementptr i8, i8* %0, i64 16
  %9 = bitcast i8* %8 to { i1 (i8*, i1*, i64, i64)*, i8* }**
  %10 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %9, align 8
  %11 = getelementptr i8, i8* %0, i64 24
  %12 = bitcast i8* %11 to { {}* (i8*, i1*, i64, i64, i1)*, i8* }**
  %13 = load { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }** %12, align 8
  %14 = getelementptr i8, i8* %0, i64 32
  %15 = bitcast i8* %14 to i64*
  %16 = load i64, i64* %15, align 8
  %17 = getelementptr i8, i8* %0, i64 40
  %18 = bitcast i8* %17 to i64*
  %19 = load i64, i64* %18, align 8
  %20 = tail call i8* @GC_malloc(i64 16)
  %21 = bitcast i8* %20 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x.202, {}* (i8*, i64)** %21, align 8
  %22 = getelementptr i8, i8* %20, i64 8
  %23 = bitcast i8* %22 to i8**
  store i8* %0, i8** %23, align 8
  %24 = icmp sgt i64 %16, %1
  br i1 %24, label %else_0, label %then_0

then_0:                                           ; preds = %else_0, %2
  %25 = tail call i8* @GC_malloc(i64 0)
  %26 = bitcast i8* %25 to {}*
  ret {}* %26

else_0:                                           ; preds = %2, %else_0
  %27 = phi i64 [ %48, %else_0 ], [ %19, %2 ]
  %28 = phi { {}* (i8*, i1*, i64, i64, i1)*, i8* }* [ %46, %else_0 ], [ %13, %2 ]
  %29 = phi { i1 (i8*, i1*, i64, i64)*, i8* }* [ %45, %else_0 ], [ %10, %2 ]
  %30 = phi i1* [ %44, %else_0 ], [ %7, %2 ]
  %31 = phi i1* [ %43, %else_0 ], [ %4, %2 ]
  %.tr23 = phi i64 [ %42, %else_0 ], [ %1, %2 ]
  %32 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %29, i64 0, i32 0
  %33 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %32, align 8
  %34 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %29, i64 0, i32 1
  %35 = load i8*, i8** %34, align 8
  %36 = tail call i1 %33(i8* %35, i1* %30, i64 %.tr23, i64 %27)
  %37 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %28, i64 0, i32 0
  %38 = load {}* (i8*, i1*, i64, i64, i1)*, {}* (i8*, i1*, i64, i64, i1)** %37, align 8
  %39 = getelementptr { {}* (i8*, i1*, i64, i64, i1)*, i8* }, { {}* (i8*, i1*, i64, i64, i1)*, i8* }* %28, i64 0, i32 1
  %40 = load i8*, i8** %39, align 8
  %41 = tail call {}* %38(i8* %40, i1* %31, i64 %.tr23, i64 %27, i1 %36)
  %42 = add i64 %.tr23, 1
  %43 = load i1*, i1** %3, align 8
  %44 = load i1*, i1** %6, align 8
  %45 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %9, align 8
  %46 = load { {}* (i8*, i1*, i64, i64, i1)*, i8* }*, { {}* (i8*, i1*, i64, i64, i1)*, i8* }** %12, align 8
  %47 = load i64, i64* %15, align 8
  %48 = load i64, i64* %18, align 8
  %49 = tail call i8* @GC_malloc(i64 16)
  %50 = bitcast i8* %49 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x.202, {}* (i8*, i64)** %50, align 8
  %51 = getelementptr i8, i8* %49, i64 8
  %52 = bitcast i8* %51 to i8**
  store i8* %0, i8** %52, align 8
  %53 = icmp sgt i64 %47, %42
  br i1 %53, label %else_0, label %then_0
}

define i1 @next_state.183(i8*, i1*, i64, i64) {
endif_0:
  %4 = bitcast i8* %0 to { i1 (i8*, i1*, i64, i64)*, i8* }**
  %5 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %4, align 8
  %6 = tail call i8* @GC_malloc(i64 16)
  %7 = bitcast i8* %6 to i1 (i8*, i1*, i64, i64)**
  store i1 (i8*, i1*, i64, i64)* @next_state.183, i1 (i8*, i1*, i64, i64)** %7, align 8
  %8 = getelementptr i8, i8* %6, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %0, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 8)
  %11 = bitcast i8* %10 to i64*
  %12 = add i64 %2, -1
  %13 = add i64 %3, 1
  %14 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %5, i64 0, i32 0
  %15 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %14, align 8
  %16 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %5, i64 0, i32 1
  %17 = load i8*, i8** %16, align 8
  %18 = tail call i1 %15(i8* %17, i1* %1, i64 %12, i64 %13)
  %..i = zext i1 %18 to i64
  %19 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %14, align 8
  %20 = load i8*, i8** %16, align 8
  %21 = tail call i1 %19(i8* %20, i1* %1, i64 %2, i64 %13)
  %..i1 = zext i1 %21 to i64
  %22 = add nuw nsw i64 %..i1, %..i
  %23 = add i64 %2, 1
  %24 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %14, align 8
  %25 = load i8*, i8** %16, align 8
  %26 = tail call i1 %24(i8* %25, i1* %1, i64 %23, i64 %13)
  %..i3 = zext i1 %26 to i64
  %27 = add nuw nsw i64 %22, %..i3
  store i64 %27, i64* %11, align 8
  %28 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %14, align 8
  %29 = load i8*, i8** %16, align 8
  %30 = tail call i1 %28(i8* %29, i1* %1, i64 %12, i64 %3)
  %..i5 = zext i1 %30 to i64
  %31 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %14, align 8
  %32 = load i8*, i8** %16, align 8
  %33 = tail call i1 %31(i8* %32, i1* %1, i64 %23, i64 %3)
  %..i7 = zext i1 %33 to i64
  %34 = add nuw nsw i64 %..i7, %..i5
  %35 = load i64, i64* %11, align 8
  %36 = add i64 %34, %35
  store i64 %36, i64* %11, align 8
  %37 = add i64 %3, -1
  %38 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %14, align 8
  %39 = load i8*, i8** %16, align 8
  %40 = tail call i1 %38(i8* %39, i1* %1, i64 %12, i64 %37)
  %..i6 = zext i1 %40 to i64
  %41 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %14, align 8
  %42 = load i8*, i8** %16, align 8
  %43 = tail call i1 %41(i8* %42, i1* %1, i64 %2, i64 %37)
  %..i4 = zext i1 %43 to i64
  %44 = add nuw nsw i64 %..i4, %..i6
  %45 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %14, align 8
  %46 = load i8*, i8** %16, align 8
  %47 = tail call i1 %45(i8* %46, i1* %1, i64 %23, i64 %37)
  %..i2 = zext i1 %47 to i64
  %48 = add nuw nsw i64 %44, %..i2
  %49 = load i64, i64* %11, align 8
  %50 = add i64 %48, %49
  store i64 %50, i64* %11, align 8
  %51 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %14, align 8
  %52 = load i8*, i8** %16, align 8
  %53 = tail call i1 %51(i8* %52, i1* %1, i64 %2, i64 %3)
  %54 = load i64, i64* %11, align 8
  %55 = zext i1 %53 to i64
  %.0.in = or i64 %54, %55
  %.0 = icmp eq i64 %.0.in, 3
  ret i1 %.0
}

; Function Attrs: norecurse nounwind readnone
define i64 @to_int.128(i1) local_unnamed_addr #0 {
endif_0:
  %. = zext i1 %0 to i64
  ret i64 %.
}

define {}* @print_cells.124(i8*, i1*) {
  %3 = bitcast i8* %0 to <2 x i64>*
  %4 = load <2 x i64>, <2 x i64>* %3, align 8
  %5 = getelementptr i8, i8* %0, i64 16
  %6 = bitcast i8* %5 to i64*
  %7 = load i64, i64* %6, align 8
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to {}* (i8*, i1*)**
  store {}* (i8*, i1*)* @print_cells.124, {}* (i8*, i1*)** %9, align 8
  %10 = getelementptr i8, i8* %8, i64 8
  %11 = bitcast i8* %10 to i8**
  store i8* %0, i8** %11, align 8
  %12 = tail call i8* @GC_malloc(i64 32)
  %13 = bitcast i8* %12 to i1**
  store i1* %1, i1** %13, align 8
  %14 = getelementptr i8, i8* %12, i64 8
  %15 = bitcast i8* %14 to <2 x i64>*
  store <2 x i64> %4, <2 x i64>* %15, align 8
  %16 = getelementptr i8, i8* %12, i64 24
  %17 = bitcast i8* %16 to i64*
  store i64 %7, i64* %17, align 8
  %18 = tail call i8* @GC_malloc(i64 16)
  %19 = bitcast i8* %18 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_y.122, {}* (i8*, i64)** %19, align 8
  %20 = getelementptr i8, i8* %18, i64 8
  %21 = bitcast i8* %20 to i8**
  store i8* %12, i8** %21, align 8
  %22 = tail call {}* @go_y.122(i8* %12, i64 0)
  ret {}* %22
}

define {}* @go_y.122(i8*, i64) {
  %3 = getelementptr i8, i8* %0, i64 16
  %4 = bitcast i8* %3 to i64*
  %5 = getelementptr i8, i8* %0, i64 24
  %6 = bitcast i8* %5 to i64*
  %7 = bitcast i8* %0 to <2 x i64>*
  br label %tailrecurse

tailrecurse:                                      ; preds = %else_0, %2
  %.tr2 = phi i64 [ %1, %2 ], [ %30, %else_0 ]
  %8 = load <2 x i64>, <2 x i64>* %7, align 8
  %9 = load i64, i64* %4, align 8
  %10 = load i64, i64* %6, align 8
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_y.122, {}* (i8*, i64)** %12, align 8
  %13 = getelementptr i8, i8* %11, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %0, i8** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 32)
  %16 = bitcast i8* %15 to <2 x i64>*
  store <2 x i64> %8, <2 x i64>* %16, align 8
  %17 = getelementptr i8, i8* %15, i64 16
  %18 = bitcast i8* %17 to i64*
  store i64 %9, i64* %18, align 8
  %19 = getelementptr i8, i8* %15, i64 24
  %20 = bitcast i8* %19 to i64*
  store i64 %.tr2, i64* %20, align 8
  %21 = tail call i8* @GC_malloc(i64 16)
  %22 = bitcast i8* %21 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x.115, {}* (i8*, i64)** %22, align 8
  %23 = getelementptr i8, i8* %21, i64 8
  %24 = bitcast i8* %23 to i8**
  store i8* %15, i8** %24, align 8
  %25 = icmp sgt i64 %10, %.tr2
  br i1 %25, label %else_0, label %then_0

then_0:                                           ; preds = %tailrecurse
  %26 = tail call i8* @GC_malloc(i64 0)
  %27 = bitcast i8* %26 to {}*
  ret {}* %27

else_0:                                           ; preds = %tailrecurse
  %28 = tail call {}* @go_x.115(i8* %15, i64 0)
  %29 = tail call {}* @newline()
  %30 = add i64 %.tr2, 1
  br label %tailrecurse
}

define {}* @go_x.115(i8*, i64) {
  %3 = bitcast i8* %0 to i1**
  %4 = load i1*, i1** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to { i1 (i8*, i1*, i64, i64)*, i8* }**
  %7 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %6, align 8
  %8 = getelementptr i8, i8* %0, i64 16
  %9 = bitcast i8* %8 to i64*
  %10 = load i64, i64* %9, align 8
  %11 = getelementptr i8, i8* %0, i64 24
  %12 = bitcast i8* %11 to i64*
  %13 = load i64, i64* %12, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x.115, {}* (i8*, i64)** %15, align 8
  %16 = getelementptr i8, i8* %14, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %0, i8** %17, align 8
  %18 = icmp sgt i64 %10, %1
  br i1 %18, label %else_0, label %then_0

then_0:                                           ; preds = %else_0, %2
  %19 = tail call i8* @GC_malloc(i64 0)
  %20 = bitcast i8* %19 to {}*
  ret {}* %20

else_0:                                           ; preds = %2, %else_0
  %21 = phi i64 [ %34, %else_0 ], [ %13, %2 ]
  %22 = phi { i1 (i8*, i1*, i64, i64)*, i8* }* [ %32, %else_0 ], [ %7, %2 ]
  %23 = phi i1* [ %31, %else_0 ], [ %4, %2 ]
  %.tr23 = phi i64 [ %30, %else_0 ], [ %1, %2 ]
  %24 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %22, i64 0, i32 0
  %25 = load i1 (i8*, i1*, i64, i64)*, i1 (i8*, i1*, i64, i64)** %24, align 8
  %26 = getelementptr { i1 (i8*, i1*, i64, i64)*, i8* }, { i1 (i8*, i1*, i64, i64)*, i8* }* %22, i64 0, i32 1
  %27 = load i8*, i8** %26, align 8
  %28 = tail call i1 %25(i8* %27, i1* %23, i64 %.tr23, i64 %21)
  %..i = select i1 %28, i8 35, i8 95
  %29 = tail call {}* @print_char(i8 %..i)
  %30 = add i64 %.tr23, 1
  %31 = load i1*, i1** %3, align 8
  %32 = load { i1 (i8*, i1*, i64, i64)*, i8* }*, { i1 (i8*, i1*, i64, i64)*, i8* }** %6, align 8
  %33 = load i64, i64* %9, align 8
  %34 = load i64, i64* %12, align 8
  %35 = tail call i8* @GC_malloc(i64 16)
  %36 = bitcast i8* %35 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x.115, {}* (i8*, i64)** %36, align 8
  %37 = getelementptr i8, i8* %35, i64 8
  %38 = bitcast i8* %37 to i8**
  store i8* %0, i8** %38, align 8
  %39 = icmp sgt i64 %33, %30
  br i1 %39, label %else_0, label %then_0
}

; Function Attrs: norecurse nounwind readnone
define i8 @to_char.98(i1) local_unnamed_addr #0 {
endif_0:
  %. = select i1 %0, i8 35, i8 95
  ret i8 %.
}

define {}* @init_cells.94(i8*, i1* nocapture, i64) {
  %4 = bitcast i8* %0 to i64*
  %5 = load i64, i64* %4, align 8
  %6 = getelementptr i8, i8* %0, i64 8
  %7 = bitcast i8* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to {}* (i8*, i1*, i64)**
  store {}* (i8*, i1*, i64)* @init_cells.94, {}* (i8*, i1*, i64)** %10, align 8
  %11 = getelementptr i8, i8* %9, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %0, i8** %12, align 8
  %13 = mul i64 %8, %5
  %14 = icmp sgt i64 %13, %2
  br i1 %14, label %else_0, label %then_0

then_0:                                           ; preds = %else_0, %3
  %15 = tail call i8* @GC_malloc(i64 0)
  %16 = bitcast i8* %15 to {}*
  ret {}* %16

else_0:                                           ; preds = %3, %else_0
  %.tr34 = phi i64 [ %19, %else_0 ], [ %2, %3 ]
  %17 = tail call i1 @rand_bool()
  %18 = getelementptr i1, i1* %1, i64 %.tr34
  store i1 %17, i1* %18, align 1
  %19 = add i64 %.tr34, 1
  %20 = load i64, i64* %4, align 8
  %21 = load i64, i64* %7, align 8
  %22 = tail call i8* @GC_malloc(i64 16)
  %23 = bitcast i8* %22 to {}* (i8*, i1*, i64)**
  store {}* (i8*, i1*, i64)* @init_cells.94, {}* (i8*, i1*, i64)** %23, align 8
  %24 = getelementptr i8, i8* %22, i64 8
  %25 = bitcast i8* %24 to i8**
  store i8* %0, i8** %25, align 8
  %26 = mul i64 %21, %20
  %27 = icmp sgt i64 %26, %19
  br i1 %27, label %else_0, label %then_0
}

define noalias {}* @set.83(i8*, i1* nocapture, i64, i64, i1) {
  %6 = bitcast i8* %0 to i64*
  %7 = load i64, i64* %6, align 8
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to {}* (i8*, i1*, i64, i64, i1)**
  store {}* (i8*, i1*, i64, i64, i1)* @set.83, {}* (i8*, i1*, i64, i64, i1)** %9, align 8
  %10 = getelementptr i8, i8* %8, i64 8
  %11 = bitcast i8* %10 to i8**
  store i8* %0, i8** %11, align 8
  %12 = mul i64 %7, %3
  %13 = add i64 %12, %2
  %14 = getelementptr i1, i1* %1, i64 %13
  store i1 %4, i1* %14, align 1
  ret {}* undef
}

define i1 @view.74(i8*, i1* nocapture readonly, i64, i64) {
  %5 = bitcast i8* %0 to i64*
  %6 = load i64, i64* %5, align 8
  %7 = getelementptr i8, i8* %0, i64 8
  %8 = bitcast i8* %7 to i64*
  %9 = load i64, i64* %8, align 8
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to i1 (i8*, i1*, i64, i64)**
  store i1 (i8*, i1*, i64, i64)* @view.74, i1 (i8*, i1*, i64, i64)** %11, align 8
  %12 = getelementptr i8, i8* %10, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %0, i8** %13, align 8
  %14 = icmp sle i64 %6, %2
  %15 = icmp sle i64 %9, %3
  %16 = or i1 %14, %15
  %17 = or i64 %3, %2
  %18 = icmp slt i64 %17, 0
  %19 = or i1 %18, %16
  br i1 %19, label %endif_0, label %else_0

else_0:                                           ; preds = %4
  %20 = mul i64 %6, %3
  %21 = add i64 %20, %2
  %22 = getelementptr i1, i1* %1, i64 %21
  %23 = load i1, i1* %22, align 1
  br label %endif_0

endif_0:                                          ; preds = %4, %else_0
  %.0 = phi i1 [ %23, %else_0 ], [ false, %4 ]
  ret i1 %.0
}

declare {}* @pulsar(i1*) local_unnamed_addr

define {}* @pulsar.52(i1*) local_unnamed_addr {
  %2 = tail call {}* @pulsar(i1* %0)
  ret {}* %2
}

declare i1* @copy_bool_array(i1*, i64) local_unnamed_addr

define i1* @copy_bool_array.51(i1*, i64) local_unnamed_addr {
  %3 = tail call i1* @copy_bool_array(i1* %0, i64 %1)
  ret i1* %3
}

declare {}* @malgo_sleep(i64) local_unnamed_addr

define {}* @sleep.50(i64) local_unnamed_addr {
  %2 = tail call {}* @malgo_sleep(i64 %0)
  ret {}* %2
}

declare i1 @rand_bool() local_unnamed_addr

define i1 @rand_bool.49() local_unnamed_addr {
  %1 = tail call i1 @rand_bool()
  ret i1 %1
}

declare {}* @gen_seed() local_unnamed_addr

define {}* @gen_seed.48() local_unnamed_addr {
  %1 = tail call {}* @gen_seed()
  ret {}* %1
}

declare {}* @newline() local_unnamed_addr

define {}* @newline.47() local_unnamed_addr {
  %1 = tail call {}* @newline()
  ret {}* %1
}

declare {}* @print_char(i8) local_unnamed_addr

define {}* @print_char.46(i8) local_unnamed_addr {
  %2 = tail call {}* @print_char(i8 %0)
  ret {}* %2
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 1000)
  %2 = bitcast i8* %1 to i1*
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to <2 x i64>*
  store <2 x i64> <i64 50, i64 20>, <2 x i64>* %4, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i1 (i8*, i1*, i64, i64)**
  store i1 (i8*, i1*, i64, i64)* @view.74, i1 (i8*, i1*, i64, i64)** %6, align 8
  %7 = getelementptr i8, i8* %5, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %3, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 8)
  %10 = bitcast i8* %9 to i64*
  store i64 50, i64* %10, align 8
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to {}* (i8*, i1*, i64, i64, i1)**
  store {}* (i8*, i1*, i64, i64, i1)* @set.83, {}* (i8*, i1*, i64, i64, i1)** %12, align 8
  %13 = getelementptr i8, i8* %11, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %9, i8** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 16)
  %16 = bitcast i8* %15 to <2 x i64>*
  store <2 x i64> <i64 50, i64 20>, <2 x i64>* %16, align 8
  %17 = tail call i8* @GC_malloc(i64 16)
  %18 = bitcast i8* %17 to {}* (i8*, i1*, i64)**
  store {}* (i8*, i1*, i64)* @init_cells.94, {}* (i8*, i1*, i64)** %18, align 8
  %19 = getelementptr i8, i8* %17, i64 8
  %20 = bitcast i8* %19 to i8**
  store i8* %15, i8** %20, align 8
  %21 = tail call i8* @GC_malloc(i64 24)
  %22 = bitcast i8* %21 to i8**
  store i8* %5, i8** %22, align 8
  %23 = getelementptr i8, i8* %21, i64 8
  %24 = bitcast i8* %23 to <2 x i64>*
  store <2 x i64> <i64 50, i64 20>, <2 x i64>* %24, align 8
  %25 = tail call i8* @GC_malloc(i64 16)
  %26 = bitcast i8* %25 to {}* (i8*, i1*)**
  store {}* (i8*, i1*)* @print_cells.124, {}* (i8*, i1*)** %26, align 8
  %27 = getelementptr i8, i8* %25, i64 8
  %28 = bitcast i8* %27 to i8**
  store i8* %21, i8** %28, align 8
  %29 = tail call i8* @GC_malloc(i64 8)
  %30 = bitcast i8* %29 to i8**
  store i8* %5, i8** %30, align 8
  %31 = tail call i8* @GC_malloc(i64 16)
  %32 = bitcast i8* %31 to i1 (i8*, i1*, i64, i64)**
  store i1 (i8*, i1*, i64, i64)* @next_state.183, i1 (i8*, i1*, i64, i64)** %32, align 8
  %33 = getelementptr i8, i8* %31, i64 8
  %34 = bitcast i8* %33 to i8**
  store i8* %29, i8** %34, align 8
  %35 = tail call i8* @GC_malloc(i64 32)
  %36 = bitcast i8* %35 to i8**
  store i8* %31, i8** %36, align 8
  %37 = getelementptr i8, i8* %35, i64 8
  %38 = bitcast i8* %37 to i8**
  store i8* %11, i8** %38, align 8
  %39 = getelementptr i8, i8* %35, i64 16
  %40 = bitcast i8* %39 to <2 x i64>*
  store <2 x i64> <i64 50, i64 20>, <2 x i64>* %40, align 8
  %41 = tail call i8* @GC_malloc(i64 16)
  %42 = bitcast i8* %41 to {}* (i8*, i1*)**
  store {}* (i8*, i1*)* @update_cells.210, {}* (i8*, i1*)** %42, align 8
  %43 = getelementptr i8, i8* %41, i64 8
  %44 = bitcast i8* %43 to i8**
  store i8* %35, i8** %44, align 8
  %45 = tail call {}* @gen_seed()
  %46 = tail call {}* @pulsar(i1* %2)
  %47 = tail call i8* @GC_malloc(i64 24)
  %48 = bitcast i8* %47 to i8**
  store i8* %1, i8** %48, align 8
  %49 = getelementptr i8, i8* %47, i64 8
  %50 = bitcast i8* %49 to i8**
  store i8* %25, i8** %50, align 8
  %51 = getelementptr i8, i8* %47, i64 16
  %52 = bitcast i8* %51 to i8**
  store i8* %41, i8** %52, align 8
  %53 = tail call i8* @GC_malloc(i64 16)
  %54 = bitcast i8* %53 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop.224, {}* (i8*, i64)** %54, align 8
  %55 = getelementptr i8, i8* %53, i64 8
  %56 = bitcast i8* %55 to i8**
  store i8* %47, i8** %56, align 8
  %57 = tail call {}* @loop.224(i8* %47, i64 50)
  ret i32 0
}

attributes #0 = { norecurse nounwind readnone }
