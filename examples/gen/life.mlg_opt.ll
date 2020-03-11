; ModuleID = './examples/gen/life.mlg.ll'
source_filename = "./examples/life.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i1 @view10(i8*, { i1*, i64 }* nocapture readonly, i64, i64) {
  %5 = bitcast i8* %0 to i64*
  %6 = load i64, i64* %5, align 8
  %7 = getelementptr i8, i8* %0, i64 8
  %8 = bitcast i8* %7 to i64*
  %9 = load i64, i64* %8, align 8
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %11, align 8
  %12 = getelementptr i8, i8* %10, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %0, i8** %13, align 8
  %14 = icmp sle i64 %6, %2
  %15 = icmp sle i64 %9, %3
  %16 = or i1 %14, %15
  %17 = or i64 %3, %2
  %18 = icmp slt i64 %17, 0
  %19 = or i1 %18, %16
  br i1 %19, label %end_0, label %else_0

else_0:                                           ; preds = %4
  %20 = mul i64 %6, %3
  %21 = add i64 %20, %2
  %22 = getelementptr { i1*, i64 }, { i1*, i64 }* %1, i64 0, i32 0
  %23 = load i1*, i1** %22, align 8
  %24 = getelementptr i1, i1* %23, i64 %21
  %25 = load i1, i1* %24, align 1
  br label %end_0

end_0:                                            ; preds = %4, %else_0
  %.0 = phi i1 [ %25, %else_0 ], [ false, %4 ]
  ret i1 %.0
}

define {}* @update_cells37(i8*, { i1*, i64 }*) {
  %3 = bitcast i8* %0 to <2 x i64>*
  %4 = load <2 x i64>, <2 x i64>* %3, align 8
  %5 = getelementptr i8, i8* %0, i64 16
  %6 = bitcast i8* %5 to <2 x i64>*
  %7 = load <2 x i64>, <2 x i64>* %6, align 8
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells37, {}* (i8*, { i1*, i64 }*)** %9, align 8
  %10 = getelementptr i8, i8* %8, i64 8
  %11 = bitcast i8* %10 to i8**
  store i8* %0, i8** %11, align 8
  %12 = tail call { i1*, i64 }* @copy_bool_array({ i1*, i64 }* %1)
  %13 = tail call i8* @GC_malloc(i64 48)
  %14 = bitcast i8* %13 to { i1*, i64 }**
  store { i1*, i64 }* %1, { i1*, i64 }** %14, align 8
  %15 = getelementptr i8, i8* %13, i64 8
  %16 = bitcast i8* %15 to { i1*, i64 }**
  store { i1*, i64 }* %12, { i1*, i64 }** %16, align 8
  %17 = getelementptr i8, i8* %13, i64 16
  %18 = bitcast i8* %17 to <2 x i64>*
  store <2 x i64> %4, <2 x i64>* %18, align 8
  %19 = getelementptr i8, i8* %13, i64 32
  %20 = bitcast i8* %19 to <2 x i64>*
  store <2 x i64> %7, <2 x i64>* %20, align 8
  %21 = tail call i8* @GC_malloc(i64 16)
  %22 = bitcast i8* %21 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_y40, {}* (i8*, i64)** %22, align 8
  %23 = getelementptr i8, i8* %21, i64 8
  %24 = bitcast i8* %23 to i8**
  store i8* %13, i8** %24, align 8
  %25 = tail call {}* @go_y40(i8* %13, i64 0)
  ret {}* %25
}

; Function Attrs: norecurse nounwind readnone
define i64 @to_int30(i1) local_unnamed_addr #0 {
end_0:
  %. = zext i1 %0 to i64
  ret i64 %.
}

; Function Attrs: norecurse nounwind readnone
define i8 @to_char22(i1) local_unnamed_addr #0 {
end_0:
  %. = select i1 %0, i8 35, i8 95
  ret i8 %.
}

declare {}* @malgo_sleep(i64) local_unnamed_addr

define {}* @sleep4(i64) local_unnamed_addr {
  %2 = tail call {}* @malgo_sleep(i64 %0)
  ret {}* %2
}

define noalias {}* @set14(i8*, { i1*, i64 }* nocapture readonly, i64, i64, i1) {
  %6 = bitcast i8* %0 to i64*
  %7 = load i64, i64* %6, align 8
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set14, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %9, align 8
  %10 = getelementptr i8, i8* %8, i64 8
  %11 = bitcast i8* %10 to i8**
  store i8* %0, i8** %11, align 8
  %12 = mul i64 %7, %3
  %13 = add i64 %12, %2
  %14 = getelementptr { i1*, i64 }, { i1*, i64 }* %1, i64 0, i32 0
  %15 = load i1*, i1** %14, align 8
  %16 = getelementptr i1, i1* %15, i64 %13
  store i1 %4, i1* %16, align 1
  ret {}* undef
}

declare i1 @rand_bool() local_unnamed_addr

define i1 @rand_bool3() local_unnamed_addr {
  %1 = tail call i1 @rand_bool()
  ret i1 %1
}

declare {}* @pulsar({ i1*, i64 }*) local_unnamed_addr

define {}* @pulsar6({ i1*, i64 }*) local_unnamed_addr {
  %2 = tail call {}* @pulsar({ i1*, i64 }* %0)
  ret {}* %2
}

declare {}* @print_char(i8) local_unnamed_addr

define {}* @print_char0(i8) local_unnamed_addr {
  %2 = tail call {}* @print_char(i8 %0)
  ret {}* %2
}

define {}* @print_cells24(i8*, { i1*, i64 }*) {
  %3 = bitcast i8* %0 to <2 x i64>*
  %4 = load <2 x i64>, <2 x i64>* %3, align 8
  %5 = getelementptr i8, i8* %0, i64 16
  %6 = bitcast i8* %5 to i64*
  %7 = load i64, i64* %6, align 8
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells24, {}* (i8*, { i1*, i64 }*)** %9, align 8
  %10 = getelementptr i8, i8* %8, i64 8
  %11 = bitcast i8* %10 to i8**
  store i8* %0, i8** %11, align 8
  %12 = tail call i8* @GC_malloc(i64 32)
  %13 = bitcast i8* %12 to { i1*, i64 }**
  store { i1*, i64 }* %1, { i1*, i64 }** %13, align 8
  %14 = getelementptr i8, i8* %12, i64 8
  %15 = bitcast i8* %14 to <2 x i64>*
  store <2 x i64> %4, <2 x i64>* %15, align 8
  %16 = getelementptr i8, i8* %12, i64 24
  %17 = bitcast i8* %16 to i64*
  store i64 %7, i64* %17, align 8
  %18 = tail call i8* @GC_malloc(i64 16)
  %19 = bitcast i8* %18 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_y26, {}* (i8*, i64)** %19, align 8
  %20 = getelementptr i8, i8* %18, i64 8
  %21 = bitcast i8* %20 to i8**
  store i8* %12, i8** %21, align 8
  %22 = tail call {}* @go_y26(i8* %12, i64 0)
  ret {}* %22
}

define i1 @next_state32(i8*, { i1*, i64 }*, i64, i64) {
body_0:
  %4 = bitcast i8* %0 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }**
  %5 = load { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %4, align 8
  %6 = tail call i8* @GC_malloc(i64 16)
  %7 = bitcast i8* %6 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state32, i1 (i8*, { i1*, i64 }*, i64, i64)** %7, align 8
  %8 = getelementptr i8, i8* %6, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %0, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 8)
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to i64**
  %13 = bitcast i8* %11 to i8**
  store i8* %10, i8** %13, align 8
  %14 = getelementptr i8, i8* %11, i64 8
  %15 = bitcast i8* %14 to i64*
  store i64 1, i64* %15, align 8
  %16 = bitcast i8* %10 to i64*
  store i64 0, i64* %16, align 8
  %17 = add i64 %2, -1
  %18 = add i64 %3, 1
  %19 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %5, i64 0, i32 0
  %20 = load i1 (i8*, { i1*, i64 }*, i64, i64)*, i1 (i8*, { i1*, i64 }*, i64, i64)** %19, align 8
  %21 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %5, i64 0, i32 1
  %22 = load i8*, i8** %21, align 8
  %23 = tail call i1 %20(i8* %22, { i1*, i64 }* %1, i64 %17, i64 %18)
  %..i = zext i1 %23 to i64
  %24 = load i1 (i8*, { i1*, i64 }*, i64, i64)*, i1 (i8*, { i1*, i64 }*, i64, i64)** %19, align 8
  %25 = load i8*, i8** %21, align 8
  %26 = tail call i1 %24(i8* %25, { i1*, i64 }* %1, i64 %2, i64 %18)
  %..i1 = zext i1 %26 to i64
  %27 = add nuw nsw i64 %..i1, %..i
  %28 = add i64 %2, 1
  %29 = load i1 (i8*, { i1*, i64 }*, i64, i64)*, i1 (i8*, { i1*, i64 }*, i64, i64)** %19, align 8
  %30 = load i8*, i8** %21, align 8
  %31 = tail call i1 %29(i8* %30, { i1*, i64 }* %1, i64 %28, i64 %18)
  %..i3 = zext i1 %31 to i64
  %32 = add nuw nsw i64 %27, %..i3
  %33 = load i64*, i64** %12, align 8
  store i64 %32, i64* %33, align 8
  %34 = load i1 (i8*, { i1*, i64 }*, i64, i64)*, i1 (i8*, { i1*, i64 }*, i64, i64)** %19, align 8
  %35 = load i8*, i8** %21, align 8
  %36 = tail call i1 %34(i8* %35, { i1*, i64 }* %1, i64 %17, i64 %3)
  %..i5 = zext i1 %36 to i64
  %37 = load i1 (i8*, { i1*, i64 }*, i64, i64)*, i1 (i8*, { i1*, i64 }*, i64, i64)** %19, align 8
  %38 = load i8*, i8** %21, align 8
  %39 = tail call i1 %37(i8* %38, { i1*, i64 }* %1, i64 %28, i64 %3)
  %..i7 = zext i1 %39 to i64
  %40 = add nuw nsw i64 %..i7, %..i5
  %41 = load i64*, i64** %12, align 8
  %42 = load i64, i64* %41, align 8
  %43 = add i64 %40, %42
  store i64 %43, i64* %41, align 8
  %44 = add i64 %3, -1
  %45 = load i1 (i8*, { i1*, i64 }*, i64, i64)*, i1 (i8*, { i1*, i64 }*, i64, i64)** %19, align 8
  %46 = load i8*, i8** %21, align 8
  %47 = tail call i1 %45(i8* %46, { i1*, i64 }* %1, i64 %17, i64 %44)
  %..i6 = zext i1 %47 to i64
  %48 = load i1 (i8*, { i1*, i64 }*, i64, i64)*, i1 (i8*, { i1*, i64 }*, i64, i64)** %19, align 8
  %49 = load i8*, i8** %21, align 8
  %50 = tail call i1 %48(i8* %49, { i1*, i64 }* %1, i64 %2, i64 %44)
  %..i4 = zext i1 %50 to i64
  %51 = add nuw nsw i64 %..i4, %..i6
  %52 = load i1 (i8*, { i1*, i64 }*, i64, i64)*, i1 (i8*, { i1*, i64 }*, i64, i64)** %19, align 8
  %53 = load i8*, i8** %21, align 8
  %54 = tail call i1 %52(i8* %53, { i1*, i64 }* %1, i64 %28, i64 %44)
  %..i2 = zext i1 %54 to i64
  %55 = add nuw nsw i64 %51, %..i2
  %56 = load i64*, i64** %12, align 8
  %57 = load i64, i64* %56, align 8
  %58 = add i64 %55, %57
  store i64 %58, i64* %56, align 8
  %59 = load i1 (i8*, { i1*, i64 }*, i64, i64)*, i1 (i8*, { i1*, i64 }*, i64, i64)** %19, align 8
  %60 = load i8*, i8** %21, align 8
  %61 = tail call i1 %59(i8* %60, { i1*, i64 }* %1, i64 %2, i64 %3)
  %62 = load i64*, i64** %12, align 8
  %63 = load i64, i64* %62, align 8
  %64 = zext i1 %61 to i64
  %storemerge.in = or i64 %63, %64
  %storemerge = icmp eq i64 %storemerge.in, 3
  ret i1 %storemerge
}

declare {}* @newline() local_unnamed_addr

define {}* @newline1() local_unnamed_addr {
  %1 = tail call {}* @newline()
  ret {}* %1
}

define {}* @loop44(i8*, i64) {
  %3 = bitcast i8* %0 to { i1*, i64 }**
  %4 = load { i1*, i64 }*, { i1*, i64 }** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to { {}* (i8*, { i1*, i64 }*)*, i8* }**
  %7 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %6, align 8
  %8 = getelementptr i8, i8* %0, i64 16
  %9 = bitcast i8* %8 to { {}* (i8*, { i1*, i64 }*)*, i8* }**
  %10 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %9, align 8
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop44, {}* (i8*, i64)** %12, align 8
  %13 = getelementptr i8, i8* %11, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %0, i8** %14, align 8
  %15 = icmp sgt i64 %1, 0
  br i1 %15, label %then_0, label %else_0

then_0:                                           ; preds = %2, %then_0
  %16 = phi { {}* (i8*, { i1*, i64 }*)*, i8* }* [ %33, %then_0 ], [ %10, %2 ]
  %17 = phi { {}* (i8*, { i1*, i64 }*)*, i8* }* [ %32, %then_0 ], [ %7, %2 ]
  %18 = phi { i1*, i64 }* [ %31, %then_0 ], [ %4, %2 ]
  %.tr23 = phi i64 [ %30, %then_0 ], [ %1, %2 ]
  %19 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %17, i64 0, i32 0
  %20 = load {}* (i8*, { i1*, i64 }*)*, {}* (i8*, { i1*, i64 }*)** %19, align 8
  %21 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %17, i64 0, i32 1
  %22 = load i8*, i8** %21, align 8
  %23 = tail call {}* %20(i8* %22, { i1*, i64 }* %18)
  %24 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %16, i64 0, i32 0
  %25 = load {}* (i8*, { i1*, i64 }*)*, {}* (i8*, { i1*, i64 }*)** %24, align 8
  %26 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %16, i64 0, i32 1
  %27 = load i8*, i8** %26, align 8
  %28 = tail call {}* %25(i8* %27, { i1*, i64 }* %18)
  %29 = tail call {}* @newline()
  %30 = add nsw i64 %.tr23, -1
  %31 = load { i1*, i64 }*, { i1*, i64 }** %3, align 8
  %32 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %6, align 8
  %33 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %9, align 8
  %34 = tail call i8* @GC_malloc(i64 16)
  %35 = bitcast i8* %34 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop44, {}* (i8*, i64)** %35, align 8
  %36 = getelementptr i8, i8* %34, i64 8
  %37 = bitcast i8* %36 to i8**
  store i8* %0, i8** %37, align 8
  %38 = icmp sgt i64 %30, 0
  br i1 %38, label %then_0, label %else_0

else_0:                                           ; preds = %then_0, %2
  %39 = tail call i8* @GC_malloc(i64 0)
  %40 = bitcast i8* %39 to {}*
  ret {}* %40
}

define {}* @init_cells19(i8*, { i1*, i64 }* nocapture readonly, i64) {
  %4 = bitcast i8* %0 to i64*
  %5 = load i64, i64* %4, align 8
  %6 = getelementptr i8, i8* %0, i64 8
  %7 = bitcast i8* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells19, {}* (i8*, { i1*, i64 }*, i64)** %10, align 8
  %11 = getelementptr i8, i8* %9, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %0, i8** %12, align 8
  %13 = mul i64 %8, %5
  %14 = icmp sgt i64 %13, %2
  br i1 %14, label %else_0.lr.ph, label %then_0

else_0.lr.ph:                                     ; preds = %3
  %15 = getelementptr { i1*, i64 }, { i1*, i64 }* %1, i64 0, i32 0
  br label %else_0

then_0:                                           ; preds = %else_0, %3
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = bitcast i8* %16 to {}*
  ret {}* %17

else_0:                                           ; preds = %else_0.lr.ph, %else_0
  %.tr34 = phi i64 [ %2, %else_0.lr.ph ], [ %21, %else_0 ]
  %18 = tail call i1 @rand_bool()
  %19 = load i1*, i1** %15, align 8
  %20 = getelementptr i1, i1* %19, i64 %.tr34
  store i1 %18, i1* %20, align 1
  %21 = add i64 %.tr34, 1
  %22 = load i64, i64* %4, align 8
  %23 = load i64, i64* %7, align 8
  %24 = tail call i8* @GC_malloc(i64 16)
  %25 = bitcast i8* %24 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells19, {}* (i8*, { i1*, i64 }*, i64)** %25, align 8
  %26 = getelementptr i8, i8* %24, i64 8
  %27 = bitcast i8* %26 to i8**
  store i8* %0, i8** %27, align 8
  %28 = mul i64 %23, %22
  %29 = icmp sgt i64 %28, %21
  br i1 %29, label %else_0, label %then_0
}

define {}* @go_y40(i8*, i64) {
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
  store {}* (i8*, i64)* @go_y40, {}* (i8*, i64)** %15, align 8
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
  store {}* (i8*, i64)* @go_x42, {}* (i8*, i64)** %27, align 8
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
  %33 = tail call {}* @go_x42(i8* %18, i64 0)
  %34 = add i64 %.tr2, 1
  br label %tailrecurse
}

define {}* @go_y26(i8*, i64) {
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
  store {}* (i8*, i64)* @go_y26, {}* (i8*, i64)** %12, align 8
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
  store {}* (i8*, i64)* @go_x28, {}* (i8*, i64)** %22, align 8
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
  %28 = tail call {}* @go_x28(i8* %15, i64 0)
  %29 = tail call {}* @newline()
  %30 = add i64 %.tr2, 1
  br label %tailrecurse
}

define {}* @go_x42(i8*, i64) {
  %3 = bitcast i8* %0 to { i1*, i64 }**
  %4 = load { i1*, i64 }*, { i1*, i64 }** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to { i1*, i64 }**
  %7 = load { i1*, i64 }*, { i1*, i64 }** %6, align 8
  %8 = getelementptr i8, i8* %0, i64 16
  %9 = bitcast i8* %8 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }**
  %10 = load { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %9, align 8
  %11 = getelementptr i8, i8* %0, i64 24
  %12 = bitcast i8* %11 to { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }**
  %13 = load { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }** %12, align 8
  %14 = getelementptr i8, i8* %0, i64 32
  %15 = bitcast i8* %14 to i64*
  %16 = load i64, i64* %15, align 8
  %17 = getelementptr i8, i8* %0, i64 40
  %18 = bitcast i8* %17 to i64*
  %19 = load i64, i64* %18, align 8
  %20 = tail call i8* @GC_malloc(i64 16)
  %21 = bitcast i8* %20 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x42, {}* (i8*, i64)** %21, align 8
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
  %28 = phi { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* [ %46, %else_0 ], [ %13, %2 ]
  %29 = phi { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* [ %45, %else_0 ], [ %10, %2 ]
  %30 = phi { i1*, i64 }* [ %44, %else_0 ], [ %7, %2 ]
  %31 = phi { i1*, i64 }* [ %43, %else_0 ], [ %4, %2 ]
  %.tr23 = phi i64 [ %42, %else_0 ], [ %1, %2 ]
  %32 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %29, i64 0, i32 0
  %33 = load i1 (i8*, { i1*, i64 }*, i64, i64)*, i1 (i8*, { i1*, i64 }*, i64, i64)** %32, align 8
  %34 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %29, i64 0, i32 1
  %35 = load i8*, i8** %34, align 8
  %36 = tail call i1 %33(i8* %35, { i1*, i64 }* %30, i64 %.tr23, i64 %27)
  %37 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %28, i64 0, i32 0
  %38 = load {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %37, align 8
  %39 = getelementptr { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }* %28, i64 0, i32 1
  %40 = load i8*, i8** %39, align 8
  %41 = tail call {}* %38(i8* %40, { i1*, i64 }* %31, i64 %.tr23, i64 %27, i1 %36)
  %42 = add i64 %.tr23, 1
  %43 = load { i1*, i64 }*, { i1*, i64 }** %3, align 8
  %44 = load { i1*, i64 }*, { i1*, i64 }** %6, align 8
  %45 = load { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %9, align 8
  %46 = load { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }*, { {}* (i8*, { i1*, i64 }*, i64, i64, i1)*, i8* }** %12, align 8
  %47 = load i64, i64* %15, align 8
  %48 = load i64, i64* %18, align 8
  %49 = tail call i8* @GC_malloc(i64 16)
  %50 = bitcast i8* %49 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x42, {}* (i8*, i64)** %50, align 8
  %51 = getelementptr i8, i8* %49, i64 8
  %52 = bitcast i8* %51 to i8**
  store i8* %0, i8** %52, align 8
  %53 = icmp sgt i64 %47, %42
  br i1 %53, label %else_0, label %then_0
}

define {}* @go_x28(i8*, i64) {
  %3 = bitcast i8* %0 to { i1*, i64 }**
  %4 = load { i1*, i64 }*, { i1*, i64 }** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }**
  %7 = load { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %6, align 8
  %8 = getelementptr i8, i8* %0, i64 16
  %9 = bitcast i8* %8 to i64*
  %10 = load i64, i64* %9, align 8
  %11 = getelementptr i8, i8* %0, i64 24
  %12 = bitcast i8* %11 to i64*
  %13 = load i64, i64* %12, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x28, {}* (i8*, i64)** %15, align 8
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
  %22 = phi { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* [ %32, %else_0 ], [ %7, %2 ]
  %23 = phi { i1*, i64 }* [ %31, %else_0 ], [ %4, %2 ]
  %.tr23 = phi i64 [ %30, %else_0 ], [ %1, %2 ]
  %24 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %22, i64 0, i32 0
  %25 = load i1 (i8*, { i1*, i64 }*, i64, i64)*, i1 (i8*, { i1*, i64 }*, i64, i64)** %24, align 8
  %26 = getelementptr { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }* %22, i64 0, i32 1
  %27 = load i8*, i8** %26, align 8
  %28 = tail call i1 %25(i8* %27, { i1*, i64 }* %23, i64 %.tr23, i64 %21)
  %..i = select i1 %28, i8 35, i8 95
  %29 = tail call {}* @print_char(i8 %..i)
  %30 = add i64 %.tr23, 1
  %31 = load { i1*, i64 }*, { i1*, i64 }** %3, align 8
  %32 = load { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }*, { i1 (i8*, { i1*, i64 }*, i64, i64)*, i8* }** %6, align 8
  %33 = load i64, i64* %9, align 8
  %34 = load i64, i64* %12, align 8
  %35 = tail call i8* @GC_malloc(i64 16)
  %36 = bitcast i8* %35 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @go_x28, {}* (i8*, i64)** %36, align 8
  %37 = getelementptr i8, i8* %35, i64 8
  %38 = bitcast i8* %37 to i8**
  store i8* %0, i8** %38, align 8
  %39 = icmp sgt i64 %33, %30
  br i1 %39, label %else_0, label %then_0
}

declare {}* @gen_seed() local_unnamed_addr

define {}* @gen_seed2() local_unnamed_addr {
  %1 = tail call {}* @gen_seed()
  ret {}* %1
}

declare { i1*, i64 }* @copy_bool_array({ i1*, i64 }*) local_unnamed_addr

define { i1*, i64 }* @copy_bool_array5({ i1*, i64 }*) local_unnamed_addr {
  %2 = tail call { i1*, i64 }* @copy_bool_array({ i1*, i64 }* %0)
  ret { i1*, i64 }* %2
}

declare void @GC_init() local_unnamed_addr

define i32 @main() local_unnamed_addr {
  tail call void @GC_init()
  %1 = tail call i8* @GC_malloc(i64 1000)
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to i1**
  %4 = bitcast i8* %2 to i8**
  store i8* %1, i8** %4, align 8
  %5 = getelementptr i8, i8* %2, i64 8
  %6 = bitcast i8* %5 to i64*
  store i64 1000, i64* %6, align 8
  %7 = bitcast i8* %1 to i1*
  store i1 false, i1* %7, align 1
  br label %body_0.body_0_crit_edge

body_0.body_0_crit_edge:                          ; preds = %body_0.body_0_crit_edge, %0
  %8 = phi i64 [ 1, %0 ], [ %14, %body_0.body_0_crit_edge ]
  %.pre = load i1*, i1** %3, align 8
  %9 = getelementptr i1, i1* %.pre, i64 %8
  store i1 false, i1* %9, align 1
  %10 = add nuw nsw i64 %8, 1
  %.pre.1 = load i1*, i1** %3, align 8
  %11 = getelementptr i1, i1* %.pre.1, i64 %10
  store i1 false, i1* %11, align 1
  %12 = add nuw nsw i64 %8, 2
  %.pre.2 = load i1*, i1** %3, align 8
  %13 = getelementptr i1, i1* %.pre.2, i64 %12
  store i1 false, i1* %13, align 1
  %14 = add nuw nsw i64 %8, 3
  %exitcond.2 = icmp eq i64 %14, 1000
  br i1 %exitcond.2, label %end_0, label %body_0.body_0_crit_edge

end_0:                                            ; preds = %body_0.body_0_crit_edge
  %15 = bitcast i8* %2 to { i1*, i64 }*
  %16 = tail call i8* @GC_malloc(i64 16)
  %17 = bitcast i8* %16 to <2 x i64>*
  store <2 x i64> <i64 50, i64 20>, <2 x i64>* %17, align 8
  %18 = tail call i8* @GC_malloc(i64 16)
  %19 = bitcast i8* %18 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view10, i1 (i8*, { i1*, i64 }*, i64, i64)** %19, align 8
  %20 = getelementptr i8, i8* %18, i64 8
  %21 = bitcast i8* %20 to i8**
  store i8* %16, i8** %21, align 8
  %22 = tail call i8* @GC_malloc(i64 8)
  %23 = bitcast i8* %22 to i64*
  store i64 50, i64* %23, align 8
  %24 = tail call i8* @GC_malloc(i64 16)
  %25 = bitcast i8* %24 to {}* (i8*, { i1*, i64 }*, i64, i64, i1)**
  store {}* (i8*, { i1*, i64 }*, i64, i64, i1)* @set14, {}* (i8*, { i1*, i64 }*, i64, i64, i1)** %25, align 8
  %26 = getelementptr i8, i8* %24, i64 8
  %27 = bitcast i8* %26 to i8**
  store i8* %22, i8** %27, align 8
  %28 = tail call i8* @GC_malloc(i64 16)
  %29 = bitcast i8* %28 to <2 x i64>*
  store <2 x i64> <i64 50, i64 20>, <2 x i64>* %29, align 8
  %30 = tail call i8* @GC_malloc(i64 16)
  %31 = bitcast i8* %30 to {}* (i8*, { i1*, i64 }*, i64)**
  store {}* (i8*, { i1*, i64 }*, i64)* @init_cells19, {}* (i8*, { i1*, i64 }*, i64)** %31, align 8
  %32 = getelementptr i8, i8* %30, i64 8
  %33 = bitcast i8* %32 to i8**
  store i8* %28, i8** %33, align 8
  %34 = tail call i8* @GC_malloc(i64 24)
  %35 = bitcast i8* %34 to i8**
  store i8* %18, i8** %35, align 8
  %36 = getelementptr i8, i8* %34, i64 8
  %37 = bitcast i8* %36 to <2 x i64>*
  store <2 x i64> <i64 50, i64 20>, <2 x i64>* %37, align 8
  %38 = tail call i8* @GC_malloc(i64 16)
  %39 = bitcast i8* %38 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @print_cells24, {}* (i8*, { i1*, i64 }*)** %39, align 8
  %40 = getelementptr i8, i8* %38, i64 8
  %41 = bitcast i8* %40 to i8**
  store i8* %34, i8** %41, align 8
  %42 = tail call i8* @GC_malloc(i64 8)
  %43 = bitcast i8* %42 to i8**
  store i8* %18, i8** %43, align 8
  %44 = tail call i8* @GC_malloc(i64 16)
  %45 = bitcast i8* %44 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @next_state32, i1 (i8*, { i1*, i64 }*, i64, i64)** %45, align 8
  %46 = getelementptr i8, i8* %44, i64 8
  %47 = bitcast i8* %46 to i8**
  store i8* %42, i8** %47, align 8
  %48 = tail call i8* @GC_malloc(i64 32)
  %49 = bitcast i8* %48 to i8**
  store i8* %44, i8** %49, align 8
  %50 = getelementptr i8, i8* %48, i64 8
  %51 = bitcast i8* %50 to i8**
  store i8* %24, i8** %51, align 8
  %52 = getelementptr i8, i8* %48, i64 16
  %53 = bitcast i8* %52 to <2 x i64>*
  store <2 x i64> <i64 50, i64 20>, <2 x i64>* %53, align 8
  %54 = tail call i8* @GC_malloc(i64 16)
  %55 = bitcast i8* %54 to {}* (i8*, { i1*, i64 }*)**
  store {}* (i8*, { i1*, i64 }*)* @update_cells37, {}* (i8*, { i1*, i64 }*)** %55, align 8
  %56 = getelementptr i8, i8* %54, i64 8
  %57 = bitcast i8* %56 to i8**
  store i8* %48, i8** %57, align 8
  %58 = tail call {}* @gen_seed()
  %59 = tail call {}* @pulsar({ i1*, i64 }* %15)
  %60 = tail call i8* @GC_malloc(i64 24)
  %61 = bitcast i8* %60 to i8**
  store i8* %2, i8** %61, align 8
  %62 = getelementptr i8, i8* %60, i64 8
  %63 = bitcast i8* %62 to i8**
  store i8* %38, i8** %63, align 8
  %64 = getelementptr i8, i8* %60, i64 16
  %65 = bitcast i8* %64 to i8**
  store i8* %54, i8** %65, align 8
  %66 = tail call i8* @GC_malloc(i64 16)
  %67 = bitcast i8* %66 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop44, {}* (i8*, i64)** %67, align 8
  %68 = getelementptr i8, i8* %66, i64 8
  %69 = bitcast i8* %68 to i8**
  store i8* %60, i8** %69, align 8
  %70 = bitcast i8* %60 to { i1*, i64 }**
  %71 = load { i1*, i64 }*, { i1*, i64 }** %70, align 8
  %72 = bitcast i8* %62 to { {}* (i8*, { i1*, i64 }*)*, i8* }**
  %73 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %72, align 8
  %74 = bitcast i8* %64 to { {}* (i8*, { i1*, i64 }*)*, i8* }**
  %75 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %74, align 8
  %76 = tail call i8* @GC_malloc(i64 16)
  %77 = bitcast i8* %76 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop44, {}* (i8*, i64)** %77, align 8
  %78 = getelementptr i8, i8* %76, i64 8
  %79 = bitcast i8* %78 to i8**
  store i8* %60, i8** %79, align 8
  br label %then_0.i

then_0.i:                                         ; preds = %then_0.i, %end_0
  %80 = phi { {}* (i8*, { i1*, i64 }*)*, i8* }* [ %97, %then_0.i ], [ %75, %end_0 ]
  %81 = phi { {}* (i8*, { i1*, i64 }*)*, i8* }* [ %96, %then_0.i ], [ %73, %end_0 ]
  %82 = phi { i1*, i64 }* [ %95, %then_0.i ], [ %71, %end_0 ]
  %.tr23.i = phi i64 [ %94, %then_0.i ], [ 10, %end_0 ]
  %83 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %81, i64 0, i32 0
  %84 = load {}* (i8*, { i1*, i64 }*)*, {}* (i8*, { i1*, i64 }*)** %83, align 8
  %85 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %81, i64 0, i32 1
  %86 = load i8*, i8** %85, align 8
  %87 = tail call {}* %84(i8* %86, { i1*, i64 }* %82)
  %88 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %80, i64 0, i32 0
  %89 = load {}* (i8*, { i1*, i64 }*)*, {}* (i8*, { i1*, i64 }*)** %88, align 8
  %90 = getelementptr { {}* (i8*, { i1*, i64 }*)*, i8* }, { {}* (i8*, { i1*, i64 }*)*, i8* }* %80, i64 0, i32 1
  %91 = load i8*, i8** %90, align 8
  %92 = tail call {}* %89(i8* %91, { i1*, i64 }* %82)
  %93 = tail call {}* @newline()
  %94 = add nsw i64 %.tr23.i, -1
  %95 = load { i1*, i64 }*, { i1*, i64 }** %70, align 8
  %96 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %72, align 8
  %97 = load { {}* (i8*, { i1*, i64 }*)*, i8* }*, { {}* (i8*, { i1*, i64 }*)*, i8* }** %74, align 8
  %98 = tail call i8* @GC_malloc(i64 16)
  %99 = bitcast i8* %98 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop44, {}* (i8*, i64)** %99, align 8
  %100 = getelementptr i8, i8* %98, i64 8
  %101 = bitcast i8* %100 to i8**
  store i8* %60, i8** %101, align 8
  %102 = icmp eq i64 %94, 0
  br i1 %102, label %loop44.exit, label %then_0.i

loop44.exit:                                      ; preds = %then_0.i
  %103 = tail call i8* @GC_malloc(i64 0)
  ret i32 0
}

attributes #0 = { norecurse nounwind readnone }
