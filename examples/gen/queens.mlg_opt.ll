; ModuleID = './examples/gen/queens.mlg.ll'
source_filename = "./examples/queens.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

@"$globle_str_353" = unnamed_addr constant [3 x i8] c" O\00"
@"$globle_str_354" = unnamed_addr constant [3 x i8] c" .\00"
@"$globle_str_355" = unnamed_addr constant [2 x i8] c"\0A\00"
@"$globle_str_356" = unnamed_addr constant [2 x i8] c"\0A\00"

declare i8* @GC_malloc(i64) local_unnamed_addr

define {}* @try11(i8*, i64) {
  %3 = bitcast i8* %0 to <2 x i64>*
  %4 = load <2 x i64>, <2 x i64>* %3, align 8
  %5 = getelementptr i8, i8* %0, i64 16
  %6 = bitcast i8* %5 to <2 x i64>*
  %7 = load <2 x i64>, <2 x i64>* %6, align 8
  %8 = getelementptr i8, i8* %0, i64 32
  %9 = bitcast i8* %8 to { {}* (i8*)*, i8* }**
  %10 = load { {}* (i8*)*, i8* }*, { {}* (i8*)*, i8* }** %9, align 8
  %11 = getelementptr i8, i8* %0, i64 40
  %12 = bitcast i8* %11 to i64*
  %13 = load i64, i64* %12, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @try11, {}* (i8*, i64)** %15, align 8
  %16 = getelementptr i8, i8* %14, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %0, i8** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 56)
  %19 = bitcast i8* %18 to i64*
  store i64 %1, i64* %19, align 8
  %20 = getelementptr i8, i8* %18, i64 8
  %21 = bitcast i8* %20 to <2 x i64>*
  store <2 x i64> %4, <2 x i64>* %21, align 8
  %22 = getelementptr i8, i8* %18, i64 24
  %23 = bitcast i8* %22 to <2 x i64>*
  store <2 x i64> %7, <2 x i64>* %23, align 8
  %24 = getelementptr i8, i8* %18, i64 40
  %25 = bitcast i8* %24 to i64*
  store i64 %13, i64* %25, align 8
  %26 = getelementptr i8, i8* %18, i64 48
  %27 = bitcast i8* %26 to i8**
  store i8* %14, i8** %27, align 8
  %28 = tail call i8* @GC_malloc(i64 16)
  %29 = bitcast i8* %28 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop13, {}* (i8*, i64)** %29, align 8
  %30 = getelementptr i8, i8* %28, i64 8
  %31 = bitcast i8* %30 to i8**
  store i8* %18, i8** %31, align 8
  %32 = extractelement <2 x i64> %7, i32 1
  %33 = icmp eq i64 %32, %1
  br i1 %33, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %34 = getelementptr { {}* (i8*)*, i8* }, { {}* (i8*)*, i8* }* %10, i64 0, i32 0
  %35 = load {}* (i8*)*, {}* (i8*)** %34, align 8
  %36 = getelementptr { {}* (i8*)*, i8* }, { {}* (i8*)*, i8* }* %10, i64 0, i32 1
  %37 = load i8*, i8** %36, align 8
  %38 = tail call {}* %35(i8* %37)
  br label %end_0

else_0:                                           ; preds = %2
  %39 = tail call {}* @loop13(i8* %18, i64 0)
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %.0 = phi {}* [ %38, %then_0 ], [ %39, %else_0 ]
  ret {}* %.0
}

define {}* @printboard6(i8*) {
  %2 = bitcast i8* %0 to <2 x i64>*
  %3 = load <2 x i64>, <2 x i64>* %2, align 8
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to {}* (i8*)**
  store {}* (i8*)* @printboard6, {}* (i8*)** %5, align 8
  %6 = getelementptr i8, i8* %4, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to <2 x i64>*
  store <2 x i64> %3, <2 x i64>* %9, align 8
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loopi7, {}* (i8*, i64)** %11, align 8
  %12 = getelementptr i8, i8* %10, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %8, i8** %13, align 8
  %14 = tail call {}* @loopi7(i8* %8, i64 0)
  ret {}* %14
}

declare {}* @print(i8*) local_unnamed_addr

define {}* @print0(i8*) local_unnamed_addr {
  %2 = tail call {}* @print(i8* %0)
  ret {}* %2
}

define {}* @loopj9(i8*, i64) {
  %3 = bitcast i8* %0 to { i64*, i64 }**
  %4 = load { i64*, i64 }*, { i64*, i64 }** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i64*
  %7 = load i64, i64* %6, align 8
  %8 = getelementptr i8, i8* %0, i64 16
  %9 = bitcast i8* %8 to i64*
  %10 = load i64, i64* %9, align 8
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loopj9, {}* (i8*, i64)** %12, align 8
  %13 = getelementptr i8, i8* %11, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %0, i8** %14, align 8
  %15 = icmp sgt i64 %10, %1
  br i1 %15, label %then_0, label %else_1

then_0:                                           ; preds = %2, %then_0
  %16 = phi i64 [ %26, %then_0 ], [ %7, %2 ]
  %17 = phi { i64*, i64 }* [ %25, %then_0 ], [ %4, %2 ]
  %.tr23 = phi i64 [ %24, %then_0 ], [ %1, %2 ]
  %18 = getelementptr { i64*, i64 }, { i64*, i64 }* %17, i64 0, i32 0
  %19 = load i64*, i64** %18, align 8
  %20 = getelementptr i64, i64* %19, i64 %16
  %21 = load i64, i64* %20, align 8
  %22 = icmp eq i64 %21, %.tr23
  %. = select i1 %22, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @"$globle_str_353", i64 0, i64 0), i8* getelementptr inbounds ([3 x i8], [3 x i8]* @"$globle_str_354", i64 0, i64 0)
  %23 = tail call {}* @print(i8* %.)
  %24 = add i64 %.tr23, 1
  %25 = load { i64*, i64 }*, { i64*, i64 }** %3, align 8
  %26 = load i64, i64* %6, align 8
  %27 = load i64, i64* %9, align 8
  %28 = tail call i8* @GC_malloc(i64 16)
  %29 = bitcast i8* %28 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loopj9, {}* (i8*, i64)** %29, align 8
  %30 = getelementptr i8, i8* %28, i64 8
  %31 = bitcast i8* %30 to i8**
  store i8* %0, i8** %31, align 8
  %32 = icmp sgt i64 %27, %24
  br i1 %32, label %then_0, label %else_1

else_1:                                           ; preds = %then_0, %2
  %33 = tail call {}* @print(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @"$globle_str_355", i64 0, i64 0))
  ret {}* %33
}

define {}* @loopi7(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i64*
  %7 = load i64, i64* %6, align 8
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loopi7, {}* (i8*, i64)** %9, align 8
  %10 = getelementptr i8, i8* %8, i64 8
  %11 = bitcast i8* %10 to i8**
  store i8* %0, i8** %11, align 8
  %12 = icmp sgt i64 %7, %1
  br i1 %12, label %then_0, label %else_0

then_0:                                           ; preds = %2, %loopj9.exit
  %13 = phi i64 [ %54, %loopj9.exit ], [ %7, %2 ]
  %14 = phi i64 [ %53, %loopj9.exit ], [ %4, %2 ]
  %.tr34 = phi i64 [ %52, %loopj9.exit ], [ %1, %2 ]
  %15 = tail call i8* @GC_malloc(i64 24)
  %16 = bitcast i8* %15 to i64*
  store i64 %14, i64* %16, align 8
  %17 = getelementptr i8, i8* %15, i64 8
  %18 = bitcast i8* %17 to i64*
  store i64 %.tr34, i64* %18, align 8
  %19 = getelementptr i8, i8* %15, i64 16
  %20 = bitcast i8* %19 to i64*
  store i64 %13, i64* %20, align 8
  %21 = tail call i8* @GC_malloc(i64 16)
  %22 = bitcast i8* %21 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loopj9, {}* (i8*, i64)** %22, align 8
  %23 = getelementptr i8, i8* %21, i64 8
  %24 = bitcast i8* %23 to i8**
  store i8* %15, i8** %24, align 8
  %25 = bitcast i8* %15 to { i64*, i64 }**
  %26 = load { i64*, i64 }*, { i64*, i64 }** %25, align 8
  %27 = load i64, i64* %18, align 8
  %28 = load i64, i64* %20, align 8
  %29 = tail call i8* @GC_malloc(i64 16)
  %30 = bitcast i8* %29 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loopj9, {}* (i8*, i64)** %30, align 8
  %31 = getelementptr i8, i8* %29, i64 8
  %32 = bitcast i8* %31 to i8**
  store i8* %15, i8** %32, align 8
  %33 = icmp sgt i64 %28, 0
  br i1 %33, label %then_0.i, label %loopj9.exit

then_0.i:                                         ; preds = %then_0, %then_0.i
  %34 = phi i64 [ %44, %then_0.i ], [ %27, %then_0 ]
  %35 = phi { i64*, i64 }* [ %43, %then_0.i ], [ %26, %then_0 ]
  %.tr23.i = phi i64 [ %42, %then_0.i ], [ 0, %then_0 ]
  %36 = getelementptr { i64*, i64 }, { i64*, i64 }* %35, i64 0, i32 0
  %37 = load i64*, i64** %36, align 8
  %38 = getelementptr i64, i64* %37, i64 %34
  %39 = load i64, i64* %38, align 8
  %40 = icmp eq i64 %39, %.tr23.i
  %..i = select i1 %40, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @"$globle_str_353", i64 0, i64 0), i8* getelementptr inbounds ([3 x i8], [3 x i8]* @"$globle_str_354", i64 0, i64 0)
  %41 = tail call {}* @print(i8* %..i)
  %42 = add i64 %.tr23.i, 1
  %43 = load { i64*, i64 }*, { i64*, i64 }** %25, align 8
  %44 = load i64, i64* %18, align 8
  %45 = load i64, i64* %20, align 8
  %46 = tail call i8* @GC_malloc(i64 16)
  %47 = bitcast i8* %46 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loopj9, {}* (i8*, i64)** %47, align 8
  %48 = getelementptr i8, i8* %46, i64 8
  %49 = bitcast i8* %48 to i8**
  store i8* %15, i8** %49, align 8
  %50 = icmp sgt i64 %45, %42
  br i1 %50, label %then_0.i, label %loopj9.exit

loopj9.exit:                                      ; preds = %then_0.i, %then_0
  %51 = tail call {}* @print(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @"$globle_str_355", i64 0, i64 0))
  %52 = add i64 %.tr34, 1
  %53 = load i64, i64* %3, align 8
  %54 = load i64, i64* %6, align 8
  %55 = tail call i8* @GC_malloc(i64 16)
  %56 = bitcast i8* %55 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loopi7, {}* (i8*, i64)** %56, align 8
  %57 = getelementptr i8, i8* %55, i64 8
  %58 = bitcast i8* %57 to i8**
  store i8* %0, i8** %58, align 8
  %59 = icmp sgt i64 %54, %52
  br i1 %59, label %then_0, label %else_0

else_0:                                           ; preds = %loopj9.exit, %2
  %60 = tail call {}* @print(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @"$globle_str_356", i64 0, i64 0))
  ret {}* %60
}

define {}* @loop13(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to { i64*, i64 }**
  %7 = load { i64*, i64 }*, { i64*, i64 }** %6, align 8
  %8 = getelementptr i8, i8* %0, i64 16
  %9 = bitcast i8* %8 to { i64*, i64 }**
  %10 = load { i64*, i64 }*, { i64*, i64 }** %9, align 8
  %11 = getelementptr i8, i8* %0, i64 24
  %12 = bitcast i8* %11 to { i64*, i64 }**
  %13 = load { i64*, i64 }*, { i64*, i64 }** %12, align 8
  %14 = getelementptr i8, i8* %0, i64 32
  %15 = bitcast i8* %14 to i64*
  %16 = load i64, i64* %15, align 8
  %17 = getelementptr i8, i8* %0, i64 40
  %18 = bitcast i8* %17 to { i64*, i64 }**
  %19 = load { i64*, i64 }*, { i64*, i64 }** %18, align 8
  %20 = getelementptr i8, i8* %0, i64 48
  %21 = bitcast i8* %20 to { {}* (i8*, i64)*, i8* }**
  %22 = load { {}* (i8*, i64)*, i8* }*, { {}* (i8*, i64)*, i8* }** %21, align 8
  %23 = tail call i8* @GC_malloc(i64 16)
  %24 = bitcast i8* %23 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop13, {}* (i8*, i64)** %24, align 8
  %25 = getelementptr i8, i8* %23, i64 8
  %26 = bitcast i8* %25 to i8**
  store i8* %0, i8** %26, align 8
  %27 = icmp sgt i64 %16, %1
  br i1 %27, label %then_0, label %else_1

then_0:                                           ; preds = %2, %tailrecurse.backedge
  %28 = phi { {}* (i8*, i64)*, i8* }* [ %77, %tailrecurse.backedge ], [ %22, %2 ]
  %29 = phi { i64*, i64 }* [ %76, %tailrecurse.backedge ], [ %19, %2 ]
  %30 = phi { i64*, i64 }* [ %74, %tailrecurse.backedge ], [ %13, %2 ]
  %31 = phi { i64*, i64 }* [ %73, %tailrecurse.backedge ], [ %10, %2 ]
  %32 = phi { i64*, i64 }* [ %72, %tailrecurse.backedge ], [ %7, %2 ]
  %33 = phi i64 [ %71, %tailrecurse.backedge ], [ %4, %2 ]
  %.tr23 = phi i64 [ %.tr2.be, %tailrecurse.backedge ], [ %1, %2 ]
  %34 = getelementptr { i64*, i64 }, { i64*, i64 }* %29, i64 0, i32 0
  %35 = load i64*, i64** %34, align 8
  %36 = getelementptr i64, i64* %35, i64 %.tr23
  %37 = load i64, i64* %36, align 8
  %38 = add i64 %33, %.tr23
  %39 = getelementptr { i64*, i64 }, { i64*, i64 }* %31, i64 0, i32 0
  %40 = load i64*, i64** %39, align 8
  %41 = getelementptr i64, i64* %40, i64 %38
  %42 = load i64, i64* %41, align 8
  %43 = or i64 %42, %37
  %44 = add i64 %.tr23, 7
  %45 = sub i64 %44, %33
  %46 = getelementptr { i64*, i64 }, { i64*, i64 }* %30, i64 0, i32 0
  %47 = load i64*, i64** %46, align 8
  %48 = getelementptr i64, i64* %47, i64 %45
  %49 = load i64, i64* %48, align 8
  %50 = or i64 %43, %49
  %51 = icmp eq i64 %50, 0
  br i1 %51, label %then_1, label %tailrecurse.backedge

then_1:                                           ; preds = %then_0
  store i64 1, i64* %36, align 8
  %52 = load i64*, i64** %39, align 8
  %53 = getelementptr i64, i64* %52, i64 %38
  store i64 1, i64* %53, align 8
  %54 = load i64*, i64** %46, align 8
  %55 = getelementptr i64, i64* %54, i64 %45
  store i64 1, i64* %55, align 8
  %56 = getelementptr { i64*, i64 }, { i64*, i64 }* %32, i64 0, i32 0
  %57 = load i64*, i64** %56, align 8
  %58 = getelementptr i64, i64* %57, i64 %33
  store i64 %.tr23, i64* %58, align 8
  %59 = add i64 %33, 1
  %60 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %28, i64 0, i32 0
  %61 = load {}* (i8*, i64)*, {}* (i8*, i64)** %60, align 8
  %62 = getelementptr { {}* (i8*, i64)*, i8* }, { {}* (i8*, i64)*, i8* }* %28, i64 0, i32 1
  %63 = load i8*, i8** %62, align 8
  %64 = tail call {}* %61(i8* %63, i64 %59)
  %65 = load i64*, i64** %34, align 8
  %66 = getelementptr i64, i64* %65, i64 %.tr23
  store i64 0, i64* %66, align 8
  %67 = load i64*, i64** %39, align 8
  %68 = getelementptr i64, i64* %67, i64 %38
  store i64 0, i64* %68, align 8
  %69 = load i64*, i64** %46, align 8
  %70 = getelementptr i64, i64* %69, i64 %45
  store i64 0, i64* %70, align 8
  br label %tailrecurse.backedge

tailrecurse.backedge:                             ; preds = %then_0, %then_1
  %.tr2.be = add i64 %.tr23, 1
  %71 = load i64, i64* %3, align 8
  %72 = load { i64*, i64 }*, { i64*, i64 }** %6, align 8
  %73 = load { i64*, i64 }*, { i64*, i64 }** %9, align 8
  %74 = load { i64*, i64 }*, { i64*, i64 }** %12, align 8
  %75 = load i64, i64* %15, align 8
  %76 = load { i64*, i64 }*, { i64*, i64 }** %18, align 8
  %77 = load { {}* (i8*, i64)*, i8* }*, { {}* (i8*, i64)*, i8* }** %21, align 8
  %78 = tail call i8* @GC_malloc(i64 16)
  %79 = bitcast i8* %78 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop13, {}* (i8*, i64)** %79, align 8
  %80 = getelementptr i8, i8* %78, i64 8
  %81 = bitcast i8* %80 to i8**
  store i8* %0, i8** %81, align 8
  %82 = icmp sgt i64 %75, %.tr2.be
  br i1 %82, label %then_0, label %else_1

else_1:                                           ; preds = %tailrecurse.backedge, %2
  %83 = tail call i8* @GC_malloc(i64 0)
  %84 = bitcast i8* %83 to {}*
  ret {}* %84
}

declare void @GC_init() local_unnamed_addr

define i32 @main() local_unnamed_addr {
body_0:
  tail call void @GC_init()
  %0 = tail call i8* @GC_malloc(i64 64)
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to i64**
  %3 = bitcast i8* %1 to i8**
  store i8* %0, i8** %3, align 8
  %4 = getelementptr i8, i8* %1, i64 8
  %5 = bitcast i8* %4 to i64*
  store i64 8, i64* %5, align 8
  %6 = bitcast i8* %0 to i64*
  store i64 0, i64* %6, align 8
  %7 = load i64*, i64** %2, align 8
  %8 = getelementptr i64, i64* %7, i64 1
  store i64 0, i64* %8, align 8
  %9 = load i64*, i64** %2, align 8
  %10 = getelementptr i64, i64* %9, i64 2
  store i64 0, i64* %10, align 8
  %11 = load i64*, i64** %2, align 8
  %12 = getelementptr i64, i64* %11, i64 3
  store i64 0, i64* %12, align 8
  %13 = load i64*, i64** %2, align 8
  %14 = getelementptr i64, i64* %13, i64 4
  store i64 0, i64* %14, align 8
  %15 = load i64*, i64** %2, align 8
  %16 = getelementptr i64, i64* %15, i64 5
  store i64 0, i64* %16, align 8
  %17 = load i64*, i64** %2, align 8
  %18 = getelementptr i64, i64* %17, i64 6
  store i64 0, i64* %18, align 8
  %19 = load i64*, i64** %2, align 8
  %20 = getelementptr i64, i64* %19, i64 7
  store i64 0, i64* %20, align 8
  %21 = tail call i8* @GC_malloc(i64 64)
  %22 = tail call i8* @GC_malloc(i64 16)
  %23 = bitcast i8* %22 to i64**
  %24 = bitcast i8* %22 to i8**
  store i8* %21, i8** %24, align 8
  %25 = getelementptr i8, i8* %22, i64 8
  %26 = bitcast i8* %25 to i64*
  store i64 8, i64* %26, align 8
  %27 = bitcast i8* %21 to i64*
  store i64 0, i64* %27, align 8
  %28 = load i64*, i64** %23, align 8
  %29 = getelementptr i64, i64* %28, i64 1
  store i64 0, i64* %29, align 8
  %30 = load i64*, i64** %23, align 8
  %31 = getelementptr i64, i64* %30, i64 2
  store i64 0, i64* %31, align 8
  %32 = load i64*, i64** %23, align 8
  %33 = getelementptr i64, i64* %32, i64 3
  store i64 0, i64* %33, align 8
  %34 = load i64*, i64** %23, align 8
  %35 = getelementptr i64, i64* %34, i64 4
  store i64 0, i64* %35, align 8
  %36 = load i64*, i64** %23, align 8
  %37 = getelementptr i64, i64* %36, i64 5
  store i64 0, i64* %37, align 8
  %38 = load i64*, i64** %23, align 8
  %39 = getelementptr i64, i64* %38, i64 6
  store i64 0, i64* %39, align 8
  %40 = load i64*, i64** %23, align 8
  %41 = getelementptr i64, i64* %40, i64 7
  store i64 0, i64* %41, align 8
  %42 = tail call i8* @GC_malloc(i64 120)
  %43 = tail call i8* @GC_malloc(i64 16)
  %44 = bitcast i8* %43 to i64**
  %45 = bitcast i8* %43 to i8**
  store i8* %42, i8** %45, align 8
  %46 = getelementptr i8, i8* %43, i64 8
  %47 = bitcast i8* %46 to i64*
  store i64 15, i64* %47, align 8
  %48 = bitcast i8* %42 to i64*
  store i64 0, i64* %48, align 8
  %49 = load i64*, i64** %44, align 8
  %50 = getelementptr i64, i64* %49, i64 1
  store i64 0, i64* %50, align 8
  %51 = load i64*, i64** %44, align 8
  %52 = getelementptr i64, i64* %51, i64 2
  store i64 0, i64* %52, align 8
  %53 = load i64*, i64** %44, align 8
  %54 = getelementptr i64, i64* %53, i64 3
  store i64 0, i64* %54, align 8
  %55 = load i64*, i64** %44, align 8
  %56 = getelementptr i64, i64* %55, i64 4
  store i64 0, i64* %56, align 8
  %57 = load i64*, i64** %44, align 8
  %58 = getelementptr i64, i64* %57, i64 5
  store i64 0, i64* %58, align 8
  %59 = load i64*, i64** %44, align 8
  %60 = getelementptr i64, i64* %59, i64 6
  store i64 0, i64* %60, align 8
  %61 = load i64*, i64** %44, align 8
  %62 = getelementptr i64, i64* %61, i64 7
  store i64 0, i64* %62, align 8
  %63 = load i64*, i64** %44, align 8
  %64 = getelementptr i64, i64* %63, i64 8
  store i64 0, i64* %64, align 8
  %65 = load i64*, i64** %44, align 8
  %66 = getelementptr i64, i64* %65, i64 9
  store i64 0, i64* %66, align 8
  %67 = load i64*, i64** %44, align 8
  %68 = getelementptr i64, i64* %67, i64 10
  store i64 0, i64* %68, align 8
  %69 = load i64*, i64** %44, align 8
  %70 = getelementptr i64, i64* %69, i64 11
  store i64 0, i64* %70, align 8
  %71 = load i64*, i64** %44, align 8
  %72 = getelementptr i64, i64* %71, i64 12
  store i64 0, i64* %72, align 8
  %73 = load i64*, i64** %44, align 8
  %74 = getelementptr i64, i64* %73, i64 13
  store i64 0, i64* %74, align 8
  %75 = load i64*, i64** %44, align 8
  %76 = getelementptr i64, i64* %75, i64 14
  store i64 0, i64* %76, align 8
  %77 = tail call i8* @GC_malloc(i64 120)
  %78 = tail call i8* @GC_malloc(i64 16)
  %79 = bitcast i8* %78 to i64**
  %80 = bitcast i8* %78 to i8**
  store i8* %77, i8** %80, align 8
  %81 = getelementptr i8, i8* %78, i64 8
  %82 = bitcast i8* %81 to i64*
  store i64 15, i64* %82, align 8
  %83 = bitcast i8* %77 to i64*
  store i64 0, i64* %83, align 8
  %84 = load i64*, i64** %79, align 8
  %85 = getelementptr i64, i64* %84, i64 1
  store i64 0, i64* %85, align 8
  %86 = load i64*, i64** %79, align 8
  %87 = getelementptr i64, i64* %86, i64 2
  store i64 0, i64* %87, align 8
  %88 = load i64*, i64** %79, align 8
  %89 = getelementptr i64, i64* %88, i64 3
  store i64 0, i64* %89, align 8
  %90 = load i64*, i64** %79, align 8
  %91 = getelementptr i64, i64* %90, i64 4
  store i64 0, i64* %91, align 8
  %92 = load i64*, i64** %79, align 8
  %93 = getelementptr i64, i64* %92, i64 5
  store i64 0, i64* %93, align 8
  %94 = load i64*, i64** %79, align 8
  %95 = getelementptr i64, i64* %94, i64 6
  store i64 0, i64* %95, align 8
  %96 = load i64*, i64** %79, align 8
  %97 = getelementptr i64, i64* %96, i64 7
  store i64 0, i64* %97, align 8
  %98 = load i64*, i64** %79, align 8
  %99 = getelementptr i64, i64* %98, i64 8
  store i64 0, i64* %99, align 8
  %100 = load i64*, i64** %79, align 8
  %101 = getelementptr i64, i64* %100, i64 9
  store i64 0, i64* %101, align 8
  %102 = load i64*, i64** %79, align 8
  %103 = getelementptr i64, i64* %102, i64 10
  store i64 0, i64* %103, align 8
  %104 = load i64*, i64** %79, align 8
  %105 = getelementptr i64, i64* %104, i64 11
  store i64 0, i64* %105, align 8
  %106 = load i64*, i64** %79, align 8
  %107 = getelementptr i64, i64* %106, i64 12
  store i64 0, i64* %107, align 8
  %108 = load i64*, i64** %79, align 8
  %109 = getelementptr i64, i64* %108, i64 13
  store i64 0, i64* %109, align 8
  %110 = load i64*, i64** %79, align 8
  %111 = getelementptr i64, i64* %110, i64 14
  store i64 0, i64* %111, align 8
  %112 = tail call i8* @GC_malloc(i64 16)
  %113 = bitcast i8* %112 to i8**
  store i8* %22, i8** %113, align 8
  %114 = getelementptr i8, i8* %112, i64 8
  %115 = bitcast i8* %114 to i64*
  store i64 8, i64* %115, align 8
  %116 = tail call i8* @GC_malloc(i64 16)
  %117 = bitcast i8* %116 to {}* (i8*)**
  store {}* (i8*)* @printboard6, {}* (i8*)** %117, align 8
  %118 = getelementptr i8, i8* %116, i64 8
  %119 = bitcast i8* %118 to i8**
  store i8* %112, i8** %119, align 8
  %120 = tail call i8* @GC_malloc(i64 48)
  %121 = bitcast i8* %120 to i8**
  store i8* %22, i8** %121, align 8
  %122 = getelementptr i8, i8* %120, i64 8
  %123 = bitcast i8* %122 to i8**
  store i8* %43, i8** %123, align 8
  %124 = getelementptr i8, i8* %120, i64 16
  %125 = bitcast i8* %124 to i8**
  store i8* %78, i8** %125, align 8
  %126 = getelementptr i8, i8* %120, i64 24
  %127 = bitcast i8* %126 to i64*
  store i64 8, i64* %127, align 8
  %128 = getelementptr i8, i8* %120, i64 32
  %129 = bitcast i8* %128 to i8**
  store i8* %116, i8** %129, align 8
  %130 = getelementptr i8, i8* %120, i64 40
  %131 = bitcast i8* %130 to i8**
  store i8* %1, i8** %131, align 8
  %132 = tail call i8* @GC_malloc(i64 16)
  %133 = bitcast i8* %132 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @try11, {}* (i8*, i64)** %133, align 8
  %134 = getelementptr i8, i8* %132, i64 8
  %135 = bitcast i8* %134 to i8**
  store i8* %120, i8** %135, align 8
  %136 = bitcast i8* %120 to <2 x i64>*
  %137 = load <2 x i64>, <2 x i64>* %136, align 8
  %138 = bitcast i8* %124 to <2 x i64>*
  %139 = load <2 x i64>, <2 x i64>* %138, align 8
  %140 = bitcast i8* %128 to { {}* (i8*)*, i8* }**
  %141 = load { {}* (i8*)*, i8* }*, { {}* (i8*)*, i8* }** %140, align 8
  %142 = bitcast i8* %130 to i64*
  %143 = load i64, i64* %142, align 8
  %144 = tail call i8* @GC_malloc(i64 16)
  %145 = bitcast i8* %144 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @try11, {}* (i8*, i64)** %145, align 8
  %146 = getelementptr i8, i8* %144, i64 8
  %147 = bitcast i8* %146 to i8**
  store i8* %120, i8** %147, align 8
  %148 = tail call i8* @GC_malloc(i64 56)
  %149 = bitcast i8* %148 to i64*
  store i64 0, i64* %149, align 8
  %150 = getelementptr i8, i8* %148, i64 8
  %151 = bitcast i8* %150 to <2 x i64>*
  store <2 x i64> %137, <2 x i64>* %151, align 8
  %152 = getelementptr i8, i8* %148, i64 24
  %153 = bitcast i8* %152 to <2 x i64>*
  store <2 x i64> %139, <2 x i64>* %153, align 8
  %154 = getelementptr i8, i8* %148, i64 40
  %155 = bitcast i8* %154 to i64*
  store i64 %143, i64* %155, align 8
  %156 = getelementptr i8, i8* %148, i64 48
  %157 = bitcast i8* %156 to i8**
  store i8* %144, i8** %157, align 8
  %158 = tail call i8* @GC_malloc(i64 16)
  %159 = bitcast i8* %158 to {}* (i8*, i64)**
  store {}* (i8*, i64)* @loop13, {}* (i8*, i64)** %159, align 8
  %160 = getelementptr i8, i8* %158, i64 8
  %161 = bitcast i8* %160 to i8**
  store i8* %148, i8** %161, align 8
  %162 = extractelement <2 x i64> %139, i32 1
  %163 = icmp eq i64 %162, 0
  br i1 %163, label %then_0.i, label %else_0.i

then_0.i:                                         ; preds = %body_0
  %164 = getelementptr { {}* (i8*)*, i8* }, { {}* (i8*)*, i8* }* %141, i64 0, i32 0
  %165 = load {}* (i8*)*, {}* (i8*)** %164, align 8
  %166 = getelementptr { {}* (i8*)*, i8* }, { {}* (i8*)*, i8* }* %141, i64 0, i32 1
  %167 = load i8*, i8** %166, align 8
  %168 = tail call {}* %165(i8* %167)
  br label %try11.exit

else_0.i:                                         ; preds = %body_0
  %169 = tail call {}* @loop13(i8* %148, i64 0)
  br label %try11.exit

try11.exit:                                       ; preds = %then_0.i, %else_0.i
  ret i32 0
}
