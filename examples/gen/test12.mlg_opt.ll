; ModuleID = './examples/gen/test12.mlg.ll'
source_filename = "./examples/test12.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i1 @view6(i8*, { i1*, i64 }* nocapture readonly, i64, i64) {
  %5 = bitcast i8* %0 to i64*
  %6 = load i64, i64* %5, align 8
  %7 = getelementptr i8, i8* %0, i64 8
  %8 = bitcast i8* %7 to i64*
  %9 = load i64, i64* %8, align 8
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view6, i1 (i8*, { i1*, i64 }*, i64, i64)** %11, align 8
  %12 = getelementptr i8, i8* %10, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %0, i8** %13, align 8
  %14 = icmp sle i64 %6, %2
  %15 = icmp sle i64 %9, %3
  %16 = or i1 %14, %15
  %17 = or i64 %3, %2
  %18 = icmp slt i64 %17, 0
  %19 = or i1 %18, %16
  br i1 %19, label %end_2, label %else_0

else_0:                                           ; preds = %4
  %20 = mul i64 %6, %3
  %21 = add i64 %20, %2
  %22 = getelementptr { i1*, i64 }, { i1*, i64 }* %1, i64 0, i32 0
  %23 = load i1*, i1** %22, align 8
  %24 = getelementptr { i1*, i64 }, { i1*, i64 }* %1, i64 0, i32 1
  %25 = load i64, i64* %24, align 8
  %26 = shl i64 %25, 3
  %27 = tail call i8* @GC_malloc(i64 %26)
  %28 = tail call i8* @GC_malloc(i64 16)
  %29 = bitcast i8* %28 to i8**
  store i8* %27, i8** %29, align 8
  %30 = getelementptr i8, i8* %28, i64 8
  %31 = bitcast i8* %30 to i64*
  %.cast = bitcast i8* %27 to i8**
  store i64 %25, i64* %31, align 8
  %32 = icmp sgt i64 %25, 0
  br i1 %32, label %body_0.preheader, label %end_0

body_0.preheader:                                 ; preds = %else_0
  %33 = add i64 %25, -1
  %xtraiter10 = and i64 %25, 3
  %34 = icmp ult i64 %33, 3
  br i1 %34, label %end_0.loopexit.unr-lcssa, label %body_0.preheader.new

body_0.preheader.new:                             ; preds = %body_0.preheader
  %unroll_iter13 = sub i64 %25, %xtraiter10
  br label %body_0

body_0:                                           ; preds = %body_0, %body_0.preheader.new
  %storemerge46 = phi i64 [ 0, %body_0.preheader.new ], [ %58, %body_0 ]
  %niter14 = phi i64 [ %unroll_iter13, %body_0.preheader.new ], [ %niter14.nsub.3, %body_0 ]
  %35 = getelementptr i1, i1* %23, i64 %storemerge46
  %36 = load i1, i1* %35, align 1
  %37 = zext i1 %36 to i64
  %38 = inttoptr i64 %37 to i8*
  %39 = getelementptr i8*, i8** %.cast, i64 %storemerge46
  store i8* %38, i8** %39, align 8
  %40 = or i64 %storemerge46, 1
  %41 = getelementptr i1, i1* %23, i64 %40
  %42 = load i1, i1* %41, align 1
  %43 = zext i1 %42 to i64
  %44 = inttoptr i64 %43 to i8*
  %45 = getelementptr i8*, i8** %.cast, i64 %40
  store i8* %44, i8** %45, align 8
  %46 = or i64 %storemerge46, 2
  %47 = getelementptr i1, i1* %23, i64 %46
  %48 = load i1, i1* %47, align 1
  %49 = zext i1 %48 to i64
  %50 = inttoptr i64 %49 to i8*
  %51 = getelementptr i8*, i8** %.cast, i64 %46
  store i8* %50, i8** %51, align 8
  %52 = or i64 %storemerge46, 3
  %53 = getelementptr i1, i1* %23, i64 %52
  %54 = load i1, i1* %53, align 1
  %55 = zext i1 %54 to i64
  %56 = inttoptr i64 %55 to i8*
  %57 = getelementptr i8*, i8** %.cast, i64 %52
  store i8* %56, i8** %57, align 8
  %58 = add nuw nsw i64 %storemerge46, 4
  %niter14.nsub.3 = add i64 %niter14, -4
  %niter14.ncmp.3 = icmp eq i64 %niter14.nsub.3, 0
  br i1 %niter14.ncmp.3, label %end_0.loopexit.unr-lcssa, label %body_0

end_0.loopexit.unr-lcssa:                         ; preds = %body_0, %body_0.preheader
  %storemerge46.unr = phi i64 [ 0, %body_0.preheader ], [ %58, %body_0 ]
  %lcmp.mod12 = icmp eq i64 %xtraiter10, 0
  br i1 %lcmp.mod12, label %end_0.loopexit, label %body_0.epil

body_0.epil:                                      ; preds = %end_0.loopexit.unr-lcssa, %body_0.epil
  %storemerge46.epil = phi i64 [ %64, %body_0.epil ], [ %storemerge46.unr, %end_0.loopexit.unr-lcssa ]
  %epil.iter11 = phi i64 [ %epil.iter11.sub, %body_0.epil ], [ %xtraiter10, %end_0.loopexit.unr-lcssa ]
  %59 = getelementptr i1, i1* %23, i64 %storemerge46.epil
  %60 = load i1, i1* %59, align 1
  %61 = zext i1 %60 to i64
  %62 = inttoptr i64 %61 to i8*
  %63 = getelementptr i8*, i8** %.cast, i64 %storemerge46.epil
  store i8* %62, i8** %63, align 8
  %64 = add nuw nsw i64 %storemerge46.epil, 1
  %epil.iter11.sub = add i64 %epil.iter11, -1
  %epil.iter11.cmp = icmp eq i64 %epil.iter11.sub, 0
  br i1 %epil.iter11.cmp, label %end_0.loopexit, label %body_0.epil, !llvm.loop !0

end_0.loopexit:                                   ; preds = %body_0.epil, %end_0.loopexit.unr-lcssa
  %.phi.trans.insert = bitcast i8* %28 to i8***
  %.pre = load i8**, i8*** %.phi.trans.insert, align 8
  %.pre8 = load i64, i64* %31, align 8
  br label %end_0

end_0:                                            ; preds = %else_0, %end_0.loopexit
  %65 = phi i64 [ %.pre8, %end_0.loopexit ], [ %25, %else_0 ]
  %66 = phi i8** [ %.pre, %end_0.loopexit ], [ %.cast, %else_0 ]
  %67 = getelementptr i8*, i8** %66, i64 %21
  %68 = bitcast i8** %67 to i64*
  %69 = load i64, i64* %68, align 8
  %70 = tail call i8* @GC_malloc(i64 %65)
  %71 = tail call i8* @GC_malloc(i64 16)
  %72 = bitcast i8* %71 to i8**
  store i8* %70, i8** %72, align 8
  %73 = getelementptr i8, i8* %71, i64 8
  %74 = bitcast i8* %73 to i64*
  store i64 %65, i64* %74, align 8
  %75 = icmp sgt i64 %65, 0
  br i1 %75, label %body_1.preheader, label %end_1

body_1.preheader:                                 ; preds = %end_0
  %76 = add i64 %65, -1
  %xtraiter = and i64 %65, 3
  %77 = icmp ult i64 %76, 3
  br i1 %77, label %end_1.loopexit.unr-lcssa, label %body_1.preheader.new

body_1.preheader.new:                             ; preds = %body_1.preheader
  %unroll_iter = sub i64 %65, %xtraiter
  br label %body_1

body_1:                                           ; preds = %body_1, %body_1.preheader.new
  %storemerge5 = phi i64 [ 0, %body_1.preheader.new ], [ %109, %body_1 ]
  %niter = phi i64 [ %unroll_iter, %body_1.preheader.new ], [ %niter.nsub.3, %body_1 ]
  %78 = getelementptr i8*, i8** %66, i64 %storemerge5
  %79 = bitcast i8** %78 to i64*
  %80 = load i64, i64* %79, align 8
  %81 = and i64 %80, 1
  %82 = icmp ne i64 %81, 0
  %83 = getelementptr i8, i8* %70, i64 %storemerge5
  %84 = bitcast i8* %83 to i1*
  store i1 %82, i1* %84, align 1
  %85 = or i64 %storemerge5, 1
  %86 = getelementptr i8*, i8** %66, i64 %85
  %87 = bitcast i8** %86 to i64*
  %88 = load i64, i64* %87, align 8
  %89 = and i64 %88, 1
  %90 = icmp ne i64 %89, 0
  %91 = getelementptr i8, i8* %70, i64 %85
  %92 = bitcast i8* %91 to i1*
  store i1 %90, i1* %92, align 1
  %93 = or i64 %storemerge5, 2
  %94 = getelementptr i8*, i8** %66, i64 %93
  %95 = bitcast i8** %94 to i64*
  %96 = load i64, i64* %95, align 8
  %97 = and i64 %96, 1
  %98 = icmp ne i64 %97, 0
  %99 = getelementptr i8, i8* %70, i64 %93
  %100 = bitcast i8* %99 to i1*
  store i1 %98, i1* %100, align 1
  %101 = or i64 %storemerge5, 3
  %102 = getelementptr i8*, i8** %66, i64 %101
  %103 = bitcast i8** %102 to i64*
  %104 = load i64, i64* %103, align 8
  %105 = and i64 %104, 1
  %106 = icmp ne i64 %105, 0
  %107 = getelementptr i8, i8* %70, i64 %101
  %108 = bitcast i8* %107 to i1*
  store i1 %106, i1* %108, align 1
  %109 = add nuw nsw i64 %storemerge5, 4
  %niter.nsub.3 = add i64 %niter, -4
  %niter.ncmp.3 = icmp eq i64 %niter.nsub.3, 0
  br i1 %niter.ncmp.3, label %end_1.loopexit.unr-lcssa, label %body_1

end_1.loopexit.unr-lcssa:                         ; preds = %body_1, %body_1.preheader
  %storemerge5.unr = phi i64 [ 0, %body_1.preheader ], [ %109, %body_1 ]
  %lcmp.mod = icmp eq i64 %xtraiter, 0
  br i1 %lcmp.mod, label %end_1, label %body_1.epil

body_1.epil:                                      ; preds = %end_1.loopexit.unr-lcssa, %body_1.epil
  %storemerge5.epil = phi i64 [ %117, %body_1.epil ], [ %storemerge5.unr, %end_1.loopexit.unr-lcssa ]
  %epil.iter = phi i64 [ %epil.iter.sub, %body_1.epil ], [ %xtraiter, %end_1.loopexit.unr-lcssa ]
  %110 = getelementptr i8*, i8** %66, i64 %storemerge5.epil
  %111 = bitcast i8** %110 to i64*
  %112 = load i64, i64* %111, align 8
  %113 = and i64 %112, 1
  %114 = icmp ne i64 %113, 0
  %115 = getelementptr i8, i8* %70, i64 %storemerge5.epil
  %116 = bitcast i8* %115 to i1*
  store i1 %114, i1* %116, align 1
  %117 = add nuw nsw i64 %storemerge5.epil, 1
  %epil.iter.sub = add i64 %epil.iter, -1
  %epil.iter.cmp = icmp eq i64 %epil.iter.sub, 0
  br i1 %epil.iter.cmp, label %end_1, label %body_1.epil, !llvm.loop !2

end_1:                                            ; preds = %end_1.loopexit.unr-lcssa, %body_1.epil, %end_0
  %118 = and i64 %69, 1
  %119 = icmp ne i64 %118, 0
  br label %end_2

end_2:                                            ; preds = %4, %end_1
  %.0 = phi i1 [ %119, %end_1 ], [ false, %4 ]
  ret i1 %.0
}

; Function Attrs: norecurse nounwind readonly
define i8* @sub3({ i8**, i64 }* nocapture readonly, i64) local_unnamed_addr #0 {
  %3 = getelementptr { i8**, i64 }, { i8**, i64 }* %0, i64 0, i32 0
  %4 = load i8**, i8*** %3, align 8
  %5 = getelementptr i8*, i8** %4, i64 %1
  %6 = load i8*, i8** %5, align 8
  ret i8* %6
}

declare void @GC_init() local_unnamed_addr

define i32 @main() local_unnamed_addr {
body_0:
  tail call void @GC_init()
  %0 = tail call i8* @GC_malloc(i64 3)
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to i1**
  %3 = bitcast i8* %1 to i8**
  store i8* %0, i8** %3, align 8
  %4 = getelementptr i8, i8* %1, i64 8
  %5 = bitcast i8* %4 to i64*
  store i64 3, i64* %5, align 8
  %.cast = bitcast i8* %0 to i1*
  store i1 true, i1* %.cast, align 1
  %6 = load i1*, i1** %2, align 8
  %7 = getelementptr i1, i1* %6, i64 1
  store i1 true, i1* %7, align 1
  %8 = load i1*, i1** %2, align 8
  %9 = getelementptr i1, i1* %8, i64 2
  store i1 true, i1* %9, align 1
  %10 = load i1*, i1** %2, align 8
  %11 = bitcast i8* %1 to { i1*, i64 }*
  %12 = getelementptr i1, i1* %10, i64 1
  store i1 true, i1* %12, align 1
  %13 = load i1*, i1** %2, align 8
  %14 = getelementptr i1, i1* %13, i64 2
  store i1 true, i1* %14, align 1
  %15 = tail call i8* @GC_malloc(i64 16)
  %16 = bitcast i8* %15 to <2 x i64>*
  store <2 x i64> <i64 10, i64 10>, <2 x i64>* %16, align 8
  %17 = tail call i8* @GC_malloc(i64 16)
  %18 = bitcast i8* %17 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view6, i1 (i8*, { i1*, i64 }*, i64, i64)** %18, align 8
  %19 = getelementptr i8, i8* %17, i64 8
  %20 = bitcast i8* %19 to i8**
  store i8* %15, i8** %20, align 8
  %21 = tail call i1 @view6(i8* %15, { i1*, i64 }* nonnull %11, i64 0, i64 0)
  ret i32 0
}

attributes #0 = { norecurse nounwind readonly }

!0 = distinct !{!0, !1}
!1 = !{!"llvm.loop.unroll.disable"}
!2 = distinct !{!2, !1}
