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
  br i1 %19, label %end_1, label %else_0

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
  %xtraiter = and i64 %25, 3
  %34 = icmp ult i64 %33, 3
  br i1 %34, label %end_0.loopexit.unr-lcssa, label %body_0.preheader.new

body_0.preheader.new:                             ; preds = %body_0.preheader
  %unroll_iter = sub i64 %25, %xtraiter
  br label %body_0

body_0:                                           ; preds = %body_0, %body_0.preheader.new
  %storemerge2 = phi i64 [ 0, %body_0.preheader.new ], [ %58, %body_0 ]
  %niter = phi i64 [ %unroll_iter, %body_0.preheader.new ], [ %niter.nsub.3, %body_0 ]
  %35 = getelementptr i1, i1* %23, i64 %storemerge2
  %36 = load i1, i1* %35, align 1
  %37 = zext i1 %36 to i64
  %38 = inttoptr i64 %37 to i8*
  %39 = getelementptr i8*, i8** %.cast, i64 %storemerge2
  store i8* %38, i8** %39, align 8
  %40 = or i64 %storemerge2, 1
  %41 = getelementptr i1, i1* %23, i64 %40
  %42 = load i1, i1* %41, align 1
  %43 = zext i1 %42 to i64
  %44 = inttoptr i64 %43 to i8*
  %45 = getelementptr i8*, i8** %.cast, i64 %40
  store i8* %44, i8** %45, align 8
  %46 = or i64 %storemerge2, 2
  %47 = getelementptr i1, i1* %23, i64 %46
  %48 = load i1, i1* %47, align 1
  %49 = zext i1 %48 to i64
  %50 = inttoptr i64 %49 to i8*
  %51 = getelementptr i8*, i8** %.cast, i64 %46
  store i8* %50, i8** %51, align 8
  %52 = or i64 %storemerge2, 3
  %53 = getelementptr i1, i1* %23, i64 %52
  %54 = load i1, i1* %53, align 1
  %55 = zext i1 %54 to i64
  %56 = inttoptr i64 %55 to i8*
  %57 = getelementptr i8*, i8** %.cast, i64 %52
  store i8* %56, i8** %57, align 8
  %58 = add nuw nsw i64 %storemerge2, 4
  %niter.nsub.3 = add i64 %niter, -4
  %niter.ncmp.3 = icmp eq i64 %niter.nsub.3, 0
  br i1 %niter.ncmp.3, label %end_0.loopexit.unr-lcssa, label %body_0

end_0.loopexit.unr-lcssa:                         ; preds = %body_0, %body_0.preheader
  %storemerge2.unr = phi i64 [ 0, %body_0.preheader ], [ %58, %body_0 ]
  %lcmp.mod = icmp eq i64 %xtraiter, 0
  br i1 %lcmp.mod, label %end_0.loopexit, label %body_0.epil

body_0.epil:                                      ; preds = %end_0.loopexit.unr-lcssa, %body_0.epil
  %storemerge2.epil = phi i64 [ %64, %body_0.epil ], [ %storemerge2.unr, %end_0.loopexit.unr-lcssa ]
  %epil.iter = phi i64 [ %epil.iter.sub, %body_0.epil ], [ %xtraiter, %end_0.loopexit.unr-lcssa ]
  %59 = getelementptr i1, i1* %23, i64 %storemerge2.epil
  %60 = load i1, i1* %59, align 1
  %61 = zext i1 %60 to i64
  %62 = inttoptr i64 %61 to i8*
  %63 = getelementptr i8*, i8** %.cast, i64 %storemerge2.epil
  store i8* %62, i8** %63, align 8
  %64 = add nuw nsw i64 %storemerge2.epil, 1
  %epil.iter.sub = add i64 %epil.iter, -1
  %epil.iter.cmp = icmp eq i64 %epil.iter.sub, 0
  br i1 %epil.iter.cmp, label %end_0.loopexit, label %body_0.epil, !llvm.loop !0

end_0.loopexit:                                   ; preds = %body_0.epil, %end_0.loopexit.unr-lcssa
  %.phi.trans.insert = bitcast i8* %28 to i8***
  %.pre = load i8**, i8*** %.phi.trans.insert, align 8
  br label %end_0

end_0:                                            ; preds = %else_0, %end_0.loopexit
  %65 = phi i8** [ %.pre, %end_0.loopexit ], [ %.cast, %else_0 ]
  %66 = getelementptr i8*, i8** %65, i64 %21
  %67 = bitcast i8** %66 to i64*
  %68 = load i64, i64* %67, align 8
  %69 = and i64 %68, 1
  %70 = icmp ne i64 %69, 0
  br label %end_1

end_1:                                            ; preds = %4, %end_0
  %.0 = phi i1 [ %70, %end_0 ], [ false, %4 ]
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
  %11 = getelementptr i1, i1* %10, i64 1
  store i1 true, i1* %11, align 1
  %12 = load i1*, i1** %2, align 8
  %13 = getelementptr i1, i1* %12, i64 2
  store i1 true, i1* %13, align 1
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to i64*
  %16 = getelementptr i8, i8* %14, i64 8
  %17 = bitcast i8* %16 to i64*
  %18 = bitcast i8* %14 to <2 x i64>*
  store <2 x i64> <i64 10, i64 10>, <2 x i64>* %18, align 8
  %19 = tail call i8* @GC_malloc(i64 16)
  %20 = bitcast i8* %19 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view6, i1 (i8*, { i1*, i64 }*, i64, i64)** %20, align 8
  %21 = getelementptr i8, i8* %19, i64 8
  %22 = bitcast i8* %21 to i8**
  store i8* %14, i8** %22, align 8
  %23 = load i64, i64* %15, align 8
  %24 = load i64, i64* %17, align 8
  %25 = tail call i8* @GC_malloc(i64 16)
  %26 = bitcast i8* %25 to i1 (i8*, { i1*, i64 }*, i64, i64)**
  store i1 (i8*, { i1*, i64 }*, i64, i64)* @view6, i1 (i8*, { i1*, i64 }*, i64, i64)** %26, align 8
  %27 = getelementptr i8, i8* %25, i64 8
  %28 = bitcast i8* %27 to i8**
  store i8* %14, i8** %28, align 8
  %29 = icmp slt i64 %23, 1
  %30 = icmp slt i64 %24, 1
  %31 = or i1 %29, %30
  br i1 %31, label %view6.exit, label %else_0.i

else_0.i:                                         ; preds = %body_0
  %32 = load i1*, i1** %2, align 8
  %33 = load i64, i64* %5, align 8
  %34 = shl i64 %33, 3
  %35 = tail call i8* @GC_malloc(i64 %34)
  %36 = tail call i8* @GC_malloc(i64 16)
  %37 = bitcast i8* %36 to i8**
  store i8* %35, i8** %37, align 8
  %38 = getelementptr i8, i8* %36, i64 8
  %39 = bitcast i8* %38 to i64*
  %.cast.i = bitcast i8* %35 to i8**
  store i64 %33, i64* %39, align 8
  %40 = icmp sgt i64 %33, 0
  br i1 %40, label %body_0.i.preheader, label %view6.exit

body_0.i.preheader:                               ; preds = %else_0.i
  %41 = add i64 %33, -1
  %xtraiter = and i64 %33, 3
  %42 = icmp ult i64 %41, 3
  br i1 %42, label %view6.exit.loopexit.unr-lcssa, label %body_0.i.preheader.new

body_0.i.preheader.new:                           ; preds = %body_0.i.preheader
  %unroll_iter = sub i64 %33, %xtraiter
  br label %body_0.i

body_0.i:                                         ; preds = %body_0.i, %body_0.i.preheader.new
  %storemerge2.i = phi i64 [ 0, %body_0.i.preheader.new ], [ %66, %body_0.i ]
  %niter = phi i64 [ %unroll_iter, %body_0.i.preheader.new ], [ %niter.nsub.3, %body_0.i ]
  %43 = getelementptr i1, i1* %32, i64 %storemerge2.i
  %44 = load i1, i1* %43, align 1
  %45 = zext i1 %44 to i64
  %46 = inttoptr i64 %45 to i8*
  %47 = getelementptr i8*, i8** %.cast.i, i64 %storemerge2.i
  store i8* %46, i8** %47, align 8
  %48 = or i64 %storemerge2.i, 1
  %49 = getelementptr i1, i1* %32, i64 %48
  %50 = load i1, i1* %49, align 1
  %51 = zext i1 %50 to i64
  %52 = inttoptr i64 %51 to i8*
  %53 = getelementptr i8*, i8** %.cast.i, i64 %48
  store i8* %52, i8** %53, align 8
  %54 = or i64 %storemerge2.i, 2
  %55 = getelementptr i1, i1* %32, i64 %54
  %56 = load i1, i1* %55, align 1
  %57 = zext i1 %56 to i64
  %58 = inttoptr i64 %57 to i8*
  %59 = getelementptr i8*, i8** %.cast.i, i64 %54
  store i8* %58, i8** %59, align 8
  %60 = or i64 %storemerge2.i, 3
  %61 = getelementptr i1, i1* %32, i64 %60
  %62 = load i1, i1* %61, align 1
  %63 = zext i1 %62 to i64
  %64 = inttoptr i64 %63 to i8*
  %65 = getelementptr i8*, i8** %.cast.i, i64 %60
  store i8* %64, i8** %65, align 8
  %66 = add nuw nsw i64 %storemerge2.i, 4
  %niter.nsub.3 = add i64 %niter, -4
  %niter.ncmp.3 = icmp eq i64 %niter.nsub.3, 0
  br i1 %niter.ncmp.3, label %view6.exit.loopexit.unr-lcssa, label %body_0.i

view6.exit.loopexit.unr-lcssa:                    ; preds = %body_0.i, %body_0.i.preheader
  %storemerge2.i.unr = phi i64 [ 0, %body_0.i.preheader ], [ %66, %body_0.i ]
  %lcmp.mod = icmp eq i64 %xtraiter, 0
  br i1 %lcmp.mod, label %view6.exit, label %body_0.i.epil

body_0.i.epil:                                    ; preds = %view6.exit.loopexit.unr-lcssa, %body_0.i.epil
  %storemerge2.i.epil = phi i64 [ %72, %body_0.i.epil ], [ %storemerge2.i.unr, %view6.exit.loopexit.unr-lcssa ]
  %epil.iter = phi i64 [ %epil.iter.sub, %body_0.i.epil ], [ %xtraiter, %view6.exit.loopexit.unr-lcssa ]
  %67 = getelementptr i1, i1* %32, i64 %storemerge2.i.epil
  %68 = load i1, i1* %67, align 1
  %69 = zext i1 %68 to i64
  %70 = inttoptr i64 %69 to i8*
  %71 = getelementptr i8*, i8** %.cast.i, i64 %storemerge2.i.epil
  store i8* %70, i8** %71, align 8
  %72 = add nuw nsw i64 %storemerge2.i.epil, 1
  %epil.iter.sub = add i64 %epil.iter, -1
  %epil.iter.cmp = icmp eq i64 %epil.iter.sub, 0
  br i1 %epil.iter.cmp, label %view6.exit, label %body_0.i.epil, !llvm.loop !2

view6.exit:                                       ; preds = %view6.exit.loopexit.unr-lcssa, %body_0.i.epil, %else_0.i, %body_0
  ret i32 0
}

attributes #0 = { norecurse nounwind readonly }

!0 = distinct !{!0, !1}
!1 = !{!"llvm.loop.unroll.disable"}
!2 = distinct !{!2, !1}
