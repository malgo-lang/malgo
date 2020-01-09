; ModuleID = './examples/gen/polytuple.mlg.ll'
source_filename = "./examples/polytuple.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

; Function Attrs: norecurse nounwind
define noalias {}* @f0({ i8**, i64 }* nocapture readonly, i8*) local_unnamed_addr #0 {
  %3 = getelementptr { i8**, i64 }, { i8**, i64 }* %0, i64 0, i32 0
  %4 = load i8**, i8*** %3, align 8
  store i8* %1, i8** %4, align 8
  ret {}* undef
}

declare i8* @GC_malloc(i64) local_unnamed_addr

define i32 @main() local_unnamed_addr {
body_0:
  %0 = tail call i8* @GC_malloc(i64 80)
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to i64**
  %3 = bitcast i8* %1 to i8**
  store i8* %0, i8** %3, align 8
  %4 = getelementptr i8, i8* %1, i64 8
  %5 = bitcast i8* %4 to i64*
  store i64 10, i64* %5, align 8
  %.cast4 = bitcast i8* %0 to i64*
  store i64 1, i64* %.cast4, align 8
  %6 = load i64*, i64** %2, align 8
  %7 = getelementptr i64, i64* %6, i64 1
  store i64 1, i64* %7, align 8
  %8 = load i64*, i64** %2, align 8
  %9 = getelementptr i64, i64* %8, i64 2
  store i64 1, i64* %9, align 8
  %10 = load i64*, i64** %2, align 8
  %11 = getelementptr i64, i64* %10, i64 3
  store i64 1, i64* %11, align 8
  %12 = load i64*, i64** %2, align 8
  %13 = getelementptr i64, i64* %12, i64 4
  store i64 1, i64* %13, align 8
  %14 = load i64*, i64** %2, align 8
  %15 = getelementptr i64, i64* %14, i64 5
  store i64 1, i64* %15, align 8
  %16 = load i64*, i64** %2, align 8
  %17 = getelementptr i64, i64* %16, i64 6
  store i64 1, i64* %17, align 8
  %18 = load i64*, i64** %2, align 8
  %19 = getelementptr i64, i64* %18, i64 7
  store i64 1, i64* %19, align 8
  %20 = load i64*, i64** %2, align 8
  %21 = getelementptr i64, i64* %20, i64 8
  store i64 1, i64* %21, align 8
  %22 = load i64*, i64** %2, align 8
  %23 = getelementptr i64, i64* %22, i64 9
  store i64 1, i64* %23, align 8
  %24 = load i64*, i64** %2, align 8
  %25 = bitcast i64* %24 to i8*
  %26 = load i64, i64* %5, align 8
  %27 = shl i64 %26, 3
  %28 = tail call i8* @GC_malloc(i64 %27)
  %29 = tail call i8* @GC_malloc(i64 16)
  %30 = bitcast i8* %29 to i8**
  store i8* %28, i8** %30, align 8
  %31 = getelementptr i8, i8* %29, i64 8
  %32 = bitcast i8* %31 to i64*
  %.cast = bitcast i8* %28 to i8**
  store i64 %26, i64* %32, align 8
  %33 = icmp sgt i64 %26, 0
  br i1 %33, label %body_1.preheader, label %end_1

body_1.preheader:                                 ; preds = %body_0
  %min.iters.check = icmp ult i64 %26, 4
  br i1 %min.iters.check, label %body_1.preheader11, label %vector.memcheck

vector.memcheck:                                  ; preds = %body_1.preheader
  %34 = shl i64 %26, 3
  %scevgep = getelementptr i8, i8* %28, i64 %34
  %scevgep7 = getelementptr i64, i64* %24, i64 %26
  %scevgep78 = bitcast i64* %scevgep7 to i8*
  %bound0 = icmp ult i8* %28, %scevgep78
  %bound1 = icmp ugt i8* %scevgep, %25
  %found.conflict = and i1 %bound0, %bound1
  br i1 %found.conflict, label %body_1.preheader11, label %vector.ph

vector.ph:                                        ; preds = %vector.memcheck
  %n.vec = and i64 %26, -4
  %35 = add i64 %n.vec, -4
  %36 = lshr exact i64 %35, 2
  %37 = add nuw nsw i64 %36, 1
  %xtraiter12 = and i64 %37, 3
  %38 = icmp ult i64 %35, 12
  br i1 %38, label %middle.block.unr-lcssa, label %vector.ph.new

vector.ph.new:                                    ; preds = %vector.ph
  %unroll_iter = sub nsw i64 %37, %xtraiter12
  br label %vector.body

vector.body:                                      ; preds = %vector.body, %vector.ph.new
  %index = phi i64 [ 0, %vector.ph.new ], [ %index.next.3, %vector.body ]
  %niter = phi i64 [ %unroll_iter, %vector.ph.new ], [ %niter.nsub.3, %vector.body ]
  %39 = getelementptr i64, i64* %24, i64 %index
  %40 = bitcast i64* %39 to <2 x i64>*
  %wide.load = load <2 x i64>, <2 x i64>* %40, align 8, !alias.scope !0
  %41 = getelementptr i64, i64* %39, i64 2
  %42 = bitcast i64* %41 to <2 x i64>*
  %wide.load10 = load <2 x i64>, <2 x i64>* %42, align 8, !alias.scope !0
  %43 = getelementptr i8*, i8** %.cast, i64 %index
  %44 = bitcast i8** %43 to <2 x i64>*
  store <2 x i64> %wide.load, <2 x i64>* %44, align 8, !alias.scope !3, !noalias !0
  %45 = getelementptr i8*, i8** %43, i64 2
  %46 = bitcast i8** %45 to <2 x i64>*
  store <2 x i64> %wide.load10, <2 x i64>* %46, align 8, !alias.scope !3, !noalias !0
  %index.next = or i64 %index, 4
  %47 = getelementptr i64, i64* %24, i64 %index.next
  %48 = bitcast i64* %47 to <2 x i64>*
  %wide.load.1 = load <2 x i64>, <2 x i64>* %48, align 8, !alias.scope !0
  %49 = getelementptr i64, i64* %47, i64 2
  %50 = bitcast i64* %49 to <2 x i64>*
  %wide.load10.1 = load <2 x i64>, <2 x i64>* %50, align 8, !alias.scope !0
  %51 = getelementptr i8*, i8** %.cast, i64 %index.next
  %52 = bitcast i8** %51 to <2 x i64>*
  store <2 x i64> %wide.load.1, <2 x i64>* %52, align 8, !alias.scope !3, !noalias !0
  %53 = getelementptr i8*, i8** %51, i64 2
  %54 = bitcast i8** %53 to <2 x i64>*
  store <2 x i64> %wide.load10.1, <2 x i64>* %54, align 8, !alias.scope !3, !noalias !0
  %index.next.1 = or i64 %index, 8
  %55 = getelementptr i64, i64* %24, i64 %index.next.1
  %56 = bitcast i64* %55 to <2 x i64>*
  %wide.load.2 = load <2 x i64>, <2 x i64>* %56, align 8, !alias.scope !0
  %57 = getelementptr i64, i64* %55, i64 2
  %58 = bitcast i64* %57 to <2 x i64>*
  %wide.load10.2 = load <2 x i64>, <2 x i64>* %58, align 8, !alias.scope !0
  %59 = getelementptr i8*, i8** %.cast, i64 %index.next.1
  %60 = bitcast i8** %59 to <2 x i64>*
  store <2 x i64> %wide.load.2, <2 x i64>* %60, align 8, !alias.scope !3, !noalias !0
  %61 = getelementptr i8*, i8** %59, i64 2
  %62 = bitcast i8** %61 to <2 x i64>*
  store <2 x i64> %wide.load10.2, <2 x i64>* %62, align 8, !alias.scope !3, !noalias !0
  %index.next.2 = or i64 %index, 12
  %63 = getelementptr i64, i64* %24, i64 %index.next.2
  %64 = bitcast i64* %63 to <2 x i64>*
  %wide.load.3 = load <2 x i64>, <2 x i64>* %64, align 8, !alias.scope !0
  %65 = getelementptr i64, i64* %63, i64 2
  %66 = bitcast i64* %65 to <2 x i64>*
  %wide.load10.3 = load <2 x i64>, <2 x i64>* %66, align 8, !alias.scope !0
  %67 = getelementptr i8*, i8** %.cast, i64 %index.next.2
  %68 = bitcast i8** %67 to <2 x i64>*
  store <2 x i64> %wide.load.3, <2 x i64>* %68, align 8, !alias.scope !3, !noalias !0
  %69 = getelementptr i8*, i8** %67, i64 2
  %70 = bitcast i8** %69 to <2 x i64>*
  store <2 x i64> %wide.load10.3, <2 x i64>* %70, align 8, !alias.scope !3, !noalias !0
  %index.next.3 = add i64 %index, 16
  %niter.nsub.3 = add i64 %niter, -4
  %niter.ncmp.3 = icmp eq i64 %niter.nsub.3, 0
  br i1 %niter.ncmp.3, label %middle.block.unr-lcssa, label %vector.body, !llvm.loop !5

middle.block.unr-lcssa:                           ; preds = %vector.body, %vector.ph
  %index.unr = phi i64 [ 0, %vector.ph ], [ %index.next.3, %vector.body ]
  %lcmp.mod13 = icmp eq i64 %xtraiter12, 0
  br i1 %lcmp.mod13, label %middle.block, label %vector.body.epil

vector.body.epil:                                 ; preds = %middle.block.unr-lcssa, %vector.body.epil
  %index.epil = phi i64 [ %index.next.epil, %vector.body.epil ], [ %index.unr, %middle.block.unr-lcssa ]
  %epil.iter = phi i64 [ %epil.iter.sub, %vector.body.epil ], [ %xtraiter12, %middle.block.unr-lcssa ]
  %71 = getelementptr i64, i64* %24, i64 %index.epil
  %72 = bitcast i64* %71 to <2 x i64>*
  %wide.load.epil = load <2 x i64>, <2 x i64>* %72, align 8, !alias.scope !0
  %73 = getelementptr i64, i64* %71, i64 2
  %74 = bitcast i64* %73 to <2 x i64>*
  %wide.load10.epil = load <2 x i64>, <2 x i64>* %74, align 8, !alias.scope !0
  %75 = getelementptr i8*, i8** %.cast, i64 %index.epil
  %76 = bitcast i8** %75 to <2 x i64>*
  store <2 x i64> %wide.load.epil, <2 x i64>* %76, align 8, !alias.scope !3, !noalias !0
  %77 = getelementptr i8*, i8** %75, i64 2
  %78 = bitcast i8** %77 to <2 x i64>*
  store <2 x i64> %wide.load10.epil, <2 x i64>* %78, align 8, !alias.scope !3, !noalias !0
  %index.next.epil = add i64 %index.epil, 4
  %epil.iter.sub = add i64 %epil.iter, -1
  %epil.iter.cmp = icmp eq i64 %epil.iter.sub, 0
  br i1 %epil.iter.cmp, label %middle.block, label %vector.body.epil, !llvm.loop !7

middle.block:                                     ; preds = %vector.body.epil, %middle.block.unr-lcssa
  %cmp.n = icmp eq i64 %26, %n.vec
  br i1 %cmp.n, label %end_1.loopexit, label %body_1.preheader11

body_1.preheader11:                               ; preds = %middle.block, %vector.memcheck, %body_1.preheader
  %storemerge2.ph = phi i64 [ 0, %vector.memcheck ], [ 0, %body_1.preheader ], [ %n.vec, %middle.block ]
  %79 = sub i64 %26, %storemerge2.ph
  %80 = xor i64 %storemerge2.ph, -1
  %81 = add i64 %26, %80
  %xtraiter = and i64 %79, 7
  %lcmp.mod = icmp eq i64 %xtraiter, 0
  br i1 %lcmp.mod, label %body_1.prol.loopexit, label %body_1.prol

body_1.prol:                                      ; preds = %body_1.preheader11, %body_1.prol
  %storemerge2.prol = phi i64 [ %86, %body_1.prol ], [ %storemerge2.ph, %body_1.preheader11 ]
  %prol.iter = phi i64 [ %prol.iter.sub, %body_1.prol ], [ %xtraiter, %body_1.preheader11 ]
  %82 = getelementptr i64, i64* %24, i64 %storemerge2.prol
  %83 = load i64, i64* %82, align 8
  %84 = getelementptr i8*, i8** %.cast, i64 %storemerge2.prol
  %85 = bitcast i8** %84 to i64*
  store i64 %83, i64* %85, align 8
  %86 = add nuw nsw i64 %storemerge2.prol, 1
  %prol.iter.sub = add i64 %prol.iter, -1
  %prol.iter.cmp = icmp eq i64 %prol.iter.sub, 0
  br i1 %prol.iter.cmp, label %body_1.prol.loopexit, label %body_1.prol, !llvm.loop !9

body_1.prol.loopexit:                             ; preds = %body_1.prol, %body_1.preheader11
  %storemerge2.unr = phi i64 [ %storemerge2.ph, %body_1.preheader11 ], [ %86, %body_1.prol ]
  %87 = icmp ult i64 %81, 7
  br i1 %87, label %end_1.loopexit, label %body_1

body_1:                                           ; preds = %body_1.prol.loopexit, %body_1
  %storemerge2 = phi i64 [ %127, %body_1 ], [ %storemerge2.unr, %body_1.prol.loopexit ]
  %88 = getelementptr i64, i64* %24, i64 %storemerge2
  %89 = load i64, i64* %88, align 8
  %90 = getelementptr i8*, i8** %.cast, i64 %storemerge2
  %91 = bitcast i8** %90 to i64*
  store i64 %89, i64* %91, align 8
  %92 = add nuw nsw i64 %storemerge2, 1
  %93 = getelementptr i64, i64* %24, i64 %92
  %94 = load i64, i64* %93, align 8
  %95 = getelementptr i8*, i8** %.cast, i64 %92
  %96 = bitcast i8** %95 to i64*
  store i64 %94, i64* %96, align 8
  %97 = add nsw i64 %storemerge2, 2
  %98 = getelementptr i64, i64* %24, i64 %97
  %99 = load i64, i64* %98, align 8
  %100 = getelementptr i8*, i8** %.cast, i64 %97
  %101 = bitcast i8** %100 to i64*
  store i64 %99, i64* %101, align 8
  %102 = add nsw i64 %storemerge2, 3
  %103 = getelementptr i64, i64* %24, i64 %102
  %104 = load i64, i64* %103, align 8
  %105 = getelementptr i8*, i8** %.cast, i64 %102
  %106 = bitcast i8** %105 to i64*
  store i64 %104, i64* %106, align 8
  %107 = add nsw i64 %storemerge2, 4
  %108 = getelementptr i64, i64* %24, i64 %107
  %109 = load i64, i64* %108, align 8
  %110 = getelementptr i8*, i8** %.cast, i64 %107
  %111 = bitcast i8** %110 to i64*
  store i64 %109, i64* %111, align 8
  %112 = add nsw i64 %storemerge2, 5
  %113 = getelementptr i64, i64* %24, i64 %112
  %114 = load i64, i64* %113, align 8
  %115 = getelementptr i8*, i8** %.cast, i64 %112
  %116 = bitcast i8** %115 to i64*
  store i64 %114, i64* %116, align 8
  %117 = add nsw i64 %storemerge2, 6
  %118 = getelementptr i64, i64* %24, i64 %117
  %119 = load i64, i64* %118, align 8
  %120 = getelementptr i8*, i8** %.cast, i64 %117
  %121 = bitcast i8** %120 to i64*
  store i64 %119, i64* %121, align 8
  %122 = add nsw i64 %storemerge2, 7
  %123 = getelementptr i64, i64* %24, i64 %122
  %124 = load i64, i64* %123, align 8
  %125 = getelementptr i8*, i8** %.cast, i64 %122
  %126 = bitcast i8** %125 to i64*
  store i64 %124, i64* %126, align 8
  %127 = add nsw i64 %storemerge2, 8
  %exitcond.7 = icmp eq i64 %127, %26
  br i1 %exitcond.7, label %end_1.loopexit, label %body_1, !llvm.loop !10

end_1.loopexit:                                   ; preds = %body_1.prol.loopexit, %body_1, %middle.block
  %.phi.trans.insert = bitcast i8* %29 to i8***
  %.pre = load i8**, i8*** %.phi.trans.insert, align 8
  br label %end_1

end_1:                                            ; preds = %body_0, %end_1.loopexit
  %128 = phi i8** [ %.pre, %end_1.loopexit ], [ %.cast, %body_0 ]
  store i8* inttoptr (i64 10 to i8*), i8** %128, align 8
  %129 = tail call i8* @GC_malloc(i64 0)
  ret i32 0
}

attributes #0 = { norecurse nounwind }

!0 = !{!1}
!1 = distinct !{!1, !2}
!2 = distinct !{!2, !"LVerDomain"}
!3 = !{!4}
!4 = distinct !{!4, !2}
!5 = distinct !{!5, !6}
!6 = !{!"llvm.loop.isvectorized", i32 1}
!7 = distinct !{!7, !8}
!8 = !{!"llvm.loop.unroll.disable"}
!9 = distinct !{!9, !8}
!10 = distinct !{!10, !6}
