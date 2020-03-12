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

declare void @GC_init() local_unnamed_addr

declare i8* @GC_malloc(i64) local_unnamed_addr

define i32 @main() local_unnamed_addr {
body_0:
  tail call void @GC_init()
  %0 = tail call i8* @GC_malloc(i64 80)
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to i64**
  %3 = bitcast i8* %1 to i8**
  store i8* %0, i8** %3, align 8
  %4 = getelementptr i8, i8* %1, i64 8
  %5 = bitcast i8* %4 to i64*
  store i64 10, i64* %5, align 8
  %.cast8 = bitcast i8* %0 to i64*
  store i64 1, i64* %.cast8, align 8
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
  %30 = bitcast i8* %29 to i8***
  %31 = bitcast i8* %29 to i8**
  store i8* %28, i8** %31, align 8
  %32 = getelementptr i8, i8* %29, i64 8
  %33 = bitcast i8* %32 to i64*
  %.cast = bitcast i8* %28 to i8**
  store i64 %26, i64* %33, align 8
  %34 = icmp sgt i64 %26, 0
  br i1 %34, label %body_1.preheader, label %end_1

body_1.preheader:                                 ; preds = %body_0
  %min.iters.check = icmp ult i64 %26, 4
  br i1 %min.iters.check, label %body_1.preheader42, label %vector.memcheck

vector.memcheck:                                  ; preds = %body_1.preheader
  %35 = shl i64 %26, 3
  %scevgep = getelementptr i8, i8* %28, i64 %35
  %scevgep11 = getelementptr i64, i64* %24, i64 %26
  %scevgep1112 = bitcast i64* %scevgep11 to i8*
  %bound0 = icmp ult i8* %28, %scevgep1112
  %bound1 = icmp ugt i8* %scevgep, %25
  %found.conflict = and i1 %bound0, %bound1
  br i1 %found.conflict, label %body_1.preheader42, label %vector.ph

vector.ph:                                        ; preds = %vector.memcheck
  %n.vec = and i64 %26, -4
  %36 = add i64 %n.vec, -4
  %37 = lshr exact i64 %36, 2
  %38 = add nuw nsw i64 %37, 1
  %xtraiter48 = and i64 %38, 3
  %39 = icmp ult i64 %36, 12
  br i1 %39, label %middle.block.unr-lcssa, label %vector.ph.new

vector.ph.new:                                    ; preds = %vector.ph
  %unroll_iter51 = sub nsw i64 %38, %xtraiter48
  br label %vector.body

vector.body:                                      ; preds = %vector.body, %vector.ph.new
  %index = phi i64 [ 0, %vector.ph.new ], [ %index.next.3, %vector.body ]
  %niter52 = phi i64 [ %unroll_iter51, %vector.ph.new ], [ %niter52.nsub.3, %vector.body ]
  %40 = getelementptr i64, i64* %24, i64 %index
  %41 = bitcast i64* %40 to <2 x i64>*
  %wide.load = load <2 x i64>, <2 x i64>* %41, align 8, !alias.scope !0
  %42 = getelementptr i64, i64* %40, i64 2
  %43 = bitcast i64* %42 to <2 x i64>*
  %wide.load14 = load <2 x i64>, <2 x i64>* %43, align 8, !alias.scope !0
  %44 = getelementptr i8*, i8** %.cast, i64 %index
  %45 = bitcast i8** %44 to <2 x i64>*
  store <2 x i64> %wide.load, <2 x i64>* %45, align 8, !alias.scope !3, !noalias !0
  %46 = getelementptr i8*, i8** %44, i64 2
  %47 = bitcast i8** %46 to <2 x i64>*
  store <2 x i64> %wide.load14, <2 x i64>* %47, align 8, !alias.scope !3, !noalias !0
  %index.next = or i64 %index, 4
  %48 = getelementptr i64, i64* %24, i64 %index.next
  %49 = bitcast i64* %48 to <2 x i64>*
  %wide.load.1 = load <2 x i64>, <2 x i64>* %49, align 8, !alias.scope !0
  %50 = getelementptr i64, i64* %48, i64 2
  %51 = bitcast i64* %50 to <2 x i64>*
  %wide.load14.1 = load <2 x i64>, <2 x i64>* %51, align 8, !alias.scope !0
  %52 = getelementptr i8*, i8** %.cast, i64 %index.next
  %53 = bitcast i8** %52 to <2 x i64>*
  store <2 x i64> %wide.load.1, <2 x i64>* %53, align 8, !alias.scope !3, !noalias !0
  %54 = getelementptr i8*, i8** %52, i64 2
  %55 = bitcast i8** %54 to <2 x i64>*
  store <2 x i64> %wide.load14.1, <2 x i64>* %55, align 8, !alias.scope !3, !noalias !0
  %index.next.1 = or i64 %index, 8
  %56 = getelementptr i64, i64* %24, i64 %index.next.1
  %57 = bitcast i64* %56 to <2 x i64>*
  %wide.load.2 = load <2 x i64>, <2 x i64>* %57, align 8, !alias.scope !0
  %58 = getelementptr i64, i64* %56, i64 2
  %59 = bitcast i64* %58 to <2 x i64>*
  %wide.load14.2 = load <2 x i64>, <2 x i64>* %59, align 8, !alias.scope !0
  %60 = getelementptr i8*, i8** %.cast, i64 %index.next.1
  %61 = bitcast i8** %60 to <2 x i64>*
  store <2 x i64> %wide.load.2, <2 x i64>* %61, align 8, !alias.scope !3, !noalias !0
  %62 = getelementptr i8*, i8** %60, i64 2
  %63 = bitcast i8** %62 to <2 x i64>*
  store <2 x i64> %wide.load14.2, <2 x i64>* %63, align 8, !alias.scope !3, !noalias !0
  %index.next.2 = or i64 %index, 12
  %64 = getelementptr i64, i64* %24, i64 %index.next.2
  %65 = bitcast i64* %64 to <2 x i64>*
  %wide.load.3 = load <2 x i64>, <2 x i64>* %65, align 8, !alias.scope !0
  %66 = getelementptr i64, i64* %64, i64 2
  %67 = bitcast i64* %66 to <2 x i64>*
  %wide.load14.3 = load <2 x i64>, <2 x i64>* %67, align 8, !alias.scope !0
  %68 = getelementptr i8*, i8** %.cast, i64 %index.next.2
  %69 = bitcast i8** %68 to <2 x i64>*
  store <2 x i64> %wide.load.3, <2 x i64>* %69, align 8, !alias.scope !3, !noalias !0
  %70 = getelementptr i8*, i8** %68, i64 2
  %71 = bitcast i8** %70 to <2 x i64>*
  store <2 x i64> %wide.load14.3, <2 x i64>* %71, align 8, !alias.scope !3, !noalias !0
  %index.next.3 = add i64 %index, 16
  %niter52.nsub.3 = add i64 %niter52, -4
  %niter52.ncmp.3 = icmp eq i64 %niter52.nsub.3, 0
  br i1 %niter52.ncmp.3, label %middle.block.unr-lcssa, label %vector.body, !llvm.loop !5

middle.block.unr-lcssa:                           ; preds = %vector.body, %vector.ph
  %index.unr = phi i64 [ 0, %vector.ph ], [ %index.next.3, %vector.body ]
  %lcmp.mod50 = icmp eq i64 %xtraiter48, 0
  br i1 %lcmp.mod50, label %middle.block, label %vector.body.epil

vector.body.epil:                                 ; preds = %middle.block.unr-lcssa, %vector.body.epil
  %index.epil = phi i64 [ %index.next.epil, %vector.body.epil ], [ %index.unr, %middle.block.unr-lcssa ]
  %epil.iter49 = phi i64 [ %epil.iter49.sub, %vector.body.epil ], [ %xtraiter48, %middle.block.unr-lcssa ]
  %72 = getelementptr i64, i64* %24, i64 %index.epil
  %73 = bitcast i64* %72 to <2 x i64>*
  %wide.load.epil = load <2 x i64>, <2 x i64>* %73, align 8, !alias.scope !0
  %74 = getelementptr i64, i64* %72, i64 2
  %75 = bitcast i64* %74 to <2 x i64>*
  %wide.load14.epil = load <2 x i64>, <2 x i64>* %75, align 8, !alias.scope !0
  %76 = getelementptr i8*, i8** %.cast, i64 %index.epil
  %77 = bitcast i8** %76 to <2 x i64>*
  store <2 x i64> %wide.load.epil, <2 x i64>* %77, align 8, !alias.scope !3, !noalias !0
  %78 = getelementptr i8*, i8** %76, i64 2
  %79 = bitcast i8** %78 to <2 x i64>*
  store <2 x i64> %wide.load14.epil, <2 x i64>* %79, align 8, !alias.scope !3, !noalias !0
  %index.next.epil = add i64 %index.epil, 4
  %epil.iter49.sub = add i64 %epil.iter49, -1
  %epil.iter49.cmp = icmp eq i64 %epil.iter49.sub, 0
  br i1 %epil.iter49.cmp, label %middle.block, label %vector.body.epil, !llvm.loop !7

middle.block:                                     ; preds = %vector.body.epil, %middle.block.unr-lcssa
  %cmp.n = icmp eq i64 %26, %n.vec
  br i1 %cmp.n, label %end_1.loopexit, label %body_1.preheader42

body_1.preheader42:                               ; preds = %middle.block, %vector.memcheck, %body_1.preheader
  %storemerge46.ph = phi i64 [ 0, %vector.memcheck ], [ 0, %body_1.preheader ], [ %n.vec, %middle.block ]
  %80 = sub i64 %26, %storemerge46.ph
  %81 = xor i64 %storemerge46.ph, -1
  %82 = add i64 %26, %81
  %xtraiter45 = and i64 %80, 7
  %lcmp.mod46 = icmp eq i64 %xtraiter45, 0
  br i1 %lcmp.mod46, label %body_1.prol.loopexit, label %body_1.prol

body_1.prol:                                      ; preds = %body_1.preheader42, %body_1.prol
  %storemerge46.prol = phi i64 [ %87, %body_1.prol ], [ %storemerge46.ph, %body_1.preheader42 ]
  %prol.iter47 = phi i64 [ %prol.iter47.sub, %body_1.prol ], [ %xtraiter45, %body_1.preheader42 ]
  %83 = getelementptr i64, i64* %24, i64 %storemerge46.prol
  %84 = load i64, i64* %83, align 8
  %85 = getelementptr i8*, i8** %.cast, i64 %storemerge46.prol
  %86 = bitcast i8** %85 to i64*
  store i64 %84, i64* %86, align 8
  %87 = add nuw nsw i64 %storemerge46.prol, 1
  %prol.iter47.sub = add i64 %prol.iter47, -1
  %prol.iter47.cmp = icmp eq i64 %prol.iter47.sub, 0
  br i1 %prol.iter47.cmp, label %body_1.prol.loopexit, label %body_1.prol, !llvm.loop !9

body_1.prol.loopexit:                             ; preds = %body_1.prol, %body_1.preheader42
  %storemerge46.unr = phi i64 [ %storemerge46.ph, %body_1.preheader42 ], [ %87, %body_1.prol ]
  %88 = icmp ult i64 %82, 7
  br i1 %88, label %end_1.loopexit, label %body_1

body_1:                                           ; preds = %body_1.prol.loopexit, %body_1
  %storemerge46 = phi i64 [ %128, %body_1 ], [ %storemerge46.unr, %body_1.prol.loopexit ]
  %89 = getelementptr i64, i64* %24, i64 %storemerge46
  %90 = load i64, i64* %89, align 8
  %91 = getelementptr i8*, i8** %.cast, i64 %storemerge46
  %92 = bitcast i8** %91 to i64*
  store i64 %90, i64* %92, align 8
  %93 = add nuw nsw i64 %storemerge46, 1
  %94 = getelementptr i64, i64* %24, i64 %93
  %95 = load i64, i64* %94, align 8
  %96 = getelementptr i8*, i8** %.cast, i64 %93
  %97 = bitcast i8** %96 to i64*
  store i64 %95, i64* %97, align 8
  %98 = add nsw i64 %storemerge46, 2
  %99 = getelementptr i64, i64* %24, i64 %98
  %100 = load i64, i64* %99, align 8
  %101 = getelementptr i8*, i8** %.cast, i64 %98
  %102 = bitcast i8** %101 to i64*
  store i64 %100, i64* %102, align 8
  %103 = add nsw i64 %storemerge46, 3
  %104 = getelementptr i64, i64* %24, i64 %103
  %105 = load i64, i64* %104, align 8
  %106 = getelementptr i8*, i8** %.cast, i64 %103
  %107 = bitcast i8** %106 to i64*
  store i64 %105, i64* %107, align 8
  %108 = add nsw i64 %storemerge46, 4
  %109 = getelementptr i64, i64* %24, i64 %108
  %110 = load i64, i64* %109, align 8
  %111 = getelementptr i8*, i8** %.cast, i64 %108
  %112 = bitcast i8** %111 to i64*
  store i64 %110, i64* %112, align 8
  %113 = add nsw i64 %storemerge46, 5
  %114 = getelementptr i64, i64* %24, i64 %113
  %115 = load i64, i64* %114, align 8
  %116 = getelementptr i8*, i8** %.cast, i64 %113
  %117 = bitcast i8** %116 to i64*
  store i64 %115, i64* %117, align 8
  %118 = add nsw i64 %storemerge46, 6
  %119 = getelementptr i64, i64* %24, i64 %118
  %120 = load i64, i64* %119, align 8
  %121 = getelementptr i8*, i8** %.cast, i64 %118
  %122 = bitcast i8** %121 to i64*
  store i64 %120, i64* %122, align 8
  %123 = add nsw i64 %storemerge46, 7
  %124 = getelementptr i64, i64* %24, i64 %123
  %125 = load i64, i64* %124, align 8
  %126 = getelementptr i8*, i8** %.cast, i64 %123
  %127 = bitcast i8** %126 to i64*
  store i64 %125, i64* %127, align 8
  %128 = add nsw i64 %storemerge46, 8
  %exitcond9.7 = icmp eq i64 %128, %26
  br i1 %exitcond9.7, label %end_1.loopexit, label %body_1, !llvm.loop !10

end_1.loopexit:                                   ; preds = %body_1.prol.loopexit, %body_1, %middle.block
  %.pre = load i8**, i8*** %30, align 8
  br label %end_1

end_1:                                            ; preds = %end_1.loopexit, %body_0
  %129 = phi i8** [ %.pre, %end_1.loopexit ], [ %.cast, %body_0 ]
  store i8* inttoptr (i64 10 to i8*), i8** %129, align 8
  %130 = load i8**, i8*** %30, align 8
  %131 = bitcast i8** %130 to i8*
  %132 = load i64, i64* %33, align 8
  %133 = shl i64 %132, 3
  %134 = tail call i8* @GC_malloc(i64 %133)
  %135 = tail call i8* @GC_malloc(i64 16)
  %136 = bitcast i8* %135 to i8**
  store i8* %134, i8** %136, align 8
  %137 = getelementptr i8, i8* %135, i64 8
  %138 = bitcast i8* %137 to i64*
  %.cast3 = bitcast i8* %134 to i64*
  store i64 %132, i64* %138, align 8
  %139 = icmp sgt i64 %132, 0
  br i1 %139, label %body_2.preheader, label %end_2

body_2.preheader:                                 ; preds = %end_1
  %min.iters.check18 = icmp ult i64 %132, 4
  br i1 %min.iters.check18, label %body_2.preheader41, label %vector.memcheck27

vector.memcheck27:                                ; preds = %body_2.preheader
  %140 = shl i64 %132, 3
  %scevgep20 = getelementptr i8, i8* %134, i64 %140
  %scevgep21 = getelementptr i8*, i8** %130, i64 %132
  %scevgep2122 = bitcast i8** %scevgep21 to i8*
  %bound023 = icmp ult i8* %134, %scevgep2122
  %bound124 = icmp ugt i8* %scevgep20, %131
  %found.conflict25 = and i1 %bound023, %bound124
  br i1 %found.conflict25, label %body_2.preheader41, label %vector.ph28

vector.ph28:                                      ; preds = %vector.memcheck27
  %n.vec30 = and i64 %132, -4
  %141 = add i64 %n.vec30, -4
  %142 = lshr exact i64 %141, 2
  %143 = add nuw nsw i64 %142, 1
  %xtraiter43 = and i64 %143, 3
  %144 = icmp ult i64 %141, 12
  br i1 %144, label %middle.block16.unr-lcssa, label %vector.ph28.new

vector.ph28.new:                                  ; preds = %vector.ph28
  %unroll_iter = sub nsw i64 %143, %xtraiter43
  br label %vector.body15

vector.body15:                                    ; preds = %vector.body15, %vector.ph28.new
  %index31 = phi i64 [ 0, %vector.ph28.new ], [ %index.next32.3, %vector.body15 ]
  %niter = phi i64 [ %unroll_iter, %vector.ph28.new ], [ %niter.nsub.3, %vector.body15 ]
  %145 = getelementptr i8*, i8** %130, i64 %index31
  %146 = bitcast i8** %145 to <2 x i64>*
  %wide.load39 = load <2 x i64>, <2 x i64>* %146, align 8, !alias.scope !11
  %147 = getelementptr i8*, i8** %145, i64 2
  %148 = bitcast i8** %147 to <2 x i64>*
  %wide.load40 = load <2 x i64>, <2 x i64>* %148, align 8, !alias.scope !11
  %149 = getelementptr i64, i64* %.cast3, i64 %index31
  %150 = bitcast i64* %149 to <2 x i64>*
  store <2 x i64> %wide.load39, <2 x i64>* %150, align 8, !alias.scope !14, !noalias !11
  %151 = getelementptr i64, i64* %149, i64 2
  %152 = bitcast i64* %151 to <2 x i64>*
  store <2 x i64> %wide.load40, <2 x i64>* %152, align 8, !alias.scope !14, !noalias !11
  %index.next32 = or i64 %index31, 4
  %153 = getelementptr i8*, i8** %130, i64 %index.next32
  %154 = bitcast i8** %153 to <2 x i64>*
  %wide.load39.1 = load <2 x i64>, <2 x i64>* %154, align 8, !alias.scope !11
  %155 = getelementptr i8*, i8** %153, i64 2
  %156 = bitcast i8** %155 to <2 x i64>*
  %wide.load40.1 = load <2 x i64>, <2 x i64>* %156, align 8, !alias.scope !11
  %157 = getelementptr i64, i64* %.cast3, i64 %index.next32
  %158 = bitcast i64* %157 to <2 x i64>*
  store <2 x i64> %wide.load39.1, <2 x i64>* %158, align 8, !alias.scope !14, !noalias !11
  %159 = getelementptr i64, i64* %157, i64 2
  %160 = bitcast i64* %159 to <2 x i64>*
  store <2 x i64> %wide.load40.1, <2 x i64>* %160, align 8, !alias.scope !14, !noalias !11
  %index.next32.1 = or i64 %index31, 8
  %161 = getelementptr i8*, i8** %130, i64 %index.next32.1
  %162 = bitcast i8** %161 to <2 x i64>*
  %wide.load39.2 = load <2 x i64>, <2 x i64>* %162, align 8, !alias.scope !11
  %163 = getelementptr i8*, i8** %161, i64 2
  %164 = bitcast i8** %163 to <2 x i64>*
  %wide.load40.2 = load <2 x i64>, <2 x i64>* %164, align 8, !alias.scope !11
  %165 = getelementptr i64, i64* %.cast3, i64 %index.next32.1
  %166 = bitcast i64* %165 to <2 x i64>*
  store <2 x i64> %wide.load39.2, <2 x i64>* %166, align 8, !alias.scope !14, !noalias !11
  %167 = getelementptr i64, i64* %165, i64 2
  %168 = bitcast i64* %167 to <2 x i64>*
  store <2 x i64> %wide.load40.2, <2 x i64>* %168, align 8, !alias.scope !14, !noalias !11
  %index.next32.2 = or i64 %index31, 12
  %169 = getelementptr i8*, i8** %130, i64 %index.next32.2
  %170 = bitcast i8** %169 to <2 x i64>*
  %wide.load39.3 = load <2 x i64>, <2 x i64>* %170, align 8, !alias.scope !11
  %171 = getelementptr i8*, i8** %169, i64 2
  %172 = bitcast i8** %171 to <2 x i64>*
  %wide.load40.3 = load <2 x i64>, <2 x i64>* %172, align 8, !alias.scope !11
  %173 = getelementptr i64, i64* %.cast3, i64 %index.next32.2
  %174 = bitcast i64* %173 to <2 x i64>*
  store <2 x i64> %wide.load39.3, <2 x i64>* %174, align 8, !alias.scope !14, !noalias !11
  %175 = getelementptr i64, i64* %173, i64 2
  %176 = bitcast i64* %175 to <2 x i64>*
  store <2 x i64> %wide.load40.3, <2 x i64>* %176, align 8, !alias.scope !14, !noalias !11
  %index.next32.3 = add i64 %index31, 16
  %niter.nsub.3 = add i64 %niter, -4
  %niter.ncmp.3 = icmp eq i64 %niter.nsub.3, 0
  br i1 %niter.ncmp.3, label %middle.block16.unr-lcssa, label %vector.body15, !llvm.loop !16

middle.block16.unr-lcssa:                         ; preds = %vector.body15, %vector.ph28
  %index31.unr = phi i64 [ 0, %vector.ph28 ], [ %index.next32.3, %vector.body15 ]
  %lcmp.mod44 = icmp eq i64 %xtraiter43, 0
  br i1 %lcmp.mod44, label %middle.block16, label %vector.body15.epil

vector.body15.epil:                               ; preds = %middle.block16.unr-lcssa, %vector.body15.epil
  %index31.epil = phi i64 [ %index.next32.epil, %vector.body15.epil ], [ %index31.unr, %middle.block16.unr-lcssa ]
  %epil.iter = phi i64 [ %epil.iter.sub, %vector.body15.epil ], [ %xtraiter43, %middle.block16.unr-lcssa ]
  %177 = getelementptr i8*, i8** %130, i64 %index31.epil
  %178 = bitcast i8** %177 to <2 x i64>*
  %wide.load39.epil = load <2 x i64>, <2 x i64>* %178, align 8, !alias.scope !11
  %179 = getelementptr i8*, i8** %177, i64 2
  %180 = bitcast i8** %179 to <2 x i64>*
  %wide.load40.epil = load <2 x i64>, <2 x i64>* %180, align 8, !alias.scope !11
  %181 = getelementptr i64, i64* %.cast3, i64 %index31.epil
  %182 = bitcast i64* %181 to <2 x i64>*
  store <2 x i64> %wide.load39.epil, <2 x i64>* %182, align 8, !alias.scope !14, !noalias !11
  %183 = getelementptr i64, i64* %181, i64 2
  %184 = bitcast i64* %183 to <2 x i64>*
  store <2 x i64> %wide.load40.epil, <2 x i64>* %184, align 8, !alias.scope !14, !noalias !11
  %index.next32.epil = add i64 %index31.epil, 4
  %epil.iter.sub = add i64 %epil.iter, -1
  %epil.iter.cmp = icmp eq i64 %epil.iter.sub, 0
  br i1 %epil.iter.cmp, label %middle.block16, label %vector.body15.epil, !llvm.loop !17

middle.block16:                                   ; preds = %vector.body15.epil, %middle.block16.unr-lcssa
  %cmp.n34 = icmp eq i64 %132, %n.vec30
  br i1 %cmp.n34, label %end_2, label %body_2.preheader41

body_2.preheader41:                               ; preds = %middle.block16, %vector.memcheck27, %body_2.preheader
  %storemerge5.ph = phi i64 [ 0, %vector.memcheck27 ], [ 0, %body_2.preheader ], [ %n.vec30, %middle.block16 ]
  %185 = xor i64 %storemerge5.ph, -1
  %186 = add i64 %132, %185
  %xtraiter = and i64 %132, 3
  %lcmp.mod = icmp eq i64 %xtraiter, 0
  br i1 %lcmp.mod, label %body_2.prol.loopexit, label %body_2.prol

body_2.prol:                                      ; preds = %body_2.preheader41, %body_2.prol
  %storemerge5.prol = phi i64 [ %191, %body_2.prol ], [ %storemerge5.ph, %body_2.preheader41 ]
  %prol.iter = phi i64 [ %prol.iter.sub, %body_2.prol ], [ %xtraiter, %body_2.preheader41 ]
  %187 = getelementptr i8*, i8** %130, i64 %storemerge5.prol
  %188 = bitcast i8** %187 to i64*
  %189 = load i64, i64* %188, align 8
  %190 = getelementptr i64, i64* %.cast3, i64 %storemerge5.prol
  store i64 %189, i64* %190, align 8
  %191 = add nuw nsw i64 %storemerge5.prol, 1
  %prol.iter.sub = add i64 %prol.iter, -1
  %prol.iter.cmp = icmp eq i64 %prol.iter.sub, 0
  br i1 %prol.iter.cmp, label %body_2.prol.loopexit, label %body_2.prol, !llvm.loop !18

body_2.prol.loopexit:                             ; preds = %body_2.prol, %body_2.preheader41
  %storemerge5.unr = phi i64 [ %storemerge5.ph, %body_2.preheader41 ], [ %191, %body_2.prol ]
  %192 = icmp ult i64 %186, 3
  br i1 %192, label %end_2, label %body_2

body_2:                                           ; preds = %body_2.prol.loopexit, %body_2
  %storemerge5 = phi i64 [ %212, %body_2 ], [ %storemerge5.unr, %body_2.prol.loopexit ]
  %193 = getelementptr i8*, i8** %130, i64 %storemerge5
  %194 = bitcast i8** %193 to i64*
  %195 = load i64, i64* %194, align 8
  %196 = getelementptr i64, i64* %.cast3, i64 %storemerge5
  store i64 %195, i64* %196, align 8
  %197 = add nuw nsw i64 %storemerge5, 1
  %198 = getelementptr i8*, i8** %130, i64 %197
  %199 = bitcast i8** %198 to i64*
  %200 = load i64, i64* %199, align 8
  %201 = getelementptr i64, i64* %.cast3, i64 %197
  store i64 %200, i64* %201, align 8
  %202 = add nsw i64 %storemerge5, 2
  %203 = getelementptr i8*, i8** %130, i64 %202
  %204 = bitcast i8** %203 to i64*
  %205 = load i64, i64* %204, align 8
  %206 = getelementptr i64, i64* %.cast3, i64 %202
  store i64 %205, i64* %206, align 8
  %207 = add nsw i64 %storemerge5, 3
  %208 = getelementptr i8*, i8** %130, i64 %207
  %209 = bitcast i8** %208 to i64*
  %210 = load i64, i64* %209, align 8
  %211 = getelementptr i64, i64* %.cast3, i64 %207
  store i64 %210, i64* %211, align 8
  %212 = add nsw i64 %storemerge5, 4
  %exitcond.3 = icmp eq i64 %212, %132
  br i1 %exitcond.3, label %end_2, label %body_2, !llvm.loop !19

end_2:                                            ; preds = %body_2.prol.loopexit, %body_2, %middle.block16, %end_1
  %213 = tail call i8* @GC_malloc(i64 0)
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
!11 = !{!12}
!12 = distinct !{!12, !13}
!13 = distinct !{!13, !"LVerDomain"}
!14 = !{!15}
!15 = distinct !{!15, !13}
!16 = distinct !{!16, !6}
!17 = distinct !{!17, !8}
!18 = distinct !{!18, !8}
!19 = distinct !{!19, !6}
