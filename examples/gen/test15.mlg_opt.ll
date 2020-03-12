; ModuleID = './examples/gen/test15.mlg.ll'
source_filename = "./examples/test15.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

; Function Attrs: norecurse nounwind
define noalias {}* @update1({ i8**, i64 }* nocapture readonly, i64, i8*) local_unnamed_addr #0 {
  %4 = getelementptr { i8**, i64 }, { i8**, i64 }* %0, i64 0, i32 0
  %5 = load i8**, i8*** %4, align 8
  %6 = getelementptr i8*, i8** %5, i64 %1
  store i8* %2, i8** %6, align 8
  ret {}* undef
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int5(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

declare void @GC_init() local_unnamed_addr

declare i8* @GC_malloc(i64) local_unnamed_addr

define i32 @main() local_unnamed_addr {
body_0:
  tail call void @GC_init()
  %0 = tail call i8* @GC_malloc(i64 24)
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to i64**
  %3 = bitcast i8* %1 to i8**
  store i8* %0, i8** %3, align 8
  %4 = getelementptr i8, i8* %1, i64 8
  %5 = bitcast i8* %4 to i64*
  store i64 3, i64* %5, align 8
  %.cast8 = bitcast i8* %0 to i64*
  store i64 1, i64* %.cast8, align 8
  %6 = load i64*, i64** %2, align 8
  %7 = getelementptr i64, i64* %6, i64 1
  store i64 1, i64* %7, align 8
  %8 = load i64*, i64** %2, align 8
  %9 = getelementptr i64, i64* %8, i64 2
  store i64 1, i64* %9, align 8
  %10 = load i64*, i64** %2, align 8
  %11 = getelementptr i64, i64* %10, i64 1
  store i64 2, i64* %11, align 8
  %12 = load i64*, i64** %2, align 8
  %13 = getelementptr i64, i64* %12, i64 2
  store i64 3, i64* %13, align 8
  %14 = load i64*, i64** %2, align 8
  %15 = bitcast i64* %14 to i8*
  %16 = load i64, i64* %5, align 8
  %17 = shl i64 %16, 3
  %18 = tail call i8* @GC_malloc(i64 %17)
  %19 = tail call i8* @GC_malloc(i64 16)
  %20 = bitcast i8* %19 to i8***
  %21 = bitcast i8* %19 to i8**
  store i8* %18, i8** %21, align 8
  %22 = getelementptr i8, i8* %19, i64 8
  %23 = bitcast i8* %22 to i64*
  %.cast = bitcast i8* %18 to i8**
  store i64 %16, i64* %23, align 8
  %24 = icmp sgt i64 %16, 0
  br i1 %24, label %body_1.preheader, label %end_1

body_1.preheader:                                 ; preds = %body_0
  %min.iters.check = icmp ult i64 %16, 4
  br i1 %min.iters.check, label %body_1.preheader43, label %vector.memcheck

vector.memcheck:                                  ; preds = %body_1.preheader
  %25 = shl i64 %16, 3
  %scevgep = getelementptr i8, i8* %18, i64 %25
  %scevgep12 = getelementptr i64, i64* %14, i64 %16
  %scevgep1213 = bitcast i64* %scevgep12 to i8*
  %bound0 = icmp ult i8* %18, %scevgep1213
  %bound1 = icmp ugt i8* %scevgep, %15
  %found.conflict = and i1 %bound0, %bound1
  br i1 %found.conflict, label %body_1.preheader43, label %vector.ph

vector.ph:                                        ; preds = %vector.memcheck
  %n.vec = and i64 %16, -4
  %26 = add i64 %n.vec, -4
  %27 = lshr exact i64 %26, 2
  %28 = add nuw nsw i64 %27, 1
  %xtraiter49 = and i64 %28, 3
  %29 = icmp ult i64 %26, 12
  br i1 %29, label %middle.block.unr-lcssa, label %vector.ph.new

vector.ph.new:                                    ; preds = %vector.ph
  %unroll_iter52 = sub nsw i64 %28, %xtraiter49
  br label %vector.body

vector.body:                                      ; preds = %vector.body, %vector.ph.new
  %index = phi i64 [ 0, %vector.ph.new ], [ %index.next.3, %vector.body ]
  %niter53 = phi i64 [ %unroll_iter52, %vector.ph.new ], [ %niter53.nsub.3, %vector.body ]
  %30 = getelementptr i64, i64* %14, i64 %index
  %31 = bitcast i64* %30 to <2 x i64>*
  %wide.load = load <2 x i64>, <2 x i64>* %31, align 8, !alias.scope !0
  %32 = getelementptr i64, i64* %30, i64 2
  %33 = bitcast i64* %32 to <2 x i64>*
  %wide.load15 = load <2 x i64>, <2 x i64>* %33, align 8, !alias.scope !0
  %34 = getelementptr i8*, i8** %.cast, i64 %index
  %35 = bitcast i8** %34 to <2 x i64>*
  store <2 x i64> %wide.load, <2 x i64>* %35, align 8, !alias.scope !3, !noalias !0
  %36 = getelementptr i8*, i8** %34, i64 2
  %37 = bitcast i8** %36 to <2 x i64>*
  store <2 x i64> %wide.load15, <2 x i64>* %37, align 8, !alias.scope !3, !noalias !0
  %index.next = or i64 %index, 4
  %38 = getelementptr i64, i64* %14, i64 %index.next
  %39 = bitcast i64* %38 to <2 x i64>*
  %wide.load.1 = load <2 x i64>, <2 x i64>* %39, align 8, !alias.scope !0
  %40 = getelementptr i64, i64* %38, i64 2
  %41 = bitcast i64* %40 to <2 x i64>*
  %wide.load15.1 = load <2 x i64>, <2 x i64>* %41, align 8, !alias.scope !0
  %42 = getelementptr i8*, i8** %.cast, i64 %index.next
  %43 = bitcast i8** %42 to <2 x i64>*
  store <2 x i64> %wide.load.1, <2 x i64>* %43, align 8, !alias.scope !3, !noalias !0
  %44 = getelementptr i8*, i8** %42, i64 2
  %45 = bitcast i8** %44 to <2 x i64>*
  store <2 x i64> %wide.load15.1, <2 x i64>* %45, align 8, !alias.scope !3, !noalias !0
  %index.next.1 = or i64 %index, 8
  %46 = getelementptr i64, i64* %14, i64 %index.next.1
  %47 = bitcast i64* %46 to <2 x i64>*
  %wide.load.2 = load <2 x i64>, <2 x i64>* %47, align 8, !alias.scope !0
  %48 = getelementptr i64, i64* %46, i64 2
  %49 = bitcast i64* %48 to <2 x i64>*
  %wide.load15.2 = load <2 x i64>, <2 x i64>* %49, align 8, !alias.scope !0
  %50 = getelementptr i8*, i8** %.cast, i64 %index.next.1
  %51 = bitcast i8** %50 to <2 x i64>*
  store <2 x i64> %wide.load.2, <2 x i64>* %51, align 8, !alias.scope !3, !noalias !0
  %52 = getelementptr i8*, i8** %50, i64 2
  %53 = bitcast i8** %52 to <2 x i64>*
  store <2 x i64> %wide.load15.2, <2 x i64>* %53, align 8, !alias.scope !3, !noalias !0
  %index.next.2 = or i64 %index, 12
  %54 = getelementptr i64, i64* %14, i64 %index.next.2
  %55 = bitcast i64* %54 to <2 x i64>*
  %wide.load.3 = load <2 x i64>, <2 x i64>* %55, align 8, !alias.scope !0
  %56 = getelementptr i64, i64* %54, i64 2
  %57 = bitcast i64* %56 to <2 x i64>*
  %wide.load15.3 = load <2 x i64>, <2 x i64>* %57, align 8, !alias.scope !0
  %58 = getelementptr i8*, i8** %.cast, i64 %index.next.2
  %59 = bitcast i8** %58 to <2 x i64>*
  store <2 x i64> %wide.load.3, <2 x i64>* %59, align 8, !alias.scope !3, !noalias !0
  %60 = getelementptr i8*, i8** %58, i64 2
  %61 = bitcast i8** %60 to <2 x i64>*
  store <2 x i64> %wide.load15.3, <2 x i64>* %61, align 8, !alias.scope !3, !noalias !0
  %index.next.3 = add i64 %index, 16
  %niter53.nsub.3 = add i64 %niter53, -4
  %niter53.ncmp.3 = icmp eq i64 %niter53.nsub.3, 0
  br i1 %niter53.ncmp.3, label %middle.block.unr-lcssa, label %vector.body, !llvm.loop !5

middle.block.unr-lcssa:                           ; preds = %vector.body, %vector.ph
  %index.unr = phi i64 [ 0, %vector.ph ], [ %index.next.3, %vector.body ]
  %lcmp.mod51 = icmp eq i64 %xtraiter49, 0
  br i1 %lcmp.mod51, label %middle.block, label %vector.body.epil

vector.body.epil:                                 ; preds = %middle.block.unr-lcssa, %vector.body.epil
  %index.epil = phi i64 [ %index.next.epil, %vector.body.epil ], [ %index.unr, %middle.block.unr-lcssa ]
  %epil.iter50 = phi i64 [ %epil.iter50.sub, %vector.body.epil ], [ %xtraiter49, %middle.block.unr-lcssa ]
  %62 = getelementptr i64, i64* %14, i64 %index.epil
  %63 = bitcast i64* %62 to <2 x i64>*
  %wide.load.epil = load <2 x i64>, <2 x i64>* %63, align 8, !alias.scope !0
  %64 = getelementptr i64, i64* %62, i64 2
  %65 = bitcast i64* %64 to <2 x i64>*
  %wide.load15.epil = load <2 x i64>, <2 x i64>* %65, align 8, !alias.scope !0
  %66 = getelementptr i8*, i8** %.cast, i64 %index.epil
  %67 = bitcast i8** %66 to <2 x i64>*
  store <2 x i64> %wide.load.epil, <2 x i64>* %67, align 8, !alias.scope !3, !noalias !0
  %68 = getelementptr i8*, i8** %66, i64 2
  %69 = bitcast i8** %68 to <2 x i64>*
  store <2 x i64> %wide.load15.epil, <2 x i64>* %69, align 8, !alias.scope !3, !noalias !0
  %index.next.epil = add i64 %index.epil, 4
  %epil.iter50.sub = add i64 %epil.iter50, -1
  %epil.iter50.cmp = icmp eq i64 %epil.iter50.sub, 0
  br i1 %epil.iter50.cmp, label %middle.block, label %vector.body.epil, !llvm.loop !7

middle.block:                                     ; preds = %vector.body.epil, %middle.block.unr-lcssa
  %cmp.n = icmp eq i64 %16, %n.vec
  br i1 %cmp.n, label %end_1.loopexit, label %body_1.preheader43

body_1.preheader43:                               ; preds = %middle.block, %vector.memcheck, %body_1.preheader
  %storemerge46.ph = phi i64 [ 0, %vector.memcheck ], [ 0, %body_1.preheader ], [ %n.vec, %middle.block ]
  %70 = sub i64 %16, %storemerge46.ph
  %71 = xor i64 %storemerge46.ph, -1
  %72 = add i64 %16, %71
  %xtraiter46 = and i64 %70, 7
  %lcmp.mod47 = icmp eq i64 %xtraiter46, 0
  br i1 %lcmp.mod47, label %body_1.prol.loopexit, label %body_1.prol

body_1.prol:                                      ; preds = %body_1.preheader43, %body_1.prol
  %storemerge46.prol = phi i64 [ %77, %body_1.prol ], [ %storemerge46.ph, %body_1.preheader43 ]
  %prol.iter48 = phi i64 [ %prol.iter48.sub, %body_1.prol ], [ %xtraiter46, %body_1.preheader43 ]
  %73 = getelementptr i64, i64* %14, i64 %storemerge46.prol
  %74 = load i64, i64* %73, align 8
  %75 = getelementptr i8*, i8** %.cast, i64 %storemerge46.prol
  %76 = bitcast i8** %75 to i64*
  store i64 %74, i64* %76, align 8
  %77 = add nuw nsw i64 %storemerge46.prol, 1
  %prol.iter48.sub = add i64 %prol.iter48, -1
  %prol.iter48.cmp = icmp eq i64 %prol.iter48.sub, 0
  br i1 %prol.iter48.cmp, label %body_1.prol.loopexit, label %body_1.prol, !llvm.loop !9

body_1.prol.loopexit:                             ; preds = %body_1.prol, %body_1.preheader43
  %storemerge46.unr = phi i64 [ %storemerge46.ph, %body_1.preheader43 ], [ %77, %body_1.prol ]
  %78 = icmp ult i64 %72, 7
  br i1 %78, label %end_1.loopexit, label %body_1

body_1:                                           ; preds = %body_1.prol.loopexit, %body_1
  %storemerge46 = phi i64 [ %118, %body_1 ], [ %storemerge46.unr, %body_1.prol.loopexit ]
  %79 = getelementptr i64, i64* %14, i64 %storemerge46
  %80 = load i64, i64* %79, align 8
  %81 = getelementptr i8*, i8** %.cast, i64 %storemerge46
  %82 = bitcast i8** %81 to i64*
  store i64 %80, i64* %82, align 8
  %83 = add nuw nsw i64 %storemerge46, 1
  %84 = getelementptr i64, i64* %14, i64 %83
  %85 = load i64, i64* %84, align 8
  %86 = getelementptr i8*, i8** %.cast, i64 %83
  %87 = bitcast i8** %86 to i64*
  store i64 %85, i64* %87, align 8
  %88 = add nsw i64 %storemerge46, 2
  %89 = getelementptr i64, i64* %14, i64 %88
  %90 = load i64, i64* %89, align 8
  %91 = getelementptr i8*, i8** %.cast, i64 %88
  %92 = bitcast i8** %91 to i64*
  store i64 %90, i64* %92, align 8
  %93 = add nsw i64 %storemerge46, 3
  %94 = getelementptr i64, i64* %14, i64 %93
  %95 = load i64, i64* %94, align 8
  %96 = getelementptr i8*, i8** %.cast, i64 %93
  %97 = bitcast i8** %96 to i64*
  store i64 %95, i64* %97, align 8
  %98 = add nsw i64 %storemerge46, 4
  %99 = getelementptr i64, i64* %14, i64 %98
  %100 = load i64, i64* %99, align 8
  %101 = getelementptr i8*, i8** %.cast, i64 %98
  %102 = bitcast i8** %101 to i64*
  store i64 %100, i64* %102, align 8
  %103 = add nsw i64 %storemerge46, 5
  %104 = getelementptr i64, i64* %14, i64 %103
  %105 = load i64, i64* %104, align 8
  %106 = getelementptr i8*, i8** %.cast, i64 %103
  %107 = bitcast i8** %106 to i64*
  store i64 %105, i64* %107, align 8
  %108 = add nsw i64 %storemerge46, 6
  %109 = getelementptr i64, i64* %14, i64 %108
  %110 = load i64, i64* %109, align 8
  %111 = getelementptr i8*, i8** %.cast, i64 %108
  %112 = bitcast i8** %111 to i64*
  store i64 %110, i64* %112, align 8
  %113 = add nsw i64 %storemerge46, 7
  %114 = getelementptr i64, i64* %14, i64 %113
  %115 = load i64, i64* %114, align 8
  %116 = getelementptr i8*, i8** %.cast, i64 %113
  %117 = bitcast i8** %116 to i64*
  store i64 %115, i64* %117, align 8
  %118 = add nsw i64 %storemerge46, 8
  %exitcond9.7 = icmp eq i64 %118, %16
  br i1 %exitcond9.7, label %end_1.loopexit, label %body_1, !llvm.loop !10

end_1.loopexit:                                   ; preds = %body_1.prol.loopexit, %body_1, %middle.block
  %.pre = load i8**, i8*** %20, align 8
  br label %end_1

end_1:                                            ; preds = %end_1.loopexit, %body_0
  %119 = phi i8** [ %.pre, %end_1.loopexit ], [ %.cast, %body_0 ]
  store i8* inttoptr (i64 42 to i8*), i8** %119, align 8
  %120 = load i8**, i8*** %20, align 8
  %121 = bitcast i8** %120 to i8*
  %122 = load i64, i64* %23, align 8
  %123 = shl i64 %122, 3
  %124 = tail call i8* @GC_malloc(i64 %123)
  %125 = tail call i8* @GC_malloc(i64 16)
  %126 = bitcast i8* %125 to i64**
  %127 = bitcast i8* %125 to i8**
  store i8* %124, i8** %127, align 8
  %128 = getelementptr i8, i8* %125, i64 8
  %129 = bitcast i8* %128 to i64*
  %.cast3 = bitcast i8* %124 to i64*
  store i64 %122, i64* %129, align 8
  %130 = icmp sgt i64 %122, 0
  br i1 %130, label %body_2.preheader, label %end_2

body_2.preheader:                                 ; preds = %end_1
  %min.iters.check19 = icmp ult i64 %122, 4
  br i1 %min.iters.check19, label %body_2.preheader42, label %vector.memcheck28

vector.memcheck28:                                ; preds = %body_2.preheader
  %131 = shl i64 %122, 3
  %scevgep21 = getelementptr i8, i8* %124, i64 %131
  %scevgep22 = getelementptr i8*, i8** %120, i64 %122
  %scevgep2223 = bitcast i8** %scevgep22 to i8*
  %bound024 = icmp ult i8* %124, %scevgep2223
  %bound125 = icmp ugt i8* %scevgep21, %121
  %found.conflict26 = and i1 %bound024, %bound125
  br i1 %found.conflict26, label %body_2.preheader42, label %vector.ph29

vector.ph29:                                      ; preds = %vector.memcheck28
  %n.vec31 = and i64 %122, -4
  %132 = add i64 %n.vec31, -4
  %133 = lshr exact i64 %132, 2
  %134 = add nuw nsw i64 %133, 1
  %xtraiter44 = and i64 %134, 3
  %135 = icmp ult i64 %132, 12
  br i1 %135, label %middle.block17.unr-lcssa, label %vector.ph29.new

vector.ph29.new:                                  ; preds = %vector.ph29
  %unroll_iter = sub nsw i64 %134, %xtraiter44
  br label %vector.body16

vector.body16:                                    ; preds = %vector.body16, %vector.ph29.new
  %index32 = phi i64 [ 0, %vector.ph29.new ], [ %index.next33.3, %vector.body16 ]
  %niter = phi i64 [ %unroll_iter, %vector.ph29.new ], [ %niter.nsub.3, %vector.body16 ]
  %136 = getelementptr i8*, i8** %120, i64 %index32
  %137 = bitcast i8** %136 to <2 x i64>*
  %wide.load40 = load <2 x i64>, <2 x i64>* %137, align 8, !alias.scope !11
  %138 = getelementptr i8*, i8** %136, i64 2
  %139 = bitcast i8** %138 to <2 x i64>*
  %wide.load41 = load <2 x i64>, <2 x i64>* %139, align 8, !alias.scope !11
  %140 = getelementptr i64, i64* %.cast3, i64 %index32
  %141 = bitcast i64* %140 to <2 x i64>*
  store <2 x i64> %wide.load40, <2 x i64>* %141, align 8, !alias.scope !14, !noalias !11
  %142 = getelementptr i64, i64* %140, i64 2
  %143 = bitcast i64* %142 to <2 x i64>*
  store <2 x i64> %wide.load41, <2 x i64>* %143, align 8, !alias.scope !14, !noalias !11
  %index.next33 = or i64 %index32, 4
  %144 = getelementptr i8*, i8** %120, i64 %index.next33
  %145 = bitcast i8** %144 to <2 x i64>*
  %wide.load40.1 = load <2 x i64>, <2 x i64>* %145, align 8, !alias.scope !11
  %146 = getelementptr i8*, i8** %144, i64 2
  %147 = bitcast i8** %146 to <2 x i64>*
  %wide.load41.1 = load <2 x i64>, <2 x i64>* %147, align 8, !alias.scope !11
  %148 = getelementptr i64, i64* %.cast3, i64 %index.next33
  %149 = bitcast i64* %148 to <2 x i64>*
  store <2 x i64> %wide.load40.1, <2 x i64>* %149, align 8, !alias.scope !14, !noalias !11
  %150 = getelementptr i64, i64* %148, i64 2
  %151 = bitcast i64* %150 to <2 x i64>*
  store <2 x i64> %wide.load41.1, <2 x i64>* %151, align 8, !alias.scope !14, !noalias !11
  %index.next33.1 = or i64 %index32, 8
  %152 = getelementptr i8*, i8** %120, i64 %index.next33.1
  %153 = bitcast i8** %152 to <2 x i64>*
  %wide.load40.2 = load <2 x i64>, <2 x i64>* %153, align 8, !alias.scope !11
  %154 = getelementptr i8*, i8** %152, i64 2
  %155 = bitcast i8** %154 to <2 x i64>*
  %wide.load41.2 = load <2 x i64>, <2 x i64>* %155, align 8, !alias.scope !11
  %156 = getelementptr i64, i64* %.cast3, i64 %index.next33.1
  %157 = bitcast i64* %156 to <2 x i64>*
  store <2 x i64> %wide.load40.2, <2 x i64>* %157, align 8, !alias.scope !14, !noalias !11
  %158 = getelementptr i64, i64* %156, i64 2
  %159 = bitcast i64* %158 to <2 x i64>*
  store <2 x i64> %wide.load41.2, <2 x i64>* %159, align 8, !alias.scope !14, !noalias !11
  %index.next33.2 = or i64 %index32, 12
  %160 = getelementptr i8*, i8** %120, i64 %index.next33.2
  %161 = bitcast i8** %160 to <2 x i64>*
  %wide.load40.3 = load <2 x i64>, <2 x i64>* %161, align 8, !alias.scope !11
  %162 = getelementptr i8*, i8** %160, i64 2
  %163 = bitcast i8** %162 to <2 x i64>*
  %wide.load41.3 = load <2 x i64>, <2 x i64>* %163, align 8, !alias.scope !11
  %164 = getelementptr i64, i64* %.cast3, i64 %index.next33.2
  %165 = bitcast i64* %164 to <2 x i64>*
  store <2 x i64> %wide.load40.3, <2 x i64>* %165, align 8, !alias.scope !14, !noalias !11
  %166 = getelementptr i64, i64* %164, i64 2
  %167 = bitcast i64* %166 to <2 x i64>*
  store <2 x i64> %wide.load41.3, <2 x i64>* %167, align 8, !alias.scope !14, !noalias !11
  %index.next33.3 = add i64 %index32, 16
  %niter.nsub.3 = add i64 %niter, -4
  %niter.ncmp.3 = icmp eq i64 %niter.nsub.3, 0
  br i1 %niter.ncmp.3, label %middle.block17.unr-lcssa, label %vector.body16, !llvm.loop !16

middle.block17.unr-lcssa:                         ; preds = %vector.body16, %vector.ph29
  %index32.unr = phi i64 [ 0, %vector.ph29 ], [ %index.next33.3, %vector.body16 ]
  %lcmp.mod45 = icmp eq i64 %xtraiter44, 0
  br i1 %lcmp.mod45, label %middle.block17, label %vector.body16.epil

vector.body16.epil:                               ; preds = %middle.block17.unr-lcssa, %vector.body16.epil
  %index32.epil = phi i64 [ %index.next33.epil, %vector.body16.epil ], [ %index32.unr, %middle.block17.unr-lcssa ]
  %epil.iter = phi i64 [ %epil.iter.sub, %vector.body16.epil ], [ %xtraiter44, %middle.block17.unr-lcssa ]
  %168 = getelementptr i8*, i8** %120, i64 %index32.epil
  %169 = bitcast i8** %168 to <2 x i64>*
  %wide.load40.epil = load <2 x i64>, <2 x i64>* %169, align 8, !alias.scope !11
  %170 = getelementptr i8*, i8** %168, i64 2
  %171 = bitcast i8** %170 to <2 x i64>*
  %wide.load41.epil = load <2 x i64>, <2 x i64>* %171, align 8, !alias.scope !11
  %172 = getelementptr i64, i64* %.cast3, i64 %index32.epil
  %173 = bitcast i64* %172 to <2 x i64>*
  store <2 x i64> %wide.load40.epil, <2 x i64>* %173, align 8, !alias.scope !14, !noalias !11
  %174 = getelementptr i64, i64* %172, i64 2
  %175 = bitcast i64* %174 to <2 x i64>*
  store <2 x i64> %wide.load41.epil, <2 x i64>* %175, align 8, !alias.scope !14, !noalias !11
  %index.next33.epil = add i64 %index32.epil, 4
  %epil.iter.sub = add i64 %epil.iter, -1
  %epil.iter.cmp = icmp eq i64 %epil.iter.sub, 0
  br i1 %epil.iter.cmp, label %middle.block17, label %vector.body16.epil, !llvm.loop !17

middle.block17:                                   ; preds = %vector.body16.epil, %middle.block17.unr-lcssa
  %cmp.n35 = icmp eq i64 %122, %n.vec31
  br i1 %cmp.n35, label %end_2.loopexit, label %body_2.preheader42

body_2.preheader42:                               ; preds = %middle.block17, %vector.memcheck28, %body_2.preheader
  %storemerge5.ph = phi i64 [ 0, %vector.memcheck28 ], [ 0, %body_2.preheader ], [ %n.vec31, %middle.block17 ]
  %176 = xor i64 %storemerge5.ph, -1
  %177 = add i64 %122, %176
  %xtraiter = and i64 %122, 3
  %lcmp.mod = icmp eq i64 %xtraiter, 0
  br i1 %lcmp.mod, label %body_2.prol.loopexit, label %body_2.prol

body_2.prol:                                      ; preds = %body_2.preheader42, %body_2.prol
  %storemerge5.prol = phi i64 [ %182, %body_2.prol ], [ %storemerge5.ph, %body_2.preheader42 ]
  %prol.iter = phi i64 [ %prol.iter.sub, %body_2.prol ], [ %xtraiter, %body_2.preheader42 ]
  %178 = getelementptr i8*, i8** %120, i64 %storemerge5.prol
  %179 = bitcast i8** %178 to i64*
  %180 = load i64, i64* %179, align 8
  %181 = getelementptr i64, i64* %.cast3, i64 %storemerge5.prol
  store i64 %180, i64* %181, align 8
  %182 = add nuw nsw i64 %storemerge5.prol, 1
  %prol.iter.sub = add i64 %prol.iter, -1
  %prol.iter.cmp = icmp eq i64 %prol.iter.sub, 0
  br i1 %prol.iter.cmp, label %body_2.prol.loopexit, label %body_2.prol, !llvm.loop !18

body_2.prol.loopexit:                             ; preds = %body_2.prol, %body_2.preheader42
  %storemerge5.unr = phi i64 [ %storemerge5.ph, %body_2.preheader42 ], [ %182, %body_2.prol ]
  %183 = icmp ult i64 %177, 3
  br i1 %183, label %end_2.loopexit, label %body_2

body_2:                                           ; preds = %body_2.prol.loopexit, %body_2
  %storemerge5 = phi i64 [ %203, %body_2 ], [ %storemerge5.unr, %body_2.prol.loopexit ]
  %184 = getelementptr i8*, i8** %120, i64 %storemerge5
  %185 = bitcast i8** %184 to i64*
  %186 = load i64, i64* %185, align 8
  %187 = getelementptr i64, i64* %.cast3, i64 %storemerge5
  store i64 %186, i64* %187, align 8
  %188 = add nuw nsw i64 %storemerge5, 1
  %189 = getelementptr i8*, i8** %120, i64 %188
  %190 = bitcast i8** %189 to i64*
  %191 = load i64, i64* %190, align 8
  %192 = getelementptr i64, i64* %.cast3, i64 %188
  store i64 %191, i64* %192, align 8
  %193 = add nsw i64 %storemerge5, 2
  %194 = getelementptr i8*, i8** %120, i64 %193
  %195 = bitcast i8** %194 to i64*
  %196 = load i64, i64* %195, align 8
  %197 = getelementptr i64, i64* %.cast3, i64 %193
  store i64 %196, i64* %197, align 8
  %198 = add nsw i64 %storemerge5, 3
  %199 = getelementptr i8*, i8** %120, i64 %198
  %200 = bitcast i8** %199 to i64*
  %201 = load i64, i64* %200, align 8
  %202 = getelementptr i64, i64* %.cast3, i64 %198
  store i64 %201, i64* %202, align 8
  %203 = add nsw i64 %storemerge5, 4
  %exitcond.3 = icmp eq i64 %203, %122
  br i1 %exitcond.3, label %end_2.loopexit, label %body_2, !llvm.loop !19

end_2.loopexit:                                   ; preds = %body_2.prol.loopexit, %body_2, %middle.block17
  %.pre11 = load i64*, i64** %126, align 8
  br label %end_2

end_2:                                            ; preds = %end_2.loopexit, %end_1
  %204 = phi i64* [ %.pre11, %end_2.loopexit ], [ %.cast3, %end_1 ]
  %205 = load i64, i64* %204, align 8
  %206 = tail call {}* @print_int(i64 %205)
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
