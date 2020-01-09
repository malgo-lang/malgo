; ModuleID = './examples/gen/polyfun.mlg.ll'
source_filename = "./examples/polyfun.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

define i64 @"$fo164"(i8* nocapture readonly, i64) {
  %3 = inttoptr i64 %1 to i8*
  %4 = bitcast i8* %0 to i8* (i8*, i8*)**
  %5 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %4, align 8
  %6 = getelementptr i8, i8* %0, i64 8
  %7 = bitcast i8* %6 to i8**
  %8 = load i8*, i8** %7, align 8
  %9 = tail call i8* %5(i8* %8, i8* %3)
  %10 = ptrtoint i8* %9 to i64
  ret i64 %10
}

declare i8* @GC_malloc(i64) local_unnamed_addr

define { i64, { i8**, i64 }* }* @"$fo133"(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to { i8*, { i8**, i64 }* }* (i8*, i8*)**
  %4 = load { i8*, { i8**, i64 }* }* (i8*, i8*)*, { i8*, { i8**, i64 }* }* (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call { i8*, { i8**, i64 }* }* %4(i8* %7, i8* %1)
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to { i64, { i8**, i64 }* }*
  %11 = bitcast { i8*, { i8**, i64 }* }* %8 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = bitcast i8* %9 to i64*
  store i64 %12, i64* %13, align 8
  %14 = bitcast { i8*, { i8**, i64 }* }* %8 to { i8**, i64 }**
  %15 = load { i8**, i64 }*, { i8**, i64 }** %14, align 8
  %16 = getelementptr { i8**, i64 }, { i8**, i64 }* %15, i64 0, i32 0
  %17 = load i8**, i8*** %16, align 8
  %18 = bitcast i8** %17 to i8*
  %19 = getelementptr { i8**, i64 }, { i8**, i64 }* %15, i64 0, i32 1
  %20 = load i64, i64* %19, align 8
  %21 = shl i64 %20, 3
  %22 = tail call i8* @GC_malloc(i64 %21)
  %23 = tail call i8* @GC_malloc(i64 16)
  %24 = bitcast i8* %23 to i8**
  store i8* %22, i8** %24, align 8
  %25 = getelementptr i8, i8* %23, i64 8
  %26 = bitcast i8* %25 to i64*
  %.cast = bitcast i8* %22 to i8**
  store i64 %20, i64* %26, align 8
  %27 = icmp sgt i64 %20, 0
  br i1 %27, label %body_0.preheader, label %end_0

body_0.preheader:                                 ; preds = %2
  %min.iters.check = icmp ult i64 %20, 4
  br i1 %min.iters.check, label %body_0.preheader6, label %vector.memcheck

vector.memcheck:                                  ; preds = %body_0.preheader
  %28 = shl i64 %20, 3
  %scevgep = getelementptr i8, i8* %22, i64 %28
  %scevgep2 = getelementptr i8*, i8** %17, i64 %20
  %scevgep23 = bitcast i8** %scevgep2 to i8*
  %bound0 = icmp ult i8* %22, %scevgep23
  %bound1 = icmp ugt i8* %scevgep, %18
  %found.conflict = and i1 %bound0, %bound1
  br i1 %found.conflict, label %body_0.preheader6, label %vector.ph

vector.ph:                                        ; preds = %vector.memcheck
  %n.vec = and i64 %20, -4
  %29 = add i64 %n.vec, -4
  %30 = lshr exact i64 %29, 2
  %31 = add nuw nsw i64 %30, 1
  %xtraiter7 = and i64 %31, 3
  %32 = icmp ult i64 %29, 12
  br i1 %32, label %middle.block.unr-lcssa, label %vector.ph.new

vector.ph.new:                                    ; preds = %vector.ph
  %unroll_iter = sub nsw i64 %31, %xtraiter7
  br label %vector.body

vector.body:                                      ; preds = %vector.body, %vector.ph.new
  %index = phi i64 [ 0, %vector.ph.new ], [ %index.next.3, %vector.body ]
  %niter = phi i64 [ %unroll_iter, %vector.ph.new ], [ %niter.nsub.3, %vector.body ]
  %33 = getelementptr i8*, i8** %17, i64 %index
  %34 = bitcast i8** %33 to <2 x i64>*
  %wide.load = load <2 x i64>, <2 x i64>* %34, align 8, !alias.scope !0
  %35 = getelementptr i8*, i8** %33, i64 2
  %36 = bitcast i8** %35 to <2 x i64>*
  %wide.load5 = load <2 x i64>, <2 x i64>* %36, align 8, !alias.scope !0
  %37 = getelementptr i8*, i8** %.cast, i64 %index
  %38 = bitcast i8** %37 to <2 x i64>*
  store <2 x i64> %wide.load, <2 x i64>* %38, align 8, !alias.scope !3, !noalias !0
  %39 = getelementptr i8*, i8** %37, i64 2
  %40 = bitcast i8** %39 to <2 x i64>*
  store <2 x i64> %wide.load5, <2 x i64>* %40, align 8, !alias.scope !3, !noalias !0
  %index.next = or i64 %index, 4
  %41 = getelementptr i8*, i8** %17, i64 %index.next
  %42 = bitcast i8** %41 to <2 x i64>*
  %wide.load.1 = load <2 x i64>, <2 x i64>* %42, align 8, !alias.scope !0
  %43 = getelementptr i8*, i8** %41, i64 2
  %44 = bitcast i8** %43 to <2 x i64>*
  %wide.load5.1 = load <2 x i64>, <2 x i64>* %44, align 8, !alias.scope !0
  %45 = getelementptr i8*, i8** %.cast, i64 %index.next
  %46 = bitcast i8** %45 to <2 x i64>*
  store <2 x i64> %wide.load.1, <2 x i64>* %46, align 8, !alias.scope !3, !noalias !0
  %47 = getelementptr i8*, i8** %45, i64 2
  %48 = bitcast i8** %47 to <2 x i64>*
  store <2 x i64> %wide.load5.1, <2 x i64>* %48, align 8, !alias.scope !3, !noalias !0
  %index.next.1 = or i64 %index, 8
  %49 = getelementptr i8*, i8** %17, i64 %index.next.1
  %50 = bitcast i8** %49 to <2 x i64>*
  %wide.load.2 = load <2 x i64>, <2 x i64>* %50, align 8, !alias.scope !0
  %51 = getelementptr i8*, i8** %49, i64 2
  %52 = bitcast i8** %51 to <2 x i64>*
  %wide.load5.2 = load <2 x i64>, <2 x i64>* %52, align 8, !alias.scope !0
  %53 = getelementptr i8*, i8** %.cast, i64 %index.next.1
  %54 = bitcast i8** %53 to <2 x i64>*
  store <2 x i64> %wide.load.2, <2 x i64>* %54, align 8, !alias.scope !3, !noalias !0
  %55 = getelementptr i8*, i8** %53, i64 2
  %56 = bitcast i8** %55 to <2 x i64>*
  store <2 x i64> %wide.load5.2, <2 x i64>* %56, align 8, !alias.scope !3, !noalias !0
  %index.next.2 = or i64 %index, 12
  %57 = getelementptr i8*, i8** %17, i64 %index.next.2
  %58 = bitcast i8** %57 to <2 x i64>*
  %wide.load.3 = load <2 x i64>, <2 x i64>* %58, align 8, !alias.scope !0
  %59 = getelementptr i8*, i8** %57, i64 2
  %60 = bitcast i8** %59 to <2 x i64>*
  %wide.load5.3 = load <2 x i64>, <2 x i64>* %60, align 8, !alias.scope !0
  %61 = getelementptr i8*, i8** %.cast, i64 %index.next.2
  %62 = bitcast i8** %61 to <2 x i64>*
  store <2 x i64> %wide.load.3, <2 x i64>* %62, align 8, !alias.scope !3, !noalias !0
  %63 = getelementptr i8*, i8** %61, i64 2
  %64 = bitcast i8** %63 to <2 x i64>*
  store <2 x i64> %wide.load5.3, <2 x i64>* %64, align 8, !alias.scope !3, !noalias !0
  %index.next.3 = add i64 %index, 16
  %niter.nsub.3 = add i64 %niter, -4
  %niter.ncmp.3 = icmp eq i64 %niter.nsub.3, 0
  br i1 %niter.ncmp.3, label %middle.block.unr-lcssa, label %vector.body, !llvm.loop !5

middle.block.unr-lcssa:                           ; preds = %vector.body, %vector.ph
  %index.unr = phi i64 [ 0, %vector.ph ], [ %index.next.3, %vector.body ]
  %lcmp.mod8 = icmp eq i64 %xtraiter7, 0
  br i1 %lcmp.mod8, label %middle.block, label %vector.body.epil

vector.body.epil:                                 ; preds = %middle.block.unr-lcssa, %vector.body.epil
  %index.epil = phi i64 [ %index.next.epil, %vector.body.epil ], [ %index.unr, %middle.block.unr-lcssa ]
  %epil.iter = phi i64 [ %epil.iter.sub, %vector.body.epil ], [ %xtraiter7, %middle.block.unr-lcssa ]
  %65 = getelementptr i8*, i8** %17, i64 %index.epil
  %66 = bitcast i8** %65 to <2 x i64>*
  %wide.load.epil = load <2 x i64>, <2 x i64>* %66, align 8, !alias.scope !0
  %67 = getelementptr i8*, i8** %65, i64 2
  %68 = bitcast i8** %67 to <2 x i64>*
  %wide.load5.epil = load <2 x i64>, <2 x i64>* %68, align 8, !alias.scope !0
  %69 = getelementptr i8*, i8** %.cast, i64 %index.epil
  %70 = bitcast i8** %69 to <2 x i64>*
  store <2 x i64> %wide.load.epil, <2 x i64>* %70, align 8, !alias.scope !3, !noalias !0
  %71 = getelementptr i8*, i8** %69, i64 2
  %72 = bitcast i8** %71 to <2 x i64>*
  store <2 x i64> %wide.load5.epil, <2 x i64>* %72, align 8, !alias.scope !3, !noalias !0
  %index.next.epil = add i64 %index.epil, 4
  %epil.iter.sub = add i64 %epil.iter, -1
  %epil.iter.cmp = icmp eq i64 %epil.iter.sub, 0
  br i1 %epil.iter.cmp, label %middle.block, label %vector.body.epil, !llvm.loop !7

middle.block:                                     ; preds = %vector.body.epil, %middle.block.unr-lcssa
  %cmp.n = icmp eq i64 %20, %n.vec
  br i1 %cmp.n, label %end_0, label %body_0.preheader6

body_0.preheader6:                                ; preds = %middle.block, %vector.memcheck, %body_0.preheader
  %.01.ph = phi i64 [ 0, %vector.memcheck ], [ 0, %body_0.preheader ], [ %n.vec, %middle.block ]
  %73 = sub i64 %20, %.01.ph
  %74 = xor i64 %.01.ph, -1
  %75 = add i64 %20, %74
  %xtraiter = and i64 %73, 7
  %lcmp.mod = icmp eq i64 %xtraiter, 0
  br i1 %lcmp.mod, label %body_0.prol.loopexit, label %body_0.prol

body_0.prol:                                      ; preds = %body_0.preheader6, %body_0.prol
  %.01.prol = phi i64 [ %81, %body_0.prol ], [ %.01.ph, %body_0.preheader6 ]
  %prol.iter = phi i64 [ %prol.iter.sub, %body_0.prol ], [ %xtraiter, %body_0.preheader6 ]
  %76 = getelementptr i8*, i8** %17, i64 %.01.prol
  %77 = bitcast i8** %76 to i64*
  %78 = load i64, i64* %77, align 8
  %79 = getelementptr i8*, i8** %.cast, i64 %.01.prol
  %80 = bitcast i8** %79 to i64*
  store i64 %78, i64* %80, align 8
  %81 = add nuw nsw i64 %.01.prol, 1
  %prol.iter.sub = add i64 %prol.iter, -1
  %prol.iter.cmp = icmp eq i64 %prol.iter.sub, 0
  br i1 %prol.iter.cmp, label %body_0.prol.loopexit, label %body_0.prol, !llvm.loop !9

body_0.prol.loopexit:                             ; preds = %body_0.prol, %body_0.preheader6
  %.01.unr = phi i64 [ %.01.ph, %body_0.preheader6 ], [ %81, %body_0.prol ]
  %82 = icmp ult i64 %75, 7
  br i1 %82, label %end_0, label %body_0

body_0:                                           ; preds = %body_0.prol.loopexit, %body_0
  %.01 = phi i64 [ %130, %body_0 ], [ %.01.unr, %body_0.prol.loopexit ]
  %83 = getelementptr i8*, i8** %17, i64 %.01
  %84 = bitcast i8** %83 to i64*
  %85 = load i64, i64* %84, align 8
  %86 = getelementptr i8*, i8** %.cast, i64 %.01
  %87 = bitcast i8** %86 to i64*
  store i64 %85, i64* %87, align 8
  %88 = add nuw nsw i64 %.01, 1
  %89 = getelementptr i8*, i8** %17, i64 %88
  %90 = bitcast i8** %89 to i64*
  %91 = load i64, i64* %90, align 8
  %92 = getelementptr i8*, i8** %.cast, i64 %88
  %93 = bitcast i8** %92 to i64*
  store i64 %91, i64* %93, align 8
  %94 = add nsw i64 %.01, 2
  %95 = getelementptr i8*, i8** %17, i64 %94
  %96 = bitcast i8** %95 to i64*
  %97 = load i64, i64* %96, align 8
  %98 = getelementptr i8*, i8** %.cast, i64 %94
  %99 = bitcast i8** %98 to i64*
  store i64 %97, i64* %99, align 8
  %100 = add nsw i64 %.01, 3
  %101 = getelementptr i8*, i8** %17, i64 %100
  %102 = bitcast i8** %101 to i64*
  %103 = load i64, i64* %102, align 8
  %104 = getelementptr i8*, i8** %.cast, i64 %100
  %105 = bitcast i8** %104 to i64*
  store i64 %103, i64* %105, align 8
  %106 = add nsw i64 %.01, 4
  %107 = getelementptr i8*, i8** %17, i64 %106
  %108 = bitcast i8** %107 to i64*
  %109 = load i64, i64* %108, align 8
  %110 = getelementptr i8*, i8** %.cast, i64 %106
  %111 = bitcast i8** %110 to i64*
  store i64 %109, i64* %111, align 8
  %112 = add nsw i64 %.01, 5
  %113 = getelementptr i8*, i8** %17, i64 %112
  %114 = bitcast i8** %113 to i64*
  %115 = load i64, i64* %114, align 8
  %116 = getelementptr i8*, i8** %.cast, i64 %112
  %117 = bitcast i8** %116 to i64*
  store i64 %115, i64* %117, align 8
  %118 = add nsw i64 %.01, 6
  %119 = getelementptr i8*, i8** %17, i64 %118
  %120 = bitcast i8** %119 to i64*
  %121 = load i64, i64* %120, align 8
  %122 = getelementptr i8*, i8** %.cast, i64 %118
  %123 = bitcast i8** %122 to i64*
  store i64 %121, i64* %123, align 8
  %124 = add nsw i64 %.01, 7
  %125 = getelementptr i8*, i8** %17, i64 %124
  %126 = bitcast i8** %125 to i64*
  %127 = load i64, i64* %126, align 8
  %128 = getelementptr i8*, i8** %.cast, i64 %124
  %129 = bitcast i8** %128 to i64*
  store i64 %127, i64* %129, align 8
  %130 = add nsw i64 %.01, 8
  %exitcond.7 = icmp eq i64 %130, %20
  br i1 %exitcond.7, label %end_0, label %body_0, !llvm.loop !10

end_0:                                            ; preds = %body_0.prol.loopexit, %body_0, %middle.block, %2
  %131 = getelementptr i8, i8* %9, i64 8
  %132 = bitcast i8* %131 to i8**
  store i8* %23, i8** %132, align 8
  ret { i64, { i8**, i64 }* }* %10
}

define i8* @"$fo117"(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to i8* (i8*, i8*)**
  %4 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call i8* %4(i8* %7, i8* %1)
  ret i8* %8
}

define i8* @id0(i8*, i8* readnone returned) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id0, i8* (i8*, i8*)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 16)
  %8 = bitcast i8* %7 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne1, i64 (i8*, i64)** %8, align 8
  %9 = getelementptr i8, i8* %7, i64 8
  %10 = bitcast i8* %9 to i8**
  store i8* %0, i8** %10, align 8
  ret i8* %1
}

define i64 @addOne1(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id0, i8* (i8*, i8*)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 16)
  %8 = bitcast i8* %7 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne1, i64 (i8*, i64)** %8, align 8
  %9 = getelementptr i8, i8* %7, i64 8
  %10 = bitcast i8* %9 to i8**
  store i8* %0, i8** %10, align 8
  %11 = add i64 %1, 1
  ret i64 %11
}

define { i8*, { i8**, i64 }* }* @"$lambda32"(i8*, i8*) {
body_0:
  %2 = bitcast i8* %0 to i64*
  %3 = load i64, i64* %2, align 8
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to { i8*, { i8**, i64 }* }* (i8*, i8*)**
  store { i8*, { i8**, i64 }* }* (i8*, i8*)* @"$lambda32", { i8*, { i8**, i64 }* }* (i8*, i8*)** %5, align 8
  %6 = getelementptr i8, i8* %4, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 8)
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to i8**
  store i8* %8, i8** %10, align 8
  %11 = getelementptr i8, i8* %9, i64 8
  %12 = bitcast i8* %11 to i64*
  store i64 1, i64* %12, align 8
  %13 = bitcast i8* %8 to i8**
  store i8* %1, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to { i8*, { i8**, i64 }* }*
  %16 = bitcast i8* %14 to i64*
  store i64 %3, i64* %16, align 8
  %17 = getelementptr i8, i8* %14, i64 8
  %18 = bitcast i8* %17 to i8**
  store i8* %9, i8** %18, align 8
  ret { i8*, { i8**, i64 }* }* %15
}

define { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* @"$lambda31"(i8*, i8*) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)**
  store { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)* @"$lambda31", { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 8)
  %8 = bitcast i8* %7 to i8**
  store i8* %1, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }*
  %11 = bitcast i8* %9 to { i8*, { i8**, i64 }* }* (i8*, i8*)**
  store { i8*, { i8**, i64 }* }* (i8*, i8*)* @"$lambda32", { i8*, { i8**, i64 }* }* (i8*, i8*)** %11, align 8
  %12 = getelementptr i8, i8* %9, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %7, i8** %13, align 8
  ret { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %10
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int10(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

define i32 @main() local_unnamed_addr {
body_0:
  %0 = tail call i8* @GC_malloc(i64 0)
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id0, i8* (i8*, i8*)** %2, align 8
  %3 = getelementptr i8, i8* %1, i64 8
  %4 = bitcast i8* %3 to i8**
  store i8* %0, i8** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = tail call i8* @GC_malloc(i64 16)
  %7 = bitcast i8* %6 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne1, i64 (i8*, i64)** %7, align 8
  %8 = getelementptr i8, i8* %6, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %5, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 8)
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to { i64 (i8*, i64)*, i8* }***
  %13 = bitcast i8* %11 to i8**
  store i8* %10, i8** %13, align 8
  %14 = getelementptr i8, i8* %11, i64 8
  %15 = bitcast i8* %14 to i64*
  store i64 1, i64* %15, align 8
  %16 = bitcast i8* %10 to i8**
  store i8* %6, i8** %16, align 8
  %17 = tail call i8* @GC_malloc(i64 0)
  %18 = tail call i8* @GC_malloc(i64 16)
  %19 = bitcast i8* %18 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)**
  store { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)* @"$lambda31", { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)** %19, align 8
  %20 = getelementptr i8, i8* %18, i64 8
  %21 = bitcast i8* %20 to i8**
  store i8* %17, i8** %21, align 8
  %22 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %2, align 8
  %23 = load i8*, i8** %4, align 8
  %24 = tail call i8* %22(i8* %23, i8* %1)
  %25 = tail call i8* @GC_malloc(i64 16)
  %26 = bitcast i8* %25 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @"$fo117", i8* (i8*, i8*)** %26, align 8
  %27 = getelementptr i8, i8* %25, i64 8
  %28 = bitcast i8* %27 to i8**
  store i8* %24, i8** %28, align 8
  %29 = load { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)** %19, align 8
  %30 = load i8*, i8** %21, align 8
  %31 = tail call { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %29(i8* %30, i8* inttoptr (i64 1 to i8*))
  %32 = tail call i8* @GC_malloc(i64 16)
  %33 = bitcast i8* %32 to { i64, { i8**, i64 }* }* (i8*, i8*)**
  store { i64, { i8**, i64 }* }* (i8*, i8*)* @"$fo133", { i64, { i8**, i64 }* }* (i8*, i8*)** %33, align 8
  %34 = getelementptr i8, i8* %32, i64 8
  %35 = bitcast i8* %34 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }**
  store { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %31, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }** %35, align 8
  %36 = bitcast i8* %11 to i8***
  %37 = load i8**, i8*** %36, align 8
  %38 = tail call i8* @GC_malloc(i64 16)
  %39 = bitcast i8* %38 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$fo164", i64 (i8*, i64)** %39, align 8
  %40 = getelementptr i8, i8* %38, i64 8
  %41 = bitcast i8* %40 to i8**
  store i8* %1, i8** %41, align 8
  store i8* %38, i8** %37, align 8
  %42 = load { i64 (i8*, i64)*, i8* }**, { i64 (i8*, i64)*, i8* }*** %12, align 8
  %43 = load { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %42, align 8
  %44 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %43, i64 0, i32 0
  %45 = load i64 (i8*, i64)*, i64 (i8*, i64)** %44, align 8
  %46 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %43, i64 0, i32 1
  %47 = load i8*, i8** %46, align 8
  %48 = tail call i64 %45(i8* %47, i64 1)
  %49 = tail call {}* @print_int(i64 %48)
  ret i32 0
}

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
