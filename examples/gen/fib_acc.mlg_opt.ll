; ModuleID = './examples/gen/fib_acc.mlg.ll'
source_filename = "./examples/fib_acc.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int60(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

declare {}* @newline() local_unnamed_addr

define {}* @newline59() local_unnamed_addr {
  %1 = tail call {}* @newline()
  ret {}* %1
}

; Function Attrs: nounwind readnone
define i64 @fib_acc58(i64, i64, i64) local_unnamed_addr #0 {
  %4 = icmp slt i64 %0, 1
  br i1 %4, label %end_0, label %else_0

else_0:                                           ; preds = %3, %else_0
  %.tr36 = phi i64 [ %6, %else_0 ], [ %2, %3 ]
  %.tr25 = phi i64 [ %.tr36, %else_0 ], [ %1, %3 ]
  %.tr4 = phi i64 [ %5, %else_0 ], [ %0, %3 ]
  %5 = add nsw i64 %.tr4, -1
  %6 = add i64 %.tr36, %.tr25
  %7 = icmp slt i64 %.tr4, 2
  br i1 %7, label %end_0, label %else_0

end_0:                                            ; preds = %else_0, %3
  %.tr2.lcssa = phi i64 [ %1, %3 ], [ %.tr36, %else_0 ]
  ret i64 %.tr2.lcssa
}

define {}* @fib_loop57(i64) local_unnamed_addr {
  %2 = icmp slt i64 %0, 1
  br i1 %2, label %then_0, label %else_0.i.preheader

else_0.i.preheader:                               ; preds = %1, %fib_acc58.exit
  %.tr2 = phi i64 [ %10, %fib_acc58.exit ], [ %0, %1 ]
  br label %else_0.i

then_0:                                           ; preds = %fib_acc58.exit, %1
  %3 = tail call {}* @print_int(i64 1)
  %4 = tail call {}* @newline()
  ret {}* %4

else_0.i:                                         ; preds = %else_0.i.preheader, %else_0.i
  %.tr36.i = phi i64 [ %6, %else_0.i ], [ 1, %else_0.i.preheader ]
  %.tr25.i = phi i64 [ %.tr36.i, %else_0.i ], [ 1, %else_0.i.preheader ]
  %.tr4.i = phi i64 [ %5, %else_0.i ], [ %.tr2, %else_0.i.preheader ]
  %5 = add nsw i64 %.tr4.i, -1
  %6 = add i64 %.tr25.i, %.tr36.i
  %7 = icmp slt i64 %.tr4.i, 2
  br i1 %7, label %fib_acc58.exit, label %else_0.i

fib_acc58.exit:                                   ; preds = %else_0.i
  %8 = tail call {}* @print_int(i64 %.tr36.i)
  %9 = tail call {}* @newline()
  %10 = add nsw i64 %.tr2, -1
  %11 = icmp slt i64 %.tr2, 2
  br i1 %11, label %then_0, label %else_0.i.preheader
}

define i32 @main() local_unnamed_addr {
  br label %else_0.i.preheader.i

else_0.i.preheader.i:                             ; preds = %fib_acc58.exit.i, %0
  %indvar = phi i64 [ %indvar.next, %fib_acc58.exit.i ], [ 0, %0 ]
  %.tr2.i = phi i64 [ %15, %fib_acc58.exit.i ], [ 30, %0 ]
  %1 = sub i64 30, %indvar
  %2 = sub i64 29, %indvar
  %xtraiter = and i64 %1, 7
  %3 = icmp ult i64 %2, 7
  br i1 %3, label %fib_acc58.exit.i.unr-lcssa, label %else_0.i.preheader.i.new

else_0.i.preheader.i.new:                         ; preds = %else_0.i.preheader.i
  %unroll_iter = sub i64 %1, %xtraiter
  br label %else_0.i.i

else_0.i.i:                                       ; preds = %else_0.i.i, %else_0.i.preheader.i.new
  %.tr36.i.i = phi i64 [ 1, %else_0.i.preheader.i.new ], [ %11, %else_0.i.i ]
  %.tr25.i.i = phi i64 [ 1, %else_0.i.preheader.i.new ], [ %10, %else_0.i.i ]
  %niter = phi i64 [ %unroll_iter, %else_0.i.preheader.i.new ], [ %niter.nsub.7, %else_0.i.i ]
  %4 = add i64 %.tr25.i.i, %.tr36.i.i
  %5 = add i64 %.tr36.i.i, %4
  %6 = add i64 %4, %5
  %7 = add i64 %5, %6
  %8 = add i64 %6, %7
  %9 = add i64 %7, %8
  %10 = add i64 %8, %9
  %11 = add i64 %9, %10
  %niter.nsub.7 = add i64 %niter, -8
  %niter.ncmp.7 = icmp eq i64 %niter.nsub.7, 0
  br i1 %niter.ncmp.7, label %fib_acc58.exit.i.unr-lcssa, label %else_0.i.i

fib_acc58.exit.i.unr-lcssa:                       ; preds = %else_0.i.i, %else_0.i.preheader.i
  %.tr36.i.i.lcssa.ph = phi i64 [ undef, %else_0.i.preheader.i ], [ %10, %else_0.i.i ]
  %.tr36.i.i.unr = phi i64 [ 1, %else_0.i.preheader.i ], [ %11, %else_0.i.i ]
  %.tr25.i.i.unr = phi i64 [ 1, %else_0.i.preheader.i ], [ %10, %else_0.i.i ]
  %lcmp.mod = icmp eq i64 %xtraiter, 0
  br i1 %lcmp.mod, label %fib_acc58.exit.i, label %else_0.i.i.epil

else_0.i.i.epil:                                  ; preds = %fib_acc58.exit.i.unr-lcssa, %else_0.i.i.epil
  %.tr36.i.i.epil = phi i64 [ %12, %else_0.i.i.epil ], [ %.tr36.i.i.unr, %fib_acc58.exit.i.unr-lcssa ]
  %.tr25.i.i.epil = phi i64 [ %.tr36.i.i.epil, %else_0.i.i.epil ], [ %.tr25.i.i.unr, %fib_acc58.exit.i.unr-lcssa ]
  %epil.iter = phi i64 [ %epil.iter.sub, %else_0.i.i.epil ], [ %xtraiter, %fib_acc58.exit.i.unr-lcssa ]
  %12 = add i64 %.tr25.i.i.epil, %.tr36.i.i.epil
  %epil.iter.sub = add i64 %epil.iter, -1
  %epil.iter.cmp = icmp eq i64 %epil.iter.sub, 0
  br i1 %epil.iter.cmp, label %fib_acc58.exit.i, label %else_0.i.i.epil, !llvm.loop !0

fib_acc58.exit.i:                                 ; preds = %else_0.i.i.epil, %fib_acc58.exit.i.unr-lcssa
  %.tr36.i.i.lcssa = phi i64 [ %.tr36.i.i.lcssa.ph, %fib_acc58.exit.i.unr-lcssa ], [ %.tr36.i.i.epil, %else_0.i.i.epil ]
  %13 = tail call {}* @print_int(i64 %.tr36.i.i.lcssa)
  %14 = tail call {}* @newline()
  %15 = add nsw i64 %.tr2.i, -1
  %16 = icmp ult i64 %.tr2.i, 2
  %indvar.next = add i64 %indvar, 1
  br i1 %16, label %fib_loop57.exit, label %else_0.i.preheader.i

fib_loop57.exit:                                  ; preds = %fib_acc58.exit.i
  %17 = tail call {}* @print_int(i64 1)
  %18 = tail call {}* @newline()
  ret i32 0
}

attributes #0 = { nounwind readnone }

!0 = distinct !{!0, !1}
!1 = !{!"llvm.loop.unroll.disable"}
