; ModuleID = './examples/gen/fib.mlg.ll'
source_filename = "./examples/fib.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define {}* @fib_loop.31(i64, i64) local_unnamed_addr {
  br label %tailrecurse

tailrecurse:                                      ; preds = %tailrecurse, %2
  %.tr3 = phi i64 [ %1, %2 ], [ %9, %tailrecurse ]
  %3 = tail call i64 @fib.21(i64 %.tr3)
  %4 = tail call {}* @print_int(i64 %3)
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = bitcast i8* %5 to {}*
  %7 = tail call {}* @newline({}* %6)
  %8 = icmp slt i64 %.tr3, %0
  %9 = add i64 %.tr3, 1
  br i1 %8, label %tailrecurse, label %then_0

then_0:                                           ; preds = %tailrecurse
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  ret {}* %11
}

; Function Attrs: nounwind readnone
define i64 @fib.21(i64) local_unnamed_addr #0 {
  %2 = icmp slt i64 %0, 2
  br i1 %2, label %endif_0, label %else_0

else_0:                                           ; preds = %1, %else_0
  %.tr3 = phi i64 [ %5, %else_0 ], [ %0, %1 ]
  %accumulator.tr2 = phi i64 [ %6, %else_0 ], [ 1, %1 ]
  %3 = add i64 %.tr3, -1
  %4 = tail call i64 @fib.21(i64 %3)
  %5 = add nsw i64 %.tr3, -2
  %6 = add i64 %4, %accumulator.tr2
  %7 = icmp slt i64 %5, 2
  br i1 %7, label %endif_0, label %else_0

endif_0:                                          ; preds = %else_0, %1
  %accumulator.tr.lcssa = phi i64 [ 1, %1 ], [ %6, %else_0 ]
  ret i64 %accumulator.tr.lcssa
}

declare {}* @newline({}*) local_unnamed_addr

define {}* @newline.8({}*) local_unnamed_addr {
  %2 = tail call {}* @newline({}* %0)
  ret {}* %2
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int.7(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

define i32 @main() local_unnamed_addr {
  br label %tailrecurse.i

tailrecurse.i:                                    ; preds = %tailrecurse.i, %0
  %.tr3.i = phi i64 [ 0, %0 ], [ %6, %tailrecurse.i ]
  %1 = tail call i64 @fib.21(i64 %.tr3.i)
  %2 = tail call {}* @print_int(i64 %1)
  %3 = tail call i8* @GC_malloc(i64 0)
  %4 = bitcast i8* %3 to {}*
  %5 = tail call {}* @newline({}* %4)
  %6 = add nuw nsw i64 %.tr3.i, 1
  %exitcond = icmp eq i64 %6, 31
  br i1 %exitcond, label %fib_loop.31.exit, label %tailrecurse.i

fib_loop.31.exit:                                 ; preds = %tailrecurse.i
  %7 = tail call i8* @GC_malloc(i64 0)
  ret i32 0
}

attributes #0 = { nounwind readnone }
