; ModuleID = './examples/gen/fib.mlg.ll'
source_filename = "./examples/fib.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int0(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

declare {}* @newline() local_unnamed_addr

define {}* @newline1() local_unnamed_addr {
  %1 = tail call {}* @newline()
  ret {}* %1
}

declare i8* @GC_malloc(i64) local_unnamed_addr

define {}* @fib_loop3(i64, i64) local_unnamed_addr {
  br label %tailrecurse

tailrecurse:                                      ; preds = %tailrecurse, %2
  %.tr3 = phi i64 [ %1, %2 ], [ %7, %tailrecurse ]
  %3 = tail call i64 @fib2(i64 %.tr3)
  %4 = tail call {}* @print_int(i64 %3)
  %5 = tail call {}* @newline()
  %6 = icmp slt i64 %.tr3, %0
  %7 = add i64 %.tr3, 1
  br i1 %6, label %tailrecurse, label %then_0

then_0:                                           ; preds = %tailrecurse
  %8 = tail call i8* @GC_malloc(i64 0)
  %9 = bitcast i8* %8 to {}*
  ret {}* %9
}

; Function Attrs: nounwind readnone
define i64 @fib2(i64) local_unnamed_addr #0 {
  %2 = icmp slt i64 %0, 2
  br i1 %2, label %end_0, label %else_0

else_0:                                           ; preds = %1, %else_0
  %.tr3 = phi i64 [ %5, %else_0 ], [ %0, %1 ]
  %accumulator.tr2 = phi i64 [ %6, %else_0 ], [ 1, %1 ]
  %3 = add i64 %.tr3, -1
  %4 = tail call i64 @fib2(i64 %3)
  %5 = add nsw i64 %.tr3, -2
  %6 = add i64 %4, %accumulator.tr2
  %7 = icmp slt i64 %5, 2
  br i1 %7, label %end_0, label %else_0

end_0:                                            ; preds = %else_0, %1
  %accumulator.tr.lcssa = phi i64 [ 1, %1 ], [ %6, %else_0 ]
  ret i64 %accumulator.tr.lcssa
}

declare void @GC_init() local_unnamed_addr

define i32 @main() local_unnamed_addr {
  tail call void @GC_init()
  br label %tailrecurse.i

tailrecurse.i:                                    ; preds = %tailrecurse.i, %0
  %.tr3.i = phi i64 [ 0, %0 ], [ %4, %tailrecurse.i ]
  %1 = tail call i64 @fib2(i64 %.tr3.i)
  %2 = tail call {}* @print_int(i64 %1)
  %3 = tail call {}* @newline()
  %4 = add nuw nsw i64 %.tr3.i, 1
  %exitcond = icmp eq i64 %4, 31
  br i1 %exitcond, label %fib_loop3.exit, label %tailrecurse.i

fib_loop3.exit:                                   ; preds = %tailrecurse.i
  %5 = tail call i8* @GC_malloc(i64 0)
  ret i32 0
}

attributes #0 = { nounwind readnone }
