; ModuleID = './examples/gen/match.mlg.ll'
source_filename = "./examples/match.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

; Function Attrs: norecurse nounwind readonly
define i64 @add0({ { i64, i64 }*, i64 }* nocapture readonly) local_unnamed_addr #0 {
  %2 = getelementptr { { i64, i64 }*, i64 }, { { i64, i64 }*, i64 }* %0, i64 0, i32 0
  %3 = load { i64, i64 }*, { i64, i64 }** %2, align 8
  %4 = getelementptr { { i64, i64 }*, i64 }, { { i64, i64 }*, i64 }* %0, i64 0, i32 1
  %5 = load i64, i64* %4, align 8
  %6 = getelementptr { i64, i64 }, { i64, i64 }* %3, i64 0, i32 0
  %7 = load i64, i64* %6, align 8
  %8 = getelementptr { i64, i64 }, { i64, i64 }* %3, i64 0, i32 1
  %9 = load i64, i64* %8, align 8
  %10 = add i64 %7, %5
  %11 = add i64 %10, %9
  ret i64 %11
}

declare i8* @GC_malloc(i64) local_unnamed_addr

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to <2 x i64>*
  store <2 x i64> <i64 1, i64 2>, <2 x i64>* %2, align 8
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i8**
  store i8* %1, i8** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i64*
  store i64 3, i64* %6, align 8
  ret i32 0
}

attributes #0 = { norecurse nounwind readonly }
