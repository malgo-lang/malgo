; ModuleID = './examples/gen/sample.mlg.ll'
source_filename = "./examples/sample.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

@0 = internal global [6 x i8] c"malgo\00"
@1 = internal global [14 x i8] c"Hello, world!\00"
@2 = internal global [11 x i8] c"fib(10) = \00"
@3 = internal global [4 x i8] c"foo\00"
@4 = internal global [4 x i8] c"bar\00"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i64 @"$lambda.156"(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda.156", i64 (i8*, i64)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = shl i64 %1, 2
  ret i64 %7
}

define {}* @print_fib.96(i64) local_unnamed_addr {
  %2 = tail call i64 @fib.68(i64 %0)
  %3 = tail call {}* @print_int(i64 %2)
  ret {}* %3
}

; Function Attrs: norecurse nounwind readnone
define double @area.89(double) local_unnamed_addr #0 {
  %2 = fmul double %0, %0
  %3 = fmul double %2, 3.140000e+00
  ret double %3
}

define i64 @add.85(i64) local_unnamed_addr {
  %2 = tail call i8* @GC_malloc(i64 8)
  %3 = bitcast i8* %2 to i64*
  store i64 %0, i64* %3, align 8
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add2.83, i64 (i8*, i64)** %5, align 8
  %6 = getelementptr i8, i8* %4, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %2, i8** %7, align 8
  %8 = load i64, i64* %3, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add2.83, i64 (i8*, i64)** %10, align 8
  %11 = getelementptr i8, i8* %9, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %2, i8** %12, align 8
  %13 = add i64 %8, 2
  ret i64 %13
}

define i64 @add2.83(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add2.83, i64 (i8*, i64)** %6, align 8
  %7 = getelementptr i8, i8* %5, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %0, i8** %8, align 8
  %9 = add i64 %4, %1
  ret i64 %9
}

define {}* @do_nothing.77() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  ret {}* %2
}

; Function Attrs: nounwind readnone
define i64 @fib.68(i64) local_unnamed_addr #1 {
  %2 = icmp slt i64 %0, 2
  br i1 %2, label %endif_0, label %else_0

else_0:                                           ; preds = %1, %else_0
  %.tr3 = phi i64 [ %5, %else_0 ], [ %0, %1 ]
  %accumulator.tr2 = phi i64 [ %6, %else_0 ], [ 1, %1 ]
  %3 = add i64 %.tr3, -1
  %4 = tail call i64 @fib.68(i64 %3)
  %5 = add nsw i64 %.tr3, -2
  %6 = add i64 %4, %accumulator.tr2
  %7 = icmp slt i64 %5, 2
  br i1 %7, label %endif_0, label %else_0

endif_0:                                          ; preds = %else_0, %1
  %accumulator.tr.lcssa = phi i64 [ 1, %1 ], [ %6, %else_0 ]
  ret i64 %accumulator.tr.lcssa
}

define {}* @println_float.56(double) local_unnamed_addr {
  %2 = tail call {}* @print_float(double %0)
  %3 = tail call {}* @newline()
  ret {}* %3
}

define {}* @println_int.52(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  %3 = tail call {}* @newline()
  ret {}* %3
}

declare i1 @not(i1) local_unnamed_addr

define i1 @not.44(i1) local_unnamed_addr {
  %2 = tail call i1 @not(i1 %0)
  ret i1 %2
}

declare i8* @concat(i8*, i8*) local_unnamed_addr

define i8* @concat.43(i8*, i8*) local_unnamed_addr {
  %3 = tail call i8* @concat(i8* %0, i8* %1)
  ret i8* %3
}

declare i8* @substring(i8*, i64, i64) local_unnamed_addr

define i8* @substring.42(i8*, i64, i64) local_unnamed_addr {
  %4 = tail call i8* @substring(i8* %0, i64 %1, i64 %2)
  ret i8* %4
}

declare {}* @newline() local_unnamed_addr

define {}* @newline.41() local_unnamed_addr {
  %1 = tail call {}* @newline()
  ret {}* %1
}

declare i64 @size(i8*) local_unnamed_addr

define i64 @size.40(i8*) local_unnamed_addr {
  %2 = tail call i64 @size(i8* %0)
  ret i64 %2
}

declare i8 @chr(i64) local_unnamed_addr

define i8 @chr.39(i64) local_unnamed_addr {
  %2 = tail call i8 @chr(i64 %0)
  ret i8 %2
}

declare i64 @ord(i8) local_unnamed_addr

define i64 @ord.38(i8) local_unnamed_addr {
  %2 = tail call i64 @ord(i8 %0)
  ret i64 %2
}

; Function Attrs: nounwind
declare i8 @getchar() local_unnamed_addr #2

; Function Attrs: nounwind
define i8 @getChar.37() local_unnamed_addr #2 {
  %1 = tail call i8 @getchar()
  ret i8 %1
}

declare {}* @flush() local_unnamed_addr

define {}* @flush.36() local_unnamed_addr {
  %1 = tail call {}* @flush()
  ret {}* %1
}

declare {}* @print_float(double) local_unnamed_addr

define {}* @print_float.35(double) local_unnamed_addr {
  %2 = tail call {}* @print_float(double %0)
  ret {}* %2
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int.34(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

declare {}* @println(i8*) local_unnamed_addr

define {}* @println.33(i8*) local_unnamed_addr {
  %2 = tail call {}* @println(i8* %0)
  ret {}* %2
}

declare {}* @print(i8*) local_unnamed_addr

define {}* @print.32(i8*) local_unnamed_addr {
  %2 = tail call {}* @print(i8* %0)
  ret {}* %2
}

define i32 @main() local_unnamed_addr {
  %1 = tail call {}* @println(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @1, i64 0, i64 0))
  %2 = tail call {}* @print(i8* getelementptr inbounds ([11 x i8], [11 x i8]* @2, i64 0, i64 0))
  %3 = tail call i64 @fib.68(i64 10)
  %4 = tail call {}* @print_int(i64 %3)
  %5 = tail call {}* @newline()
  %6 = tail call i8* @GC_malloc(i64 0)
  %7 = tail call i8* @GC_malloc(i64 8)
  %8 = bitcast i8* %7 to i64*
  store i64 2, i64* %8, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add2.83, i64 (i8*, i64)** %10, align 8
  %11 = getelementptr i8, i8* %9, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %7, i8** %12, align 8
  %13 = load i64, i64* %8, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add2.83, i64 (i8*, i64)** %15, align 8
  %16 = getelementptr i8, i8* %14, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %7, i8** %17, align 8
  %18 = add i64 %13, 2
  %19 = tail call {}* @print_int(i64 %18)
  %20 = tail call {}* @newline()
  %21 = tail call {}* @print_int(i64 43)
  %22 = tail call {}* @newline()
  %23 = tail call i8* @GC_malloc(i64 0)
  %24 = tail call i8* @GC_malloc(i64 16)
  %25 = bitcast i8* %24 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda.156", i64 (i8*, i64)** %25, align 8
  %26 = getelementptr i8, i8* %24, i64 8
  %27 = bitcast i8* %26 to i8**
  store i8* %23, i8** %27, align 8
  %28 = tail call i8* @GC_malloc(i64 16)
  %29 = bitcast i8* %28 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda.156", i64 (i8*, i64)** %29, align 8
  %30 = getelementptr i8, i8* %28, i64 8
  %31 = bitcast i8* %30 to i8**
  store i8* %23, i8** %31, align 8
  %32 = tail call {}* @print_int(i64 12)
  %33 = tail call {}* @newline()
  %34 = tail call i8* @substring(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @0, i64 0, i64 0), i64 1, i64 3)
  %35 = tail call {}* @println(i8* %34)
  %36 = tail call i8* @concat(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @3, i64 0, i64 0), i8* getelementptr inbounds ([4 x i8], [4 x i8]* @4, i64 0, i64 0))
  %37 = tail call {}* @println(i8* %36)
  %38 = tail call {}* @print_float(double 3.140000e+00)
  %39 = tail call {}* @newline()
  %40 = tail call {}* @print_float(double 3.140000e+02)
  %41 = tail call {}* @newline()
  ret i32 0
}

attributes #0 = { norecurse nounwind readnone }
attributes #1 = { nounwind readnone }
attributes #2 = { nounwind }
