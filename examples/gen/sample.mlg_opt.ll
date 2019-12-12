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

define i64 @"$lambda.164"(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda.164", i64 (i8*, i64)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = shl i64 %1, 2
  ret i64 %7
}

define {}* @print_fib.98(i64) local_unnamed_addr {
  %2 = tail call i64 @fib.69(i64 %0)
  %3 = tail call {}* @print_int(i64 %2)
  ret {}* %3
}

; Function Attrs: norecurse nounwind readnone
define double @area.91(double) local_unnamed_addr #0 {
  %2 = fmul double %0, %0
  %3 = fmul double %2, 3.140000e+00
  ret double %3
}

define i64 @add.87(i64) local_unnamed_addr {
  %2 = tail call i8* @GC_malloc(i64 8)
  %3 = bitcast i8* %2 to i64*
  store i64 %0, i64* %3, align 8
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add2.85, i64 (i8*, i64)** %5, align 8
  %6 = getelementptr i8, i8* %4, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %2, i8** %7, align 8
  %8 = load i64, i64* %3, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add2.85, i64 (i8*, i64)** %10, align 8
  %11 = getelementptr i8, i8* %9, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %2, i8** %12, align 8
  %13 = add i64 %8, 2
  ret i64 %13
}

define i64 @add2.85(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add2.85, i64 (i8*, i64)** %6, align 8
  %7 = getelementptr i8, i8* %5, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %0, i8** %8, align 8
  %9 = add i64 %4, %1
  ret i64 %9
}

define {}* @do_nothing.79({}* nocapture readnone) local_unnamed_addr {
  %2 = tail call i8* @GC_malloc(i64 0)
  %3 = bitcast i8* %2 to {}*
  ret {}* %3
}

; Function Attrs: nounwind readnone
define i64 @fib.69(i64) local_unnamed_addr #1 {
  %2 = icmp slt i64 %0, 2
  br i1 %2, label %endif_0, label %else_0

else_0:                                           ; preds = %1, %else_0
  %.tr3 = phi i64 [ %5, %else_0 ], [ %0, %1 ]
  %accumulator.tr2 = phi i64 [ %6, %else_0 ], [ 1, %1 ]
  %3 = add i64 %.tr3, -1
  %4 = tail call i64 @fib.69(i64 %3)
  %5 = add nsw i64 %.tr3, -2
  %6 = add i64 %4, %accumulator.tr2
  %7 = icmp slt i64 %5, 2
  br i1 %7, label %endif_0, label %else_0

endif_0:                                          ; preds = %else_0, %1
  %accumulator.tr.lcssa = phi i64 [ 1, %1 ], [ %6, %else_0 ]
  ret i64 %accumulator.tr.lcssa
}

define {}* @println_float.57(double) local_unnamed_addr {
  %2 = tail call {}* @print_float(double %0)
  %3 = tail call i8* @GC_malloc(i64 0)
  %4 = bitcast i8* %3 to {}*
  %5 = tail call {}* @newline({}* %4)
  ret {}* %5
}

define {}* @println_int.53(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  %3 = tail call i8* @GC_malloc(i64 0)
  %4 = bitcast i8* %3 to {}*
  %5 = tail call {}* @newline({}* %4)
  ret {}* %5
}

declare i1 @not(i1) local_unnamed_addr

define i1 @not.45(i1) local_unnamed_addr {
  %2 = tail call i1 @not(i1 %0)
  ret i1 %2
}

declare i8* @concat(i8*, i8*) local_unnamed_addr

define i8* @concat.44(i8*, i8*) local_unnamed_addr {
  %3 = tail call i8* @concat(i8* %0, i8* %1)
  ret i8* %3
}

declare i8* @substring(i8*, i64, i64) local_unnamed_addr

define i8* @substring.43(i8*, i64, i64) local_unnamed_addr {
  %4 = tail call i8* @substring(i8* %0, i64 %1, i64 %2)
  ret i8* %4
}

declare {}* @newline({}*) local_unnamed_addr

define {}* @newline.42({}*) local_unnamed_addr {
  %2 = tail call {}* @newline({}* %0)
  ret {}* %2
}

declare i64 @size(i8*) local_unnamed_addr

define i64 @size.41(i8*) local_unnamed_addr {
  %2 = tail call i64 @size(i8* %0)
  ret i64 %2
}

declare i8 @chr(i64) local_unnamed_addr

define i8 @chr.40(i64) local_unnamed_addr {
  %2 = tail call i8 @chr(i64 %0)
  ret i8 %2
}

declare i64 @ord(i8) local_unnamed_addr

define i64 @ord.39(i8) local_unnamed_addr {
  %2 = tail call i64 @ord(i8 %0)
  ret i64 %2
}

declare i8 @getchar({}*) local_unnamed_addr

define i8 @getChar.38({}*) local_unnamed_addr {
  %2 = tail call i8 @getchar({}* %0)
  ret i8 %2
}

declare {}* @flush({}*) local_unnamed_addr

define {}* @flush.37({}*) local_unnamed_addr {
  %2 = tail call {}* @flush({}* %0)
  ret {}* %2
}

declare {}* @print_float(double) local_unnamed_addr

define {}* @print_float.36(double) local_unnamed_addr {
  %2 = tail call {}* @print_float(double %0)
  ret {}* %2
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int.35(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

declare {}* @println(i8*) local_unnamed_addr

define {}* @println.34(i8*) local_unnamed_addr {
  %2 = tail call {}* @println(i8* %0)
  ret {}* %2
}

declare {}* @print(i8*) local_unnamed_addr

define {}* @print.33(i8*) local_unnamed_addr {
  %2 = tail call {}* @print(i8* %0)
  ret {}* %2
}

define i32 @main() local_unnamed_addr {
  %1 = tail call {}* @println(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @1, i64 0, i64 0))
  %2 = tail call {}* @print(i8* getelementptr inbounds ([11 x i8], [11 x i8]* @2, i64 0, i64 0))
  %3 = tail call i64 @fib.69(i64 10)
  %4 = tail call {}* @print_int(i64 %3)
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = bitcast i8* %5 to {}*
  %7 = tail call {}* @newline({}* %6)
  %8 = tail call i8* @GC_malloc(i64 0)
  %9 = tail call i8* @GC_malloc(i64 0)
  %10 = tail call i8* @GC_malloc(i64 8)
  %11 = bitcast i8* %10 to i64*
  store i64 2, i64* %11, align 8
  %12 = tail call i8* @GC_malloc(i64 16)
  %13 = bitcast i8* %12 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add2.85, i64 (i8*, i64)** %13, align 8
  %14 = getelementptr i8, i8* %12, i64 8
  %15 = bitcast i8* %14 to i8**
  store i8* %10, i8** %15, align 8
  %16 = load i64, i64* %11, align 8
  %17 = tail call i8* @GC_malloc(i64 16)
  %18 = bitcast i8* %17 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add2.85, i64 (i8*, i64)** %18, align 8
  %19 = getelementptr i8, i8* %17, i64 8
  %20 = bitcast i8* %19 to i8**
  store i8* %10, i8** %20, align 8
  %21 = add i64 %16, 2
  %22 = tail call {}* @print_int(i64 %21)
  %23 = tail call i8* @GC_malloc(i64 0)
  %24 = bitcast i8* %23 to {}*
  %25 = tail call {}* @newline({}* %24)
  %26 = tail call {}* @print_int(i64 43)
  %27 = tail call i8* @GC_malloc(i64 0)
  %28 = bitcast i8* %27 to {}*
  %29 = tail call {}* @newline({}* %28)
  %30 = tail call i8* @GC_malloc(i64 0)
  %31 = tail call i8* @GC_malloc(i64 16)
  %32 = bitcast i8* %31 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda.164", i64 (i8*, i64)** %32, align 8
  %33 = getelementptr i8, i8* %31, i64 8
  %34 = bitcast i8* %33 to i8**
  store i8* %30, i8** %34, align 8
  %35 = tail call i8* @GC_malloc(i64 16)
  %36 = bitcast i8* %35 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda.164", i64 (i8*, i64)** %36, align 8
  %37 = getelementptr i8, i8* %35, i64 8
  %38 = bitcast i8* %37 to i8**
  store i8* %30, i8** %38, align 8
  %39 = tail call {}* @print_int(i64 12)
  %40 = tail call i8* @GC_malloc(i64 0)
  %41 = bitcast i8* %40 to {}*
  %42 = tail call {}* @newline({}* %41)
  %43 = tail call i8* @substring(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @0, i64 0, i64 0), i64 1, i64 3)
  %44 = tail call {}* @println(i8* %43)
  %45 = tail call i8* @concat(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @3, i64 0, i64 0), i8* getelementptr inbounds ([4 x i8], [4 x i8]* @4, i64 0, i64 0))
  %46 = tail call {}* @println(i8* %45)
  %47 = tail call {}* @print_float(double 3.140000e+00)
  %48 = tail call i8* @GC_malloc(i64 0)
  %49 = bitcast i8* %48 to {}*
  %50 = tail call {}* @newline({}* %49)
  %51 = tail call {}* @print_float(double 3.140000e+02)
  %52 = tail call i8* @GC_malloc(i64 0)
  %53 = bitcast i8* %52 to {}*
  %54 = tail call {}* @newline({}* %53)
  ret i32 0
}

attributes #0 = { norecurse nounwind readnone }
attributes #1 = { nounwind readnone }
