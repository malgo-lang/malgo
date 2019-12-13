; ModuleID = './examples/sample.mlg'
source_filename = "./examples/sample.mlg"

@0 = global [6 x i8] c"malgo\00"
@1 = global [14 x i8] c"Hello, world!\00"
@2 = global [11 x i8] c"fib(10) = \00"
@3 = global [4 x i8] c"foo\00"
@4 = global [4 x i8] c"bar\00"

declare i8* @GC_malloc(i64)

define i64 @"$lambda.156"(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i64 (i8*, i64)*, i8* }*
  %6 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.156", i64 (i8*, i64)** %6
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %7
  %8 = mul i64 %1, 4
  ret i64 %8
}

define {}* @print_fib.96(i64) {
  %2 = call i64 @fib.68(i64 %0)
  %3 = call {}* @print_int.34(i64 %2)
  ret {}* %3
}

define double @area.89(double) {
  %2 = fmul double %0, %0
  %3 = fmul double %2, 3.140000e+00
  ret double %3
}

define i64 @add.85(i64) {
  %2 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %3 = bitcast i8* %2 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  store i64 %0, i64* %4
  %5 = bitcast { i64 }* %3 to i8*
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { i64 (i8*, i64)*, i8* }*
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 0
  store i64 (i8*, i64)* @add2.83, i64 (i8*, i64)** %8
  %9 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %5, i8** %9
  %10 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 0
  %11 = load i64 (i8*, i64)*, i64 (i8*, i64)** %10
  %12 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 1
  %13 = load i8*, i8** %12
  %14 = call i64 %11(i8* %13, i64 2)
  ret i64 %14
}

define i64 @add2.83(i8*, i64) {
  %3 = bitcast i8* %0 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { i64 (i8*, i64)*, i8* }*
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 0
  store i64 (i8*, i64)* @add2.83, i64 (i8*, i64)** %8
  %9 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = add i64 %5, %1
  ret i64 %10
}

define {}* @do_nothing.77() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  ret {}* %2
}

define i64 @fib.68(i64) {
  %2 = icmp sle i64 %0, 1
  %3 = alloca i64
  br i1 %2, label %then_0, label %else_0

then_0:                                           ; preds = %1
  store i64 1, i64* %3
  br label %endif_0

else_0:                                           ; preds = %1
  %4 = sub i64 %0, 1
  %5 = call i64 @fib.68(i64 %4)
  %6 = sub i64 %0, 2
  %7 = call i64 @fib.68(i64 %6)
  %8 = add i64 %5, %7
  store i64 %8, i64* %3
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %9 = load i64, i64* %3
  ret i64 %9
}

define {}* @println_float.56(double) {
  %2 = call {}* @print_float.35(double %0)
  %3 = call {}* @newline.41()
  ret {}* %3
}

define {}* @println_int.52(i64) {
  %2 = call {}* @print_int.34(i64 %0)
  %3 = call {}* @newline.41()
  ret {}* %3
}

declare i1 @not(i1)

define i1 @not.44(i1) {
  %2 = call i1 @not(i1 %0)
  ret i1 %2
}

declare i8* @concat(i8*, i8*)

define i8* @concat.43(i8*, i8*) {
  %3 = call i8* @concat(i8* %0, i8* %1)
  ret i8* %3
}

declare i8* @substring(i8*, i64, i64)

define i8* @substring.42(i8*, i64, i64) {
  %4 = call i8* @substring(i8* %0, i64 %1, i64 %2)
  ret i8* %4
}

declare {}* @newline()

define {}* @newline.41() {
  %1 = call {}* @newline()
  ret {}* %1
}

declare i64 @size(i8*)

define i64 @size.40(i8*) {
  %2 = call i64 @size(i8* %0)
  ret i64 %2
}

declare i8 @chr(i64)

define i8 @chr.39(i64) {
  %2 = call i8 @chr(i64 %0)
  ret i8 %2
}

declare i64 @ord(i8)

define i64 @ord.38(i8) {
  %2 = call i64 @ord(i8 %0)
  ret i64 %2
}

declare i8 @getchar()

define i8 @getChar.37() {
  %1 = call i8 @getchar()
  ret i8 %1
}

declare {}* @flush()

define {}* @flush.36() {
  %1 = call {}* @flush()
  ret {}* %1
}

declare {}* @print_float(double)

define {}* @print_float.35(double) {
  %2 = call {}* @print_float(double %0)
  ret {}* %2
}

declare {}* @print_int(i64)

define {}* @print_int.34(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

declare {}* @println(i8*)

define {}* @println.33(i8*) {
  %2 = call {}* @println(i8* %0)
  ret {}* %2
}

declare {}* @print(i8*)

define {}* @print.32(i8*) {
  %2 = call {}* @print(i8* %0)
  ret {}* %2
}

define i32 @main() {
  %1 = add i64 42, 1
  %2 = bitcast [6 x i8]* @0 to i8*
  %3 = bitcast [14 x i8]* @1 to i8*
  %4 = call {}* @println.33(i8* %3)
  %5 = bitcast [11 x i8]* @2 to i8*
  %6 = call {}* @print.32(i8* %5)
  %7 = call i64 @fib.68(i64 10)
  %8 = call {}* @println_int.52(i64 %7)
  %9 = call {}* @do_nothing.77()
  %10 = call i64 @add.85(i64 2)
  %11 = call {}* @println_int.52(i64 %10)
  %12 = call {}* @println_int.52(i64 %1)
  %13 = call i8* @GC_malloc(i64 0)
  %14 = bitcast i8* %13 to {}*
  %15 = bitcast {}* %14 to i8*
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { i64 (i8*, i64)*, i8* }*
  %18 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %17, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.156", i64 (i8*, i64)** %18
  %19 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %17, i32 0, i32 1
  store i8* %15, i8** %19
  %20 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %17, i32 0, i32 0
  %21 = load i64 (i8*, i64)*, i64 (i8*, i64)** %20
  %22 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %17, i32 0, i32 1
  %23 = load i8*, i8** %22
  %24 = call i64 %21(i8* %23, i64 3)
  %25 = call {}* @println_int.52(i64 %24)
  %26 = call i8* @substring.42(i8* %2, i64 1, i64 3)
  %27 = call {}* @println.33(i8* %26)
  %28 = bitcast [4 x i8]* @3 to i8*
  %29 = bitcast [4 x i8]* @4 to i8*
  %30 = call i8* @concat.43(i8* %28, i8* %29)
  %31 = call {}* @println.33(i8* %30)
  %32 = call {}* @println_float.56(double 3.140000e+00)
  %33 = call double @area.89(double 1.000000e+01)
  %34 = call {}* @println_float.56(double %33)
  ret i32 0
}
