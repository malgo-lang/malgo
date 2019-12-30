; ModuleID = './examples/sample.mlg'
source_filename = "./examples/sample.mlg"

@0 = global [6 x i8] c"malgo\00"
@1 = global [14 x i8] c"Hello, world!\00"
@2 = global [11 x i8] c"fib(10) = \00"
@3 = global [4 x i8] c"foo\00"
@4 = global [4 x i8] c"bar\00"

declare {}* @print(i8*)

define {}* @print195(i8*) {
  %2 = call {}* @print(i8* %0)
  ret {}* %2
}

declare {}* @println(i8*)

define {}* @println194(i8*) {
  %2 = call {}* @println(i8* %0)
  ret {}* %2
}

declare {}* @print_int(i64)

define {}* @print_int193(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

declare {}* @print_float(double)

define {}* @print_float192(double) {
  %2 = call {}* @print_float(double %0)
  ret {}* %2
}

declare {}* @flush()

define {}* @flush191() {
  %1 = call {}* @flush()
  ret {}* %1
}

declare i8 @getchar()

define i8 @getChar190() {
  %1 = call i8 @getchar()
  ret i8 %1
}

declare i64 @ord(i8)

define i64 @ord189(i8) {
  %2 = call i64 @ord(i8 %0)
  ret i64 %2
}

declare i8 @chr(i64)

define i8 @chr188(i64) {
  %2 = call i8 @chr(i64 %0)
  ret i8 %2
}

declare i64 @size(i8*)

define i64 @size187(i8*) {
  %2 = call i64 @size(i8* %0)
  ret i64 %2
}

declare {}* @newline()

define {}* @newline186() {
  %1 = call {}* @newline()
  ret {}* %1
}

declare i8* @substring(i8*, i64, i64)

define i8* @substring185(i8*, i64, i64) {
  %4 = call i8* @substring(i8* %0, i64 %1, i64 %2)
  ret i8* %4
}

declare i8* @concat(i8*, i8*)

define i8* @concat184(i8*, i8*) {
  %3 = call i8* @concat(i8* %0, i8* %1)
  ret i8* %3
}

declare i1 @not(i1)

define i1 @not183(i1) {
  %2 = call i1 @not(i1 %0)
  ret i1 %2
}

define {}* @println_int182(i64) {
  %2 = call {}* @print_int193(i64 %0)
  %3 = call {}* @newline186()
  ret {}* %3
}

define {}* @println_float181(double) {
  %2 = call {}* @print_float192(double %0)
  %3 = call {}* @newline186()
  ret {}* %3
}

define i64 @fib180(i64) {
  %2 = icmp sle i64 %0, 1
  %3 = alloca i64
  br i1 %2, label %then_0, label %else_0

then_0:                                           ; preds = %1
  store i64 1, i64* %3
  br label %end_0

else_0:                                           ; preds = %1
  %4 = sub i64 %0, 1
  %5 = call i64 @fib180(i64 %4)
  %6 = sub i64 %0, 2
  %7 = call i64 @fib180(i64 %6)
  %8 = add i64 %5, %7
  store i64 %8, i64* %3
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %9 = load i64, i64* %3
  ret i64 %9
}

declare i8* @GC_malloc(i64)

define {}* @do_nothing179() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  ret {}* %2
}

define i64 @add2178(i8*, i64) {
  %3 = bitcast i8* %0 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { i64 (i8*, i64)*, i8* }*
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 0
  store i64 (i8*, i64)* @add2178, i64 (i8*, i64)** %8
  %9 = call i8* @GC_malloc(i64 0)
  %10 = bitcast i8* %9 to {}*
  %11 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %11
  %12 = call i8* @GC_malloc(i64 0)
  %13 = bitcast i8* %12 to {}*
  %14 = add i64 %5, %1
  ret i64 %14
}

define i64 @add177(i64) {
  %2 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %3 = bitcast i8* %2 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  store i64 %0, i64* %4
  %5 = call i8* @GC_malloc(i64 0)
  %6 = bitcast i8* %5 to {}*
  %7 = bitcast { i64 }* %3 to i8*
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i64 (i8*, i64)*, i8* }*
  %10 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 0
  store i64 (i8*, i64)* @add2178, i64 (i8*, i64)** %10
  %11 = call i8* @GC_malloc(i64 0)
  %12 = bitcast i8* %11 to {}*
  %13 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %7, i8** %13
  %14 = call i8* @GC_malloc(i64 0)
  %15 = bitcast i8* %14 to {}*
  %16 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 0
  %17 = load i64 (i8*, i64)*, i64 (i8*, i64)** %16
  %18 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 1
  %19 = load i8*, i8** %18
  %20 = call i64 %17(i8* %19, i64 2)
  ret i64 %20
}

define double @area176(double) {
  %2 = fmul double %0, %0
  %3 = fmul double %2, 3.140000e+00
  ret double %3
}

define {}* @print_fib175(i64) {
  %2 = call i64 @fib180(i64 %0)
  %3 = call {}* @print_int193(i64 %2)
  ret {}* %3
}

define i64 @"$lambda174"(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i64 (i8*, i64)*, i8* }*
  %6 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda174", i64 (i8*, i64)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = mul i64 %1, 4
  ret i64 %12
}

define i32 @main() {
  %1 = add i64 42, 1
  %2 = bitcast [6 x i8]* @0 to i8*
  %3 = bitcast [14 x i8]* @1 to i8*
  %4 = call {}* @println194(i8* %3)
  %5 = bitcast [11 x i8]* @2 to i8*
  %6 = call {}* @print195(i8* %5)
  %7 = call i64 @fib180(i64 10)
  %8 = call {}* @println_int182(i64 %7)
  %9 = call {}* @do_nothing179()
  %10 = call i64 @add177(i64 2)
  %11 = call {}* @println_int182(i64 %10)
  %12 = call {}* @println_int182(i64 %1)
  %13 = call i8* @GC_malloc(i64 0)
  %14 = bitcast i8* %13 to {}*
  %15 = bitcast {}* %14 to i8*
  %16 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %17 = bitcast i8* %16 to { i64 (i8*, i64)*, i8* }*
  %18 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %17, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda174", i64 (i8*, i64)** %18
  %19 = call i8* @GC_malloc(i64 0)
  %20 = bitcast i8* %19 to {}*
  %21 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %17, i32 0, i32 1
  store i8* %15, i8** %21
  %22 = call i8* @GC_malloc(i64 0)
  %23 = bitcast i8* %22 to {}*
  %24 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %17, i32 0, i32 0
  %25 = load i64 (i8*, i64)*, i64 (i8*, i64)** %24
  %26 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %17, i32 0, i32 1
  %27 = load i8*, i8** %26
  %28 = call i64 %25(i8* %27, i64 3)
  %29 = call {}* @println_int182(i64 %28)
  %30 = call i8* @substring185(i8* %2, i64 1, i64 3)
  %31 = call {}* @println194(i8* %30)
  %32 = bitcast [4 x i8]* @3 to i8*
  %33 = bitcast [4 x i8]* @4 to i8*
  %34 = call i8* @concat184(i8* %32, i8* %33)
  %35 = call {}* @println194(i8* %34)
  %36 = call {}* @println_float181(double 3.140000e+00)
  %37 = call double @area176(double 1.000000e+01)
  %38 = call {}* @println_float181(double %37)
  ret i32 0
}
