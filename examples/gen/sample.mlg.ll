; ModuleID = './examples/sample.mlg'
source_filename = "./examples/sample.mlg"

@0 = unnamed_addr constant [6 x i8] c"malgo\00"
@1 = unnamed_addr constant [14 x i8] c"Hello, world!\00"
@2 = unnamed_addr constant [11 x i8] c"fib(10) = \00"
@3 = unnamed_addr constant [4 x i8] c"foo\00"
@4 = unnamed_addr constant [4 x i8] c"bar\00"

declare i8* @substring(i8*, i64, i64)

define i8* @substring10(i8*, i64, i64) {
  %4 = call i8* @substring(i8* %0, i64 %1, i64 %2)
  ret i8* %4
}

declare i64 @size(i8*)

define i64 @size8(i8*) {
  %2 = call i64 @size(i8* %0)
  ret i64 %2
}

define {}* @println_int13(i64) {
  %2 = call {}* @print_int2(i64 %0)
  %3 = call {}* @newline9()
  ret {}* %3
}

define {}* @println_float14(double) {
  %2 = call {}* @print_float3(double %0)
  %3 = call {}* @newline9()
  ret {}* %3
}

declare {}* @println(i8*)

define {}* @println1(i8*) {
  %2 = call {}* @println(i8* %0)
  ret {}* %2
}

declare {}* @print_int(i64)

define {}* @print_int2(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

declare {}* @print_float(double)

define {}* @print_float3(double) {
  %2 = call {}* @print_float(double %0)
  ret {}* %2
}

define {}* @print_fib29(i64) {
  %2 = call i64 @fib18(i64 %0)
  %3 = call {}* @print_int2(i64 %2)
  ret {}* %3
}

declare {}* @print(i8*)

define {}* @print0(i8*) {
  %2 = call {}* @print(i8* %0)
  ret {}* %2
}

declare i64 @ord(i8)

define i64 @ord6(i8) {
  %2 = call i64 @ord(i8 %0)
  ret i64 %2
}

declare i1 @not(i1)

define i1 @not12(i1) {
  %2 = call i1 @not(i1 %0)
  ret i1 %2
}

declare {}* @newline()

define {}* @newline9() {
  %1 = call {}* @newline()
  ret {}* %1
}

declare i8 @getchar()

define i8 @getChar5() {
  %1 = call i8 @getchar()
  ret i8 %1
}

declare {}* @flush()

define {}* @flush4() {
  %1 = call {}* @flush()
  ret {}* %1
}

define i64 @fib18(i64) {
  %2 = icmp sle i64 %0, 1
  %3 = alloca i64
  br i1 %2, label %then_0, label %else_0

then_0:                                           ; preds = %1
  store i64 1, i64* %3
  br label %end_0

else_0:                                           ; preds = %1
  %4 = sub i64 %0, 1
  %5 = call i64 @fib18(i64 %4)
  %6 = sub i64 %0, 2
  %7 = call i64 @fib18(i64 %6)
  %8 = add i64 %5, %7
  store i64 %8, i64* %3
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %9 = load i64, i64* %3
  ret i64 %9
}

declare i8* @GC_malloc(i64)

define {}* @do_nothing21() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  ret {}* %2
}

declare i8* @concat(i8*, i8*)

define i8* @concat11(i8*, i8*) {
  %3 = call i8* @concat(i8* %0, i8* %1)
  ret i8* %3
}

declare i8 @chr(i64)

define i8 @chr7(i64) {
  %2 = call i8 @chr(i64 %0)
  ret i8 %2
}

define double @area23(double) {
  %2 = fmul double %0, %0
  %3 = fmul double %2, 3.140000e+00
  ret double %3
}

define i64 @add225(i8*, i64) {
  %3 = bitcast i8* %0 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { i64 (i8*, i64)*, i8* }*
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 0
  store i64 (i8*, i64)* @add225, i64 (i8*, i64)** %8
  %9 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = add i64 %5, %1
  ret i64 %10
}

define i64 @add22(i64) {
  %2 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %3 = bitcast i8* %2 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  store i64 %0, i64* %4
  %5 = bitcast { i64 }* %3 to i8*
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { i64 (i8*, i64)*, i8* }*
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 0
  store i64 (i8*, i64)* @add225, i64 (i8*, i64)** %8
  %9 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %5, i8** %9
  %10 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 0
  %11 = load i64 (i8*, i64)*, i64 (i8*, i64)** %10
  %12 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 1
  %13 = load i8*, i8** %12
  %14 = call i64 %11(i8* %13, i64 2)
  ret i64 %14
}

define i64 @"$lambda117"(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i64 (i8*, i64)*, i8* }*
  %6 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda117", i64 (i8*, i64)** %6
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %7
  %8 = mul i64 %1, 4
  ret i64 %8
}

define i32 @main() {
  %1 = add i64 42, 1
  %2 = call {}* @println1(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @1, i32 0, i32 0))
  %3 = call {}* @print0(i8* getelementptr inbounds ([11 x i8], [11 x i8]* @2, i32 0, i32 0))
  %4 = call i64 @fib18(i64 10)
  %5 = call {}* @println_int13(i64 %4)
  %6 = call {}* @do_nothing21()
  %7 = call i64 @add22(i64 2)
  %8 = call {}* @println_int13(i64 %7)
  %9 = call {}* @println_int13(i64 %1)
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = bitcast {}* %11 to i8*
  %13 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %14 = bitcast i8* %13 to { i64 (i8*, i64)*, i8* }*
  %15 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %14, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda117", i64 (i8*, i64)** %15
  %16 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %14, i32 0, i32 1
  store i8* %12, i8** %16
  %17 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %14, i32 0, i32 0
  %18 = load i64 (i8*, i64)*, i64 (i8*, i64)** %17
  %19 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %14, i32 0, i32 1
  %20 = load i8*, i8** %19
  %21 = call i64 %18(i8* %20, i64 3)
  %22 = call {}* @println_int13(i64 %21)
  %23 = call i8* @substring10(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @0, i32 0, i32 0), i64 1, i64 3)
  %24 = call {}* @println1(i8* %23)
  %25 = call i8* @concat11(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @3, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8], [4 x i8]* @4, i32 0, i32 0))
  %26 = call {}* @println1(i8* %25)
  %27 = call {}* @println_float14(double 3.140000e+00)
  %28 = call double @area23(double 1.000000e+01)
  %29 = call {}* @println_float14(double %28)
  ret i32 0
}
