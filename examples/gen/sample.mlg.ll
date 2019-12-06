; ModuleID = './examples/sample.mlg'
source_filename = "./examples/sample.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

declare i64 @add_i64(i64, i64)

define {} @main.475() {
  %1 = call i64 @add_i64(i64 42, i64 1)
  %2 = call i8* @malloc_gc(i64 6)
  %3 = getelementptr i8, i8* %2, i64 0
  store i8 109, i8* %3
  %4 = getelementptr i8, i8* %2, i64 1
  store i8 97, i8* %4
  %5 = getelementptr i8, i8* %2, i64 2
  store i8 108, i8* %5
  %6 = getelementptr i8, i8* %2, i64 3
  store i8 103, i8* %6
  %7 = getelementptr i8, i8* %2, i64 4
  store i8 111, i8* %7
  %8 = getelementptr i8, i8* %2, i64 5
  store i8 0, i8* %8
  %9 = call i8* @malloc_gc(i64 14)
  %10 = getelementptr i8, i8* %9, i64 0
  store i8 72, i8* %10
  %11 = getelementptr i8, i8* %9, i64 1
  store i8 101, i8* %11
  %12 = getelementptr i8, i8* %9, i64 2
  store i8 108, i8* %12
  %13 = getelementptr i8, i8* %9, i64 3
  store i8 108, i8* %13
  %14 = getelementptr i8, i8* %9, i64 4
  store i8 111, i8* %14
  %15 = getelementptr i8, i8* %9, i64 5
  store i8 44, i8* %15
  %16 = getelementptr i8, i8* %9, i64 6
  store i8 32, i8* %16
  %17 = getelementptr i8, i8* %9, i64 7
  store i8 119, i8* %17
  %18 = getelementptr i8, i8* %9, i64 8
  store i8 111, i8* %18
  %19 = getelementptr i8, i8* %9, i64 9
  store i8 114, i8* %19
  %20 = getelementptr i8, i8* %9, i64 10
  store i8 108, i8* %20
  %21 = getelementptr i8, i8* %9, i64 11
  store i8 100, i8* %21
  %22 = getelementptr i8, i8* %9, i64 12
  store i8 33, i8* %22
  %23 = getelementptr i8, i8* %9, i64 13
  store i8 0, i8* %23
  %24 = call {} @println.288(i8* %9)
  %25 = call i8* @malloc_gc(i64 11)
  %26 = getelementptr i8, i8* %25, i64 0
  store i8 102, i8* %26
  %27 = getelementptr i8, i8* %25, i64 1
  store i8 105, i8* %27
  %28 = getelementptr i8, i8* %25, i64 2
  store i8 98, i8* %28
  %29 = getelementptr i8, i8* %25, i64 3
  store i8 40, i8* %29
  %30 = getelementptr i8, i8* %25, i64 4
  store i8 49, i8* %30
  %31 = getelementptr i8, i8* %25, i64 5
  store i8 48, i8* %31
  %32 = getelementptr i8, i8* %25, i64 6
  store i8 41, i8* %32
  %33 = getelementptr i8, i8* %25, i64 7
  store i8 32, i8* %33
  %34 = getelementptr i8, i8* %25, i64 8
  store i8 61, i8* %34
  %35 = getelementptr i8, i8* %25, i64 9
  store i8 32, i8* %35
  %36 = getelementptr i8, i8* %25, i64 10
  store i8 0, i8* %36
  %37 = call {} @print.285(i8* %25)
  %38 = call i64 @fib.344(i64 10)
  %39 = call {} @println_int.327(i64 %38)
  %40 = call {} @do_nothing.361({} undef)
  %41 = call i64 @add.375(i64 2)
  %42 = call {} @println_int.327(i64 %41)
  %43 = call {} @println_int.327(i64 %1)
  %44 = getelementptr {}, {}* null, i64 1
  %45 = ptrtoint {}* %44 to i64
  %46 = call i8* @malloc_gc(i64 %45)
  %47 = bitcast i8* %46 to {}*
  %48 = bitcast {}* %47 to i8*
  %49 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %50 = ptrtoint { i64 (i8*, i64)*, i8* }* %49 to i64
  %51 = call i8* @malloc_gc(i64 %50)
  %52 = bitcast i8* %51 to { i64 (i8*, i64)*, i8* }*
  %53 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %52, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.420", i64 (i8*, i64)** %53
  %54 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %52, i32 0, i32 1
  store i8* %48, i8** %54
  %55 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %52, i32 0, i32 0
  %56 = load i64 (i8*, i64)*, i64 (i8*, i64)** %55
  %57 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %52, i32 0, i32 1
  %58 = load i8*, i8** %57
  %59 = call i64 %56(i8* %58, i64 3)
  %60 = call {} @println_int.327(i64 %59)
  %61 = call i8* @substring.315(i8* %2, i64 1, i64 3)
  %62 = call {} @println.288(i8* %61)
  %63 = call i8* @malloc_gc(i64 4)
  %64 = getelementptr i8, i8* %63, i64 0
  store i8 102, i8* %64
  %65 = getelementptr i8, i8* %63, i64 1
  store i8 111, i8* %65
  %66 = getelementptr i8, i8* %63, i64 2
  store i8 111, i8* %66
  %67 = getelementptr i8, i8* %63, i64 3
  store i8 0, i8* %67
  %68 = call i8* @malloc_gc(i64 4)
  %69 = getelementptr i8, i8* %68, i64 0
  store i8 98, i8* %69
  %70 = getelementptr i8, i8* %68, i64 1
  store i8 97, i8* %70
  %71 = getelementptr i8, i8* %68, i64 2
  store i8 114, i8* %71
  %72 = getelementptr i8, i8* %68, i64 3
  store i8 0, i8* %72
  %73 = call i8* @concat.320(i8* %63, i8* %68)
  %74 = call {} @println.288(i8* %73)
  %75 = call {} @println_float.335(double 3.140000e+00)
  %76 = call double @area.389(double 1.000000e+01)
  %77 = call {} @println_float.335(double %76)
  ret {} %77
}

declare i64 @mul_i64(i64, i64)

define i64 @"$lambda.420"(i8*, i64) {
  %3 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i64 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }*
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.420", i64 (i8*, i64)** %7
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to {}*
  %10 = call i64 @mul_i64(i64 %1, i64 4)
  ret i64 %10
}

define {} @print_fib.404(i64) {
  %2 = call i64 @fib.344(i64 %0)
  %3 = call {} @print_int.291(i64 %2)
  ret {} %3
}

define {} @do_nothing.361({}) {
  ret {} undef
}

define i64 @add.363(i64) {
  %2 = getelementptr { i64 }, { i64 }* null, i64 1
  %3 = ptrtoint { i64 }* %2 to i64
  %4 = call i8* @malloc_gc(i64 %3)
  %5 = bitcast i8* %4 to { i64 }*
  %6 = getelementptr { i64 }, { i64 }* %5, i32 0, i32 0
  store i64 %0, i64* %6
  %7 = bitcast { i64 }* %5 to i8*
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %9 = ptrtoint { i64 (i8*, i64)*, i8* }* %8 to i64
  %10 = call i8* @malloc_gc(i64 %9)
  %11 = bitcast i8* %10 to { i64 (i8*, i64)*, i8* }*
  %12 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %11, i32 0, i32 0
  store i64 (i8*, i64)* @add2.371, i64 (i8*, i64)** %12
  %13 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %7, i8** %13
  %14 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %11, i32 0, i32 0
  %15 = load i64 (i8*, i64)*, i64 (i8*, i64)** %14
  %16 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %11, i32 0, i32 1
  %17 = load i8*, i8** %16
  %18 = call i64 %15(i8* %17, i64 2)
  ret i64 %18
}

define i64 @add2.371(i8*, i64) {
  %3 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i64 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }*
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, i64)* @add2.371, i64 (i8*, i64)** %7
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i64 }*
  %10 = getelementptr { i64 }, { i64 }* %9, i32 0, i32 0
  %11 = load i64, i64* %10
  %12 = call i64 @add_i64(i64 %11, i64 %1)
  ret i64 %12
}

declare double @mul_double(double, double)

define double @area.365(double) {
  %2 = call double @mul_double(double %0, double %0)
  %3 = call double @mul_double(double %2, double 3.140000e+00)
  ret double %3
}

define i64 @add.375(i64) {
  %2 = getelementptr { i64 }, { i64 }* null, i64 1
  %3 = ptrtoint { i64 }* %2 to i64
  %4 = call i8* @malloc_gc(i64 %3)
  %5 = bitcast i8* %4 to { i64 }*
  %6 = getelementptr { i64 }, { i64 }* %5, i32 0, i32 0
  store i64 %0, i64* %6
  %7 = bitcast { i64 }* %5 to i8*
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %9 = ptrtoint { i64 (i8*, i64)*, i8* }* %8 to i64
  %10 = call i8* @malloc_gc(i64 %9)
  %11 = bitcast i8* %10 to { i64 (i8*, i64)*, i8* }*
  %12 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %11, i32 0, i32 0
  store i64 (i8*, i64)* @add2.385, i64 (i8*, i64)** %12
  %13 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %7, i8** %13
  %14 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %11, i32 0, i32 0
  %15 = load i64 (i8*, i64)*, i64 (i8*, i64)** %14
  %16 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %11, i32 0, i32 1
  %17 = load i8*, i8** %16
  %18 = call i64 %15(i8* %17, i64 2)
  ret i64 %18
}

define i64 @add2.385(i8*, i64) {
  %3 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i64 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }*
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, i64)* @add2.385, i64 (i8*, i64)** %7
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i64 }*
  %10 = getelementptr { i64 }, { i64 }* %9, i32 0, i32 0
  %11 = load i64, i64* %10
  %12 = call i64 @add_i64(i64 %11, i64 %1)
  ret i64 %12
}

define {} @do_nothing.377({}) {
  ret {} undef
}

define double @area.379(double) {
  %2 = call double @mul_double(double %0, double %0)
  %3 = call double @mul_double(double %2, double 3.140000e+00)
  ret double %3
}

define double @area.389(double) {
  %2 = call double @mul_double(double %0, double %0)
  %3 = call double @mul_double(double %2, double 3.140000e+00)
  ret double %3
}

define i64 @add.391(i64) {
  %2 = getelementptr { i64 }, { i64 }* null, i64 1
  %3 = ptrtoint { i64 }* %2 to i64
  %4 = call i8* @malloc_gc(i64 %3)
  %5 = bitcast i8* %4 to { i64 }*
  %6 = getelementptr { i64 }, { i64 }* %5, i32 0, i32 0
  store i64 %0, i64* %6
  %7 = bitcast { i64 }* %5 to i8*
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %9 = ptrtoint { i64 (i8*, i64)*, i8* }* %8 to i64
  %10 = call i8* @malloc_gc(i64 %9)
  %11 = bitcast i8* %10 to { i64 (i8*, i64)*, i8* }*
  %12 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %11, i32 0, i32 0
  store i64 (i8*, i64)* @add2.395, i64 (i8*, i64)** %12
  %13 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %11, i32 0, i32 1
  store i8* %7, i8** %13
  %14 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %11, i32 0, i32 0
  %15 = load i64 (i8*, i64)*, i64 (i8*, i64)** %14
  %16 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %11, i32 0, i32 1
  %17 = load i8*, i8** %16
  %18 = call i64 %15(i8* %17, i64 2)
  ret i64 %18
}

define i64 @add2.395(i8*, i64) {
  %3 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i64 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }*
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, i64)* @add2.395, i64 (i8*, i64)** %7
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i64 }*
  %10 = getelementptr { i64 }, { i64 }* %9, i32 0, i32 0
  %11 = load i64, i64* %10
  %12 = call i64 @add_i64(i64 %11, i64 %1)
  ret i64 %12
}

define {} @do_nothing.393({}) {
  ret {} undef
}

declare i1 @le_i64(i64, i64)

declare i64 @sub_i64(i64, i64)

define i64 @fib.344(i64) {
  %2 = call i1 @le_i64(i64 %0, i64 1)
  %3 = alloca i64
  br i1 %2, label %then_0, label %else_0

then_0:                                           ; preds = %1
  store i64 1, i64* %3
  br label %endif_0

else_0:                                           ; preds = %1
  %4 = call i64 @sub_i64(i64 %0, i64 1)
  %5 = call i64 @fib.344(i64 %4)
  %6 = call i64 @sub_i64(i64 %0, i64 2)
  %7 = call i64 @fib.344(i64 %6)
  %8 = call i64 @add_i64(i64 %5, i64 %7)
  store i64 %8, i64* %3
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %9 = load i64, i64* %3
  ret i64 %9
}

define {} @println_int.327(i64) {
  %2 = call {} @print_int.291(i64 %0)
  %3 = call {} @newline.312({} undef)
  ret {} %3
}

define {} @println_float.329(double) {
  %2 = call {} @print_float.294(double %0)
  %3 = call {} @newline.312({} undef)
  ret {} %3
}

define {} @println_float.335(double) {
  %2 = call {} @print_float.294(double %0)
  %3 = call {} @newline.312({} undef)
  ret {} %3
}

define {} @println_int.337(i64) {
  %2 = call {} @print_int.291(i64 %0)
  %3 = call {} @newline.312({} undef)
  ret {} %3
}

declare i1 @not(i1)

define i1 @not.324(i1) {
  %2 = call i1 @not(i1 %0)
  ret i1 %2
}

declare i8* @concat(i8*, i8*)

define i8* @concat.320(i8*, i8*) {
  %3 = call i8* @concat(i8* %0, i8* %1)
  ret i8* %3
}

declare i8* @substring(i8*, i64, i64)

define i8* @substring.315(i8*, i64, i64) {
  %4 = call i8* @substring(i8* %0, i64 %1, i64 %2)
  ret i8* %4
}

declare {} @newline({})

define {} @newline.312({}) {
  %2 = call {} @newline({} %0)
  ret {} %2
}

declare i64 @size(i8*)

define i64 @size.309(i8*) {
  %2 = call i64 @size(i8* %0)
  ret i64 %2
}

declare i8 @chr(i64)

define i8 @chr.306(i64) {
  %2 = call i8 @chr(i64 %0)
  ret i8 %2
}

declare i64 @ord(i8)

define i64 @ord.303(i8) {
  %2 = call i64 @ord(i8 %0)
  ret i64 %2
}

declare i8 @getchar({})

define i8 @getChar.300({}) {
  %2 = call i8 @getchar({} %0)
  ret i8 %2
}

declare {} @flush({})

define {} @flush.297({}) {
  %2 = call {} @flush({} %0)
  ret {} %2
}

declare {} @print_float(double)

define {} @print_float.294(double) {
  %2 = call {} @print_float(double %0)
  ret {} %2
}

declare {} @print_int(i64)

define {} @print_int.291(i64) {
  %2 = call {} @print_int(i64 %0)
  ret {} %2
}

declare {} @println(i8*)

define {} @println.288(i8*) {
  %2 = call {} @println(i8* %0)
  ret {} %2
}

declare {} @print(i8*)

define {} @print.285(i8*) {
  %2 = call {} @print(i8* %0)
  ret {} %2
}

define i32 @main() {
  call void @init_gc()
  %1 = call {} @main.475()
  ret i32 0
}
