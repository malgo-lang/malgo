; ModuleID = './examples/lambda.mlg'
source_filename = "./examples/lambda.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

define {} @main.129() {
  %1 = getelementptr {}, {}* null, i64 1
  %2 = ptrtoint {}* %1 to i64
  %3 = call i8* @malloc_gc(i64 %2)
  %4 = bitcast i8* %3 to {}*
  %5 = bitcast {}* %4 to i8*
  %6 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %7 = ptrtoint { i64 (i8*, i64)*, i8* }* %6 to i64
  %8 = call i8* @malloc_gc(i64 %7)
  %9 = bitcast i8* %8 to { i64 (i8*, i64)*, i8* }*
  %10 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.74", i64 (i8*, i64)** %10
  %11 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %5, i8** %11
  %12 = getelementptr {}, {}* null, i64 1
  %13 = ptrtoint {}* %12 to i64
  %14 = call i8* @malloc_gc(i64 %13)
  %15 = bitcast i8* %14 to {}*
  %16 = bitcast {}* %15 to i8*
  %17 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* null, i64 1
  %18 = ptrtoint { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %17 to i64
  %19 = call i8* @malloc_gc(i64 %18)
  %20 = bitcast i8* %19 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*
  %21 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %20, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @"$lambda.79", { i64 (i8*, i64)*, i8* }* (i8*, i64)** %21
  %22 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %20, i32 0, i32 1
  store i8* %16, i8** %22
  %23 = alloca i64
  br i1 true, label %then_0, label %else_0

then_0:                                           ; preds = %0
  store i64 42, i64* %23
  br label %endif_0

else_0:                                           ; preds = %0
  store i64 48, i64* %23
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %24 = load i64, i64* %23
  %25 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 0
  %26 = load i64 (i8*, i64)*, i64 (i8*, i64)** %25
  %27 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %9, i32 0, i32 1
  %28 = load i8*, i8** %27
  %29 = call i64 %26(i8* %28, i64 41)
  %30 = call {} @print_int.67(i64 %29)
  %31 = call {} @newline.70({} undef)
  %32 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %20, i32 0, i32 0
  %33 = load { i64 (i8*, i64)*, i8* }* (i8*, i64)*, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %32
  %34 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %20, i32 0, i32 1
  %35 = load i8*, i8** %34
  %36 = call { i64 (i8*, i64)*, i8* }* %33(i8* %35, i64 2)
  %37 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %36, i32 0, i32 0
  %38 = load i64 (i8*, i64)*, i64 (i8*, i64)** %37
  %39 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %36, i32 0, i32 1
  %40 = load i8*, i8** %39
  %41 = call i64 %38(i8* %40, i64 3)
  %42 = call {} @print_int.67(i64 %41)
  %43 = call {} @newline.70({} undef)
  ret {} %43
}

define { i64 (i8*, i64)*, i8* }* @"$lambda.79"(i8*, i64) {
  %3 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*
  %7 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @"$lambda.79", { i64 (i8*, i64)*, i8* }* (i8*, i64)** %7
  %8 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to {}*
  %10 = getelementptr { i64 }, { i64 }* null, i64 1
  %11 = ptrtoint { i64 }* %10 to i64
  %12 = call i8* @malloc_gc(i64 %11)
  %13 = bitcast i8* %12 to { i64 }*
  %14 = getelementptr { i64 }, { i64 }* %13, i32 0, i32 0
  store i64 %1, i64* %14
  %15 = bitcast { i64 }* %13 to i8*
  %16 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %17 = ptrtoint { i64 (i8*, i64)*, i8* }* %16 to i64
  %18 = call i8* @malloc_gc(i64 %17)
  %19 = bitcast i8* %18 to { i64 (i8*, i64)*, i8* }*
  %20 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %19, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.81", i64 (i8*, i64)** %20
  %21 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %19, i32 0, i32 1
  store i8* %15, i8** %21
  ret { i64 (i8*, i64)*, i8* }* %19
}

declare i64 @add_i64(i64, i64)

define i64 @"$lambda.81"(i8*, i64) {
  %3 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i64 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }*
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.81", i64 (i8*, i64)** %7
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i64 }*
  %10 = getelementptr { i64 }, { i64 }* %9, i32 0, i32 0
  %11 = load i64, i64* %10
  %12 = call i64 @add_i64(i64 %11, i64 %1)
  ret i64 %12
}

define i64 @"$lambda.74"(i8*, i64) {
  %3 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i64 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }*
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.74", i64 (i8*, i64)** %7
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to {}*
  %10 = call i64 @add_i64(i64 %1, i64 1)
  ret i64 %10
}

declare {} @newline({})

define {} @newline.70({}) {
  %2 = call {} @newline({} %0)
  ret {} %2
}

declare {} @print_int(i64)

define {} @print_int.67(i64) {
  %2 = call {} @print_int(i64 %0)
  ret {} %2
}

define i32 @main() {
  call void @init_gc()
  %1 = call {} @main.129()
  ret i32 0
}
