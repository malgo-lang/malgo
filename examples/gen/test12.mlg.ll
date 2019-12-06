; ModuleID = './examples/test12.mlg'
source_filename = "./examples/test12.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

define {} @main.146() {
  %1 = getelementptr { i64 }, { i64 }* null, i64 1
  %2 = ptrtoint { i64 }* %1 to i64
  %3 = call i8* @malloc_gc(i64 %2)
  %4 = bitcast i8* %3 to { i64 }*
  %5 = getelementptr { i64 }, { i64 }* %4, i32 0, i32 0
  store i64 0, i64* %5
  %6 = bitcast { i64 }* %4 to i8*
  %7 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* null, i64 1
  %8 = ptrtoint { i1 (i8*, i64)*, i8* }* %7 to i64
  %9 = call i8* @malloc_gc(i64 %8)
  %10 = bitcast i8* %9 to { i1 (i8*, i64)*, i8* }*
  %11 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %10, i32 0, i32 0
  store i1 (i8*, i64)* @odd.65, i1 (i8*, i64)** %11
  %12 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %10, i32 0, i32 1
  store i8* %6, i8** %12
  %13 = getelementptr { i64 }, { i64 }* null, i64 1
  %14 = ptrtoint { i64 }* %13 to i64
  %15 = call i8* @malloc_gc(i64 %14)
  %16 = bitcast i8* %15 to { i64 }*
  %17 = getelementptr { i64 }, { i64 }* %16, i32 0, i32 0
  store i64 0, i64* %17
  %18 = bitcast { i64 }* %16 to i8*
  %19 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* null, i64 1
  %20 = ptrtoint { i1 (i8*, i64)*, i8* }* %19 to i64
  %21 = call i8* @malloc_gc(i64 %20)
  %22 = bitcast i8* %21 to { i1 (i8*, i64)*, i8* }*
  %23 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %22, i32 0, i32 0
  store i1 (i8*, i64)* @even.51, i1 (i8*, i64)** %23
  %24 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %22, i32 0, i32 1
  store i8* %18, i8** %24
  %25 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %10, i32 0, i32 0
  %26 = load i1 (i8*, i64)*, i1 (i8*, i64)** %25
  %27 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %10, i32 0, i32 1
  %28 = load i8*, i8** %27
  %29 = call i1 %26(i8* %28, i64 34)
  %30 = call {} @print_bool.79(i1 %29)
  ret {} %30
}

declare {} @print_bool(i1)

define {} @print_bool.79(i1) {
  %2 = call {} @print_bool(i1 %0)
  ret {} %2
}

declare i1 @eq_i64(i64, i64)

declare i64 @sub_i64(i64, i64)

define i1 @even.51(i8*, i64) {
  %3 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i1 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i1 (i8*, i64)*, i8* }*
  %7 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i1 (i8*, i64)* @even.51, i1 (i8*, i64)** %7
  %8 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i64 }*
  %10 = getelementptr { i64 }, { i64 }* %9, i32 0, i32 0
  %11 = load i64, i64* %10
  %12 = getelementptr { i64, { i1 (i8*, i64)*, i8* }* }, { i64, { i1 (i8*, i64)*, i8* }* }* null, i64 1
  %13 = ptrtoint { i64, { i1 (i8*, i64)*, i8* }* }* %12 to i64
  %14 = call i8* @malloc_gc(i64 %13)
  %15 = bitcast i8* %14 to { i64, { i1 (i8*, i64)*, i8* }* }*
  %16 = getelementptr { i64, { i1 (i8*, i64)*, i8* }* }, { i64, { i1 (i8*, i64)*, i8* }* }* %15, i32 0, i32 0
  store i64 %11, i64* %16
  %17 = getelementptr { i64, { i1 (i8*, i64)*, i8* }* }, { i64, { i1 (i8*, i64)*, i8* }* }* %15, i32 0, i32 1
  store { i1 (i8*, i64)*, i8* }* %6, { i1 (i8*, i64)*, i8* }** %17
  %18 = bitcast { i64, { i1 (i8*, i64)*, i8* }* }* %15 to i8*
  %19 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* null, i64 1
  %20 = ptrtoint { i1 (i8*, i64)*, i8* }* %19 to i64
  %21 = call i8* @malloc_gc(i64 %20)
  %22 = bitcast i8* %21 to { i1 (i8*, i64)*, i8* }*
  %23 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %22, i32 0, i32 0
  store i1 (i8*, i64)* @odd.53, i1 (i8*, i64)** %23
  %24 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %22, i32 0, i32 1
  store i8* %18, i8** %24
  %25 = call i1 @eq_i64(i64 %1, i64 %11)
  %26 = alloca i1
  br i1 %25, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store i1 true, i1* %26
  br label %endif_0

else_0:                                           ; preds = %2
  %27 = call i64 @sub_i64(i64 %1, i64 1)
  %28 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %22, i32 0, i32 0
  %29 = load i1 (i8*, i64)*, i1 (i8*, i64)** %28
  %30 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %22, i32 0, i32 1
  %31 = load i8*, i8** %30
  %32 = call i1 %29(i8* %31, i64 %27)
  store i1 %32, i1* %26
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %33 = load i1, i1* %26
  ret i1 %33
}

define i1 @odd.53(i8*, i64) {
  %3 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i1 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i1 (i8*, i64)*, i8* }*
  %7 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i1 (i8*, i64)* @odd.53, i1 (i8*, i64)** %7
  %8 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i64, { i1 (i8*, i64)*, i8* }* }*
  %10 = getelementptr { i64, { i1 (i8*, i64)*, i8* }* }, { i64, { i1 (i8*, i64)*, i8* }* }* %9, i32 0, i32 0
  %11 = load i64, i64* %10
  %12 = getelementptr { i64, { i1 (i8*, i64)*, i8* }* }, { i64, { i1 (i8*, i64)*, i8* }* }* %9, i32 0, i32 1
  %13 = load { i1 (i8*, i64)*, i8* }*, { i1 (i8*, i64)*, i8* }** %12
  %14 = call i1 @eq_i64(i64 %1, i64 %11)
  %15 = alloca i1
  br i1 %14, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store i1 false, i1* %15
  br label %endif_0

else_0:                                           ; preds = %2
  %16 = call i64 @sub_i64(i64 %1, i64 1)
  %17 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %13, i32 0, i32 0
  %18 = load i1 (i8*, i64)*, i1 (i8*, i64)** %17
  %19 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %13, i32 0, i32 1
  %20 = load i8*, i8** %19
  %21 = call i1 %18(i8* %20, i64 %16)
  store i1 %21, i1* %15
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %22 = load i1, i1* %15
  ret i1 %22
}

define i1 @odd.65(i8*, i64) {
  %3 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i1 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i1 (i8*, i64)*, i8* }*
  %7 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i1 (i8*, i64)* @odd.65, i1 (i8*, i64)** %7
  %8 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i64 }*
  %10 = getelementptr { i64 }, { i64 }* %9, i32 0, i32 0
  %11 = load i64, i64* %10
  %12 = getelementptr { i64, { i1 (i8*, i64)*, i8* }* }, { i64, { i1 (i8*, i64)*, i8* }* }* null, i64 1
  %13 = ptrtoint { i64, { i1 (i8*, i64)*, i8* }* }* %12 to i64
  %14 = call i8* @malloc_gc(i64 %13)
  %15 = bitcast i8* %14 to { i64, { i1 (i8*, i64)*, i8* }* }*
  %16 = getelementptr { i64, { i1 (i8*, i64)*, i8* }* }, { i64, { i1 (i8*, i64)*, i8* }* }* %15, i32 0, i32 0
  store i64 %11, i64* %16
  %17 = getelementptr { i64, { i1 (i8*, i64)*, i8* }* }, { i64, { i1 (i8*, i64)*, i8* }* }* %15, i32 0, i32 1
  store { i1 (i8*, i64)*, i8* }* %6, { i1 (i8*, i64)*, i8* }** %17
  %18 = bitcast { i64, { i1 (i8*, i64)*, i8* }* }* %15 to i8*
  %19 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* null, i64 1
  %20 = ptrtoint { i1 (i8*, i64)*, i8* }* %19 to i64
  %21 = call i8* @malloc_gc(i64 %20)
  %22 = bitcast i8* %21 to { i1 (i8*, i64)*, i8* }*
  %23 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %22, i32 0, i32 0
  store i1 (i8*, i64)* @even.67, i1 (i8*, i64)** %23
  %24 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %22, i32 0, i32 1
  store i8* %18, i8** %24
  %25 = call i1 @eq_i64(i64 %1, i64 %11)
  %26 = alloca i1
  br i1 %25, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store i1 false, i1* %26
  br label %endif_0

else_0:                                           ; preds = %2
  %27 = call i64 @sub_i64(i64 %1, i64 1)
  %28 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %22, i32 0, i32 0
  %29 = load i1 (i8*, i64)*, i1 (i8*, i64)** %28
  %30 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %22, i32 0, i32 1
  %31 = load i8*, i8** %30
  %32 = call i1 %29(i8* %31, i64 %27)
  store i1 %32, i1* %26
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %33 = load i1, i1* %26
  ret i1 %33
}

define i1 @even.67(i8*, i64) {
  %3 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { i1 (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { i1 (i8*, i64)*, i8* }*
  %7 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store i1 (i8*, i64)* @even.67, i1 (i8*, i64)** %7
  %8 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to { i64, { i1 (i8*, i64)*, i8* }* }*
  %10 = getelementptr { i64, { i1 (i8*, i64)*, i8* }* }, { i64, { i1 (i8*, i64)*, i8* }* }* %9, i32 0, i32 0
  %11 = load i64, i64* %10
  %12 = getelementptr { i64, { i1 (i8*, i64)*, i8* }* }, { i64, { i1 (i8*, i64)*, i8* }* }* %9, i32 0, i32 1
  %13 = load { i1 (i8*, i64)*, i8* }*, { i1 (i8*, i64)*, i8* }** %12
  %14 = call i1 @eq_i64(i64 %1, i64 %11)
  %15 = alloca i1
  br i1 %14, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store i1 true, i1* %15
  br label %endif_0

else_0:                                           ; preds = %2
  %16 = call i64 @sub_i64(i64 %1, i64 1)
  %17 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %13, i32 0, i32 0
  %18 = load i1 (i8*, i64)*, i1 (i8*, i64)** %17
  %19 = getelementptr { i1 (i8*, i64)*, i8* }, { i1 (i8*, i64)*, i8* }* %13, i32 0, i32 1
  %20 = load i8*, i8** %19
  %21 = call i1 %18(i8* %20, i64 %16)
  store i1 %21, i1* %15
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %22 = load i1, i1* %15
  ret i1 %22
}

define i32 @main() {
  call void @init_gc()
  %1 = call {} @main.146()
  ret i32 0
}
