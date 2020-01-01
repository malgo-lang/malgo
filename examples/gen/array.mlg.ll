; ModuleID = './examples/array.mlg'
source_filename = "./examples/array.mlg"

declare {}* @print_int(i64)

define {}* @print_int28(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

declare {}* @newline()

define {}* @newline27() {
  %1 = call {}* @newline()
  ret {}* %1
}

declare i8* @GC_malloc(i64)

define i32 @main() {
  %1 = mul i64 ptrtoint (i64* getelementptr inbounds (i64, i64* null, i32 1) to i64), 10
  %2 = call i8* @GC_malloc(i64 %1)
  %3 = bitcast i8* %2 to i64*
  %4 = call i8* @GC_malloc(i64 ptrtoint ({ i64*, i64 }* getelementptr inbounds ({ i64*, i64 }, { i64*, i64 }* null, i32 1) to i64))
  %5 = bitcast i8* %4 to { i64*, i64 }*
  %6 = getelementptr { i64*, i64 }, { i64*, i64 }* %5, i32 0, i32 0
  store i64* %3, i64** %6
  %7 = getelementptr { i64*, i64 }, { i64*, i64 }* %5, i32 0, i32 1
  store i64 10, i64* %7
  %8 = alloca i64
  store i64 0, i64* %8
  br label %cond_0

cond_0:                                           ; preds = %body_0, %0
  %9 = load i64, i64* %8
  %10 = icmp slt i64 %9, 10
  br i1 %10, label %body_0, label %end_0

body_0:                                           ; preds = %cond_0
  %11 = getelementptr { i64*, i64 }, { i64*, i64 }* %5, i32 0, i32 0
  %12 = load i64*, i64** %11
  %13 = getelementptr i64, i64* %12, i64 %9
  store i64 0, i64* %13
  %14 = call i8* @GC_malloc(i64 0)
  %15 = bitcast i8* %14 to {}*
  %16 = add i64 %9, 1
  store i64 %16, i64* %8
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %17 = call i8* @GC_malloc(i64 0)
  %18 = bitcast i8* %17 to {}*
  %19 = getelementptr { i64*, i64 }, { i64*, i64 }* %5, i32 0, i32 0
  %20 = load i64*, i64** %19
  %21 = getelementptr i64, i64* %20, i64 1
  %22 = load i64, i64* %21
  %23 = getelementptr { i64*, i64 }, { i64*, i64 }* %5, i32 0, i32 0
  %24 = load i64*, i64** %23
  %25 = getelementptr i64, i64* %24, i64 2
  store i64 42, i64* %25
  %26 = call i8* @GC_malloc(i64 0)
  %27 = bitcast i8* %26 to {}*
  %28 = getelementptr { i64*, i64 }, { i64*, i64 }* %5, i32 0, i32 0
  %29 = load i64*, i64** %28
  %30 = getelementptr i64, i64* %29, i64 2
  %31 = load i64, i64* %30
  %32 = call {}* @print_int28(i64 %22)
  %33 = call {}* @newline27()
  %34 = call {}* @print_int28(i64 %31)
  %35 = call {}* @newline27()
  ret i32 0
}
