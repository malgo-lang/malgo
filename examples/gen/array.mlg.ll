; ModuleID = './examples/array.mlg'
source_filename = "./examples/array.mlg"

declare i8* @GC_malloc(i64)

define i32 @main27() {
  %1 = mul i64 ptrtoint (i64* getelementptr inbounds (i64, i64* null, i32 1) to i64), 10
  %2 = call i8* @GC_malloc(i64 %1)
  %3 = bitcast i8* %2 to i64*
  %4 = alloca i64
  store i64 0, i64* %4
  br label %cond_0

cond_0:                                           ; preds = %copyelem_0, %0
  %5 = load i64, i64* %4
  %6 = icmp slt i64 %5, 10
  br i1 %6, label %copyelem_0, label %end_0

copyelem_0:                                       ; preds = %cond_0
  %7 = getelementptr i64, i64* %3, i64 %5
  store i64 0, i64* %7
  %8 = add i64 %5, 1
  store i64 %8, i64* %4
  br label %cond_0

end_0:                                            ; preds = %cond_0
  %9 = getelementptr i64, i64* %3, i64 1
  %10 = load i64, i64* %9
  %11 = getelementptr i64, i64* %3, i64 2
  store i64 42, i64* %11
  %12 = call i8* @GC_malloc(i64 0)
  %13 = bitcast i8* %12 to {}*
  %14 = getelementptr i64, i64* %3, i64 2
  %15 = load i64, i64* %14
  %16 = call {}* @print_int29(i64 %10)
  %17 = call {}* @newline28()
  %18 = call {}* @print_int29(i64 %15)
  %19 = call {}* @newline28()
  ret i32 0
}

declare {}* @newline()

define {}* @newline28() {
  %1 = call {}* @newline()
  ret {}* %1
}

declare {}* @print_int(i64)

define {}* @print_int29(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

define i32 @main() {
  %1 = call i32 @main27()
  ret i32 0
}
