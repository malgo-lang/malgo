; ModuleID = './examples/array.mlg'
source_filename = "./examples/array.mlg"

declare {}* @newline({}*)

define {}* @newline.6({}*) {
  %2 = call {}* @newline({}* %0)
  ret {}* %2
}

declare {}* @print_int(i64)

define {}* @print_int.5(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

declare i8* @GC_malloc(i64)

define i32 @main() {
  %1 = mul i64 10, ptrtoint (i64* getelementptr inbounds (i64, i64* null, i32 1) to i64)
  %2 = call i8* @GC_malloc(i64 %1)
  %3 = bitcast i8* %2 to i64*
  %4 = getelementptr i64, i64* %3, i64 1
  %5 = load i64, i64* %4
  %6 = getelementptr i64, i64* %3, i64 2
  store i64 42, i64* %6
  %7 = getelementptr i64, i64* %3, i64 2
  %8 = load i64, i64* %7
  %9 = call {}* @print_int.5(i64 %5)
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = call {}* @newline.6({}* %11)
  %13 = call {}* @print_int.5(i64 %8)
  %14 = call i8* @GC_malloc(i64 0)
  %15 = bitcast i8* %14 to {}*
  %16 = call {}* @newline.6({}* %15)
  ret i32 0
}
