; ModuleID = './examples/array.mlg'
source_filename = "./examples/array.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

define {} @main.66() {
  %1 = getelementptr i64, i64* null, i64 1
  %2 = ptrtoint i64* %1 to i64
  %3 = mul i64 10, %2
  %4 = call i8* @malloc_gc(i64 %3)
  %5 = bitcast i8* %4 to i64*
  %6 = getelementptr i64, i64* %5, i64 1
  %7 = load i64, i64* %6
  %8 = getelementptr i64, i64* %5, i64 2
  store i64 42, i64* %8
  %9 = getelementptr i64, i64* %5, i64 2
  %10 = load i64, i64* %9
  %11 = call {} @print_int.46(i64 %7)
  %12 = call {} @newline.49({} undef)
  %13 = call {} @print_int.46(i64 %10)
  %14 = call {} @newline.49({} undef)
  ret {} %14
}

declare {} @newline({})

define {} @newline.49({}) {
  %2 = call {} @newline({} %0)
  ret {} %2
}

declare {} @print_int(i64)

define {} @print_int.46(i64) {
  %2 = call {} @print_int(i64 %0)
  ret {} %2
}

define i32 @main() {
  call void @init_gc()
  %1 = call {} @main.66()
  ret i32 0
}
