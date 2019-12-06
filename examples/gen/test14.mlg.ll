; ModuleID = './examples/test14.mlg'
source_filename = "./examples/test14.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

declare i1 @eq_i64(i64, i64)

declare i1 @eq_double(double, double)

declare i1 @eq_i8(i8, i8)

declare i1 @eq_i1(i1, i1)

define {} @main.73() {
  %1 = call i1 @eq_i64(i64 1, i64 1)
  %2 = call {} @void.52(i1 %1)
  %3 = call i1 @eq_double(double 1.100000e+00, double 1.100000e+00)
  %4 = call {} @void.52(i1 %3)
  %5 = call i1 @eq_i8(i8 97, i8 97)
  %6 = call {} @void.52(i1 %5)
  %7 = call i1 @eq_i1(i1 true, i1 true)
  %8 = call {} @void.52(i1 %7)
  ret {} %8
}

define {} @void.52(i1) {
  ret {} undef
}

define i32 @main() {
  call void @init_gc()
  %1 = call {} @main.73()
  ret i32 0
}
