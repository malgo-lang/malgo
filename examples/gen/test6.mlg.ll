; ModuleID = './examples/test6.mlg'
source_filename = "./examples/test6.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

declare i64 @add_i64(i64, i64)

define {} @main.56() {
  %1 = call i64 @id.44(i64 42)
  %2 = call i64 @id.44(i64 %1)
  %3 = call i64 @add_i64(i64 %2, i64 42)
  %4 = call i64 @id.44(i64 42)
  %5 = call i64 @add_i64(i64 %3, i64 %4)
  %6 = call {} @print_int.46(i64 %5)
  ret {} %6
}

declare {} @print_int(i64)

define {} @print_int.46(i64) {
  %2 = call {} @print_int(i64 %0)
  ret {} %2
}

define i64 @id.44(i64) {
  ret i64 %0
}

define i32 @main() {
  call void @init_gc()
  %1 = call {} @main.56()
  ret i32 0
}
