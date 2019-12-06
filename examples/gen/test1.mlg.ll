; ModuleID = './examples/test1.mlg'
source_filename = "./examples/test1.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

define {} @main.0() {
  ret {} undef
}

define i32 @main() {
  call void @init_gc()
  %1 = call {} @main.0()
  ret i32 0
}
