; ModuleID = './examples/test.mlg'
source_filename = "./examples/test.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

define {} @main.8() {
  ret {} undef
}

define i64 @answer.6({}) {
  ret i64 42
}

define i32 @main() {
  call void @init_gc()
  %1 = call {} @main.8()
  ret i32 0
}
