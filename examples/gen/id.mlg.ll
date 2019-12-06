; ModuleID = './examples/id.mlg'
source_filename = "./examples/id.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

define i64 @main.12() {
  %1 = call i64 @id.9(i64 4)
  ret i64 %1
}

define i64 @id.9(i64) {
  ret i64 %0
}

define i32 @main() {
  call void @init_gc()
  %1 = call i64 @main.12()
  ret i32 0
}
