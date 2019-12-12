; ModuleID = './examples/id.mlg'
source_filename = "./examples/id.mlg"

define i64 @id.5(i64) {
  ret i64 %0
}

define i32 @main() {
  %1 = call i64 @id.5(i64 4)
  ret i32 0
}
