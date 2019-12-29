; ModuleID = './examples/id.mlg'
source_filename = "./examples/id.mlg"

define i32 @main8() {
  %1 = call i64 @id9(i64 4)
  ret i32 0
}

define i64 @id9(i64) {
  ret i64 %0
}

define i32 @main() {
  %1 = call i32 @main8()
  ret i32 0
}
