; ModuleID = './examples/test.mlg'
source_filename = "./examples/test.mlg"

define i64 @answer4() {
  ret i64 42
}

declare i8* @GC_malloc(i64)

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  ret i32 0
}
