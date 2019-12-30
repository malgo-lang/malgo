; ModuleID = './examples/test1.mlg'
source_filename = "./examples/test1.mlg"

declare i8* @GC_malloc(i64)

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  ret i32 0
}
