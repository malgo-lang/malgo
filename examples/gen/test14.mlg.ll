; ModuleID = './examples/test14.mlg'
source_filename = "./examples/test14.mlg"

declare i8* @GC_malloc(i64)

define {}* @void.5(i1) {
  %2 = call i8* @GC_malloc(i64 0)
  %3 = bitcast i8* %2 to {}*
  ret {}* %3
}

define i32 @main() {
  %1 = icmp eq i64 1, 1
  %2 = call {}* @void.5(i1 %1)
  %3 = call {}* @void.5(i1 true)
  %4 = icmp eq i8 97, 97
  %5 = call {}* @void.5(i1 %4)
  %6 = icmp eq i1 true, true
  %7 = call {}* @void.5(i1 %6)
  ret i32 0
}
