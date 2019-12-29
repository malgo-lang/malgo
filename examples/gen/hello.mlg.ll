; ModuleID = './examples/hello.mlg'
source_filename = "./examples/hello.mlg"

@0 = global [13 x i8] c"Hello, world\00"

define i32 @main5() {
  %1 = bitcast [13 x i8]* @0 to i8*
  %2 = call {}* @println6(i8* %1)
  ret i32 0
}

declare {}* @println(i8*)

define {}* @println6(i8*) {
  %2 = call {}* @println(i8* %0)
  ret {}* %2
}

define i32 @main() {
  %1 = call i32 @main5()
  ret i32 0
}
