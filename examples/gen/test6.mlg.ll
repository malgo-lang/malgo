; ModuleID = './examples/test6.mlg'
source_filename = "./examples/test6.mlg"

define i32 @main26() {
  %1 = call i64 @id28(i64 42)
  %2 = call i64 @id28(i64 %1)
  %3 = add i64 %2, 42
  %4 = call i64 @id28(i64 42)
  %5 = add i64 %3, %4
  %6 = call {}* @print_int27(i64 %5)
  ret i32 0
}

declare {}* @print_int(i64)

define {}* @print_int27(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

define i64 @id28(i64) {
  ret i64 %0
}

define i32 @main() {
  %1 = call i32 @main26()
  ret i32 0
}
