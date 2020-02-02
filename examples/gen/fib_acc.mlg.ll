; ModuleID = './examples/fib_acc.mlg'
source_filename = "./examples/fib_acc.mlg"

declare {}* @print_int(i64)

define {}* @print_int0(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

declare {}* @newline()

define {}* @newline1() {
  %1 = call {}* @newline()
  ret {}* %1
}

define {}* @fib_loop3(i64) {
  %2 = icmp sle i64 %0, 0
  %3 = alloca {}*
  br i1 %2, label %then_0, label %else_0

then_0:                                           ; preds = %1
  %4 = call i64 @fib_acc2(i64 0, i64 1, i64 1)
  %5 = call {}* @print_int0(i64 %4)
  %6 = call {}* @newline1()
  store {}* %6, {}** %3
  br label %end_0

else_0:                                           ; preds = %1
  %7 = call i64 @fib_acc2(i64 %0, i64 1, i64 1)
  %8 = call {}* @print_int0(i64 %7)
  %9 = call {}* @newline1()
  %10 = sub i64 %0, 1
  %11 = call {}* @fib_loop3(i64 %10)
  store {}* %11, {}** %3
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %12 = load {}*, {}** %3
  ret {}* %12
}

define i64 @fib_acc2(i64, i64, i64) {
  %4 = icmp sle i64 %0, 0
  %5 = alloca i64
  br i1 %4, label %then_0, label %else_0

then_0:                                           ; preds = %3
  store i64 %1, i64* %5
  br label %end_0

else_0:                                           ; preds = %3
  %6 = sub i64 %0, 1
  %7 = add i64 %1, %2
  %8 = call i64 @fib_acc2(i64 %6, i64 %2, i64 %7)
  store i64 %8, i64* %5
  br label %end_0

end_0:                                            ; preds = %else_0, %then_0
  %9 = load i64, i64* %5
  ret i64 %9
}

define i32 @main() {
  %1 = call {}* @fib_loop3(i64 30)
  ret i32 0
}
