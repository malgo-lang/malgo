; ModuleID = './examples/test11.mlg'
source_filename = "./examples/test11.mlg"

define i32 @main() {
  %1 = alloca i1
  br i1 true, label %then_0, label %else_0

then_0:                                           ; preds = %0
  store i1 false, i1* %1
  br label %end_2

else_0:                                           ; preds = %0
  %2 = alloca i1
  br i1 false, label %then_1, label %else_2

then_1:                                           ; preds = %else_0
  %3 = alloca i1
  br i1 false, label %then_2, label %else_1

then_2:                                           ; preds = %then_1
  store i1 true, i1* %3
  br label %end_0

else_1:                                           ; preds = %then_1
  store i1 false, i1* %3
  br label %end_0

end_0:                                            ; preds = %else_1, %then_2
  %4 = load i1, i1* %3
  store i1 %4, i1* %2
  br label %end_1

else_2:                                           ; preds = %else_0
  store i1 true, i1* %2
  br label %end_1

end_1:                                            ; preds = %else_2, %end_0
  %5 = load i1, i1* %2
  store i1 %5, i1* %1
  br label %end_2

end_2:                                            ; preds = %end_1, %then_0
  %6 = load i1, i1* %1
  ret i32 0
}
