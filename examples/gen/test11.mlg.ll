source_filename = "./examples/test11.mlg"
; ModuleID = './examples/test11.mlg'


 


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
; <label>:0:
   call ccc  void  @GC_init()  
  %1 = alloca i1 
  br i1 1, label %then_0, label %else_0 
then_0:
  store  i1 0, i1* %1 
  br label %end_2 
else_0:
  %2 = alloca i1 
  br i1 0, label %then_1, label %else_2 
then_1:
  %3 = alloca i1 
  br i1 0, label %then_2, label %else_1 
then_2:
  store  i1 1, i1* %3 
  br label %end_0 
else_1:
  store  i1 0, i1* %3 
  br label %end_0 
end_0:
  %4 = load  i1, i1* %3 
  store  i1 %4, i1* %2 
  br label %end_1 
else_2:
  store  i1 1, i1* %2 
  br label %end_1 
end_1:
  %5 = load  i1, i1* %2 
  store  i1 %5, i1* %1 
  br label %end_2 
end_2:
  %6 = load  i1, i1* %1 
  ret i32 0 
}
