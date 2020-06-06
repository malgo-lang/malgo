source_filename = "./examples/fib_acc.mlg"
; ModuleID = './examples/fib_acc.mlg'


 


define external ccc  {}* @fib_loop6(i64 )    {
; <label>:1:
  %2 = icmp sle i64 %0, 0 
  %3 = alloca {}* 
  br i1 %2, label %then_0, label %else_0 
then_0:
  %4 =  call ccc  i64  @fib_acc2(i64  0, i64  1, i64  1)  
  %5 =  call ccc  {}*  @print_int0(i64  %4)  
  %6 =  call ccc  {}*  @newline1()  
  store  {}* %6, {}** %3 
  br label %end_0 
else_0:
  %7 =  call ccc  i64  @fib_acc2(i64  %0, i64  1, i64  1)  
  %8 =  call ccc  {}*  @print_int0(i64  %7)  
  %9 =  call ccc  {}*  @newline1()  
  %10 = sub   i64 %0, 1 
  %11 =  call ccc  {}*  @fib_loop6(i64  %10)  
  store  {}* %11, {}** %3 
  br label %end_0 
end_0:
  %12 = load  {}*, {}** %3 
  ret {}* %12 
}


define external ccc  i64 @fib_acc2(i64 , i64 , i64 )    {
; <label>:3:
  %4 = icmp sle i64 %0, 0 
  %5 = alloca i64 
  br i1 %4, label %then_0, label %else_0 
then_0:
  store  i64 %1, i64* %5 
  br label %end_0 
else_0:
  %6 = sub   i64 %0, 1 
  %7 = add   i64 %1, %2 
  %8 =  call ccc  i64  @fib_acc2(i64  %6, i64  %2, i64  %7)  
  store  i64 %8, i64* %5 
  br label %end_0 
end_0:
  %9 = load  i64, i64* %5 
  ret i64 %9 
}


declare external ccc  {}* @newline()    


define external ccc  {}* @newline1()    {
  %1 =  call ccc  {}*  @newline()  
  ret {}* %1 
}


declare external ccc  {}* @print_int(i64)    


define external ccc  {}* @print_int0(i64 )    {
  %2 =  call ccc  {}*  @print_int(i64  %0)  
  ret {}* %2 
}


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
   call ccc  void  @GC_init()  
  %1 =  call ccc  {}*  @fib_loop6(i64  30)  
  ret i32 0 
}