source_filename = "./examples/fib.mlg"
; ModuleID = './examples/fib.mlg'


 


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  {}* @fib_loop4(i64 , i64 )    {
; <label>:2:
  %3 =  call ccc  i64  @fib2(i64  %1)  
  %4 =  call ccc  {}*  @print_int0(i64  %3)  
  %5 =  call ccc  {}*  @newline1()  
  %6 = icmp sle i64 %0, %1 
  %7 = alloca {}* 
  br i1 %6, label %then_0, label %else_0 
then_0:
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {}* 
  store  {}* %9, {}** %7 
  br label %end_0 
else_0:
  %10 = add   i64 %1, 1 
  %11 =  call ccc  {}*  @fib_loop4(i64  %0, i64  %10)  
  store  {}* %11, {}** %7 
  br label %end_0 
end_0:
  %12 = load  {}*, {}** %7 
  ret {}* %12 
}


define external ccc  i64 @fib2(i64 )    {
; <label>:1:
  %2 = icmp sle i64 %0, 1 
  %3 = alloca i64 
  br i1 %2, label %then_0, label %else_0 
then_0:
  store  i64 1, i64* %3 
  br label %end_0 
else_0:
  %4 = sub   i64 %0, 1 
  %5 =  call ccc  i64  @fib2(i64  %4)  
  %6 = sub   i64 %0, 2 
  %7 =  call ccc  i64  @fib2(i64  %6)  
  %8 = add   i64 %5, %7 
  store  i64 %8, i64* %3 
  br label %end_0 
end_0:
  %9 = load  i64, i64* %3 
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
  %1 =  call ccc  {}*  @fib_loop4(i64  30, i64  0)  
  ret i32 0 
}
