source_filename = "./examples/test6.mlg"
; ModuleID = './examples/test6.mlg'


 


declare external ccc  {}* @print_int(i64)    


define external ccc  {}* @print_int5(i64 )    {
  %2 =  call ccc  {}*  @print_int(i64  %0)  
  ret {}* %2 
}


define external ccc  i64 @id3(i64 )    {
  ret i64 %0 
}


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
   call ccc  void  @GC_init()  
  %1 =  call ccc  i64  @id3(i64  42)  
  %2 =  call ccc  i64  @id3(i64  %1)  
  %3 = add   i64 %2, 42 
  %4 =  call ccc  i64  @id3(i64  42)  
  %5 = add   i64 %3, %4 
  %6 =  call ccc  {}*  @print_int5(i64  %5)  
  ret i32 0 
}
