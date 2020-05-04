source_filename = "./examples/test1.mlg"
; ModuleID = './examples/test1.mlg'


 


define external ccc  i64 @answer0()    {
  ret i64 42 
}


declare external ccc  void @GC_init()    


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  i32 @main()    {
   call ccc  void  @GC_init()  
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {}* 
  ret i32 0 
}
