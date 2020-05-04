source_filename = "./examples/id.mlg"
; ModuleID = './examples/id.mlg'


 


define external ccc  i64 @id0(i64 )    {
  ret i64 %0 
}


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
   call ccc  void  @GC_init()  
  %1 =  call ccc  i64  @id0(i64  4)  
  ret i32 0 
}
