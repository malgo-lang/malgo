; ModuleID = './examples/test1.mlg'
source_filename = "./examples/test1.mlg"


 


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  i32 @main()    {
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {}* 
  ret i32 0 
}
