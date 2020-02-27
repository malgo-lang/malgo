; ModuleID = './examples/id.mlg'
source_filename = "./examples/id.mlg"


 


define external ccc  i64 @id0(i64 )    {
  ret i64 %0 
}


define external ccc  i32 @main()    {
  %1 =  call ccc  i64  @id0(i64  4)  
  ret i32 0 
}
