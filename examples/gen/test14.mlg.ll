; ModuleID = './examples/test14.mlg'
source_filename = "./examples/test14.mlg"


 


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  {}* @void0(i1 )    {
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {}* 
  ret {}* %3 
}


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
   call ccc  void  @GC_init()  
  %1 = icmp eq i64 1, 1 
  %2 =  call ccc  {}*  @void0(i1  %1)  
  %3 = fcmp oeq double 1.100000e0, 1.100000e0 
  %4 =  call ccc  {}*  @void0(i1  %3)  
  %5 = icmp eq i8 97, 97 
  %6 =  call ccc  {}*  @void0(i1  %5)  
  %7 = icmp eq i1 1, 1 
  %8 =  call ccc  {}*  @void0(i1  %7)  
  ret i32 0 
}
