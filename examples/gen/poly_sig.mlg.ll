; ModuleID = './examples/poly_sig.mlg'
source_filename = "./examples/poly_sig.mlg"


 


define external ccc  i8* @id0(i8* )    {
  ret i8* %0 
}


define external ccc  i8* @const3(i8* , i8* )    {
  ret i8* %0 
}


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
   call ccc  void  @GC_init()  
  %1 = inttoptr i64 4 to i8* 
  %2 =  call ccc  i8*  @id0(i8*  %1)  
  %3 = ptrtoint i8* %1 to i64 
  %4 = ptrtoint i8* %2 to i64 
  %5 = inttoptr i64 2 to i8* 
  %6 =  call ccc  i8*  @id0(i8*  %5)  
  %7 = ptrtoint i8* %5 to i64 
  %8 = ptrtoint i8* %6 to i64 
  %9 = inttoptr i64 %4 to i8* 
  %10 = inttoptr i64 %8 to i8* 
  %11 =  call ccc  i8*  @const3(i8*  %9, i8*  %10)  
  %12 = ptrtoint i8* %9 to i64 
  %13 = ptrtoint i8* %10 to i64 
  ret i32 0 
}
