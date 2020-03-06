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
  %3 = ptrtoint i8* %2 to i64 
  %4 = inttoptr i64 2 to i8* 
  %5 =  call ccc  i8*  @id0(i8*  %4)  
  %6 = ptrtoint i8* %5 to i64 
  %7 = inttoptr i64 %3 to i8* 
  %8 = inttoptr i64 %6 to i8* 
  %9 =  call ccc  i8*  @const3(i8*  %7, i8*  %8)  
  ret i32 0 
}
