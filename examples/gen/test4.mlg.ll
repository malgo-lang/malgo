; ModuleID = './examples/test4.mlg'
source_filename = "./examples/test4.mlg"


 


@$globle_str_12 =  unnamed_addr  constant [6 x i8] c"hello\00"


define external ccc  i8* @f0(i64 , i8 )    {
  ret i8* getelementptr inbounds ([6 x i8], [6 x i8]* @$globle_str_12, i32 0, i32 0) 
}


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
   call ccc  void  @GC_init()  
  %1 =  call ccc  i8*  @f0(i64  42, i8  97)  
  ret i32 0 
}
