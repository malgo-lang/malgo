; ModuleID = './examples/hello.mlg'
source_filename = "./examples/hello.mlg"


 


declare external ccc  {}* @println(i8*)    


define external ccc  {}* @println0(i8* )    {
  %2 =  call ccc  {}*  @println(i8*  %0)  
  ret {}* %2 
}


@$globle_str_7 =  unnamed_addr  constant [13 x i8] c"Hello, world\00"


define external ccc  i32 @main()    {
  %1 =  call ccc  {}*  @println0(i8*  getelementptr inbounds ([13 x i8], [13 x i8]* @$globle_str_7, i32 0, i32 0))  
  ret i32 0 
}
