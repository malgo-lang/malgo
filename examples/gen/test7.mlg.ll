source_filename = "./examples/test7.mlg"
; ModuleID = './examples/test7.mlg'


 


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  i64 @f1(i8* , i64 )    {
; <label>:2:
  %3 = bitcast i8* %0 to {i64}* 
  %4 = getelementptr  {i64}, {i64}* %3, i32 0, i32 0 
  %5 = load  i64, i64* %4 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i64 (i8*, i64)*, i8*}* 
  %8 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 0 
  store  i64 (i8*, i64)* @f1, i64 (i8*, i64)** %8 
  %9 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 1 
  store  i8* %0, i8** %9 
  %10 = icmp sle i64 %1, 1 
  %11 = alloca i64 
  br i1 %10, label %then_0, label %else_0 
then_0:
  store  i64 %5, i64* %11 
  br label %end_0 
else_0:
  %12 = sub   i64 %1, 1 
  %13 =  call ccc  i64  @f1(i8*  %0, i64  %12)  
  store  i64 %13, i64* %11 
  br label %end_0 
end_0:
  %14 = load  i64, i64* %11 
  ret i64 %14 
}


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
   call ccc  void  @GC_init()  
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64}* getelementptr inbounds ({i64}, {i64}* inttoptr (i32 0 to {i64}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {i64}* 
  %3 = getelementptr  {i64}, {i64}* %2, i32 0, i32 0 
  store  i64 42, i64* %3 
  %4 = bitcast {i64}* %2 to i8* 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i64 (i8*, i64)*, i8*}* 
  %7 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %6, i32 0, i32 0 
  store  i64 (i8*, i64)* @f1, i64 (i8*, i64)** %7 
  %8 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %6, i32 0, i32 1 
  store  i8* %4, i8** %8 
  %9 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %6, i32 0, i32 0 
  %10 = load  i64 (i8*, i64)*, i64 (i8*, i64)** %9 
  %11 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %6, i32 0, i32 1 
  %12 = load  i8*, i8** %11 
  %13 =  call ccc  i64  %10(i8*  %12, i64  3)  
  ret i32 0 
}
