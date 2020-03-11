; ModuleID = './examples/polyfun3.mlg'
source_filename = "./examples/polyfun3.mlg"


 


declare external ccc  {}* @print_int(i64)    


define external ccc  {}* @print_int7(i64 )    {
  %2 =  call ccc  {}*  @print_int(i64  %0)  
  ret {}* %2 
}


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  i8* @id4(i8* , i8* )    {
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8* (i8*, i8*)*, i8*}* 
  %6 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 0 
  store  i8* (i8*, i8*)* @id4, i8* (i8*, i8*)** %6 
  %7 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  ret i8* %1 
}


define external ccc  i8* @f0({{i8* (i8*, i8*)*, i8*}*, i8*}* )    {
  %2 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %0, i32 0, i32 0 
  %3 = load  {i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}** %2 
  %4 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %0, i32 0, i32 1 
  %5 = load  i8*, i8** %4 
  %6 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 0 
  %7 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %6 
  %8 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 1 
  %9 = load  i8*, i8** %8 
  %10 =  call ccc  i8*  %7(i8*  %9, i8*  %5)  
  ret i8* %10 
}


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
; <label>:0:
   call ccc  void  @GC_init()  
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {}* 
  %3 = bitcast {}* %2 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8* (i8*, i8*)*, i8*}* 
  %6 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 0 
  store  i8* (i8*, i8*)* @id4, i8* (i8*, i8*)** %6 
  %7 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 1 
  store  i8* %3, i8** %7 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i8* (i8*, i8*)*, i8*}*, i64}* getelementptr inbounds ({{i8* (i8*, i8*)*, i8*}*, i64}, {{i8* (i8*, i8*)*, i8*}*, i64}* inttoptr (i32 0 to {{i8* (i8*, i8*)*, i8*}*, i64}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {{i8* (i8*, i8*)*, i8*}*, i64}* 
  %10 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i64}, {{i8* (i8*, i8*)*, i8*}*, i64}* %9, i32 0, i32 0 
  store  {i8* (i8*, i8*)*, i8*}* %5, {i8* (i8*, i8*)*, i8*}** %10 
  %11 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i64}, {{i8* (i8*, i8*)*, i8*}*, i64}* %9, i32 0, i32 1 
  store  i64 1, i64* %11 
  %12 = mul   i64 ptrtoint ({{i8* (i8*, i8*)*, i8*}*, i64}** getelementptr inbounds ({{i8* (i8*, i8*)*, i8*}*, i64}*, {{i8* (i8*, i8*)*, i8*}*, i64}** inttoptr (i32 0 to {{i8* (i8*, i8*)*, i8*}*, i64}**), i32 1) to i64), 1 
  %13 =  call ccc  i8*  @GC_malloc(i64  %12)  
  %14 = bitcast i8* %13 to {{i8* (i8*, i8*)*, i8*}*, i64}** 
  %15 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* getelementptr inbounds ({{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* inttoptr (i32 0 to {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}*), i32 1) to i64))  
  %16 = bitcast i8* %15 to {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* 
  %17 = getelementptr  {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* %16, i32 0, i32 0 
  store  {{i8* (i8*, i8*)*, i8*}*, i64}** %14, {{i8* (i8*, i8*)*, i8*}*, i64}*** %17 
  %18 = getelementptr  {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* %16, i32 0, i32 1 
  store  i64 1, i64* %18 
  %19 = alloca i64 
  store  i64 0, i64* %19 
  br label %cond_0 
cond_0:
  %20 = load  i64, i64* %19 
  %21 = icmp slt i64 %20, 1 
  br i1 %21, label %body_0, label %end_0 
body_0:
  %22 = getelementptr  {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* %16, i32 0, i32 0 
  %23 = load  {{i8* (i8*, i8*)*, i8*}*, i64}**, {{i8* (i8*, i8*)*, i8*}*, i64}*** %22 
  %24 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i64}*, {{i8* (i8*, i8*)*, i8*}*, i64}** %23, i64 %20 
  store  {{i8* (i8*, i8*)*, i8*}*, i64}* %9, {{i8* (i8*, i8*)*, i8*}*, i64}** %24 
  %25 = add   i64 %20, 1 
  store  i64 %25, i64* %19 
  br label %cond_0 
end_0:
  %26 = getelementptr  {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* %16, i32 0, i32 0 
  %27 = load  {{i8* (i8*, i8*)*, i8*}*, i64}**, {{i8* (i8*, i8*)*, i8*}*, i64}*** %26 
  %28 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i64}*, {{i8* (i8*, i8*)*, i8*}*, i64}** %27, i64 0 
  %29 = load  {{i8* (i8*, i8*)*, i8*}*, i64}*, {{i8* (i8*, i8*)*, i8*}*, i64}** %28 
  %30 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i8* (i8*, i8*)*, i8*}*, i8*}* getelementptr inbounds ({{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* inttoptr (i32 0 to {{i8* (i8*, i8*)*, i8*}*, i8*}*), i32 1) to i64))  
  %31 = bitcast i8* %30 to {{i8* (i8*, i8*)*, i8*}*, i8*}* 
  %32 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i64}, {{i8* (i8*, i8*)*, i8*}*, i64}* %29, i32 0, i32 0 
  %33 = load  {i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}** %32 
  %34 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %31, i32 0, i32 0 
  store  {i8* (i8*, i8*)*, i8*}* %33, {i8* (i8*, i8*)*, i8*}** %34 
  %35 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i64}, {{i8* (i8*, i8*)*, i8*}*, i64}* %29, i32 0, i32 1 
  %36 = load  i64, i64* %35 
  %37 = inttoptr i64 %36 to i8* 
  %38 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %31, i32 0, i32 1 
  store  i8* %37, i8** %38 
  %39 =  call ccc  i8*  @f0({{i8* (i8*, i8*)*, i8*}*, i8*}*  %31)  
  %40 = ptrtoint i8* %39 to i64 
  %41 =  call ccc  {}*  @print_int7(i64  %40)  
  ret i32 0 
}
