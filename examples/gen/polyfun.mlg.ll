; ModuleID = './examples/polyfun.mlg'
source_filename = "./examples/polyfun.mlg"


 


define external ccc  i64 @$fo156(i8* , i64 )    {
  %3 = bitcast i8* %0 to {i8* (i8*, i8*)*, i8*}* 
  %4 = inttoptr i64 %1 to i8* 
  %5 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 0 
  %6 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %5 
  %7 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 1 
  %8 = load  i8*, i8** %7 
  %9 =  call ccc  i8*  %6(i8*  %8, i8*  %4)  
  %10 = ptrtoint i8* %9 to i64 
  ret i64 %10 
}


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  {i64, {i8**, i64}*}* @$fo140(i8* , i8* )    {
  %3 = bitcast i8* %0 to {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* 
  %4 = getelementptr  {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}, {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* %3, i32 0, i32 0 
  %5 = load  {i8*, {i8**, i64}*}* (i8*, i8*)*, {i8*, {i8**, i64}*}* (i8*, i8*)** %4 
  %6 = getelementptr  {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}, {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* %3, i32 0, i32 1 
  %7 = load  i8*, i8** %6 
  %8 =  call ccc  {i8*, {i8**, i64}*}*  %5(i8*  %7, i8*  %1)  
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64, {i8**, i64}*}* getelementptr inbounds ({i64, {i8**, i64}*}, {i64, {i8**, i64}*}* inttoptr (i32 0 to {i64, {i8**, i64}*}*), i32 1) to i64))  
  %10 = bitcast i8* %9 to {i64, {i8**, i64}*}* 
  %11 = getelementptr  {i8*, {i8**, i64}*}, {i8*, {i8**, i64}*}* %8, i32 0, i32 0 
  %12 = load  i8*, i8** %11 
  %13 = ptrtoint i8* %12 to i64 
  %14 = getelementptr  {i64, {i8**, i64}*}, {i64, {i8**, i64}*}* %10, i32 0, i32 0 
  store  i64 %13, i64* %14 
  %15 = getelementptr  {i8*, {i8**, i64}*}, {i8*, {i8**, i64}*}* %8, i32 0, i32 1 
  %16 = load  {i8**, i64}*, {i8**, i64}** %15 
  %17 = getelementptr  {i64, {i8**, i64}*}, {i64, {i8**, i64}*}* %10, i32 0, i32 1 
  store  {i8**, i64}* %16, {i8**, i64}** %17 
  ret {i64, {i8**, i64}*}* %10 
}


define external ccc  i8* @$fo126(i8* , i8* )    {
  %3 = bitcast i8* %0 to {i8* (i8*, i8*)*, i8*}* 
  %4 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 0 
  %5 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %4 
  %6 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 1 
  %7 = load  i8*, i8** %6 
  %8 =  call ccc  i8*  %5(i8*  %7, i8*  %1)  
  ret i8* %8 
}


define external ccc  i64 @$fo109(i8* , i64 )    {
  %3 = bitcast i8* %0 to {i64 (i8*, i8*)*, i8*}* 
  %4 = inttoptr i64 %1 to i8* 
  %5 = getelementptr  {i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* %3, i32 0, i32 0 
  %6 = load  i64 (i8*, i8*)*, i64 (i8*, i8*)** %5 
  %7 = getelementptr  {i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* %3, i32 0, i32 1 
  %8 = load  i8*, i8** %7 
  %9 =  call ccc  i64  %6(i8*  %8, i8*  %4)  
  ret i64 %9 
}


define external ccc  i64 @$fo85(i8* , i8* )    {
  %3 = bitcast i8* %0 to {i64 (i8*, i64)*, i8*}* 
  %4 = ptrtoint i8* %1 to i64 
  %5 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %3, i32 0, i32 0 
  %6 = load  i64 (i8*, i64)*, i64 (i8*, i64)** %5 
  %7 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %3, i32 0, i32 1 
  %8 = load  i8*, i8** %7 
  %9 =  call ccc  i64  %6(i8*  %8, i64  %4)  
  ret i64 %9 
}


declare external ccc  {}* @print_int(i64)    


define external ccc  {}* @print_int10(i64 )    {
  %2 =  call ccc  {}*  @print_int(i64  %0)  
  ret {}* %2 
}


define external ccc  i8* @id0(i8* , i8* )    {
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8* (i8*, i8*)*, i8*}* 
  %6 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 0 
  store  i8* (i8*, i8*)* @id0, i8* (i8*, i8*)** %6 
  %7 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i64 (i8*, i64)*, i8*}* 
  %10 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %9, i32 0, i32 0 
  store  i64 (i8*, i64)* @addOne1, i64 (i8*, i64)** %10 
  %11 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %9, i32 0, i32 1 
  store  i8* %0, i8** %11 
  ret i8* %1 
}


define external ccc  i64 @addOne1(i8* , i64 )    {
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8* (i8*, i8*)*, i8*}* 
  %6 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 0 
  store  i8* (i8*, i8*)* @id0, i8* (i8*, i8*)** %6 
  %7 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i64 (i8*, i64)*, i8*}* 
  %10 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %9, i32 0, i32 0 
  store  i64 (i8*, i64)* @addOne1, i64 (i8*, i64)** %10 
  %11 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %9, i32 0, i32 1 
  store  i8* %0, i8** %11 
  %12 = add   i64 %1, 1 
  ret i64 %12 
}


define external ccc  {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* @$lambda36(i8* , i8* )    {
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}* getelementptr inbounds ({{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}, {{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}* 
  %6 = getelementptr  {{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}, {{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}* %5, i32 0, i32 0 
  store  {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)* @$lambda36, {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)** %6 
  %7 = getelementptr  {{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}, {{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i8*}* 
  %10 = getelementptr  {i8*}, {i8*}* %9, i32 0, i32 0 
  store  i8* %1, i8** %10 
  %11 = bitcast {i8*}* %9 to i8* 
  %12 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* getelementptr inbounds ({{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}, {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %13 = bitcast i8* %12 to {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* 
  %14 = getelementptr  {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}, {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* %13, i32 0, i32 0 
  store  {i8*, {i8**, i64}*}* (i8*, i8*)* @$lambda35, {i8*, {i8**, i64}*}* (i8*, i8*)** %14 
  %15 = getelementptr  {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}, {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* %13, i32 0, i32 1 
  store  i8* %11, i8** %15 
  ret {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* %13 
}


define external ccc  {i8*, {i8**, i64}*}* @$lambda35(i8* , i8* )    {
; <label>:2:
  %3 = bitcast i8* %0 to {i8*}* 
  %4 = getelementptr  {i8*}, {i8*}* %3, i32 0, i32 0 
  %5 = load  i8*, i8** %4 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* getelementptr inbounds ({{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}, {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* 
  %8 = getelementptr  {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}, {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* %7, i32 0, i32 0 
  store  {i8*, {i8**, i64}*}* (i8*, i8*)* @$lambda35, {i8*, {i8**, i64}*}* (i8*, i8*)** %8 
  %9 = getelementptr  {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}, {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* %7, i32 0, i32 1 
  store  i8* %0, i8** %9 
  %10 = mul   i64 ptrtoint (i8** getelementptr inbounds (i8*, i8** inttoptr (i32 0 to i8**), i32 1) to i64), 1 
  %11 =  call ccc  i8*  @GC_malloc(i64  %10)  
  %12 = bitcast i8* %11 to i8** 
  %13 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8**, i64}* getelementptr inbounds ({i8**, i64}, {i8**, i64}* inttoptr (i32 0 to {i8**, i64}*), i32 1) to i64))  
  %14 = bitcast i8* %13 to {i8**, i64}* 
  %15 = getelementptr  {i8**, i64}, {i8**, i64}* %14, i32 0, i32 0 
  store  i8** %12, i8*** %15 
  %16 = getelementptr  {i8**, i64}, {i8**, i64}* %14, i32 0, i32 1 
  store  i64 1, i64* %16 
  %17 = alloca i64 
  store  i64 0, i64* %17 
  br label %cond_0 
cond_0:
  %18 = load  i64, i64* %17 
  %19 = icmp slt i64 %18, 1 
  br i1 %19, label %body_0, label %end_0 
body_0:
  %20 = getelementptr  {i8**, i64}, {i8**, i64}* %14, i32 0, i32 0 
  %21 = load  i8**, i8*** %20 
  %22 = getelementptr  i8*, i8** %21, i64 %18 
  store  i8* %1, i8** %22 
  %23 = add   i64 %18, 1 
  store  i64 %23, i64* %17 
  br label %cond_0 
end_0:
  %24 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, {i8**, i64}*}* getelementptr inbounds ({i8*, {i8**, i64}*}, {i8*, {i8**, i64}*}* inttoptr (i32 0 to {i8*, {i8**, i64}*}*), i32 1) to i64))  
  %25 = bitcast i8* %24 to {i8*, {i8**, i64}*}* 
  %26 = getelementptr  {i8*, {i8**, i64}*}, {i8*, {i8**, i64}*}* %25, i32 0, i32 0 
  store  i8* %5, i8** %26 
  %27 = getelementptr  {i8*, {i8**, i64}*}, {i8*, {i8**, i64}*}* %25, i32 0, i32 1 
  store  {i8**, i64}* %14, {i8**, i64}** %27 
  ret {i8*, {i8**, i64}*}* %25 
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
  store  i8* (i8*, i8*)* @id0, i8* (i8*, i8*)** %6 
  %7 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 1 
  store  i8* %3, i8** %7 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {}* 
  %10 = bitcast {}* %9 to i8* 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %12 = bitcast i8* %11 to {i64 (i8*, i64)*, i8*}* 
  %13 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %12, i32 0, i32 0 
  store  i64 (i8*, i64)* @addOne1, i64 (i8*, i64)** %13 
  %14 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %12, i32 0, i32 1 
  store  i8* %10, i8** %14 
  %15 = bitcast {i64 (i8*, i64)*, i8*}* %12 to i8* 
  %16 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i8*)*, i8*}* getelementptr inbounds ({i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %17 = bitcast i8* %16 to {i64 (i8*, i8*)*, i8*}* 
  %18 = getelementptr  {i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* %17, i32 0, i32 0 
  store  i64 (i8*, i8*)* @$fo85, i64 (i8*, i8*)** %18 
  %19 = getelementptr  {i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* %17, i32 0, i32 1 
  store  i8* %15, i8** %19 
  %20 = mul   i64 ptrtoint ({i64 (i8*, i8*)*, i8*}** getelementptr inbounds ({i64 (i8*, i8*)*, i8*}*, {i64 (i8*, i8*)*, i8*}** inttoptr (i32 0 to {i64 (i8*, i8*)*, i8*}**), i32 1) to i64), 1 
  %21 =  call ccc  i8*  @GC_malloc(i64  %20)  
  %22 = bitcast i8* %21 to {i64 (i8*, i8*)*, i8*}** 
  %23 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64 (i8*, i8*)*, i8*}**, i64}* getelementptr inbounds ({{i64 (i8*, i8*)*, i8*}**, i64}, {{i64 (i8*, i8*)*, i8*}**, i64}* inttoptr (i32 0 to {{i64 (i8*, i8*)*, i8*}**, i64}*), i32 1) to i64))  
  %24 = bitcast i8* %23 to {{i64 (i8*, i8*)*, i8*}**, i64}* 
  %25 = getelementptr  {{i64 (i8*, i8*)*, i8*}**, i64}, {{i64 (i8*, i8*)*, i8*}**, i64}* %24, i32 0, i32 0 
  store  {i64 (i8*, i8*)*, i8*}** %22, {i64 (i8*, i8*)*, i8*}*** %25 
  %26 = getelementptr  {{i64 (i8*, i8*)*, i8*}**, i64}, {{i64 (i8*, i8*)*, i8*}**, i64}* %24, i32 0, i32 1 
  store  i64 1, i64* %26 
  %27 = alloca i64 
  store  i64 0, i64* %27 
  br label %cond_0 
cond_0:
  %28 = load  i64, i64* %27 
  %29 = icmp slt i64 %28, 1 
  br i1 %29, label %body_0, label %end_0 
body_0:
  %30 = getelementptr  {{i64 (i8*, i8*)*, i8*}**, i64}, {{i64 (i8*, i8*)*, i8*}**, i64}* %24, i32 0, i32 0 
  %31 = load  {i64 (i8*, i8*)*, i8*}**, {i64 (i8*, i8*)*, i8*}*** %30 
  %32 = getelementptr  {i64 (i8*, i8*)*, i8*}*, {i64 (i8*, i8*)*, i8*}** %31, i64 %28 
  store  {i64 (i8*, i8*)*, i8*}* %17, {i64 (i8*, i8*)*, i8*}** %32 
  %33 = add   i64 %28, 1 
  store  i64 %33, i64* %27 
  br label %cond_0 
end_0:
  %34 = getelementptr  {{i64 (i8*, i8*)*, i8*}**, i64}, {{i64 (i8*, i8*)*, i8*}**, i64}* %24, i32 0, i32 0 
  %35 = load  {i64 (i8*, i8*)*, i8*}**, {i64 (i8*, i8*)*, i8*}*** %34 
  %36 = getelementptr  {{i64 (i8*, i8*)*, i8*}**, i64}, {{i64 (i8*, i8*)*, i8*}**, i64}* %24, i32 0, i32 1 
  %37 = load  i64, i64* %36 
  %38 = mul   i64 ptrtoint ({i64 (i8*, i64)*, i8*}** getelementptr inbounds ({i64 (i8*, i64)*, i8*}*, {i64 (i8*, i64)*, i8*}** inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}**), i32 1) to i64), %37 
  %39 =  call ccc  i8*  @GC_malloc(i64  %38)  
  %40 = bitcast i8* %39 to {i64 (i8*, i64)*, i8*}** 
  %41 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64 (i8*, i64)*, i8*}**, i64}* getelementptr inbounds ({{i64 (i8*, i64)*, i8*}**, i64}, {{i64 (i8*, i64)*, i8*}**, i64}* inttoptr (i32 0 to {{i64 (i8*, i64)*, i8*}**, i64}*), i32 1) to i64))  
  %42 = bitcast i8* %41 to {{i64 (i8*, i64)*, i8*}**, i64}* 
  %43 = getelementptr  {{i64 (i8*, i64)*, i8*}**, i64}, {{i64 (i8*, i64)*, i8*}**, i64}* %42, i32 0, i32 0 
  store  {i64 (i8*, i64)*, i8*}** %40, {i64 (i8*, i64)*, i8*}*** %43 
  %44 = getelementptr  {{i64 (i8*, i64)*, i8*}**, i64}, {{i64 (i8*, i64)*, i8*}**, i64}* %42, i32 0, i32 1 
  store  i64 %37, i64* %44 
  %45 = getelementptr  {{i64 (i8*, i64)*, i8*}**, i64}, {{i64 (i8*, i64)*, i8*}**, i64}* %42, i32 0, i32 0 
  %46 = load  {i64 (i8*, i64)*, i8*}**, {i64 (i8*, i64)*, i8*}*** %45 
  %47 = getelementptr  {{i64 (i8*, i64)*, i8*}**, i64}, {{i64 (i8*, i64)*, i8*}**, i64}* %42, i32 0, i32 1 
  store  i64 %37, i64* %47 
  %48 = alloca i64 
  store  i64 0, i64* %48 
  br label %cond_1 
cond_1:
  %49 = load  i64, i64* %48 
  %50 = icmp slt i64 %49, %37 
  br i1 %50, label %body_1, label %end_1 
body_1:
  %51 = getelementptr  {i64 (i8*, i8*)*, i8*}*, {i64 (i8*, i8*)*, i8*}** %35, i64 %49 
  %52 = load  {i64 (i8*, i8*)*, i8*}*, {i64 (i8*, i8*)*, i8*}** %51 
  %53 = bitcast {i64 (i8*, i8*)*, i8*}* %52 to i8* 
  %54 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %55 = bitcast i8* %54 to {i64 (i8*, i64)*, i8*}* 
  %56 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %55, i32 0, i32 0 
  store  i64 (i8*, i64)* @$fo109, i64 (i8*, i64)** %56 
  %57 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %55, i32 0, i32 1 
  store  i8* %53, i8** %57 
  %58 = getelementptr  {i64 (i8*, i64)*, i8*}*, {i64 (i8*, i64)*, i8*}** %46, i64 %49 
  store  {i64 (i8*, i64)*, i8*}* %55, {i64 (i8*, i64)*, i8*}** %58 
  %59 = add   i64 %49, 1 
  store  i64 %59, i64* %48 
  br label %cond_1 
end_1:
  %60 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %61 = bitcast i8* %60 to {}* 
  %62 = bitcast {}* %61 to i8* 
  %63 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}* getelementptr inbounds ({{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}, {{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %64 = bitcast i8* %63 to {{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}* 
  %65 = getelementptr  {{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}, {{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}* %64, i32 0, i32 0 
  store  {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)* @$lambda36, {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)** %65 
  %66 = getelementptr  {{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}, {{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}* %64, i32 0, i32 1 
  store  i8* %62, i8** %66 
  %67 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 0 
  %68 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %67 
  %69 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 1 
  %70 = load  i8*, i8** %69 
  %71 = bitcast {i8* (i8*, i8*)*, i8*}* %5 to i8* 
  %72 =  call ccc  i8*  %68(i8*  %70, i8*  %71)  
  %73 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %74 = bitcast i8* %73 to {i8* (i8*, i8*)*, i8*}* 
  %75 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %74, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$fo126, i8* (i8*, i8*)** %75 
  %76 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %74, i32 0, i32 1 
  store  i8* %72, i8** %76 
  %77 = getelementptr  {{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}, {{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}* %64, i32 0, i32 0 
  %78 = load  {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)** %77 
  %79 = getelementptr  {{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}, {{{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* (i8*, i8*)*, i8*}* %64, i32 0, i32 1 
  %80 = load  i8*, i8** %79 
  %81 = inttoptr i64 1 to i8* 
  %82 =  call ccc  {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}*  %78(i8*  %80, i8*  %81)  
  %83 = bitcast {{i8*, {i8**, i64}*}* (i8*, i8*)*, i8*}* %82 to i8* 
  %84 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64, {i8**, i64}*}* (i8*, i8*)*, i8*}* getelementptr inbounds ({{i64, {i8**, i64}*}* (i8*, i8*)*, i8*}, {{i64, {i8**, i64}*}* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {{i64, {i8**, i64}*}* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %85 = bitcast i8* %84 to {{i64, {i8**, i64}*}* (i8*, i8*)*, i8*}* 
  %86 = getelementptr  {{i64, {i8**, i64}*}* (i8*, i8*)*, i8*}, {{i64, {i8**, i64}*}* (i8*, i8*)*, i8*}* %85, i32 0, i32 0 
  store  {i64, {i8**, i64}*}* (i8*, i8*)* @$fo140, {i64, {i8**, i64}*}* (i8*, i8*)** %86 
  %87 = getelementptr  {{i64, {i8**, i64}*}* (i8*, i8*)*, i8*}, {{i64, {i8**, i64}*}* (i8*, i8*)*, i8*}* %85, i32 0, i32 1 
  store  i8* %83, i8** %87 
  %88 = getelementptr  {{i64 (i8*, i64)*, i8*}**, i64}, {{i64 (i8*, i64)*, i8*}**, i64}* %42, i32 0, i32 0 
  %89 = load  {i64 (i8*, i64)*, i8*}**, {i64 (i8*, i64)*, i8*}*** %88 
  %90 = bitcast {i8* (i8*, i8*)*, i8*}* %5 to i8* 
  %91 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %92 = bitcast i8* %91 to {i64 (i8*, i64)*, i8*}* 
  %93 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %92, i32 0, i32 0 
  store  i64 (i8*, i64)* @$fo156, i64 (i8*, i64)** %93 
  %94 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %92, i32 0, i32 1 
  store  i8* %90, i8** %94 
  %95 = getelementptr  {i64 (i8*, i64)*, i8*}*, {i64 (i8*, i64)*, i8*}** %89, i64 0 
  store  {i64 (i8*, i64)*, i8*}* %92, {i64 (i8*, i64)*, i8*}** %95 
  %96 = getelementptr  {{i64 (i8*, i64)*, i8*}**, i64}, {{i64 (i8*, i64)*, i8*}**, i64}* %42, i32 0, i32 0 
  %97 = load  {i64 (i8*, i64)*, i8*}**, {i64 (i8*, i64)*, i8*}*** %96 
  %98 = getelementptr  {i64 (i8*, i64)*, i8*}*, {i64 (i8*, i64)*, i8*}** %97, i64 0 
  %99 = load  {i64 (i8*, i64)*, i8*}*, {i64 (i8*, i64)*, i8*}** %98 
  %100 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %99, i32 0, i32 0 
  %101 = load  i64 (i8*, i64)*, i64 (i8*, i64)** %100 
  %102 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %99, i32 0, i32 1 
  %103 = load  i8*, i8** %102 
  %104 =  call ccc  i64  %101(i8*  %103, i64  1)  
  %105 =  call ccc  {}*  @print_int10(i64  %104)  
  ret i32 0 
}
