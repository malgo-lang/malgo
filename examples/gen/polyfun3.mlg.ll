; ModuleID = './examples/polyfun3.mlg'
source_filename = "./examples/polyfun3.mlg"


 


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  i8* @$fo56(i8* , i8* )    {
  %3 = bitcast i8* %0 to {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* 
  %4 = bitcast i8* %1 to {i8*, i8*}* 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i8* (i8*, i8*)*, i8*}*, i8*}* getelementptr inbounds ({{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* inttoptr (i32 0 to {{i8* (i8*, i8*)*, i8*}*, i8*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {{i8* (i8*, i8*)*, i8*}*, i8*}* 
  %7 = getelementptr  {i8*, i8*}, {i8*, i8*}* %4, i32 0, i32 0 
  %8 = load  i8*, i8** %7 
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %10 = bitcast i8* %9 to {i8* (i8*, i8*)*, i8*}* 
  %11 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %10, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$fo63, i8* (i8*, i8*)** %11 
  %12 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %10, i32 0, i32 1 
  store  i8* %8, i8** %12 
  %13 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %6, i32 0, i32 0 
  store  {i8* (i8*, i8*)*, i8*}* %10, {i8* (i8*, i8*)*, i8*}** %13 
  %14 = getelementptr  {i8*, i8*}, {i8*, i8*}* %4, i32 0, i32 1 
  %15 = load  i8*, i8** %14 
  %16 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %6, i32 0, i32 1 
  store  i8* %15, i8** %16 
  %17 = getelementptr  {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* %3, i32 0, i32 0 
  %18 = load  i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)** %17 
  %19 = getelementptr  {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* %3, i32 0, i32 1 
  %20 = load  i8*, i8** %19 
  %21 =  call ccc  i8*  %18(i8*  %20, {{i8* (i8*, i8*)*, i8*}*, i8*}*  %6)  
  ret i8* %21 
}


define external ccc  i8* @$fo63(i8* , i8* )    {
  %3 = bitcast i8* %0 to {i8* (i8*, i8*)*, i8*}* 
  %4 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 0 
  %5 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %4 
  %6 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 1 
  %7 = load  i8*, i8** %6 
  %8 =  call ccc  i8*  %5(i8*  %7, i8*  %1)  
  ret i8* %8 
}


declare external ccc  {}* @print_int(i64)    


define external ccc  {}* @print_int7(i64 )    {
  %2 =  call ccc  {}*  @print_int(i64  %0)  
  ret {}* %2 
}


define external ccc  i8* @id1(i8* , i8* )    {
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* getelementptr inbounds ({i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* 
  %6 = getelementptr  {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* %5, i32 0, i32 0 
  store  i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)* @f0, i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)** %6 
  %7 = getelementptr  {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i8* (i8*, i8*)*, i8*}* 
  %10 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %9, i32 0, i32 0 
  store  i8* (i8*, i8*)* @id1, i8* (i8*, i8*)** %10 
  %11 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %9, i32 0, i32 1 
  store  i8* %0, i8** %11 
  ret i8* %1 
}


define external ccc  i8* @f0(i8* , {{i8* (i8*, i8*)*, i8*}*, i8*}* )    {
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* getelementptr inbounds ({i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* 
  %6 = getelementptr  {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* %5, i32 0, i32 0 
  store  i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)* @f0, i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)** %6 
  %7 = getelementptr  {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i8* (i8*, i8*)*, i8*}* 
  %10 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %9, i32 0, i32 0 
  store  i8* (i8*, i8*)* @id1, i8* (i8*, i8*)** %10 
  %11 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %9, i32 0, i32 1 
  store  i8* %0, i8** %11 
  %12 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %1, i32 0, i32 0 
  %13 = load  {i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}** %12 
  %14 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %1, i32 0, i32 1 
  %15 = load  i8*, i8** %14 
  %16 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %13, i32 0, i32 0 
  %17 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %16 
  %18 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %13, i32 0, i32 1 
  %19 = load  i8*, i8** %18 
  %20 =  call ccc  i8*  %17(i8*  %19, i8*  %15)  
  ret i8* %20 
}


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
; <label>:0:
   call ccc  void  @GC_init()  
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {}* 
  %3 = bitcast {}* %2 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* getelementptr inbounds ({i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* 
  %6 = getelementptr  {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* %5, i32 0, i32 0 
  store  i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)* @f0, i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)** %6 
  %7 = getelementptr  {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* %5, i32 0, i32 1 
  store  i8* %3, i8** %7 
  %8 = bitcast {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* %5 to i8* 
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %10 = bitcast i8* %9 to {i8* (i8*, i8*)*, i8*}* 
  %11 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %10, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$fo56, i8* (i8*, i8*)** %11 
  %12 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %10, i32 0, i32 1 
  store  i8* %8, i8** %12 
  %13 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %14 = bitcast i8* %13 to {}* 
  %15 = bitcast {}* %14 to i8* 
  %16 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %17 = bitcast i8* %16 to {i8* (i8*, i8*)*, i8*}* 
  %18 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %17, i32 0, i32 0 
  store  i8* (i8*, i8*)* @id1, i8* (i8*, i8*)** %18 
  %19 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %17, i32 0, i32 1 
  store  i8* %15, i8** %19 
  %20 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i8* (i8*, i8*)*, i8*}*, i64}* getelementptr inbounds ({{i8* (i8*, i8*)*, i8*}*, i64}, {{i8* (i8*, i8*)*, i8*}*, i64}* inttoptr (i32 0 to {{i8* (i8*, i8*)*, i8*}*, i64}*), i32 1) to i64))  
  %21 = bitcast i8* %20 to {{i8* (i8*, i8*)*, i8*}*, i64}* 
  %22 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i64}, {{i8* (i8*, i8*)*, i8*}*, i64}* %21, i32 0, i32 0 
  store  {i8* (i8*, i8*)*, i8*}* %17, {i8* (i8*, i8*)*, i8*}** %22 
  %23 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i64}, {{i8* (i8*, i8*)*, i8*}*, i64}* %21, i32 0, i32 1 
  store  i64 1, i64* %23 
  %24 = mul   i64 ptrtoint ({{i8* (i8*, i8*)*, i8*}*, i64}** getelementptr inbounds ({{i8* (i8*, i8*)*, i8*}*, i64}*, {{i8* (i8*, i8*)*, i8*}*, i64}** inttoptr (i32 0 to {{i8* (i8*, i8*)*, i8*}*, i64}**), i32 1) to i64), 1 
  %25 =  call ccc  i8*  @GC_malloc(i64  %24)  
  %26 = bitcast i8* %25 to {{i8* (i8*, i8*)*, i8*}*, i64}** 
  %27 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* getelementptr inbounds ({{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* inttoptr (i32 0 to {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}*), i32 1) to i64))  
  %28 = bitcast i8* %27 to {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* 
  %29 = getelementptr  {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* %28, i32 0, i32 0 
  store  {{i8* (i8*, i8*)*, i8*}*, i64}** %26, {{i8* (i8*, i8*)*, i8*}*, i64}*** %29 
  %30 = getelementptr  {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* %28, i32 0, i32 1 
  store  i64 1, i64* %30 
  %31 = alloca i64 
  store  i64 0, i64* %31 
  br label %cond_0 
cond_0:
  %32 = load  i64, i64* %31 
  %33 = icmp slt i64 %32, 1 
  br i1 %33, label %body_0, label %end_0 
body_0:
  %34 = getelementptr  {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* %28, i32 0, i32 0 
  %35 = load  {{i8* (i8*, i8*)*, i8*}*, i64}**, {{i8* (i8*, i8*)*, i8*}*, i64}*** %34 
  %36 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i64}*, {{i8* (i8*, i8*)*, i8*}*, i64}** %35, i64 %32 
  store  {{i8* (i8*, i8*)*, i8*}*, i64}* %21, {{i8* (i8*, i8*)*, i8*}*, i64}** %36 
  %37 = add   i64 %32, 1 
  store  i64 %37, i64* %31 
  br label %cond_0 
end_0:
  %38 = getelementptr  {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* %28, i32 0, i32 0 
  %39 = load  {{i8* (i8*, i8*)*, i8*}*, i64}**, {{i8* (i8*, i8*)*, i8*}*, i64}*** %38 
  %40 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i64}*, {{i8* (i8*, i8*)*, i8*}*, i64}** %39, i64 0 
  %41 = load  {{i8* (i8*, i8*)*, i8*}*, i64}*, {{i8* (i8*, i8*)*, i8*}*, i64}** %40 
  %42 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %10, i32 0, i32 0 
  %43 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %42 
  %44 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %10, i32 0, i32 1 
  %45 = load  i8*, i8** %44 
  %46 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %47 = bitcast i8* %46 to {i8*, i8*}* 
  %48 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i64}, {{i8* (i8*, i8*)*, i8*}*, i64}* %41, i32 0, i32 0 
  %49 = load  {i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}** %48 
  %50 = bitcast {i8* (i8*, i8*)*, i8*}* %49 to i8* 
  %51 = getelementptr  {i8*, i8*}, {i8*, i8*}* %47, i32 0, i32 0 
  store  i8* %50, i8** %51 
  %52 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i64}, {{i8* (i8*, i8*)*, i8*}*, i64}* %41, i32 0, i32 1 
  %53 = load  i64, i64* %52 
  %54 = inttoptr i64 %53 to i8* 
  %55 = getelementptr  {i8*, i8*}, {i8*, i8*}* %47, i32 0, i32 1 
  store  i8* %54, i8** %55 
  %56 = bitcast {i8*, i8*}* %47 to i8* 
  %57 =  call ccc  i8*  %43(i8*  %45, i8*  %56)  
  %58 = ptrtoint i8* %57 to i64 
  %59 =  call ccc  {}*  @print_int7(i64  %58)  
  ret i32 0 
}
