source_filename = "./examples/polyfun1.mlg"
; ModuleID = './examples/polyfun1.mlg'


 


define external ccc  i64 @$f95(i8* , i8* )    {
  %3 = bitcast i8* %0 to {i8* (i8*, i8*)*, i8*}* 
  %4 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 0 
  %5 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %4 
  %6 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 1 
  %7 = load  i8*, i8** %6 
  %8 =  call ccc  i8*  %5(i8*  %7, i8*  %1)  
  %9 = ptrtoint i8* %8 to i64 
  ret i64 %9 
}


define external ccc  i8* @$f80(i8* , i8* )    {
  %3 = bitcast i8* %0 to {i64 (i8*, i8*)*, i8*}* 
  %4 = getelementptr  {i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* %3, i32 0, i32 0 
  %5 = load  i64 (i8*, i8*)*, i64 (i8*, i8*)** %4 
  %6 = getelementptr  {i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* %3, i32 0, i32 1 
  %7 = load  i8*, i8** %6 
  %8 =  call ccc  i64  %5(i8*  %7, i8*  %1)  
  %9 = inttoptr i64 %8 to i8* 
  ret i8* %9 
}


define external ccc  i64 @$f55(i8* , i8* )    {
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


define external ccc  {}* @print_int7(i64 )    {
  %2 =  call ccc  {}*  @print_int(i64  %0)  
  ret {}* %2 
}


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  i64 @succ4(i8* , i64 )    {
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i64 (i8*, i64)*, i8*}* 
  %6 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %5, i32 0, i32 0 
  store  i64 (i8*, i64)* @succ4, i64 (i8*, i64)** %6 
  %7 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  %8 = add   i64 %1, 1 
  ret i64 %8 
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
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i64 (i8*, i64)*, i8*}* 
  %6 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %5, i32 0, i32 0 
  store  i64 (i8*, i64)* @succ4, i64 (i8*, i64)** %6 
  %7 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %5, i32 0, i32 1 
  store  i8* %3, i8** %7 
  %8 = bitcast {i64 (i8*, i64)*, i8*}* %5 to i8* 
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i8*)*, i8*}* getelementptr inbounds ({i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %10 = bitcast i8* %9 to {i64 (i8*, i8*)*, i8*}* 
  %11 = getelementptr  {i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* %10, i32 0, i32 0 
  store  i64 (i8*, i8*)* @$f55, i64 (i8*, i8*)** %11 
  %12 = getelementptr  {i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* %10, i32 0, i32 1 
  store  i8* %8, i8** %12 
  %13 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64 (i8*, i8*)*, i8*}*, i64}* getelementptr inbounds ({{i64 (i8*, i8*)*, i8*}*, i64}, {{i64 (i8*, i8*)*, i8*}*, i64}* inttoptr (i32 0 to {{i64 (i8*, i8*)*, i8*}*, i64}*), i32 1) to i64))  
  %14 = bitcast i8* %13 to {{i64 (i8*, i8*)*, i8*}*, i64}* 
  %15 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}, {{i64 (i8*, i8*)*, i8*}*, i64}* %14, i32 0, i32 0 
  store  {i64 (i8*, i8*)*, i8*}* %10, {i64 (i8*, i8*)*, i8*}** %15 
  %16 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}, {{i64 (i8*, i8*)*, i8*}*, i64}* %14, i32 0, i32 1 
  store  i64 1, i64* %16 
  %17 = mul   i64 ptrtoint ({{i64 (i8*, i8*)*, i8*}*, i64}** getelementptr inbounds ({{i64 (i8*, i8*)*, i8*}*, i64}*, {{i64 (i8*, i8*)*, i8*}*, i64}** inttoptr (i32 0 to {{i64 (i8*, i8*)*, i8*}*, i64}**), i32 1) to i64), 1 
  %18 =  call ccc  i8*  @GC_malloc(i64  %17)  
  %19 = bitcast i8* %18 to {{i64 (i8*, i8*)*, i8*}*, i64}** 
  %20 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* getelementptr inbounds ({{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* inttoptr (i32 0 to {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}*), i32 1) to i64))  
  %21 = bitcast i8* %20 to {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* 
  %22 = getelementptr  {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* %21, i32 0, i32 0 
  store  {{i64 (i8*, i8*)*, i8*}*, i64}** %19, {{i64 (i8*, i8*)*, i8*}*, i64}*** %22 
  %23 = getelementptr  {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* %21, i32 0, i32 1 
  store  i64 1, i64* %23 
  %24 = alloca i64 
  store  i64 0, i64* %24 
  br label %cond_0 
cond_0:
  %25 = load  i64, i64* %24 
  %26 = icmp slt i64 %25, 1 
  br i1 %26, label %body_0, label %end_0 
body_0:
  %27 = getelementptr  {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* %21, i32 0, i32 0 
  %28 = load  {{i64 (i8*, i8*)*, i8*}*, i64}**, {{i64 (i8*, i8*)*, i8*}*, i64}*** %27 
  %29 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}*, {{i64 (i8*, i8*)*, i8*}*, i64}** %28, i64 %25 
  store  {{i64 (i8*, i8*)*, i8*}*, i64}* %14, {{i64 (i8*, i8*)*, i8*}*, i64}** %29 
  %30 = add   i64 %25, 1 
  store  i64 %30, i64* %24 
  br label %cond_0 
end_0:
  %31 = getelementptr  {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* %21, i32 0, i32 0 
  %32 = load  {{i64 (i8*, i8*)*, i8*}*, i64}**, {{i64 (i8*, i8*)*, i8*}*, i64}*** %31 
  %33 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}*, {{i64 (i8*, i8*)*, i8*}*, i64}** %32, i64 0 
  %34 = load  {{i64 (i8*, i8*)*, i8*}*, i64}*, {{i64 (i8*, i8*)*, i8*}*, i64}** %33 
  %35 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i8* (i8*, i8*)*, i8*}*, i8*}* getelementptr inbounds ({{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* inttoptr (i32 0 to {{i8* (i8*, i8*)*, i8*}*, i8*}*), i32 1) to i64))  
  %36 = bitcast i8* %35 to {{i8* (i8*, i8*)*, i8*}*, i8*}* 
  %37 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}, {{i64 (i8*, i8*)*, i8*}*, i64}* %34, i32 0, i32 0 
  %38 = load  {i64 (i8*, i8*)*, i8*}*, {i64 (i8*, i8*)*, i8*}** %37 
  %39 = bitcast {i64 (i8*, i8*)*, i8*}* %38 to i8* 
  %40 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %41 = bitcast i8* %40 to {i8* (i8*, i8*)*, i8*}* 
  %42 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %41, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$f80, i8* (i8*, i8*)** %42 
  %43 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %41, i32 0, i32 1 
  store  i8* %39, i8** %43 
  %44 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %36, i32 0, i32 0 
  store  {i8* (i8*, i8*)*, i8*}* %41, {i8* (i8*, i8*)*, i8*}** %44 
  %45 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}, {{i64 (i8*, i8*)*, i8*}*, i64}* %34, i32 0, i32 1 
  %46 = load  i64, i64* %45 
  %47 = inttoptr i64 %46 to i8* 
  %48 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %36, i32 0, i32 1 
  store  i8* %47, i8** %48 
  %49 =  call ccc  i8*  @f0({{i8* (i8*, i8*)*, i8*}*, i8*}*  %36)  
  %50 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64 (i8*, i8*)*, i8*}*, i64}* getelementptr inbounds ({{i64 (i8*, i8*)*, i8*}*, i64}, {{i64 (i8*, i8*)*, i8*}*, i64}* inttoptr (i32 0 to {{i64 (i8*, i8*)*, i8*}*, i64}*), i32 1) to i64))  
  %51 = bitcast i8* %50 to {{i64 (i8*, i8*)*, i8*}*, i64}* 
  %52 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %36, i32 0, i32 0 
  %53 = load  {i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}** %52 
  %54 = bitcast {i8* (i8*, i8*)*, i8*}* %53 to i8* 
  %55 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i8*)*, i8*}* getelementptr inbounds ({i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %56 = bitcast i8* %55 to {i64 (i8*, i8*)*, i8*}* 
  %57 = getelementptr  {i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* %56, i32 0, i32 0 
  store  i64 (i8*, i8*)* @$f95, i64 (i8*, i8*)** %57 
  %58 = getelementptr  {i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* %56, i32 0, i32 1 
  store  i8* %54, i8** %58 
  %59 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}, {{i64 (i8*, i8*)*, i8*}*, i64}* %51, i32 0, i32 0 
  store  {i64 (i8*, i8*)*, i8*}* %56, {i64 (i8*, i8*)*, i8*}** %59 
  %60 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %36, i32 0, i32 1 
  %61 = load  i8*, i8** %60 
  %62 = ptrtoint i8* %61 to i64 
  %63 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}, {{i64 (i8*, i8*)*, i8*}*, i64}* %51, i32 0, i32 1 
  store  i64 %62, i64* %63 
  %64 = ptrtoint i8* %49 to i64 
  %65 =  call ccc  {}*  @print_int7(i64  %64)  
  %66 = inttoptr i64 %64 to i8* 
  ret i32 0 
}
