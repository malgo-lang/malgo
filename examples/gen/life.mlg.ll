; ModuleID = './examples/life.mlg'
source_filename = "./examples/life.mlg"


 


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  i1 @view10(i8* , {i1*, i64}* , i64 , i64 )    {
; <label>:4:
  %5 = bitcast i8* %0 to {i64, i64}* 
  %6 = getelementptr  {i64, i64}, {i64, i64}* %5, i32 0, i32 0 
  %7 = load  i64, i64* %6 
  %8 = getelementptr  {i64, i64}, {i64, i64}* %5, i32 0, i32 1 
  %9 = load  i64, i64* %8 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* getelementptr inbounds ({i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* inttoptr (i32 0 to {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* 
  %12 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %11, i32 0, i32 0 
  store  i1 (i8*, {i1*, i64}*, i64, i64)* @view10, i1 (i8*, {i1*, i64}*, i64, i64)** %12 
  %13 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %11, i32 0, i32 1 
  store  i8* %0, i8** %13 
  %14 = icmp sge i64 %2, %7 
  %15 = icmp sge i64 %3, %9 
  %16 = or i1 %14, %15 
  %17 = icmp slt i64 %2, 0 
  %18 = or i1 %16, %17 
  %19 = icmp slt i64 %3, 0 
  %20 = or i1 %18, %19 
  %21 = alloca i1 
  br i1 %20, label %then_0, label %else_0 
then_0:
  store  i1 0, i1* %21 
  br label %end_0 
else_0:
  %22 = mul   i64 %3, %7 
  %23 = add   i64 %22, %2 
  %24 = getelementptr  {i1*, i64}, {i1*, i64}* %1, i32 0, i32 0 
  %25 = load  i1*, i1** %24 
  %26 = getelementptr  i1, i1* %25, i64 %23 
  %27 = load  i1, i1* %26 
  store  i1 %27, i1* %21 
  br label %end_0 
end_0:
  %28 = load  i1, i1* %21 
  ret i1 %28 
}


define external ccc  {}* @update_cells37(i8* , {i1*, i64}* )    {
  %3 = bitcast i8* %0 to {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* 
  %4 = getelementptr  {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %3, i32 0, i32 0 
  %5 = load  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}** %4 
  %6 = getelementptr  {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %3, i32 0, i32 1 
  %7 = load  {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}** %6 
  %8 = getelementptr  {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %3, i32 0, i32 2 
  %9 = load  i64, i64* %8 
  %10 = getelementptr  {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %3, i32 0, i32 3 
  %11 = load  i64, i64* %10 
  %12 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, {i1*, i64}*)*, i8*}* getelementptr inbounds ({{}* (i8*, {i1*, i64}*)*, i8*}, {{}* (i8*, {i1*, i64}*)*, i8*}* inttoptr (i32 0 to {{}* (i8*, {i1*, i64}*)*, i8*}*), i32 1) to i64))  
  %13 = bitcast i8* %12 to {{}* (i8*, {i1*, i64}*)*, i8*}* 
  %14 = getelementptr  {{}* (i8*, {i1*, i64}*)*, i8*}, {{}* (i8*, {i1*, i64}*)*, i8*}* %13, i32 0, i32 0 
  store  {}* (i8*, {i1*, i64}*)* @update_cells37, {}* (i8*, {i1*, i64}*)** %14 
  %15 = getelementptr  {{}* (i8*, {i1*, i64}*)*, i8*}, {{}* (i8*, {i1*, i64}*)*, i8*}* %13, i32 0, i32 1 
  store  i8* %0, i8** %15 
  %16 =  call ccc  {i1*, i64}*  @copy_bool_array5({i1*, i64}*  %1)  
  %17 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* getelementptr inbounds ({{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* inttoptr (i32 0 to {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}*), i32 1) to i64))  
  %18 = bitcast i8* %17 to {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* 
  %19 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %18, i32 0, i32 0 
  store  {i1*, i64}* %1, {i1*, i64}** %19 
  %20 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %18, i32 0, i32 1 
  store  {i1*, i64}* %16, {i1*, i64}** %20 
  %21 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %18, i32 0, i32 2 
  store  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %5, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}** %21 
  %22 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %18, i32 0, i32 3 
  store  {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}* %7, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}** %22 
  %23 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %18, i32 0, i32 4 
  store  i64 %9, i64* %23 
  %24 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %18, i32 0, i32 5 
  store  i64 %11, i64* %24 
  %25 = bitcast {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %18 to i8* 
  %26 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %27 = bitcast i8* %26 to {{}* (i8*, i64)*, i8*}* 
  %28 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %27, i32 0, i32 0 
  store  {}* (i8*, i64)* @go_y40, {}* (i8*, i64)** %28 
  %29 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %27, i32 0, i32 1 
  store  i8* %25, i8** %29 
  %30 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %27, i32 0, i32 0 
  %31 = load  {}* (i8*, i64)*, {}* (i8*, i64)** %30 
  %32 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %27, i32 0, i32 1 
  %33 = load  i8*, i8** %32 
  %34 =  call ccc  {}*  %31(i8*  %33, i64  0)  
  ret {}* %34 
}


define external ccc  i64 @to_int30(i1 )    {
; <label>:1:
  %2 = alloca i64 
  br i1 %0, label %then_0, label %else_0 
then_0:
  store  i64 1, i64* %2 
  br label %end_0 
else_0:
  store  i64 0, i64* %2 
  br label %end_0 
end_0:
  %3 = load  i64, i64* %2 
  ret i64 %3 
}


define external ccc  i8 @to_char22(i1 )    {
; <label>:1:
  %2 = alloca i8 
  br i1 %0, label %then_0, label %else_0 
then_0:
  store  i8 35, i8* %2 
  br label %end_0 
else_0:
  store  i8 95, i8* %2 
  br label %end_0 
end_0:
  %3 = load  i8, i8* %2 
  ret i8 %3 
}


declare external ccc  {}* @malgo_sleep(i64)    


define external ccc  {}* @sleep4(i64 )    {
  %2 =  call ccc  {}*  @malgo_sleep(i64  %0)  
  ret {}* %2 
}


define external ccc  {}* @set14(i8* , {i1*, i64}* , i64 , i64 , i1 )    {
  %6 = bitcast i8* %0 to {i64}* 
  %7 = getelementptr  {i64}, {i64}* %6, i32 0, i32 0 
  %8 = load  i64, i64* %7 
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}* getelementptr inbounds ({{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}* inttoptr (i32 0 to {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*), i32 1) to i64))  
  %10 = bitcast i8* %9 to {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}* 
  %11 = getelementptr  {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}* %10, i32 0, i32 0 
  store  {}* (i8*, {i1*, i64}*, i64, i64, i1)* @set14, {}* (i8*, {i1*, i64}*, i64, i64, i1)** %11 
  %12 = getelementptr  {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}* %10, i32 0, i32 1 
  store  i8* %0, i8** %12 
  %13 = mul   i64 %3, %8 
  %14 = add   i64 %13, %2 
  %15 = getelementptr  {i1*, i64}, {i1*, i64}* %1, i32 0, i32 0 
  %16 = load  i1*, i1** %15 
  %17 = getelementptr  i1, i1* %16, i64 %14 
  store  i1 %4, i1* %17 
  ret {}* undef 
}


declare external ccc  i1 @rand_bool()    


define external ccc  i1 @rand_bool3()    {
  %1 =  call ccc  i1  @rand_bool()  
  ret i1 %1 
}


declare external ccc  {}* @pulsar({i1*, i64}*)    


define external ccc  {}* @pulsar6({i1*, i64}* )    {
  %2 =  call ccc  {}*  @pulsar({i1*, i64}*  %0)  
  ret {}* %2 
}


declare external ccc  {}* @print_char(i8)    


define external ccc  {}* @print_char0(i8 )    {
  %2 =  call ccc  {}*  @print_char(i8  %0)  
  ret {}* %2 
}


define external ccc  {}* @print_cells24(i8* , {i1*, i64}* )    {
  %3 = bitcast i8* %0 to {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* 
  %4 = getelementptr  {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %3, i32 0, i32 0 
  %5 = load  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}** %4 
  %6 = getelementptr  {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %3, i32 0, i32 1 
  %7 = load  i64, i64* %6 
  %8 = getelementptr  {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %3, i32 0, i32 2 
  %9 = load  i64, i64* %8 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, {i1*, i64}*)*, i8*}* getelementptr inbounds ({{}* (i8*, {i1*, i64}*)*, i8*}, {{}* (i8*, {i1*, i64}*)*, i8*}* inttoptr (i32 0 to {{}* (i8*, {i1*, i64}*)*, i8*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {{}* (i8*, {i1*, i64}*)*, i8*}* 
  %12 = getelementptr  {{}* (i8*, {i1*, i64}*)*, i8*}, {{}* (i8*, {i1*, i64}*)*, i8*}* %11, i32 0, i32 0 
  store  {}* (i8*, {i1*, i64}*)* @print_cells24, {}* (i8*, {i1*, i64}*)** %12 
  %13 = getelementptr  {{}* (i8*, {i1*, i64}*)*, i8*}, {{}* (i8*, {i1*, i64}*)*, i8*}* %11, i32 0, i32 1 
  store  i8* %0, i8** %13 
  %14 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* getelementptr inbounds ({{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* inttoptr (i32 0 to {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}*), i32 1) to i64))  
  %15 = bitcast i8* %14 to {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* 
  %16 = getelementptr  {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %15, i32 0, i32 0 
  store  {i1*, i64}* %1, {i1*, i64}** %16 
  %17 = getelementptr  {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %15, i32 0, i32 1 
  store  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %5, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}** %17 
  %18 = getelementptr  {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %15, i32 0, i32 2 
  store  i64 %7, i64* %18 
  %19 = getelementptr  {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %15, i32 0, i32 3 
  store  i64 %9, i64* %19 
  %20 = bitcast {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %15 to i8* 
  %21 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %22 = bitcast i8* %21 to {{}* (i8*, i64)*, i8*}* 
  %23 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %22, i32 0, i32 0 
  store  {}* (i8*, i64)* @go_y26, {}* (i8*, i64)** %23 
  %24 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %22, i32 0, i32 1 
  store  i8* %20, i8** %24 
  %25 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %22, i32 0, i32 0 
  %26 = load  {}* (i8*, i64)*, {}* (i8*, i64)** %25 
  %27 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %22, i32 0, i32 1 
  %28 = load  i8*, i8** %27 
  %29 =  call ccc  {}*  %26(i8*  %28, i64  0)  
  ret {}* %29 
}


define external ccc  i1 @next_state32(i8* , {i1*, i64}* , i64 , i64 )    {
; <label>:4:
  %5 = bitcast i8* %0 to {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*}* 
  %6 = getelementptr  {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*}, {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*}* %5, i32 0, i32 0 
  %7 = load  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}** %6 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* getelementptr inbounds ({i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* inttoptr (i32 0 to {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* 
  %10 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %9, i32 0, i32 0 
  store  i1 (i8*, {i1*, i64}*, i64, i64)* @next_state32, i1 (i8*, {i1*, i64}*, i64, i64)** %10 
  %11 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %9, i32 0, i32 1 
  store  i8* %0, i8** %11 
  %12 = mul   i64 ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64), 1 
  %13 =  call ccc  i8*  @GC_malloc(i64  %12)  
  %14 = bitcast i8* %13 to i64* 
  %15 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64*, i64}* getelementptr inbounds ({i64*, i64}, {i64*, i64}* inttoptr (i32 0 to {i64*, i64}*), i32 1) to i64))  
  %16 = bitcast i8* %15 to {i64*, i64}* 
  %17 = getelementptr  {i64*, i64}, {i64*, i64}* %16, i32 0, i32 0 
  store  i64* %14, i64** %17 
  %18 = getelementptr  {i64*, i64}, {i64*, i64}* %16, i32 0, i32 1 
  store  i64 1, i64* %18 
  %19 = alloca i64 
  store  i64 0, i64* %19 
  br label %cond_0 
cond_0:
  %20 = load  i64, i64* %19 
  %21 = icmp slt i64 %20, 1 
  br i1 %21, label %body_0, label %end_0 
body_0:
  %22 = getelementptr  {i64*, i64}, {i64*, i64}* %16, i32 0, i32 0 
  %23 = load  i64*, i64** %22 
  %24 = getelementptr  i64, i64* %23, i64 %20 
  store  i64 0, i64* %24 
  %25 = add   i64 %20, 1 
  store  i64 %25, i64* %19 
  br label %cond_0 
end_0:
  %26 = sub   i64 %2, 1 
  %27 = add   i64 %3, 1 
  %28 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 0 
  %29 = load  i1 (i8*, {i1*, i64}*, i64, i64)*, i1 (i8*, {i1*, i64}*, i64, i64)** %28 
  %30 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 1 
  %31 = load  i8*, i8** %30 
  %32 =  call ccc  i1  %29(i8*  %31, {i1*, i64}*  %1, i64  %26, i64  %27)  
  %33 =  call ccc  i64  @to_int30(i1  %32)  
  %34 = add   i64 %3, 1 
  %35 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 0 
  %36 = load  i1 (i8*, {i1*, i64}*, i64, i64)*, i1 (i8*, {i1*, i64}*, i64, i64)** %35 
  %37 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 1 
  %38 = load  i8*, i8** %37 
  %39 =  call ccc  i1  %36(i8*  %38, {i1*, i64}*  %1, i64  %2, i64  %34)  
  %40 =  call ccc  i64  @to_int30(i1  %39)  
  %41 = add   i64 %33, %40 
  %42 = add   i64 %2, 1 
  %43 = add   i64 %3, 1 
  %44 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 0 
  %45 = load  i1 (i8*, {i1*, i64}*, i64, i64)*, i1 (i8*, {i1*, i64}*, i64, i64)** %44 
  %46 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 1 
  %47 = load  i8*, i8** %46 
  %48 =  call ccc  i1  %45(i8*  %47, {i1*, i64}*  %1, i64  %42, i64  %43)  
  %49 =  call ccc  i64  @to_int30(i1  %48)  
  %50 = add   i64 %41, %49 
  %51 = getelementptr  {i64*, i64}, {i64*, i64}* %16, i32 0, i32 0 
  %52 = load  i64*, i64** %51 
  %53 = getelementptr  i64, i64* %52, i64 0 
  store  i64 %50, i64* %53 
  %54 = sub   i64 %2, 1 
  %55 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 0 
  %56 = load  i1 (i8*, {i1*, i64}*, i64, i64)*, i1 (i8*, {i1*, i64}*, i64, i64)** %55 
  %57 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 1 
  %58 = load  i8*, i8** %57 
  %59 =  call ccc  i1  %56(i8*  %58, {i1*, i64}*  %1, i64  %54, i64  %3)  
  %60 =  call ccc  i64  @to_int30(i1  %59)  
  %61 = add   i64 %2, 1 
  %62 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 0 
  %63 = load  i1 (i8*, {i1*, i64}*, i64, i64)*, i1 (i8*, {i1*, i64}*, i64, i64)** %62 
  %64 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 1 
  %65 = load  i8*, i8** %64 
  %66 =  call ccc  i1  %63(i8*  %65, {i1*, i64}*  %1, i64  %61, i64  %3)  
  %67 =  call ccc  i64  @to_int30(i1  %66)  
  %68 = add   i64 %60, %67 
  %69 = getelementptr  {i64*, i64}, {i64*, i64}* %16, i32 0, i32 0 
  %70 = load  i64*, i64** %69 
  %71 = getelementptr  i64, i64* %70, i64 0 
  %72 = load  i64, i64* %71 
  %73 = add   i64 %68, %72 
  %74 = getelementptr  {i64*, i64}, {i64*, i64}* %16, i32 0, i32 0 
  %75 = load  i64*, i64** %74 
  %76 = getelementptr  i64, i64* %75, i64 0 
  store  i64 %73, i64* %76 
  %77 = sub   i64 %2, 1 
  %78 = sub   i64 %3, 1 
  %79 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 0 
  %80 = load  i1 (i8*, {i1*, i64}*, i64, i64)*, i1 (i8*, {i1*, i64}*, i64, i64)** %79 
  %81 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 1 
  %82 = load  i8*, i8** %81 
  %83 =  call ccc  i1  %80(i8*  %82, {i1*, i64}*  %1, i64  %77, i64  %78)  
  %84 =  call ccc  i64  @to_int30(i1  %83)  
  %85 = sub   i64 %3, 1 
  %86 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 0 
  %87 = load  i1 (i8*, {i1*, i64}*, i64, i64)*, i1 (i8*, {i1*, i64}*, i64, i64)** %86 
  %88 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 1 
  %89 = load  i8*, i8** %88 
  %90 =  call ccc  i1  %87(i8*  %89, {i1*, i64}*  %1, i64  %2, i64  %85)  
  %91 =  call ccc  i64  @to_int30(i1  %90)  
  %92 = add   i64 %84, %91 
  %93 = add   i64 %2, 1 
  %94 = sub   i64 %3, 1 
  %95 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 0 
  %96 = load  i1 (i8*, {i1*, i64}*, i64, i64)*, i1 (i8*, {i1*, i64}*, i64, i64)** %95 
  %97 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 1 
  %98 = load  i8*, i8** %97 
  %99 =  call ccc  i1  %96(i8*  %98, {i1*, i64}*  %1, i64  %93, i64  %94)  
  %100 =  call ccc  i64  @to_int30(i1  %99)  
  %101 = add   i64 %92, %100 
  %102 = getelementptr  {i64*, i64}, {i64*, i64}* %16, i32 0, i32 0 
  %103 = load  i64*, i64** %102 
  %104 = getelementptr  i64, i64* %103, i64 0 
  %105 = load  i64, i64* %104 
  %106 = add   i64 %101, %105 
  %107 = getelementptr  {i64*, i64}, {i64*, i64}* %16, i32 0, i32 0 
  %108 = load  i64*, i64** %107 
  %109 = getelementptr  i64, i64* %108, i64 0 
  store  i64 %106, i64* %109 
  %110 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 0 
  %111 = load  i1 (i8*, {i1*, i64}*, i64, i64)*, i1 (i8*, {i1*, i64}*, i64, i64)** %110 
  %112 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 1 
  %113 = load  i8*, i8** %112 
  %114 =  call ccc  i1  %111(i8*  %113, {i1*, i64}*  %1, i64  %2, i64  %3)  
  %115 = alloca i1 
  br i1 %114, label %then_0, label %else_0 
then_0:
  %116 = getelementptr  {i64*, i64}, {i64*, i64}* %16, i32 0, i32 0 
  %117 = load  i64*, i64** %116 
  %118 = getelementptr  i64, i64* %117, i64 0 
  %119 = load  i64, i64* %118 
  %120 = icmp eq i64 %119, 2 
  %121 = getelementptr  {i64*, i64}, {i64*, i64}* %16, i32 0, i32 0 
  %122 = load  i64*, i64** %121 
  %123 = getelementptr  i64, i64* %122, i64 0 
  %124 = load  i64, i64* %123 
  %125 = icmp eq i64 %124, 3 
  %126 = or i1 %120, %125 
  store  i1 %126, i1* %115 
  br label %end_1 
else_0:
  %127 = getelementptr  {i64*, i64}, {i64*, i64}* %16, i32 0, i32 0 
  %128 = load  i64*, i64** %127 
  %129 = getelementptr  i64, i64* %128, i64 0 
  %130 = load  i64, i64* %129 
  %131 = icmp eq i64 %130, 3 
  store  i1 %131, i1* %115 
  br label %end_1 
end_1:
  %132 = load  i1, i1* %115 
  ret i1 %132 
}


declare external ccc  {}* @newline()    


define external ccc  {}* @newline1()    {
  %1 =  call ccc  {}*  @newline()  
  ret {}* %1 
}


define external ccc  {}* @loop44(i8* , i64 )    {
; <label>:2:
  %3 = bitcast i8* %0 to {{i1*, i64}*, {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}*}* 
  %4 = getelementptr  {{i1*, i64}*, {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}*}, {{i1*, i64}*, {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}*}* %3, i32 0, i32 0 
  %5 = load  {i1*, i64}*, {i1*, i64}** %4 
  %6 = getelementptr  {{i1*, i64}*, {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}*}, {{i1*, i64}*, {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}*}* %3, i32 0, i32 1 
  %7 = load  {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}** %6 
  %8 = getelementptr  {{i1*, i64}*, {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}*}, {{i1*, i64}*, {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}*}* %3, i32 0, i32 2 
  %9 = load  {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}** %8 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {{}* (i8*, i64)*, i8*}* 
  %12 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %11, i32 0, i32 0 
  store  {}* (i8*, i64)* @loop44, {}* (i8*, i64)** %12 
  %13 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %11, i32 0, i32 1 
  store  i8* %0, i8** %13 
  %14 = icmp sgt i64 %1, 0 
  %15 = alloca {}* 
  br i1 %14, label %then_0, label %else_0 
then_0:
  %16 = getelementptr  {{}* (i8*, {i1*, i64}*)*, i8*}, {{}* (i8*, {i1*, i64}*)*, i8*}* %7, i32 0, i32 0 
  %17 = load  {}* (i8*, {i1*, i64}*)*, {}* (i8*, {i1*, i64}*)** %16 
  %18 = getelementptr  {{}* (i8*, {i1*, i64}*)*, i8*}, {{}* (i8*, {i1*, i64}*)*, i8*}* %7, i32 0, i32 1 
  %19 = load  i8*, i8** %18 
  %20 =  call ccc  {}*  %17(i8*  %19, {i1*, i64}*  %5)  
  %21 = getelementptr  {{}* (i8*, {i1*, i64}*)*, i8*}, {{}* (i8*, {i1*, i64}*)*, i8*}* %9, i32 0, i32 0 
  %22 = load  {}* (i8*, {i1*, i64}*)*, {}* (i8*, {i1*, i64}*)** %21 
  %23 = getelementptr  {{}* (i8*, {i1*, i64}*)*, i8*}, {{}* (i8*, {i1*, i64}*)*, i8*}* %9, i32 0, i32 1 
  %24 = load  i8*, i8** %23 
  %25 =  call ccc  {}*  %22(i8*  %24, {i1*, i64}*  %5)  
  %26 =  call ccc  {}*  @newline1()  
  %27 = sub   i64 %1, 1 
  %28 =  call ccc  {}*  @loop44(i8*  %0, i64  %27)  
  store  {}* %28, {}** %15 
  br label %end_0 
else_0:
  %29 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %30 = bitcast i8* %29 to {}* 
  store  {}* %30, {}** %15 
  br label %end_0 
end_0:
  %31 = load  {}*, {}** %15 
  ret {}* %31 
}


define external ccc  {}* @init_cells19(i8* , {i1*, i64}* , i64 )    {
; <label>:3:
  %4 = bitcast i8* %0 to {i64, i64}* 
  %5 = getelementptr  {i64, i64}, {i64, i64}* %4, i32 0, i32 0 
  %6 = load  i64, i64* %5 
  %7 = getelementptr  {i64, i64}, {i64, i64}* %4, i32 0, i32 1 
  %8 = load  i64, i64* %7 
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, {i1*, i64}*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, {i1*, i64}*, i64)*, i8*}, {{}* (i8*, {i1*, i64}*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, {i1*, i64}*, i64)*, i8*}*), i32 1) to i64))  
  %10 = bitcast i8* %9 to {{}* (i8*, {i1*, i64}*, i64)*, i8*}* 
  %11 = getelementptr  {{}* (i8*, {i1*, i64}*, i64)*, i8*}, {{}* (i8*, {i1*, i64}*, i64)*, i8*}* %10, i32 0, i32 0 
  store  {}* (i8*, {i1*, i64}*, i64)* @init_cells19, {}* (i8*, {i1*, i64}*, i64)** %11 
  %12 = getelementptr  {{}* (i8*, {i1*, i64}*, i64)*, i8*}, {{}* (i8*, {i1*, i64}*, i64)*, i8*}* %10, i32 0, i32 1 
  store  i8* %0, i8** %12 
  %13 = mul   i64 %6, %8 
  %14 = icmp sge i64 %2, %13 
  %15 = alloca {}* 
  br i1 %14, label %then_0, label %else_0 
then_0:
  %16 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %17 = bitcast i8* %16 to {}* 
  store  {}* %17, {}** %15 
  br label %end_0 
else_0:
  %18 =  call ccc  i1  @rand_bool3()  
  %19 = getelementptr  {i1*, i64}, {i1*, i64}* %1, i32 0, i32 0 
  %20 = load  i1*, i1** %19 
  %21 = getelementptr  i1, i1* %20, i64 %2 
  store  i1 %18, i1* %21 
  %22 = add   i64 %2, 1 
  %23 =  call ccc  {}*  @init_cells19(i8*  %0, {i1*, i64}*  %1, i64  %22)  
  store  {}* %23, {}** %15 
  br label %end_0 
end_0:
  %24 = load  {}*, {}** %15 
  ret {}* %24 
}


define external ccc  {}* @go_y40(i8* , i64 )    {
; <label>:2:
  %3 = bitcast i8* %0 to {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* 
  %4 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %3, i32 0, i32 0 
  %5 = load  {i1*, i64}*, {i1*, i64}** %4 
  %6 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %3, i32 0, i32 1 
  %7 = load  {i1*, i64}*, {i1*, i64}** %6 
  %8 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %3, i32 0, i32 2 
  %9 = load  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}** %8 
  %10 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %3, i32 0, i32 3 
  %11 = load  {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}** %10 
  %12 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %3, i32 0, i32 4 
  %13 = load  i64, i64* %12 
  %14 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %3, i32 0, i32 5 
  %15 = load  i64, i64* %14 
  %16 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %17 = bitcast i8* %16 to {{}* (i8*, i64)*, i8*}* 
  %18 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %17, i32 0, i32 0 
  store  {}* (i8*, i64)* @go_y40, {}* (i8*, i64)** %18 
  %19 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %17, i32 0, i32 1 
  store  i8* %0, i8** %19 
  %20 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* getelementptr inbounds ({{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* inttoptr (i32 0 to {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}*), i32 1) to i64))  
  %21 = bitcast i8* %20 to {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* 
  %22 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %21, i32 0, i32 0 
  store  {i1*, i64}* %5, {i1*, i64}** %22 
  %23 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %21, i32 0, i32 1 
  store  {i1*, i64}* %7, {i1*, i64}** %23 
  %24 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %21, i32 0, i32 2 
  store  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %9, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}** %24 
  %25 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %21, i32 0, i32 3 
  store  {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}* %11, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}** %25 
  %26 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %21, i32 0, i32 4 
  store  i64 %13, i64* %26 
  %27 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %21, i32 0, i32 5 
  store  i64 %1, i64* %27 
  %28 = bitcast {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %21 to i8* 
  %29 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %30 = bitcast i8* %29 to {{}* (i8*, i64)*, i8*}* 
  %31 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %30, i32 0, i32 0 
  store  {}* (i8*, i64)* @go_x42, {}* (i8*, i64)** %31 
  %32 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %30, i32 0, i32 1 
  store  i8* %28, i8** %32 
  %33 = icmp sge i64 %1, %15 
  %34 = alloca {}* 
  br i1 %33, label %then_0, label %else_0 
then_0:
  %35 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %36 = bitcast i8* %35 to {}* 
  store  {}* %36, {}** %34 
  br label %end_0 
else_0:
  %37 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %30, i32 0, i32 0 
  %38 = load  {}* (i8*, i64)*, {}* (i8*, i64)** %37 
  %39 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %30, i32 0, i32 1 
  %40 = load  i8*, i8** %39 
  %41 =  call ccc  {}*  %38(i8*  %40, i64  0)  
  %42 = add   i64 %1, 1 
  %43 =  call ccc  {}*  @go_y40(i8*  %0, i64  %42)  
  store  {}* %43, {}** %34 
  br label %end_0 
end_0:
  %44 = load  {}*, {}** %34 
  ret {}* %44 
}


define external ccc  {}* @go_y26(i8* , i64 )    {
; <label>:2:
  %3 = bitcast i8* %0 to {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* 
  %4 = getelementptr  {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %3, i32 0, i32 0 
  %5 = load  {i1*, i64}*, {i1*, i64}** %4 
  %6 = getelementptr  {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %3, i32 0, i32 1 
  %7 = load  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}** %6 
  %8 = getelementptr  {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %3, i32 0, i32 2 
  %9 = load  i64, i64* %8 
  %10 = getelementptr  {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %3, i32 0, i32 3 
  %11 = load  i64, i64* %10 
  %12 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %13 = bitcast i8* %12 to {{}* (i8*, i64)*, i8*}* 
  %14 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %13, i32 0, i32 0 
  store  {}* (i8*, i64)* @go_y26, {}* (i8*, i64)** %14 
  %15 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %13, i32 0, i32 1 
  store  i8* %0, i8** %15 
  %16 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* getelementptr inbounds ({{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* inttoptr (i32 0 to {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}*), i32 1) to i64))  
  %17 = bitcast i8* %16 to {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* 
  %18 = getelementptr  {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %17, i32 0, i32 0 
  store  {i1*, i64}* %5, {i1*, i64}** %18 
  %19 = getelementptr  {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %17, i32 0, i32 1 
  store  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}** %19 
  %20 = getelementptr  {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %17, i32 0, i32 2 
  store  i64 %9, i64* %20 
  %21 = getelementptr  {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %17, i32 0, i32 3 
  store  i64 %1, i64* %21 
  %22 = bitcast {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %17 to i8* 
  %23 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %24 = bitcast i8* %23 to {{}* (i8*, i64)*, i8*}* 
  %25 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %24, i32 0, i32 0 
  store  {}* (i8*, i64)* @go_x28, {}* (i8*, i64)** %25 
  %26 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %24, i32 0, i32 1 
  store  i8* %22, i8** %26 
  %27 = icmp sge i64 %1, %11 
  %28 = alloca {}* 
  br i1 %27, label %then_0, label %else_0 
then_0:
  %29 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %30 = bitcast i8* %29 to {}* 
  store  {}* %30, {}** %28 
  br label %end_0 
else_0:
  %31 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %24, i32 0, i32 0 
  %32 = load  {}* (i8*, i64)*, {}* (i8*, i64)** %31 
  %33 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %24, i32 0, i32 1 
  %34 = load  i8*, i8** %33 
  %35 =  call ccc  {}*  %32(i8*  %34, i64  0)  
  %36 =  call ccc  {}*  @newline1()  
  %37 = add   i64 %1, 1 
  %38 =  call ccc  {}*  @go_y26(i8*  %0, i64  %37)  
  store  {}* %38, {}** %28 
  br label %end_0 
end_0:
  %39 = load  {}*, {}** %28 
  ret {}* %39 
}


define external ccc  {}* @go_x42(i8* , i64 )    {
; <label>:2:
  %3 = bitcast i8* %0 to {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* 
  %4 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %3, i32 0, i32 0 
  %5 = load  {i1*, i64}*, {i1*, i64}** %4 
  %6 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %3, i32 0, i32 1 
  %7 = load  {i1*, i64}*, {i1*, i64}** %6 
  %8 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %3, i32 0, i32 2 
  %9 = load  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}** %8 
  %10 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %3, i32 0, i32 3 
  %11 = load  {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}** %10 
  %12 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %3, i32 0, i32 4 
  %13 = load  i64, i64* %12 
  %14 = getelementptr  {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %3, i32 0, i32 5 
  %15 = load  i64, i64* %14 
  %16 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %17 = bitcast i8* %16 to {{}* (i8*, i64)*, i8*}* 
  %18 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %17, i32 0, i32 0 
  store  {}* (i8*, i64)* @go_x42, {}* (i8*, i64)** %18 
  %19 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %17, i32 0, i32 1 
  store  i8* %0, i8** %19 
  %20 = icmp sge i64 %1, %13 
  %21 = alloca {}* 
  br i1 %20, label %then_0, label %else_0 
then_0:
  %22 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %23 = bitcast i8* %22 to {}* 
  store  {}* %23, {}** %21 
  br label %end_0 
else_0:
  %24 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %9, i32 0, i32 0 
  %25 = load  i1 (i8*, {i1*, i64}*, i64, i64)*, i1 (i8*, {i1*, i64}*, i64, i64)** %24 
  %26 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %9, i32 0, i32 1 
  %27 = load  i8*, i8** %26 
  %28 =  call ccc  i1  %25(i8*  %27, {i1*, i64}*  %7, i64  %1, i64  %15)  
  %29 = getelementptr  {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}* %11, i32 0, i32 0 
  %30 = load  {}* (i8*, {i1*, i64}*, i64, i64, i1)*, {}* (i8*, {i1*, i64}*, i64, i64, i1)** %29 
  %31 = getelementptr  {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}* %11, i32 0, i32 1 
  %32 = load  i8*, i8** %31 
  %33 =  call ccc  {}*  %30(i8*  %32, {i1*, i64}*  %5, i64  %1, i64  %15, i1  %28)  
  %34 = add   i64 %1, 1 
  %35 =  call ccc  {}*  @go_x42(i8*  %0, i64  %34)  
  store  {}* %35, {}** %21 
  br label %end_0 
end_0:
  %36 = load  {}*, {}** %21 
  ret {}* %36 
}


define external ccc  {}* @go_x28(i8* , i64 )    {
; <label>:2:
  %3 = bitcast i8* %0 to {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* 
  %4 = getelementptr  {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %3, i32 0, i32 0 
  %5 = load  {i1*, i64}*, {i1*, i64}** %4 
  %6 = getelementptr  {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %3, i32 0, i32 1 
  %7 = load  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}** %6 
  %8 = getelementptr  {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %3, i32 0, i32 2 
  %9 = load  i64, i64* %8 
  %10 = getelementptr  {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1*, i64}*, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %3, i32 0, i32 3 
  %11 = load  i64, i64* %10 
  %12 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %13 = bitcast i8* %12 to {{}* (i8*, i64)*, i8*}* 
  %14 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %13, i32 0, i32 0 
  store  {}* (i8*, i64)* @go_x28, {}* (i8*, i64)** %14 
  %15 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %13, i32 0, i32 1 
  store  i8* %0, i8** %15 
  %16 = icmp sge i64 %1, %9 
  %17 = alloca {}* 
  br i1 %16, label %then_0, label %else_0 
then_0:
  %18 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %19 = bitcast i8* %18 to {}* 
  store  {}* %19, {}** %17 
  br label %end_0 
else_0:
  %20 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 0 
  %21 = load  i1 (i8*, {i1*, i64}*, i64, i64)*, i1 (i8*, {i1*, i64}*, i64, i64)** %20 
  %22 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %7, i32 0, i32 1 
  %23 = load  i8*, i8** %22 
  %24 =  call ccc  i1  %21(i8*  %23, {i1*, i64}*  %5, i64  %1, i64  %11)  
  %25 =  call ccc  i8  @to_char22(i1  %24)  
  %26 =  call ccc  {}*  @print_char0(i8  %25)  
  %27 = add   i64 %1, 1 
  %28 =  call ccc  {}*  @go_x28(i8*  %0, i64  %27)  
  store  {}* %28, {}** %17 
  br label %end_0 
end_0:
  %29 = load  {}*, {}** %17 
  ret {}* %29 
}


declare external ccc  {}* @gen_seed()    


define external ccc  {}* @gen_seed2()    {
  %1 =  call ccc  {}*  @gen_seed()  
  ret {}* %1 
}


declare external ccc  {i1*, i64}* @copy_bool_array({i1*, i64}*)    


define external ccc  {i1*, i64}* @copy_bool_array5({i1*, i64}* )    {
  %2 =  call ccc  {i1*, i64}*  @copy_bool_array({i1*, i64}*  %0)  
  ret {i1*, i64}* %2 
}


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
; <label>:0:
   call ccc  void  @GC_init()  
  %1 = mul   i64 50, 20 
  %2 = mul   i64 ptrtoint (i1* getelementptr inbounds (i1, i1* inttoptr (i32 0 to i1*), i32 1) to i64), %1 
  %3 =  call ccc  i8*  @GC_malloc(i64  %2)  
  %4 = bitcast i8* %3 to i1* 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i1*, i64}* getelementptr inbounds ({i1*, i64}, {i1*, i64}* inttoptr (i32 0 to {i1*, i64}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i1*, i64}* 
  %7 = getelementptr  {i1*, i64}, {i1*, i64}* %6, i32 0, i32 0 
  store  i1* %4, i1** %7 
  %8 = getelementptr  {i1*, i64}, {i1*, i64}* %6, i32 0, i32 1 
  store  i64 %1, i64* %8 
  %9 = alloca i64 
  store  i64 0, i64* %9 
  br label %cond_0 
cond_0:
  %10 = load  i64, i64* %9 
  %11 = icmp slt i64 %10, %1 
  br i1 %11, label %body_0, label %end_0 
body_0:
  %12 = getelementptr  {i1*, i64}, {i1*, i64}* %6, i32 0, i32 0 
  %13 = load  i1*, i1** %12 
  %14 = getelementptr  i1, i1* %13, i64 %10 
  store  i1 0, i1* %14 
  %15 = add   i64 %10, 1 
  store  i64 %15, i64* %9 
  br label %cond_0 
end_0:
  %16 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64, i64}* getelementptr inbounds ({i64, i64}, {i64, i64}* inttoptr (i32 0 to {i64, i64}*), i32 1) to i64))  
  %17 = bitcast i8* %16 to {i64, i64}* 
  %18 = getelementptr  {i64, i64}, {i64, i64}* %17, i32 0, i32 0 
  store  i64 50, i64* %18 
  %19 = getelementptr  {i64, i64}, {i64, i64}* %17, i32 0, i32 1 
  store  i64 20, i64* %19 
  %20 = bitcast {i64, i64}* %17 to i8* 
  %21 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* getelementptr inbounds ({i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* inttoptr (i32 0 to {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*), i32 1) to i64))  
  %22 = bitcast i8* %21 to {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* 
  %23 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %22, i32 0, i32 0 
  store  i1 (i8*, {i1*, i64}*, i64, i64)* @view10, i1 (i8*, {i1*, i64}*, i64, i64)** %23 
  %24 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %22, i32 0, i32 1 
  store  i8* %20, i8** %24 
  %25 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64}* getelementptr inbounds ({i64}, {i64}* inttoptr (i32 0 to {i64}*), i32 1) to i64))  
  %26 = bitcast i8* %25 to {i64}* 
  %27 = getelementptr  {i64}, {i64}* %26, i32 0, i32 0 
  store  i64 50, i64* %27 
  %28 = bitcast {i64}* %26 to i8* 
  %29 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}* getelementptr inbounds ({{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}* inttoptr (i32 0 to {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*), i32 1) to i64))  
  %30 = bitcast i8* %29 to {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}* 
  %31 = getelementptr  {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}* %30, i32 0, i32 0 
  store  {}* (i8*, {i1*, i64}*, i64, i64, i1)* @set14, {}* (i8*, {i1*, i64}*, i64, i64, i1)** %31 
  %32 = getelementptr  {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}* %30, i32 0, i32 1 
  store  i8* %28, i8** %32 
  %33 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64, i64}* getelementptr inbounds ({i64, i64}, {i64, i64}* inttoptr (i32 0 to {i64, i64}*), i32 1) to i64))  
  %34 = bitcast i8* %33 to {i64, i64}* 
  %35 = getelementptr  {i64, i64}, {i64, i64}* %34, i32 0, i32 0 
  store  i64 50, i64* %35 
  %36 = getelementptr  {i64, i64}, {i64, i64}* %34, i32 0, i32 1 
  store  i64 20, i64* %36 
  %37 = bitcast {i64, i64}* %34 to i8* 
  %38 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, {i1*, i64}*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, {i1*, i64}*, i64)*, i8*}, {{}* (i8*, {i1*, i64}*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, {i1*, i64}*, i64)*, i8*}*), i32 1) to i64))  
  %39 = bitcast i8* %38 to {{}* (i8*, {i1*, i64}*, i64)*, i8*}* 
  %40 = getelementptr  {{}* (i8*, {i1*, i64}*, i64)*, i8*}, {{}* (i8*, {i1*, i64}*, i64)*, i8*}* %39, i32 0, i32 0 
  store  {}* (i8*, {i1*, i64}*, i64)* @init_cells19, {}* (i8*, {i1*, i64}*, i64)** %40 
  %41 = getelementptr  {{}* (i8*, {i1*, i64}*, i64)*, i8*}, {{}* (i8*, {i1*, i64}*, i64)*, i8*}* %39, i32 0, i32 1 
  store  i8* %37, i8** %41 
  %42 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* getelementptr inbounds ({{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* inttoptr (i32 0 to {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}*), i32 1) to i64))  
  %43 = bitcast i8* %42 to {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* 
  %44 = getelementptr  {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %43, i32 0, i32 0 
  store  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %22, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}** %44 
  %45 = getelementptr  {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %43, i32 0, i32 1 
  store  i64 50, i64* %45 
  %46 = getelementptr  {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}, {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %43, i32 0, i32 2 
  store  i64 20, i64* %46 
  %47 = bitcast {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, i64, i64}* %43 to i8* 
  %48 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, {i1*, i64}*)*, i8*}* getelementptr inbounds ({{}* (i8*, {i1*, i64}*)*, i8*}, {{}* (i8*, {i1*, i64}*)*, i8*}* inttoptr (i32 0 to {{}* (i8*, {i1*, i64}*)*, i8*}*), i32 1) to i64))  
  %49 = bitcast i8* %48 to {{}* (i8*, {i1*, i64}*)*, i8*}* 
  %50 = getelementptr  {{}* (i8*, {i1*, i64}*)*, i8*}, {{}* (i8*, {i1*, i64}*)*, i8*}* %49, i32 0, i32 0 
  store  {}* (i8*, {i1*, i64}*)* @print_cells24, {}* (i8*, {i1*, i64}*)** %50 
  %51 = getelementptr  {{}* (i8*, {i1*, i64}*)*, i8*}, {{}* (i8*, {i1*, i64}*)*, i8*}* %49, i32 0, i32 1 
  store  i8* %47, i8** %51 
  %52 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*}* getelementptr inbounds ({{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*}, {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*}* inttoptr (i32 0 to {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*}*), i32 1) to i64))  
  %53 = bitcast i8* %52 to {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*}* 
  %54 = getelementptr  {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*}, {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*}* %53, i32 0, i32 0 
  store  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %22, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}** %54 
  %55 = bitcast {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*}* %53 to i8* 
  %56 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* getelementptr inbounds ({i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* inttoptr (i32 0 to {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*), i32 1) to i64))  
  %57 = bitcast i8* %56 to {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* 
  %58 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %57, i32 0, i32 0 
  store  i1 (i8*, {i1*, i64}*, i64, i64)* @next_state32, i1 (i8*, {i1*, i64}*, i64, i64)** %58 
  %59 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %57, i32 0, i32 1 
  store  i8* %55, i8** %59 
  %60 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* getelementptr inbounds ({{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* inttoptr (i32 0 to {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}*), i32 1) to i64))  
  %61 = bitcast i8* %60 to {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* 
  %62 = getelementptr  {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %61, i32 0, i32 0 
  store  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %57, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}** %62 
  %63 = getelementptr  {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %61, i32 0, i32 1 
  store  {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}* %30, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}** %63 
  %64 = getelementptr  {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %61, i32 0, i32 2 
  store  i64 50, i64* %64 
  %65 = getelementptr  {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}, {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %61, i32 0, i32 3 
  store  i64 20, i64* %65 
  %66 = bitcast {{i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*, {{}* (i8*, {i1*, i64}*, i64, i64, i1)*, i8*}*, i64, i64}* %61 to i8* 
  %67 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, {i1*, i64}*)*, i8*}* getelementptr inbounds ({{}* (i8*, {i1*, i64}*)*, i8*}, {{}* (i8*, {i1*, i64}*)*, i8*}* inttoptr (i32 0 to {{}* (i8*, {i1*, i64}*)*, i8*}*), i32 1) to i64))  
  %68 = bitcast i8* %67 to {{}* (i8*, {i1*, i64}*)*, i8*}* 
  %69 = getelementptr  {{}* (i8*, {i1*, i64}*)*, i8*}, {{}* (i8*, {i1*, i64}*)*, i8*}* %68, i32 0, i32 0 
  store  {}* (i8*, {i1*, i64}*)* @update_cells37, {}* (i8*, {i1*, i64}*)** %69 
  %70 = getelementptr  {{}* (i8*, {i1*, i64}*)*, i8*}, {{}* (i8*, {i1*, i64}*)*, i8*}* %68, i32 0, i32 1 
  store  i8* %66, i8** %70 
  %71 =  call ccc  {}*  @gen_seed2()  
  %72 =  call ccc  {}*  @pulsar6({i1*, i64}*  %6)  
  %73 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i1*, i64}*, {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}*}* getelementptr inbounds ({{i1*, i64}*, {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}*}, {{i1*, i64}*, {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}*}* inttoptr (i32 0 to {{i1*, i64}*, {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}*}*), i32 1) to i64))  
  %74 = bitcast i8* %73 to {{i1*, i64}*, {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}*}* 
  %75 = getelementptr  {{i1*, i64}*, {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}*}, {{i1*, i64}*, {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}*}* %74, i32 0, i32 0 
  store  {i1*, i64}* %6, {i1*, i64}** %75 
  %76 = getelementptr  {{i1*, i64}*, {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}*}, {{i1*, i64}*, {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}*}* %74, i32 0, i32 1 
  store  {{}* (i8*, {i1*, i64}*)*, i8*}* %49, {{}* (i8*, {i1*, i64}*)*, i8*}** %76 
  %77 = getelementptr  {{i1*, i64}*, {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}*}, {{i1*, i64}*, {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}*}* %74, i32 0, i32 2 
  store  {{}* (i8*, {i1*, i64}*)*, i8*}* %68, {{}* (i8*, {i1*, i64}*)*, i8*}** %77 
  %78 = bitcast {{i1*, i64}*, {{}* (i8*, {i1*, i64}*)*, i8*}*, {{}* (i8*, {i1*, i64}*)*, i8*}*}* %74 to i8* 
  %79 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %80 = bitcast i8* %79 to {{}* (i8*, i64)*, i8*}* 
  %81 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %80, i32 0, i32 0 
  store  {}* (i8*, i64)* @loop44, {}* (i8*, i64)** %81 
  %82 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %80, i32 0, i32 1 
  store  i8* %78, i8** %82 
  %83 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %80, i32 0, i32 0 
  %84 = load  {}* (i8*, i64)*, {}* (i8*, i64)** %83 
  %85 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %80, i32 0, i32 1 
  %86 = load  i8*, i8** %85 
  %87 =  call ccc  {}*  %84(i8*  %86, i64  10)  
  ret i32 0 
}
