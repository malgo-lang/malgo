source_filename = "./examples/test12.mlg"
; ModuleID = './examples/test12.mlg'


 


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  i1 @view6(i8* , {i1*, i64}* , i64 , i64 )    {
; <label>:4:
  %5 = bitcast i8* %0 to {i64, i64}* 
  %6 = getelementptr  {i64, i64}, {i64, i64}* %5, i32 0, i32 0 
  %7 = load  i64, i64* %6 
  %8 = getelementptr  {i64, i64}, {i64, i64}* %5, i32 0, i32 1 
  %9 = load  i64, i64* %8 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* getelementptr inbounds ({i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* inttoptr (i32 0 to {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* 
  %12 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %11, i32 0, i32 0 
  store  i1 (i8*, {i1*, i64}*, i64, i64)* @view6, i1 (i8*, {i1*, i64}*, i64, i64)** %12 
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
  br label %end_2 
else_0:
  %22 = mul   i64 %3, %7 
  %23 = add   i64 %22, %2 
  %24 = getelementptr  {i1*, i64}, {i1*, i64}* %1, i32 0, i32 0 
  %25 = load  i1*, i1** %24 
  %26 = getelementptr  {i1*, i64}, {i1*, i64}* %1, i32 0, i32 1 
  %27 = load  i64, i64* %26 
  %28 = mul   i64 ptrtoint (i8** getelementptr inbounds (i8*, i8** inttoptr (i32 0 to i8**), i32 1) to i64), %27 
  %29 =  call ccc  i8*  @GC_malloc(i64  %28)  
  %30 = bitcast i8* %29 to i8** 
  %31 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8**, i64}* getelementptr inbounds ({i8**, i64}, {i8**, i64}* inttoptr (i32 0 to {i8**, i64}*), i32 1) to i64))  
  %32 = bitcast i8* %31 to {i8**, i64}* 
  %33 = getelementptr  {i8**, i64}, {i8**, i64}* %32, i32 0, i32 0 
  store  i8** %30, i8*** %33 
  %34 = getelementptr  {i8**, i64}, {i8**, i64}* %32, i32 0, i32 1 
  store  i64 %27, i64* %34 
  %35 = getelementptr  {i8**, i64}, {i8**, i64}* %32, i32 0, i32 0 
  %36 = load  i8**, i8*** %35 
  %37 = getelementptr  {i8**, i64}, {i8**, i64}* %32, i32 0, i32 1 
  store  i64 %27, i64* %37 
  %38 = alloca i64 
  store  i64 0, i64* %38 
  br label %cond_0 
cond_0:
  %39 = load  i64, i64* %38 
  %40 = icmp slt i64 %39, %27 
  br i1 %40, label %body_0, label %end_0 
body_0:
  %41 = getelementptr  i1, i1* %25, i64 %39 
  %42 = load  i1, i1* %41 
  %43 = zext i1 %42 to i64  
  %44 = inttoptr i64 %43 to i8* 
  %45 = getelementptr  i8*, i8** %36, i64 %39 
  store  i8* %44, i8** %45 
  %46 = add   i64 %39, 1 
  store  i64 %46, i64* %38 
  br label %cond_0 
end_0:
  %47 =  call ccc  i8*  @sub3({i8**, i64}*  %32, i64  %23)  
  %48 = getelementptr  {i8**, i64}, {i8**, i64}* %32, i32 0, i32 0 
  %49 = load  i8**, i8*** %48 
  %50 = getelementptr  {i8**, i64}, {i8**, i64}* %32, i32 0, i32 1 
  %51 = load  i64, i64* %50 
  %52 = mul   i64 ptrtoint (i1* getelementptr inbounds (i1, i1* inttoptr (i32 0 to i1*), i32 1) to i64), %51 
  %53 =  call ccc  i8*  @GC_malloc(i64  %52)  
  %54 = bitcast i8* %53 to i1* 
  %55 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i1*, i64}* getelementptr inbounds ({i1*, i64}, {i1*, i64}* inttoptr (i32 0 to {i1*, i64}*), i32 1) to i64))  
  %56 = bitcast i8* %55 to {i1*, i64}* 
  %57 = getelementptr  {i1*, i64}, {i1*, i64}* %56, i32 0, i32 0 
  store  i1* %54, i1** %57 
  %58 = getelementptr  {i1*, i64}, {i1*, i64}* %56, i32 0, i32 1 
  store  i64 %51, i64* %58 
  %59 = getelementptr  {i1*, i64}, {i1*, i64}* %56, i32 0, i32 0 
  %60 = load  i1*, i1** %59 
  %61 = getelementptr  {i1*, i64}, {i1*, i64}* %56, i32 0, i32 1 
  store  i64 %51, i64* %61 
  %62 = alloca i64 
  store  i64 0, i64* %62 
  br label %cond_1 
cond_1:
  %63 = load  i64, i64* %62 
  %64 = icmp slt i64 %63, %51 
  br i1 %64, label %body_1, label %end_1 
body_1:
  %65 = getelementptr  i8*, i8** %49, i64 %63 
  %66 = load  i8*, i8** %65 
  %67 = ptrtoint i8* %66 to i64 
  %68 = trunc i64 %67 to i1  
  %69 = getelementptr  i1, i1* %60, i64 %63 
  store  i1 %68, i1* %69 
  %70 = add   i64 %63, 1 
  store  i64 %70, i64* %62 
  br label %cond_1 
end_1:
  %71 = ptrtoint i8* %47 to i64 
  %72 = trunc i64 %71 to i1  
  store  i1 %72, i1* %21 
  br label %end_2 
end_2:
  %73 = load  i1, i1* %21 
  ret i1 %73 
}


define external ccc  i8* @sub3({i8**, i64}* , i64 )    {
  %3 = getelementptr  {i8**, i64}, {i8**, i64}* %0, i32 0, i32 0 
  %4 = load  i8**, i8*** %3 
  %5 = getelementptr  i8*, i8** %4, i64 %1 
  %6 = load  i8*, i8** %5 
  ret i8* %6 
}


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
; <label>:0:
   call ccc  void  @GC_init()  
  %1 = mul   i64 ptrtoint (i1* getelementptr inbounds (i1, i1* inttoptr (i32 0 to i1*), i32 1) to i64), 3 
  %2 =  call ccc  i8*  @GC_malloc(i64  %1)  
  %3 = bitcast i8* %2 to i1* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i1*, i64}* getelementptr inbounds ({i1*, i64}, {i1*, i64}* inttoptr (i32 0 to {i1*, i64}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i1*, i64}* 
  %6 = getelementptr  {i1*, i64}, {i1*, i64}* %5, i32 0, i32 0 
  store  i1* %3, i1** %6 
  %7 = getelementptr  {i1*, i64}, {i1*, i64}* %5, i32 0, i32 1 
  store  i64 3, i64* %7 
  %8 = alloca i64 
  store  i64 0, i64* %8 
  br label %cond_0 
cond_0:
  %9 = load  i64, i64* %8 
  %10 = icmp slt i64 %9, 3 
  br i1 %10, label %body_0, label %end_0 
body_0:
  %11 = getelementptr  {i1*, i64}, {i1*, i64}* %5, i32 0, i32 0 
  %12 = load  i1*, i1** %11 
  %13 = getelementptr  i1, i1* %12, i64 %9 
  store  i1 1, i1* %13 
  %14 = add   i64 %9, 1 
  store  i64 %14, i64* %8 
  br label %cond_0 
end_0:
  %15 = getelementptr  {i1*, i64}, {i1*, i64}* %5, i32 0, i32 0 
  %16 = load  i1*, i1** %15 
  %17 = getelementptr  i1, i1* %16, i64 1 
  store  i1 1, i1* %17 
  %18 = getelementptr  {i1*, i64}, {i1*, i64}* %5, i32 0, i32 0 
  %19 = load  i1*, i1** %18 
  %20 = getelementptr  i1, i1* %19, i64 2 
  store  i1 1, i1* %20 
  %21 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64, i64}* getelementptr inbounds ({i64, i64}, {i64, i64}* inttoptr (i32 0 to {i64, i64}*), i32 1) to i64))  
  %22 = bitcast i8* %21 to {i64, i64}* 
  %23 = getelementptr  {i64, i64}, {i64, i64}* %22, i32 0, i32 0 
  store  i64 10, i64* %23 
  %24 = getelementptr  {i64, i64}, {i64, i64}* %22, i32 0, i32 1 
  store  i64 10, i64* %24 
  %25 = bitcast {i64, i64}* %22 to i8* 
  %26 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* getelementptr inbounds ({i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* inttoptr (i32 0 to {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}*), i32 1) to i64))  
  %27 = bitcast i8* %26 to {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* 
  %28 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %27, i32 0, i32 0 
  store  i1 (i8*, {i1*, i64}*, i64, i64)* @view6, i1 (i8*, {i1*, i64}*, i64, i64)** %28 
  %29 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %27, i32 0, i32 1 
  store  i8* %25, i8** %29 
  %30 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %27, i32 0, i32 0 
  %31 = load  i1 (i8*, {i1*, i64}*, i64, i64)*, i1 (i8*, {i1*, i64}*, i64, i64)** %30 
  %32 = getelementptr  {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}, {i1 (i8*, {i1*, i64}*, i64, i64)*, i8*}* %27, i32 0, i32 1 
  %33 = load  i8*, i8** %32 
  %34 =  call ccc  i1  %31(i8*  %33, {i1*, i64}*  %5, i64  0, i64  0)  
  ret i32 0 
}
