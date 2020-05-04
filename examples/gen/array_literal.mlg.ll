source_filename = "./examples/array_literal.mlg"
; ModuleID = './examples/array_literal.mlg'


 


declare external ccc  {}* @newline()    


define external ccc  {}* @newline1()    {
  %1 =  call ccc  {}*  @newline()  
  ret {}* %1 
}


declare external ccc  {}* @print_int(i64)    


define external ccc  {}* @print_int0(i64 )    {
  %2 =  call ccc  {}*  @print_int(i64  %0)  
  ret {}* %2 
}


declare external ccc  void @GC_init()    


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  i32 @main()    {
; <label>:0:
   call ccc  void  @GC_init()  
  %1 = mul   i64 ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64), 10 
  %2 =  call ccc  i8*  @GC_malloc(i64  %1)  
  %3 = bitcast i8* %2 to i64* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64*, i64}* getelementptr inbounds ({i64*, i64}, {i64*, i64}* inttoptr (i32 0 to {i64*, i64}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i64*, i64}* 
  %6 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  store  i64* %3, i64** %6 
  %7 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 1 
  store  i64 10, i64* %7 
  %8 = alloca i64 
  store  i64 0, i64* %8 
  br label %cond_0 
cond_0:
  %9 = load  i64, i64* %8 
  %10 = icmp slt i64 %9, 10 
  br i1 %10, label %body_0, label %end_0 
body_0:
  %11 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %12 = load  i64*, i64** %11 
  %13 = getelementptr  i64, i64* %12, i64 %9 
  store  i64 1, i64* %13 
  %14 = add   i64 %9, 1 
  store  i64 %14, i64* %8 
  br label %cond_0 
end_0:
  %15 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %16 = load  i64*, i64** %15 
  %17 = getelementptr  i64, i64* %16, i64 1 
  store  i64 2, i64* %17 
  %18 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %19 = load  i64*, i64** %18 
  %20 = getelementptr  i64, i64* %19, i64 2 
  store  i64 3, i64* %20 
  %21 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %22 = load  i64*, i64** %21 
  %23 = getelementptr  i64, i64* %22, i64 3 
  store  i64 4, i64* %23 
  %24 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %25 = load  i64*, i64** %24 
  %26 = getelementptr  i64, i64* %25, i64 4 
  store  i64 5, i64* %26 
  %27 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %28 = load  i64*, i64** %27 
  %29 = getelementptr  i64, i64* %28, i64 5 
  store  i64 6, i64* %29 
  %30 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %31 = load  i64*, i64** %30 
  %32 = getelementptr  i64, i64* %31, i64 6 
  store  i64 7, i64* %32 
  %33 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %34 = load  i64*, i64** %33 
  %35 = getelementptr  i64, i64* %34, i64 7 
  store  i64 8, i64* %35 
  %36 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %37 = load  i64*, i64** %36 
  %38 = getelementptr  i64, i64* %37, i64 8 
  store  i64 9, i64* %38 
  %39 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %40 = load  i64*, i64** %39 
  %41 = getelementptr  i64, i64* %40, i64 9 
  store  i64 10, i64* %41 
  %42 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %43 = load  i64*, i64** %42 
  %44 = getelementptr  i64, i64* %43, i64 1 
  %45 = load  i64, i64* %44 
  %46 =  call ccc  {}*  @print_int0(i64  %45)  
  %47 =  call ccc  {}*  @newline1()  
  %48 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %49 = load  i64*, i64** %48 
  %50 = getelementptr  i64, i64* %49, i64 2 
  %51 = load  i64, i64* %50 
  %52 =  call ccc  {}*  @print_int0(i64  %51)  
  %53 =  call ccc  {}*  @newline1()  
  ret i32 0 
}
