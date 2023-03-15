use bdwgc_alloc::Allocator;
use std::alloc::Layout;
use std::ffi::{CStr, CString};
use std::io::{Read, Write};
use std::os::raw::c_void;

#[global_allocator]
static GLOBAL_ALLOCATOR: Allocator = Allocator;

const DEFAULT_ALIGNMENT: usize = 8;

#[no_mangle]
pub extern "C" fn malgo_malloc(size: usize) -> *mut c_void {
    (unsafe { std::alloc::alloc(Layout::from_size_align_unchecked(size, DEFAULT_ALIGNMENT)) })
        as *mut c_void
}

#[repr(C)]
/// A struct that represents a Malgo unit.
pub struct MalgoUnit {
    tag: u8,
    payload: (),
}

const MALGO_UNIT: MalgoUnit = MalgoUnit {
    tag: 0,
    payload: (),
};

#[no_mangle]
/// Unsafe type cast.
pub extern "C" fn malgo_unsafe_cast(ptr: *mut c_void) -> *mut c_void {
    ptr
}

#[no_mangle]
/// Panic.
pub extern "C" fn malgo_panic(message: *const u8) -> *mut c_void {
    panic!(
        "panic: {}",
        unsafe { std::ffi::CStr::from_ptr(message) }
            .to_str()
            .unwrap()
    );
}

/// Arithmetic operators
macro_rules! malgo_arith_op {
    ($name:ident, $ty:ty, $op:tt) => {
        #[no_mangle]
        pub extern "C" fn $name(x: $ty, y: $ty) -> $ty {
            x $op y
        }
    };
}

malgo_arith_op!(malgo_add_int32_t, i32, +);
malgo_arith_op!(malgo_sub_int32_t, i32, -);
malgo_arith_op!(malgo_mul_int32_t, i32, *);
malgo_arith_op!(malgo_div_int32_t, i32, /);
malgo_arith_op!(malgo_add_int64_t, i64, +);
malgo_arith_op!(malgo_sub_int64_t, i64, -);
malgo_arith_op!(malgo_mul_int64_t, i64, *);
malgo_arith_op!(malgo_div_int64_t, i64, /);
malgo_arith_op!(malgo_add_float, f32, +);
malgo_arith_op!(malgo_sub_float, f32, -);
malgo_arith_op!(malgo_mul_float, f32, *);
malgo_arith_op!(malgo_div_float, f32, /);
malgo_arith_op!(malgo_add_double, f64, +);
malgo_arith_op!(malgo_sub_double, f64, -);
malgo_arith_op!(malgo_mul_double, f64, *);
malgo_arith_op!(malgo_div_double, f64, /);

/// Comparison operators
macro_rules! malgo_cmp_op {
    ($name:ident, $ty:ty, $op:tt) => {
        #[no_mangle]
        pub extern "C" fn $name(x: $ty, y: $ty) -> i32 {
            (x $op y) as i32
        }
    };
}

malgo_cmp_op!(malgo_eq_int32_t, i32, ==);
malgo_cmp_op!(malgo_ne_int32_t, i32, !=);
malgo_cmp_op!(malgo_lt_int32_t, i32, <);
malgo_cmp_op!(malgo_gt_int32_t, i32, >);
malgo_cmp_op!(malgo_le_int32_t, i32, <=);
malgo_cmp_op!(malgo_ge_int32_t, i32, >=);
malgo_cmp_op!(malgo_eq_int64_t, i64, ==);
malgo_cmp_op!(malgo_ne_int64_t, i64, !=);
malgo_cmp_op!(malgo_lt_int64_t, i64, <);
malgo_cmp_op!(malgo_gt_int64_t, i64, >);
malgo_cmp_op!(malgo_le_int64_t, i64, <=);
malgo_cmp_op!(malgo_ge_int64_t, i64, >=);
malgo_cmp_op!(malgo_eq_float, f32, ==);
malgo_cmp_op!(malgo_ne_float, f32, !=);
malgo_cmp_op!(malgo_lt_float, f32, <);
malgo_cmp_op!(malgo_gt_float, f32, >);
malgo_cmp_op!(malgo_le_float, f32, <=);
malgo_cmp_op!(malgo_ge_float, f32, >=);
malgo_cmp_op!(malgo_eq_double, f64, ==);
malgo_cmp_op!(malgo_ne_double, f64, !=);
malgo_cmp_op!(malgo_lt_double, f64, <);
malgo_cmp_op!(malgo_gt_double, f64, >);
malgo_cmp_op!(malgo_le_double, f64, <=);
malgo_cmp_op!(malgo_ge_double, f64, >=);
malgo_cmp_op!(malgo_eq_char, u8, ==);
malgo_cmp_op!(malgo_ne_char, u8, !=);
malgo_cmp_op!(malgo_lt_char, u8, <);
malgo_cmp_op!(malgo_gt_char, u8, >);
malgo_cmp_op!(malgo_le_char, u8, <=);
malgo_cmp_op!(malgo_ge_char, u8, >=);
malgo_cmp_op!(malgo_eq_string, *const u8, ==);
malgo_cmp_op!(malgo_ne_string, *const u8, !=);
malgo_cmp_op!(malgo_lt_string, *const u8, <);
malgo_cmp_op!(malgo_gt_string, *const u8, >);
malgo_cmp_op!(malgo_le_string, *const u8, <=);
malgo_cmp_op!(malgo_ge_string, *const u8, >=);

/// Char to ord
#[no_mangle]
pub extern "C" fn malgo_char_ord(c: u8) -> i32 {
    c as i32
}

/// Check if a char is a digit
#[no_mangle]
pub extern "C" fn malgo_is_digit(c: u8) -> i32 {
    c.is_ascii_digit() as i32
}

/// Check if a char is a lower case letter
#[no_mangle]
pub extern "C" fn malgo_is_lower(c: u8) -> i32 {
    c.is_ascii_lowercase() as i32
}

/// Check if a char is an upper case letter
#[no_mangle]
pub extern "C" fn malgo_is_upper(c: u8) -> i32 {
    c.is_ascii_uppercase() as i32
}

/// Check if a char is an alphabetical letter or a digit
#[no_mangle]
pub extern "C" fn malgo_is_alphanum(c: u8) -> i32 {
    c.is_ascii_alphanumeric() as i32
}

/// Get the nth char of a string
#[no_mangle]
pub extern "C" fn malgo_string_at(i: isize, s: *const u8) -> u8 {
    unsafe { *s.offset(i) }
}

// Translated from C code below:
// char *malgo_string_cons(char c, char *str)
// {
//   char *new = GC_MALLOC(sizeof(char) * (1 + strlen(str) + 1));
//   new[0] = c;
//   new[1] = '\0';
//   strcat(new, str);
//   return new;
// }
/// Add a charactor to the head of a string
#[no_mangle]
pub extern "C" fn malgo_string_cons(c: u8, s: *const u8) -> *mut u8 {
    let slice = unsafe { CStr::from_ptr(s) };
    CString::new(format!("{}{}", c as char, slice.to_str().unwrap()))
        .unwrap()
        .into_raw()
}

/// Concatenate two strings
#[no_mangle]
pub extern "C" fn malgo_string_append(s1: *const u8, s2: *const u8) -> *mut u8 {
    let slice1 = unsafe { CStr::from_ptr(s1) };
    let slice2 = unsafe { CStr::from_ptr(s2) };
    CString::new(format!(
        "{}{}",
        slice1.to_str().unwrap(),
        slice2.to_str().unwrap()
    ))
    .unwrap()
    .into_raw()
}

/// Get the length of a string
#[no_mangle]
pub extern "C" fn malgo_string_length(s: *const u8) -> isize {
    let slice = unsafe { CStr::from_ptr(s) };
    slice.to_str().unwrap().len() as isize
}

/// Get the substring of a string
#[no_mangle]
pub extern "C" fn malgo_substring(s: *const u8, start: isize, end: isize) -> *mut u8 {
    let slice = unsafe { CStr::from_ptr(s) };
    let str = slice.to_str().unwrap();
    let start = if start < 0 { 0 } else { start as usize };
    let end = if end > str.len() as isize {
        str.len()
    } else {
        end as usize
    };
    CString::new(&str[start..end]).unwrap().into_raw()
}

/// Generate 'to_string' function for a type
macro_rules! malgo_to_string {
    ($name:ident, $type:ty) => {
        #[no_mangle]
        pub extern "C" fn $name(x: $type) -> *mut u8 {
            CString::new(x.to_string()).unwrap().into_raw()
        }
    };
}

malgo_to_string!(malgo_int32_t_to_string, i32);
malgo_to_string!(malgo_int64_t_to_string, i64);
malgo_to_string!(malgo_float_to_string, f32);
malgo_to_string!(malgo_double_to_string, f64);
malgo_to_string!(malgo_char_to_string, u8);

// Translated from C code below:
/*
const MalgoUnit *malgo_exit_failure(MalgoUnit *__attribute__((unused)) unused)
{
  exit(1);
}
 */
/// Exit with failure
#[no_mangle]
pub extern "C" fn malgo_exit_failure(_unused: *const MalgoUnit) -> *const MalgoUnit {
    std::process::exit(1);
}

// Translated from C code below:
// const MalgoUnit *malgo_newline(MalgoUnit *__attribute__((unused)) unused)
// {
//   puts("");
//   return &malgo_unit;
// }
/// Print a newline
#[no_mangle]
pub extern "C" fn malgo_newline(_unused: *const MalgoUnit) -> *const MalgoUnit {
    println!();
    &MALGO_UNIT
}

// Translated from C code below:
// const MalgoUnit *malgo_print_char(char x)
// {
//   printf("%c", x);
//   return &malgo_unit;
// }
/// Print a char
#[no_mangle]
pub extern "C" fn malgo_print_char(x: u8) -> *const MalgoUnit {
    print!("{}", x as char);
    &MALGO_UNIT
}

// Translated from C code below:
// const MalgoUnit *malgo_print_string(char *x)
// {
//   printf("%s", x);
//   return &malgo_unit;
// }
/// Print a string
#[no_mangle]
pub extern "C" fn malgo_print_string(x: *const u8) -> *const MalgoUnit {
    let slice = unsafe { CStr::from_ptr(x) };
    print!("{}", slice.to_str().unwrap());
    &MALGO_UNIT
}

// Translated from C code below:
// const MalgoUnit *malgo_flush(MalgoUnit *__attribute__((unused)) unused)
// {
//   fflush(stdout);
//   return &malgo_unit;
// }
/// Flush stdout
#[no_mangle]
pub extern "C" fn malgo_flush(_unused: *const MalgoUnit) -> *const MalgoUnit {
    std::io::stdout().flush().unwrap();
    &MALGO_UNIT
}

// Translated from C code below:
// char malgo_get_char(MalgoUnit *__attribute__((unused)) unused)
// {
//   return getchar();
// }
/// Get a char from stdin
#[no_mangle]
pub extern "C" fn malgo_get_char(_unused: *const MalgoUnit) -> u8 {
    let mut buf = [0u8; 1];
    std::io::stdin().read_exact(&mut buf).unwrap();
    buf[0]
}

// Translated from C code below:
// char *malgo_get_contents(MalgoUnit *__attribute__((unused)) unused)
// {
//   struct StringBuilder *sb = new_sb();
//
//   int c;
//   while ((c = fgetc(stdin)) != EOF)
//   {
//     sb_putc(sb, c);
//   }
//   return sb_run(sb);
// }
/// Get all contents from stdin
#[no_mangle]
pub extern "C" fn malgo_get_contents(_unused: *const MalgoUnit) -> *mut u8 {
    let mut buf = Vec::new();
    std::io::stdin().read_to_end(&mut buf).unwrap();
    CString::new(buf).unwrap().into_raw()
}

// Translated from C code below:
// void **malgo_new_vector(int64_t len, void *init)
// {
//   void **ptr = GC_MALLOC(sizeof(void *) * len);
//   for (int64_t i = 0; i < len; i++)
//   {
//     ptr[i] = init;
//   }
//   return ptr;
// }
/// Create a new vector
#[no_mangle]
pub extern "C" fn malgo_new_vector(len: i64, init: *mut u8) -> *mut *mut u8 {
    let mut vec = Vec::with_capacity(len as usize);
    for _ in 0..len {
        vec.push(init);
    }
    vec.as_mut_ptr()
}

// Translated from C code below:
// void *malgo_read_vector(int64_t index, void **ptr) { return ptr[index]; }
/// Read a vector
#[no_mangle]
pub extern "C" fn malgo_read_vector(index: i64, ptr: *mut *mut u8) -> *mut u8 {
    unsafe { *ptr.offset(index as isize) }
}

// Translated from C code below:
// const MalgoUnit *malgo_write_vector(int64_t index, void **ptr, void *val)
// {
//   ptr[index] = val;
//   return &malgo_unit;
// }
/// Write a vector
#[no_mangle]
pub extern "C" fn malgo_write_vector(
    index: i64,
    ptr: *mut *mut u8,
    val: *mut u8,
) -> *const MalgoUnit {
    unsafe { *ptr.offset(index as isize) = val };
    &MALGO_UNIT
}
