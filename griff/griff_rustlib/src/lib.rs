use bdwgc_alloc::Allocator;
use std::alloc::Layout;
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
