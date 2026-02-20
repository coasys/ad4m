//! Memory management for the WASM guest side.
//!
//! Provides `alloc`/`dealloc` implementations and helper functions for
//! reading input from and writing output to the host.

use std::alloc::{alloc, dealloc, Layout};

/// Encode a (ptr, len) pair into a single u64 "fat pointer".
#[inline]
pub fn encode_fat_ptr(ptr: u32, len: u32) -> u64 {
    ((ptr as u64) << 32) | (len as u64)
}

/// Decode a fat pointer into (ptr, len).
#[inline]
pub fn decode_fat_ptr(fat: u64) -> (u32, u32) {
    let ptr = (fat >> 32) as u32;
    let len = (fat & 0xFFFF_FFFF) as u32;
    (ptr, len)
}

/// Allocate `size` bytes of memory, returning a pointer.
/// Returns 0 on failure or if size is 0.
///
/// This is exported as `ad4m_alloc` by the macro.
pub fn wasm_alloc(size: u32) -> u32 {
    if size == 0 {
        return 0;
    }
    let layout = match Layout::from_size_align(size as usize, 1) {
        Ok(l) => l,
        Err(_) => return 0,
    };
    let ptr = unsafe { alloc(layout) };
    if ptr.is_null() {
        0
    } else {
        ptr as u32
    }
}

/// Deallocate memory previously allocated by `wasm_alloc`.
///
/// This is exported as `ad4m_dealloc` by the macro.
pub fn wasm_dealloc(ptr: u32, size: u32) {
    if ptr == 0 || size == 0 {
        return;
    }
    let layout = match Layout::from_size_align(size as usize, 1) {
        Ok(l) => l,
        Err(_) => return,
    };
    unsafe {
        dealloc(ptr as *mut u8, layout);
    }
}

/// Read input data written by the host at (ptr, len).
pub fn read_input(ptr: u32, len: u32) -> Vec<u8> {
    if ptr == 0 || len == 0 {
        return Vec::new();
    }
    let slice = unsafe { std::slice::from_raw_parts(ptr as *const u8, len as usize) };
    slice.to_vec()
}

/// Write output data and return a fat pointer for the host to read.
pub fn write_output(data: &[u8]) -> u64 {
    if data.is_empty() {
        return 0;
    }
    let ptr = wasm_alloc(data.len() as u32);
    if ptr == 0 {
        return 0;
    }
    unsafe {
        core::ptr::copy_nonoverlapping(data.as_ptr(), ptr as *mut u8, data.len());
    }
    encode_fat_ptr(ptr, data.len() as u32)
}
