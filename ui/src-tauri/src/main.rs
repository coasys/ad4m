#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

fn main() {
    app_lib::run();
}
