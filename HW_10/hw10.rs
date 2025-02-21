pub fn find_min(a: &[i32]) -> i32 {
    if a.is_empty() {
        panic!("Array is empty");
    }
    *a.iter().min().unwrap()
}

pub fn swap(a: &mut i32, b: &mut i32) {
    if a == b {
        println!("Values are identical, no swap needed");
    } else {
        std::mem::swap(a, b);
    }
}