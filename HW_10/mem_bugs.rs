pub   let val = *ptr; // Dereference before freeing
    drop(ptr); // Free memory
    println!("Value before drop: {}", val); // Compile-safe usage
}

pub fn test2() {
    let buffer = [0u8; 10];
    let input = "Too long string";
    let truncated = &input.as_bytes()[..buffer.len().min(input.len())];
    println!("Buffer content: {}", std::str::from_utf8(truncated).unwrap_or("Invalid UTF-8"));
}

pub fn test3() {
    let ptr = Box::new(42);
    drop(ptr); // First free
    println!("Rust prevents double free at compile time.");
} fn test1() {
    let ptr = Box::new(42); // Safe memory management in Rust
 