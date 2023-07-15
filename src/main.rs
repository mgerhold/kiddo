use bumpalo::Bump;

fn print_allocated_bytes(bump_allocator: &Bump) {
    const UNIT_NAMES: &[&str] = &["bytes", "kibytes", "mibytes"];

    let mut size = bump_allocator.allocated_bytes() as f64;
    let mut unit_index = 0;
    for _ in UNIT_NAMES {
        if size < 1024.0 {
            break;
        }
        size /= 1024.0;
        unit_index += 1;
    }
    let unit_name = UNIT_NAMES[unit_index];
    eprintln!("allocated a total of {size:.3} {unit_name}");
}

fn main() {
    let bump_allocator = Bump::new();
    let result = kiddo::main(&bump_allocator);
    print_allocated_bytes(&bump_allocator);
    if let Err(report) = result {
        report.print_report();
    }
}
