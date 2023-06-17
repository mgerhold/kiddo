use bumpalo::Bump;

fn main() {
    let bump_allocator = Bump::new();
    let result = kiddo::main(&bump_allocator);
    if let Err(report) = result {
        report.print_report();
    }
}
