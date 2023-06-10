fn main() {
    if let Err(report) = kiddo::main() {
        report.print_report();
    }
}
