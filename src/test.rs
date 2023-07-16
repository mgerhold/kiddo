use std::path::{Path, PathBuf};

use bumpalo::Bump;

use crate::{main, CommandLineArguments};

#[derive(Debug, Clone, Copy)]
struct TestFile<'a> {
    filename: &'a str,
    source: &'a str,
}

const TEST_DIRECTORY: &str = "tests/";
const OUT_FILENAME: &str = "output.ast";

fn get_path(parent_directory_name: &str, filename: &str) -> PathBuf {
    Path::new(TEST_DIRECTORY)
        .join(Path::new(parent_directory_name))
        .join(Path::new(filename))
}

fn write_test_file(parent_directory_name: &str, file: TestFile) {
    let path = get_path(parent_directory_name, file.filename);
    eprintln!("writing file {}", path.display());
    std::fs::write(&path, file.source).unwrap();
}

fn delete_test_file(parent_directory_name: &str, file: TestFile) {
    let path = get_path(parent_directory_name, file.filename);
    match std::fs::remove_file(&path) {
        Ok(_) => {
            eprintln!("removed file {}", path.display())
        }
        Err(_) => {
            eprintln!("failed to remove file {}", path.display())
        }
    }
}

struct TestResult<'a> {
    parent_directory_name: String,
    compiler_output: String,
    main_module: TestFile<'a>,
    other_modules: &'a [TestFile<'a>],
}

impl Drop for TestResult<'_> {
    fn drop(&mut self) {
        for file in std::iter::once(self.main_module).chain(self.other_modules.iter().copied()) {
            delete_test_file(&self.parent_directory_name, file);
        }

        std::fs::remove_file(
            Path::new(TEST_DIRECTORY)
                .join(&self.parent_directory_name)
                .join(OUT_FILENAME),
        )
        .unwrap();

        let directory_to_delete = Path::new(TEST_DIRECTORY).join(&self.parent_directory_name);
        std::fs::remove_dir(directory_to_delete).unwrap();
    }
}

fn setup_test<'a>(
    parent_directory_name: &str,
    main_module: TestFile<'a>,
    other_modules: &'a [TestFile<'a>],
) -> TestResult<'a> {
    let new_directory = Path::new(TEST_DIRECTORY).join(Path::new(parent_directory_name));
    std::fs::create_dir_all(&new_directory).unwrap();

    for file in std::iter::once(main_module).chain(other_modules.iter().copied()) {
        write_test_file(parent_directory_name, file);
    }

    let main_module_path = get_path(parent_directory_name, main_module.filename);
    let out_filename = get_path(parent_directory_name, OUT_FILENAME);

    let bump_allocator = Bump::new();
    let command_line_args = CommandLineArguments::new(
        main_module_path.clone(),
        Vec::new(),
        Some(out_filename.clone()),
    );

    if let Err(error) = main(&bump_allocator, command_line_args) {
        error.print_report(Some(&out_filename));
    }

    TestResult {
        parent_directory_name: String::from(parent_directory_name),
        compiler_output: std::fs::read_to_string(&out_filename).unwrap(),
        main_module,
        other_modules,
    }
}

macro_rules! function {
    () => {{
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);
        name.strip_suffix("::f")
            .unwrap()
            .split("::")
            .last()
            .unwrap()
    }};
}

#[test]
fn empty_source_file_compiles_successfully() {
    insta::assert_snapshot!(
        setup_test(
            function!(),
            TestFile {
                filename: "main.bs",
                source: "",
            },
            &[],
        )
        .compiler_output
    );
}

#[test]
fn importing_symbol_with_the_same_name_as_local_definition_fails() {
    insta::assert_snapshot!(
        setup_test(
            function!(),
            TestFile {
                filename: "main.bs",
                source: r#"from main import f;

export function f() { }
"#,
            },
            &[],
        )
        .compiler_output
    );
}

#[test]
fn importing_symbol_from_other_module() {
    insta::assert_snapshot!(
        setup_test(
            function!(),
            TestFile {
                filename: "main.bs",
                source: r#"from utils import f;
"#,
            },
            &[TestFile {
                filename: "utils.bs",
                source: r#"export function f() { }
"#
            }],
        )
        .compiler_output
    );
}
