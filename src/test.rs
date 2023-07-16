use utils::setup_test;
use utils::TestFile;

mod utils;

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
