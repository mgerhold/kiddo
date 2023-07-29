use utils::setup_test;
use utils::TestFile;

mod utils;

macro_rules! function_name {
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
            function_name!(),
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
            function_name!(),
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
            function_name!(),
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

#[test]
fn global_variables() {
    insta::assert_snapshot!(
        setup_test(
            function_name!(),
            TestFile {
                filename: "main.bs",
                source: r#"let global0 = 42;
let global1: U32 = 42;
let mutable global2 = 42;
let mutable global3: U32 = 42;
let const global4 = 42;
let const global5: U32 = 42;

export let global6 = 42;
export let global7: U32 = 42;
export let mutable global8 = 42;
export let mutable global9: U32 = 42;
export let const global10 = 42;
export let const global11: U32 = 42;
"#
            },
            &[]
        )
        .compiler_output
    );
}

#[test]
fn local_variables() {
    insta::assert_snapshot!(
        setup_test(
            function_name!(),
            TestFile {
                filename: "main.bs",
                source: r#"function f() {
    let a = 10;
    let const b = 20;
    let mutable c = 30;
    let d: U32 = 40;
    let const e: U32 = 50;
    let mutable f: U32 = 60;
}
"#
            },
            &[]
        )
        .compiler_output
    );
}
