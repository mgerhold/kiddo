use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
pub struct CommandLineArguments {
    #[arg(short, long)]
    pub(crate) source_file: PathBuf,

    #[arg(short, long, num_args = 0..)]
    pub(crate) import_paths: Vec<PathBuf>,

    #[arg(short, long)]
    pub(crate) ast_output_path: Option<PathBuf>,
}

impl CommandLineArguments {
    pub fn new(
        source_file: PathBuf,
        import_paths: Vec<PathBuf>,
        ast_output_path: Option<PathBuf>,
    ) -> Self {
        Self {
            source_file,
            import_paths,
            ast_output_path,
        }
    }
}
