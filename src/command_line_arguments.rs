use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
pub(crate) struct CommandLineArguments {
    #[arg(short, long)]
    pub(crate) source_file: PathBuf,

    #[clap(short, long, num_args = 0..)]
    pub(crate) import_paths: Vec<PathBuf>,
}
