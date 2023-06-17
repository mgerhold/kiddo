use std::ffi::OsStr;
use std::path::Path;

use bumpalo::Bump;

pub(crate) trait AllocPath {
    fn alloc_path(&self, path: impl AsRef<Path>) -> &Path;
}

impl AllocPath for Bump {
    fn alloc_path(&self, path: impl AsRef<Path>) -> &Path {
        Path::new(unsafe {
            OsStr::from_os_str_bytes_unchecked(
                self.alloc_slice_copy(path.as_ref().as_os_str().as_os_str_bytes()),
            )
        })
    }
}
