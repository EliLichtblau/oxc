use std::path::{Path, PathBuf};

use ignore::{overrides::OverrideBuilder, DirEntry, WalkBuilder};
use oxc_span::VALID_EXTENSIONS;

use crate::IgnoreOptions;

pub struct Walk {
    inner: ignore::Walk,
}

impl Walk {
    /// # Panics
    pub fn new(paths: &[PathBuf], options: &IgnoreOptions) -> Self {
        let mut inner = WalkBuilder::new(&paths[0]);

        if let Some(paths) = paths.get(1..) {
            for path in paths {
                inner.add(path);
            }
        }

        if !options.no_ignore {
            inner.add_custom_ignore_filename(&options.ignore_path);

            if !options.ignore_pattern.is_empty() {
                let mut override_builder = OverrideBuilder::new(Path::new("/"));
                for pattern in &options.ignore_pattern {
                    // Meaning of ignore pattern is reversed
                    // <https://docs.rs/ignore/latest/ignore/overrides/struct.OverrideBuilder.html#method.add>
                    let pattern = format!("!{pattern}");
                    override_builder.add(&pattern).unwrap();
                }
                let overrides = override_builder.build().unwrap();
                inner.overrides(overrides);
            }
        }
        // Turning off `follow_links` because:
        // * following symlinks is a really slow syscall
        // * it is super rare to have symlinked source code
        let inner = inner.ignore(false).git_global(false).follow_links(false).build();
        Self { inner }
    }

    pub fn iter(self) -> impl Iterator<Item = Box<Path>> {
        self.inner
            .filter_map(Result::ok)
            .filter(Self::is_wanted_entry)
            .map(|entry| entry.path().to_path_buf().into_boxed_path())
    }

    fn is_wanted_entry(dir_entry: &DirEntry) -> bool {
        let Some(file_type) = dir_entry.file_type() else { return false };
        if file_type.is_dir() {
            return false;
        }
        let Some(file_name) = dir_entry.path().file_name() else { return false };
        if [".min.", "-min.", "_min."].iter().any(|e| file_name.to_string_lossy().contains(e)) {
            return false;
        }
        let Some(extension) = dir_entry.path().extension() else { return false };
        VALID_EXTENSIONS.contains(&extension.to_string_lossy().as_ref())
    }
}
