use std::env;
use std::ffi::OsStr;
use std::fs::{self, File};
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::{self, Path, PathBuf};

fn main() {
    // This is a hardcoded path to the CommonMark spec because it is not situated in
    // the specs/ directory. It's in an array to easily chain it to the other iterator
    // and make it easy to eventually add other hardcoded paths in the future if needed
    let hardcoded = [
        "third_party/CommonMark/spec.txt",
        "third_party/CommonMark/smart_punct.txt",
        "third_party/GitHub/gfm_table.txt",
        "third_party/GitHub/gfm_strikethrough.txt",
        "third_party/GitHub/gfm_tasklist.txt",
    ];
    let hardcoded_iter = hardcoded.iter().map(PathBuf::from);

    let mut limit = 0;
    while !Path::new("pulldown-cmark").exists() && limit < 50 {
        env::set_current_dir("..").expect("failed to search directories");
        limit += 1;
    }
    let _ = fs::create_dir("guide/src/third_party");
    let _ = fs::create_dir("guide/src/specs");
    env::set_current_dir("pulldown-cmark").expect("failed to enter directory");

    // Create an iterator over the files in the specs/ directory that have a .txt extension
    let mut spec_files = fs::read_dir("specs")
        .expect("Could not find the 'specs' directory")
        .filter_map(Result::ok)
        .map(|d| d.path())
        .filter(|p| p.extension().map(|e| e.to_owned()).is_some())
        .chain(hardcoded_iter)
        .collect::<Vec<_>>();
    // Sort by spec names
    spec_files.sort_by(|p, q| p.file_stem().cmp(&q.file_stem()));
    let spec_files = spec_files;

    for file_path in &spec_files {
        let spec_name = file_path.file_stem().unwrap().to_str().unwrap();
        let section = if file_path.components().next() == Some(path::Component::Normal(OsStr::new("third_party"))) {
            "third_party"
        } else {
            "specs"
        };
        let input = BufReader::new(File::open(&file_path).expect("Could not read the spec file"));
        let mut output = BufWriter::new(File::create(&format!("../guide/src/{section}/{spec_name}.md")).expect("Could not write the spec guide"));
        let mut is_in_test = false;
        for line in input.lines() {
            let line = line.expect("failed to read from spec file");
            if line.starts_with("```````````````````````````````` example") {
                is_in_test = true;
                write!(&mut output, "<div class=pulldown-cmark-example>\n\n````````````````````````````````markdown\n")
                    .expect("failed to write example");
            } else if line == "." && is_in_test {
                write!(&mut output, "````````````````````````````````\n````````````````````````````````html\n")
                    .expect("failed to write example");
            } else if line == "````````````````````````````````" && is_in_test {
                is_in_test = false;
                write!(&mut output, "````````````````````````````````\n\n</div>\n")
                    .expect("failed to write example");
            } else {
                write!(&mut output, "{line}\n").expect("failed to write example");
            }
        }
    }
}
