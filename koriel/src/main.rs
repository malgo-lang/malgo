use std::io::{self, Read};

use crate::syntax::HasType;

mod closure;
mod eval;
mod name;
mod syntax;

fn main() -> io::Result<()> {
    // Read the stdin until EOF is reached
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();

    // Parse the input as JSON using serde_json
    let program: syntax::Program = serde_json::from_str(&input)?;

    // Check if the program is well-typed
    for var_def in &program.variables {
        let declared = var_def.get_type();
        let actual = var_def.value.get_type();
        assert_eq!(declared, actual);
    }
    for fun_def in &program.functions {
        let declared = fun_def.get_type();
        assert!(matches!(declared, syntax::Type::FuncT { .. }));
    }

    let closure = closure::closure_conversion(program);

    if let Err(e) = &closure {
        eprintln!("{}", e);
    }

    let mut ctx = eval::Context::new(io::stdin(), io::stdout(), io::stderr());
    eval::register_regular_primitives(&mut ctx);

    // Evaluate the program
    let result = eval::eval_program(&mut ctx, closure.unwrap());

    // Print the result
    println!("{:?}", result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::{
        io::{self, Write},
        path::PathBuf,
        process::Command,
    };

    use walkdir::WalkDir;

    #[test]
    fn eval_examples() {
        let examples = "./examples";

        // check if examples exists
        if !std::path::Path::new(examples).exists() {
            panic!("{} does not exist", examples);
        }

        let golden = golden::GoldenCtx::new("./.golden/eval");

        for entry in WalkDir::new(examples).into_iter().filter_map(|e| e.ok()) {
            let f_name = entry.file_name().to_string_lossy();

            if f_name.ends_with(".mlg") {
                let output = Command::new("malgo")
                    .arg("to-ll")
                    .arg("--only-desugar")
                    .arg(entry.path())
                    .output()
                    .expect("failed to execute process");
                io::stdout().write_all(&output.stdout).unwrap();
                io::stderr().write_all(&output.stderr).unwrap();
                assert!(output.status.success());

                let mut json_path = PathBuf::from(".malgo-work");
                json_path.push(entry.path());
                json_path.set_extension("json");

                let source = std::fs::read_to_string(&json_path).unwrap();

                let program: super::syntax::Program =
                    serde_json::from_str(&source).unwrap_or_else(|e| {
                        panic!("Error on {}: {}", json_path.to_string_lossy(), e);
                    });

                let closure = super::closure::closure_conversion(program).unwrap_or_else(|e| {
                    panic!("Error on {}: {}", json_path.to_string_lossy(), e);
                });

                let mut input = "".as_bytes();
                let mut output: Vec<u8> = Vec::new();
                let mut error: Vec<u8> = Vec::new();
                {
                    let mut ctx = super::eval::Context::new(&mut input, &mut output, &mut error);
                    super::eval::register_regular_primitives(&mut ctx);

                    let _ = super::eval::eval_program(&mut ctx, closure).unwrap_or_else(|e| {
                        panic!("Error on {}: {}", entry.path().to_string_lossy(), e);
                    });
                }

                let content = String::from_utf8(output).unwrap();

                let name = entry.path().file_stem().unwrap().to_string_lossy();
                golden.golden_assert(&name, "txt", &content)
            }
        }
    }

    #[test]
    fn parse_malgo_golden() {
        let json_dir = "../.golden/desugar";

        // check if json_dir exists
        if !std::path::Path::new(json_dir).exists() {
            panic!("{} does not exist", json_dir);
        }

        for entry in WalkDir::new(json_dir).into_iter().filter_map(|e| e.ok()) {
            let f_name = entry.file_name().to_string_lossy();

            if f_name == "golden.json" {
                let input = std::fs::read_to_string(entry.path()).unwrap();
                let program: Result<super::syntax::Program, serde_json::Error> =
                    serde_json::from_str(&input);

                match program {
                    Ok(_) => {}
                    Err(e) => {
                        panic!("Error on {}: {}", entry.path().to_string_lossy(), e);
                    }
                }
            }
        }
    }

    #[test]
    fn cc_malgo_golden() {
        let json_dir = "../.golden/desugar";

        // check if json_dir exists
        if !std::path::Path::new(json_dir).exists() {
            panic!("{} does not exist", json_dir);
        }

        for entry in WalkDir::new(json_dir).into_iter().filter_map(|e| e.ok()) {
            let f_name = entry.file_name().to_string_lossy();

            if f_name == "golden.json" {
                let input = std::fs::read_to_string(entry.path()).unwrap();
                let program: Result<super::syntax::Program, serde_json::Error> =
                    serde_json::from_str(&input);

                if let Err(e) = program {
                    panic!("Error on {}: {}", entry.path().to_string_lossy(), e);
                }

                let program = program.unwrap();

                let closure = super::closure::closure_conversion(program);

                if let Err(e) = closure {
                    panic!("Error on {}: {}", entry.path().to_string_lossy(), e);
                }
            }
        }
    }

    /// Helper module for golden tests
    mod golden {
        use std::{
            io,
            path::{Path, PathBuf},
        };

        pub struct GoldenCtx {
            pub golden_dir: Box<Path>,
        }

        impl GoldenCtx {
            pub fn new(golden_dir: &str) -> Self {
                let golden_dir: Box<Path> = Box::from(Path::new(golden_dir));
                // Create the golden directory if it does not exist
                if !golden_dir.exists() {
                    std::fs::create_dir_all(&*golden_dir).unwrap();
                }
                Self { golden_dir }
            }

            fn read_golden(&self, name: &str, ext: &str) -> io::Result<String> {
                let path = self.get_golden_path(name, ext);
                std::fs::read_to_string(path)
            }

            fn get_golden_path(&self, name: &str, ext: &str) -> PathBuf {
                let mut path: PathBuf = PathBuf::from(&*self.golden_dir);
                path.push(name);
                path.set_extension(ext);
                path
            }

            fn write_golden(&self, name: &str, ext: &str, content: &str) -> io::Result<()> {
                let path = self.get_golden_path(name, ext);
                std::fs::write(path, content)
            }

            pub fn golden_assert(&self, name: &str, ext: &str, content: &str) {
                let golden = self.read_golden(name, ext);
                match golden {
                    Ok(golden) => {
                        similar_asserts::assert_eq!(golden, content);
                    }
                    Err(_) => {
                        self.write_golden(name, ext, content).unwrap();
                        panic!("Golden file not found, creating one");
                    }
                }
            }
        }
    }
}
