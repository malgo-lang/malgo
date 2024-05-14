use std::io::{self, Read};

use syntax::Definable;

mod name;

mod syntax;

mod closure;

fn main() -> io::Result<()> {
    // Read the stdin until EOF is reached
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();

    // Parse the input as JSON using serde_json
    let program: syntax::Program = serde_json::from_str(&input)?;

    // Check if the program is well-typed
    let mut ctx = syntax::TypeContext::new();
    for var_def in &program.variables {
        var_def.add_to_context(&mut ctx);
    }
    for fun_def in &program.functions {
        fun_def.add_to_context(&mut ctx);
    }
    for ext_def in &program.externals {
        ext_def.add_to_context(&mut ctx);
    }
    for var_def in &program.variables {
        let declared = ctx.type_of(var_def);
        let actual = ctx.type_of(&var_def.value);
        dbg!(&var_def.name);
        dbg!(&declared);
        assert_eq!(declared, actual);
    }
    for fun_def in &program.functions {
        let declared = ctx.type_of(fun_def);
        dbg!(&fun_def.name);
        dbg!(&declared);
        assert!(matches!(declared, syntax::Type::FuncT { .. }));
    }

    let closure = closure::closure_conversion(program);

    match closure {
        Ok(closure) => {
            println!("{:#?}", closure);
        }
        Err(e) => {
            eprintln!("{}", e);
        }
    }

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

    use crate::syntax::Definable;

    #[test]
    fn parse_examples() {
        let examples = "./examples";

        // check if examples exists
        if !std::path::Path::new(examples).exists() {
            panic!("{} does not exist", examples);
        }

        for entry in WalkDir::new(examples).into_iter().filter_map(|e| e.ok()) {
            let f_name = entry.file_name().to_string_lossy();

            if f_name.ends_with(".mlg") {
                let output = Command::new("malgo")
                    .arg("to-ll")
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
                let program: Result<super::syntax::Program, serde_json::Error> =
                    serde_json::from_str(&source);

                match &program {
                    Ok(program) => {
                        let mut ctx = super::syntax::TypeContext::new();
                        for var_def in &program.variables {
                            var_def.add_to_context(&mut ctx);
                        }
                        for fun_def in &program.functions {
                            fun_def.add_to_context(&mut ctx);
                        }
                        for ext_def in &program.externals {
                            ext_def.add_to_context(&mut ctx);
                        }
                        for var_def in &program.variables {
                            let declared = ctx.type_of(var_def);
                            let actual = ctx.type_of(&var_def.value);
                            dbg!(&var_def.name);
                            dbg!(&declared);
                            assert_eq!(declared, actual);
                        }
                        for fun_def in &program.functions {
                            let declared = ctx.type_of(fun_def);
                            dbg!(&fun_def.name);
                            dbg!(&declared);
                            assert!(matches!(declared, super::syntax::Type::FuncT { .. }));
                        }
                    }
                    Err(e) => {
                        panic!("Error on {}: {}", json_path.to_string_lossy(), e);
                    }
                }

                let closure = super::closure::closure_conversion(program.unwrap());

                match closure {
                    Ok(_) => {}
                    Err(e) => {
                        panic!("Error on {}: {}", json_path.to_string_lossy(), e);
                    }
                }
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

                if let Ok(program) = program {
                    let closure = super::closure::closure_conversion(program);

                    if let Err(e) = closure {
                        panic!("Error on {}: {}", entry.path().to_string_lossy(), e);
                    }
                } else if let Err(e) = program {
                    panic!("Error on {}: {}", entry.path().to_string_lossy(), e);
                }
            }
        }
    }
}
