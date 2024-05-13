use std::io::{self, Read};

mod name;

mod syntax;

mod closure;

fn main() -> io::Result<()> {
    // Read the stdin until EOF is reached
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();

    // Parse the input as JSON using serde_json
    let json: syntax::Program = serde_json::from_str(&input)?;

    let closure = closure::closure_conversion(json);

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
    use walkdir::WalkDir;

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
}
