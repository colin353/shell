use std::process::ExitCode;

fn main() -> ExitCode {
    match shell::control::run(std::env::args_os().skip(1)) {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            eprintln!("shellctl: {err}");
            ExitCode::FAILURE
        }
    }
}
