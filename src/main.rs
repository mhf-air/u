use std::env;
use std::error::Error;

/**
usage:
    u ${cmd}
*/
fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let cmd = u::Config::new(args);
    if let Err(err) = cmd.run() {
        let a = format!("{}", err.to_string());
        return Err(Box::<dyn Error>::from(a));
    }
    Ok(())
}
