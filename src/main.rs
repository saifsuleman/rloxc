use clap::Parser;
use std::{
    io::{Read, Write},
    path::PathBuf,
};
use value::Value;

mod chunk;
mod compiler;
mod natives;
mod object;
mod scanner;
mod token;
mod value;
mod vm;

#[derive(Parser)]
struct Cli {
    file: Option<PathBuf>,
}

fn exit(_: &mut [Value]) -> Value {
    println!("Goodbye!");
    std::process::exit(0);
}

fn repl() {
    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();
    let mut vm = vm::VM::new();
    let mut line = String::new();

    vm.define_native("exit", exit);

    loop {
        print!(">>> ");
        stdout.flush().unwrap();
        line.clear();
        stdin.read_line(&mut line).unwrap();
        let result = vm.interpret_source(&line);
        if let Err(e) = result {
            println!("Error: {:?}", e);
        }
    }
}

fn run_file(file: PathBuf) {
    let mut file = std::fs::File::open(file).unwrap();
    let mut source = String::new();
    file.read_to_string(&mut source).unwrap();
    let mut vm = vm::VM::new();
    match vm.interpret_source(&source) {
        Ok(_) => (),
        Err(e) => println!("Error: {:?}", e),
    }
}

fn main() {
    let args = Cli::parse();
    if let Some(file) = args.file {
        run_file(file);
    } else {
        repl();
    }
}
