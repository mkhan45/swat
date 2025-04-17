use wasmtime::*;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();

    match &args[..] {
        [_, f] => {
            println!("Running {f}");
            let mut config = Config::new();
            config.cranelift_opt_level(OptLevel::Speed);

            let engine = Engine::new(&config).unwrap();

            let mut store: Store<()> = Store::new(&engine, ());
            let print_i32 = Func::wrap(&mut store, |x: i32| println!("{x}"));

            let module = Module::from_file(store.engine(), f).unwrap();
            let instance = Instance::new(&mut store, &module, &[print_i32.into()]).unwrap();
            let main = instance.get_typed_func::<(i32, i32), i32>(&mut store, "main").unwrap();
            let res = main.call(&mut store, (1_000_000_000, 0)).unwrap();
            println!("{res}");
        }
        _ => {
            println!("expected wasm file");
        }
    }
}
