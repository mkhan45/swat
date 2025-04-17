use wasmtime::*;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();

    match &args[..] {
        [_, f] => {
            println!("Running {f}");
            let mut config = Config::new();
            config.cranelift_opt_level(OptLevel::Speed);

            let engine = Engine::new(&config).unwrap();

            let mut store: Store<*mut u8> = Store::new(&engine, &mut 0);

            let mem = Memory::new(&mut store, MemoryType::new(1, None)).unwrap();
            let mem_ptr = mem.data_ptr(&store);
            *store.data_mut() = mem_ptr;

            let print_i32 = Func::wrap(&mut store, |x: i32| println!("{x}"));
            let alloc = Func::wrap(&mut store, |mut caller: Caller<'_, *mut u8>| unsafe {
                let ptr: *mut i32 = std::mem::transmute(*caller.data());
                *ptr = 42069;
            });

            let module = Module::from_file(store.engine(), f).unwrap();
            let instance = Instance::new(&mut store, &module, &[print_i32.into(), mem.into(), alloc.into()]).unwrap();
            let main = instance.get_typed_func::<(i32, i32), i32>(&mut store, "main").unwrap();
            let res = main.call(&mut store, (5, 0)).unwrap();
            println!("{res}");
        }
        _ => {
            println!("expected wasm file");
        }
    }
}
