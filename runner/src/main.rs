use wasmtime::*;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();

    match &args[..] {
        [_, f] => {
            println!("Running {f}");
            let mut config = Config::new();
            config.cranelift_opt_level(OptLevel::Speed);

            let engine = Engine::new(&config).unwrap();

            let mut store: Store<(*mut u8, i32)> = Store::new(&engine, (&mut 0, 0));

            let mem = Memory::new(&mut store, MemoryType::new(1, None)).unwrap();
            let mem_ptr = mem.data_ptr(&store);
            unsafe {
                let sz = mem.data_size(&store) / 4;
                for i in 0..sz {
                    *((mem_ptr as *mut i32).add(i)) = 2;
                }
            }
            *store.data_mut() = (mem_ptr, 0);

            // TODO: figure out when returning wasm addrs, i32 offsets
            let print_i32 = Func::wrap(&mut store, |x: i32| println!("{x}"));
            let alloc = Func::wrap(&mut store, |mut caller: Caller<'_, (*mut u8, i32)>, v1: i32, v2: i32| unsafe {
                let &mut (ref mut base, ref mut cur) = caller.data_mut();
                let base = (*base) as *mut i32;
                let cur_ptr = base.add(*cur as usize);
                let next_free = *cur_ptr;
                *cur_ptr = v1;
                *(cur_ptr.add(1)) = v2;
                let res = *cur;
                *cur += next_free;

                let mut fl = vec![];
                let mut fl_ptr = *cur;
                for _ in 0..10 {
                    fl.push(fl_ptr);
                    let next_offs = *base.add(fl_ptr as usize);
                    fl_ptr += next_offs;
                }
                println!("Alloc'd {res} with ({v1},{v2}) {:?}", fl);
                res
            });
            let free = Func::wrap(&mut store, |mut caller: Caller<'_, (*mut u8, i32)>, offs: i32| unsafe {
                let &mut (ref mut base, ref mut cur) = caller.data_mut();
                let base = (*base) as *mut i32;
                let cur_ptr = base.add(*cur as usize);

                let new_free = base.add(offs as usize);
                let offs_to_cur = *cur - offs;
                *new_free = offs_to_cur;
                *cur = offs;

                let mut fl = vec![];
                let mut fl_ptr = *cur;
                for _ in 0..10 {
                    fl.push(fl_ptr);
                    let next_offs = *base.add(fl_ptr as usize);
                    fl_ptr += next_offs;
                }
                println!("Free'd {offs} {:?}", fl);
            });

            let module = Module::from_file(store.engine(), f).unwrap();
            let instance = Instance::new(&mut store, &module, &[print_i32.into(), mem.into(), alloc.into(), free.into()]).unwrap();
            let main = instance.get_typed_func::<(i32, i32), i32>(&mut store, "main").unwrap();
            let res = main.call(&mut store, (5, 0)).unwrap();
            println!("{res}");

            unsafe {
                let mut mem_buf: Vec<i32> = vec![];
                let mut ptr = mem_ptr as *mut i32;
                for i in 0..30 {
                    mem_buf.push(*(ptr.add(i)));
                }
                println!("mem: {:?}", mem_buf);
            }
        }
        _ => {
            println!("expected wasm file");
        }
    }
}
