use wasmtime::*;
use json::JsonValue;

unsafe fn val_to_string(mem_ptr: *const u8, json: &JsonValue, tp: &JsonValue, ptr: i32) -> String {
    let tp = match tp {
        JsonValue::Short(tpname) => &json[tpname.to_string()],
        JsonValue::String(tpname) => &json[tpname],
        _ => tp
    };

    match tp {
        JsonValue::Null => "()".to_string(),
        JsonValue::Object(cases) => {
            let val_ptr = mem_ptr.add(ptr as usize) as *const i32;
            let inj = *val_ptr;
            let tag = *(val_ptr.add(1));
            let (inj_tag, inj_tp) = cases.iter().nth(tag as usize).unwrap();
            format!("{inj_tag}({})", val_to_string(mem_ptr, json, inj_tp, inj))
        }
        JsonValue::Array(pair) => {
            let val_ptr = mem_ptr.add(ptr as usize) as *const i32;
            let fst = *val_ptr.add(1);
            let snd = *(val_ptr.add(0));
            let fst_tp = &pair[0];
            let snd_tp = &pair[1];
            format!("({}, {})", val_to_string(mem_ptr, json, fst_tp, fst), val_to_string(mem_ptr, json, snd_tp, snd))
        }
        _ => unreachable!(),
    }
}

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
            *store.data_mut() = (mem_ptr, 0);

            let alloc = Func::wrap(&mut store, |mut caller: Caller<'_, (*mut u8, i32)>, v1: i32, v2: i32| unsafe {
                let &mut (ref mut base, ref mut cur) = caller.data_mut();
                let res = *cur;
                let cur_ptr = base.add(*cur as usize) as *mut i32;
                let next_free = *cur_ptr;
                *cur_ptr = v1;
                *(cur_ptr.add(1)) = v2;
                *cur += next_free;

                let mut fl = vec![];
                let mut fl_ptr = *cur;
                let i32_arr = *base as *mut i32;
                for _ in 0..10 {
                    fl.push(fl_ptr);
                    let next_offs = *i32_arr.add(fl_ptr as usize);
                    fl_ptr += next_offs;
                }
                println!("Alloc'd {res} with ({v1},{v2}) {:?}", fl);
                res
            });
            // TODO: fix free
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
            let print_val = Func::wrap(&mut store, |_tp_idx: i32, _ptr: i32| ());

            let module = Module::from_file(store.engine(), f).unwrap();
            let instance = Instance::new(&mut store, &module, &[mem.into(), alloc.into(), free.into(), print_val.into()]).unwrap();

            let serialize_types = instance.get_typed_func::<(), i32>(&mut store, "serialize_types").unwrap();
            let len = serialize_types.call(&mut store, ()).unwrap();
            let string = unsafe { std::str::from_utf8(std::slice::from_raw_parts(mem_ptr as *const u8, len as usize)).unwrap().to_string() };
            let types_json = json::parse(&string).unwrap();

            let print_val = Func::wrap(&mut store, move |caller: Caller<'_, (*mut u8, i32)>, ptr: i32, tp_idx: i32| unsafe {
                let (base, _) = caller.data();
                let (_tpname, tp) = &types_json.entries().nth(tp_idx as usize).unwrap();
                println!("{}", val_to_string(*base, &types_json, &tp, ptr));
            });

            let instance = Instance::new(&mut store, &module, &[mem.into(), alloc.into(), free.into(), print_val.into()]).unwrap();
            unsafe {
                let sz = mem.data_size(&store) / 4;
                for i in 0..sz {
                    *((mem_ptr as *mut i32).add(i)) = 8;
                }
            }

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
