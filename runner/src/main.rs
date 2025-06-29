use wasmtime::*;
use json::JsonValue;

use std::sync::{Arc, Mutex};

unsafe fn val_to_string(mem_ptr: *const u8, json: &JsonValue, tp: &JsonValue, ptr: i32) -> String {
    match tp {
        JsonValue::Short(tpname) if tpname.as_str() == "int" => {
            // ints are passed directly, they're immutable
            ptr.to_string()
        }
        JsonValue::Short(tpname) => val_to_string(mem_ptr, json, &json[tpname.as_str()], ptr),
        JsonValue::String(tpname) => val_to_string(mem_ptr, json, &json[tpname], ptr),
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
            let fst = *(val_ptr.add(0));
            let snd = *(val_ptr.add(1));
            let fst_tp = &pair[0];
            let snd_tp = &pair[1];
            format!("({}, {})", val_to_string(mem_ptr, json, fst_tp, fst), val_to_string(mem_ptr, json, snd_tp, snd))
        }
        _ => unreachable!(),
    }
}


// cells are either
// [ addr ] [ addr ]
// [ addr ] [ tag ]
// [ int ] [ tag ]
// [ int ] [ int ] ?
// can't really come up with a tagging scheme

fn main() {
    let args = std::env::args().collect::<Vec<_>>();

    match &args[..] {
        [_, f] => {
            println!("Running {f}");
            let mut config = Config::new();
            config.cranelift_opt_level(OptLevel::Speed);
            config.wasm_gc(true);
            config.wasm_function_references(true);
            config.max_wasm_stack(1048576);

            let engine = Engine::new(&config).unwrap();

            let mut store: Store<(*mut u8, i32)> = Store::new(&engine, (&mut 0, 0));

            let mem = Memory::new(&mut store, MemoryType::new(5, None)).unwrap();
            let mem_ptr = mem.data_ptr(&store);
            *store.data_mut() = (mem_ptr, 0);

            let allocs = Arc::new(Mutex::new(0));

            let alloc_alloc = allocs.clone();
            let alloc_free = allocs.clone();
            let alloc = Func::wrap(&mut store, move |mut caller: Caller<'_, (*mut u8, i32)>, v1: i32, v2: i32| unsafe {
                let &mut (ref mut base, ref mut cur) = caller.data_mut();
                let res = *cur;
                let cur_ptr = base.add(*cur as usize) as *mut i32;
                let next_free = *cur_ptr;
                *cur_ptr = v1;
                *(cur_ptr.add(1)) = v2;
                *cur += next_free;

                //*alloc_alloc.lock().unwrap() += 1;
                //println!("Alloc'd {res} with ({v1},{v2})");
                res
            });
            let free = Func::wrap(&mut store, move |mut caller: Caller<'_, (*mut u8, i32)>, offs: i32| unsafe {
                let &mut (ref mut base, ref mut cur) = caller.data_mut();
                
                //free_rec(*base, offs as usize, cur);
                let cur_ptr = base.add(*cur as usize) as *mut i32;
                let cur_snd = base.add(1);

                let new_free = base.add(offs as usize) as *mut i32;
                let offs_to_cur = *cur - offs;
                *new_free = offs_to_cur;
                *cur = offs;

                //*alloc_free.lock().unwrap() -= 1;
                //println!("Free'd {offs} {:?}", fl);
            });
            let print_val = Func::wrap(&mut store, |_tp_idx: i32, _ptr: i32| ());

            let module = Module::from_file(store.engine(), f).unwrap();
            let instance = Instance::new(&mut store, &module, &[mem.into(), print_val.into()]).unwrap();
            //let instance = Instance::new(&mut store, &module, &[mem.into(), alloc.into(), print_val.into()]).unwrap();

            let serialize_types = instance.get_typed_func::<(), i32>(&mut store, "serialize_types").unwrap();
            let len = serialize_types.call(&mut store, ()).unwrap();
            let string = unsafe { std::str::from_utf8(std::slice::from_raw_parts(mem_ptr as *const u8, len as usize)).unwrap().to_string() };
            let types_json = json::parse(&string).unwrap();

            let print_val = Func::wrap(&mut store, move |caller: Caller<'_, (*mut u8, i32)>, ptr: i32, tp_idx: i32| unsafe {
                let (base, _) = caller.data();
                let (_tpname, tp) = &types_json.entries().nth(tp_idx as usize).unwrap();
                println!("{}", val_to_string(*base, &types_json, &tp, ptr));
            });

            let instance = Instance::new(&mut store, &module, &[mem.into(), print_val.into()]).unwrap();
            //let instance = Instance::new(&mut store, &module, &[mem.into(), alloc.into(), print_val.into()]).unwrap();
            unsafe {
                let sz = mem.data_size(&store) / 4;
                for i in 0..sz {
                    *((mem_ptr as *mut i32).add(i)) = 0;
                }
            }

            let main = instance.get_typed_func::<(), i32>(&mut store, "main").unwrap();
            let _res = main.call(&mut store, ()).unwrap();

            //println!("allocs: {}", *allocs.lock().unwrap());
            //unsafe {
            //    let mut mem_buf: Vec<i32> = vec![];
            //    let mut ptr = mem_ptr as *mut i32;
            //    for i in 0..30 {
            //        mem_buf.push(*(ptr.add(i)));
            //    }
            //    println!("mem: {:?}", mem_buf);
            //}
        }
        _ => {
            println!("expected wasm file");
        }
    }
}
