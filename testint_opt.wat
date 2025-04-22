(module
 (type $4 (func (result i32)))
 (type $2 (func (param i32) (result i32)))
 (import "" "mem" (memory $0 1))
 (data $0 "{\"bool\":{\"\'false\":null,\"\'true\":null}}")
 (export "z" (func $4))
 (export "f" (func $5))
 (export "t" (func $6))
 (export "m" (func $7))
 (export "serialize_types" (func $3))
 (func $3 (result i32)
  (memory.init $0
   (i32.const 0)
   (i32.const 0)
   (i32.const 37)
  )
  (i32.const 37)
 )
 (func $4 (result i32)
  (i32.const 0)
 )
 (func $5 (result i32)
  (i32.const 5)
 )
 (func $6 (result i32)
  (i32.const 10)
 )
 (func $7 (param $0 i32) (result i32)
  (if
   (i32.ne
    (i32.load
     (i32.const 4)
    )
    (i32.const 1)
   )
   (then
    (return
     (i32.add
      (call $7
       (i32.sub
        (local.get $0)
        (i32.const 1)
       )
      )
      (local.get $0)
     )
    )
   )
  )
  (i32.const 0)
 )
)
