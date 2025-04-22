(module
 (type $6 (func (param i32 i32) (result i32)))
 (type $4 (func (result i32)))
 (type $5 (func (param i32 i32)))
 (import "" "mem" (memory $0 1))
 (import "" "alloc" (func $0 (param i32 i32) (result i32)))
 (import "" "print_val" (func $2 (param i32 i32)))
 (data $0 "{\"bin\":{\"\'b0\":\"bin\",\"\'b1\":\"bin\",\"\'e\":null},\"list\":{\"\'nil\":null,\"\'cons\":[\"bin\",\"list\"]}}")
 (export "main" (func $0_12))
 (export "serialize_types" (func $3))
 (func $3 (result i32)
  (memory.init $0
   (i32.const 0)
   (i32.const 0)
   (i32.const 87)
  )
  (i32.const 87)
 )
 (func $prepend (param $0 i32) (param $1 i32) (result i32)
  (call $0
   (call $0
    (local.get $1)
    (local.get $0)
   )
   (i32.const 1)
  )
 )
 (func $zero (result i32)
  (call $0
   (call $0
    (i32.const 0)
    (i32.const 2)
   )
   (i32.const 0)
  )
 )
 (func $one (result i32)
  (call $0
   (call $0
    (i32.const 0)
    (i32.const 2)
   )
   (i32.const 1)
  )
 )
 (func $0_12 (param $0 i32) (param $1 i32) (result i32)
  (local $2 i32)
  (local $3 i32)
  (local.set $0
   (call $zero)
  )
  (local.set $1
   (call $one)
  )
  (local.set $2
   (call $0
    (call $zero)
    (i32.const 1)
   )
  )
  (local.set $3
   (call $0
    (call $one)
    (i32.const 1)
   )
  )
  (call $2
   (call $prepend
    (call $prepend
     (call $prepend
      (call $prepend
       (call $0
        (i32.const 0)
        (i32.const 0)
       )
       (local.get $0)
      )
      (local.get $1)
     )
     (local.get $2)
    )
    (local.get $3)
   )
   (i32.const 1)
  )
  (i32.const 0)
 )
)
