(module
 (type $4 (func (result i32)))
 (type $6 (func (param i32 i32) (result i32)))
 (type $5 (func (param i32 i32)))
 (import "" "mem" (memory $0 1))
 (import "" "alloc" (func $0 (param i32 i32) (result i32)))
 (import "" "print_val" (func $1 (param i32 i32)))
 (data $0 "{\"int\":\"int\",\"bin\":{\"\'b0\":\"bin\",\"\'b1\":\"bin\",\"\'e\":null},\"list\":{\"\'nil\":null,\"\'cons\":[\"bin\",\"list\"]}}")
 (export "serialize_types" (func $2))
 (export "main" (func $12))
 (func $2 (result i32)
  (memory.init $0
   (i32.const 0)
   (i32.const 0)
   (i32.const 99)
  )
  (i32.const 99)
 )
 (func $3 (param $0 i32) (param $1 i32) (result i32)
  (if
   (i32.ne
    (i32.load offset=4
     (local.get $0)
    )
    (i32.const 1)
   )
   (then
    (return
     (local.get $1)
    )
   )
  )
  (local.set $1
   (call $0
    (call $0
     (i32.load
      (i32.load
       (local.get $0)
      )
     )
     (local.get $1)
    )
    (i32.const 1)
   )
  )
  (return_call $3
   (i32.load offset=4
    (i32.load
     (local.get $0)
    )
   )
   (local.get $1)
  )
 )
 (func $4 (result i32)
  (call $0
   (i32.const 0)
   (i32.const 0)
  )
 )
 (func $5 (param $0 i32) (param $1 i32) (result i32)
  (call $0
   (call $0
    (local.get $1)
    (local.get $0)
   )
   (i32.const 1)
  )
 )
 (func $6 (result i32)
  (call $0
   (call $0
    (i32.const 0)
    (i32.const 2)
   )
   (i32.const 0)
  )
 )
 (func $7 (result i32)
  (call $0
   (call $0
    (i32.const 0)
    (i32.const 2)
   )
   (i32.const 1)
  )
 )
 (func $10 (result i32)
  (local $0 i32)
  (local $1 i32)
  (local $2 i32)
  (local $3 i32)
  (local.set $0
   (call $6)
  )
  (local.set $1
   (call $7)
  )
  (local.set $2
   (call $0
    (call $6)
    (i32.const 1)
   )
  )
  (local.set $3
   (call $0
    (call $7)
    (i32.const 1)
   )
  )
  (return_call $5
   (call $5
    (call $5
     (call $5
      (call $4)
      (local.get $0)
     )
     (local.get $1)
    )
    (local.get $2)
   )
   (local.get $3)
  )
 )
 (func $11 (param $0 i32) (param $1 i32) (result i32)
  (if
   (i32.ne
    (i32.load offset=4
     (local.get $0)
    )
    (i32.const 1)
   )
   (then
    (return
     (local.get $1)
    )
   )
  )
  (local.set $1
   (call $11
    (i32.load offset=4
     (i32.load
      (local.get $0)
     )
    )
    (local.get $1)
   )
  )
  (call $0
   (call $0
    (i32.load
     (i32.load
      (local.get $0)
     )
    )
    (local.get $1)
   )
   (i32.const 1)
  )
 )
 (func $12 (result i32)
  (local $0 i32)
  (local $1 i32)
  (local $2 i32)
  (local $3 i32)
  (local.set $0
   (call $10)
  )
  (local.set $1
   (call $10)
  )
  (local.set $2
   (call $10)
  )
  (local.set $3
   (call $10)
  )
  (call $1
   (call $3
    (call $11
     (call $11
      (local.get $0)
      (local.get $1)
     )
     (call $11
      (local.get $2)
      (local.get $3)
     )
    )
    (call $4)
   )
   (i32.const 2)
  )
  (call $0
   (i32.const 0)
   (i32.const 0)
  )
 )
)
