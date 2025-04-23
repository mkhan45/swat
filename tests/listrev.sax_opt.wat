(module
 (type $4 (func (result i32)))
 (type $6 (func (param i32 i32) (result i32)))
 (type $5 (func (param i32 i32)))
 (import "" "mem" (memory $0 1))
 (import "" "alloc" (func $0 (param i32 i32) (result i32)))
 (import "" "free" (func $1 (param i32 i32)))
 (import "" "print_val" (func $2 (param i32 i32)))
 (data $0 "{\"int\":\"int\",\"bin\":{\"\'b0\":\"bin\",\"\'b1\":\"bin\",\"\'e\":null},\"list\":{\"\'nil\":null,\"\'cons\":[\"bin\",\"list\"]}}")
 (export "serialize_types" (func $3))
 (export "main" (func $13))
 (func $3 (result i32)
  (memory.init $0
   (i32.const 0)
   (i32.const 0)
   (i32.const 99)
  )
  (i32.const 99)
 )
 (func $4 (param $0 i32) (param $1 i32) (result i32)
  (local $2 i32)
  (local $3 i32)
  (local.set $2
   (i32.load
    (local.get $0)
   )
  )
  (local.set $3
   (i32.load offset=4
    (local.get $0)
   )
  )
  (call $1
   (local.get $0)
   (i32.const 2)
  )
  (if
   (i32.ne
    (local.get $3)
    (i32.const 1)
   )
   (then
    (return
     (local.get $1)
    )
   )
  )
  (local.set $0
   (call $0
    (call $0
     (i32.load
      (local.get $2)
     )
     (local.get $1)
    )
    (i32.const 1)
   )
  )
  (return_call $4
   (i32.load offset=4
    (local.get $2)
   )
   (local.get $0)
  )
 )
 (func $5 (result i32)
  (return_call $0
   (i32.const 0)
   (i32.const 0)
  )
 )
 (func $6 (param $0 i32) (param $1 i32) (result i32)
  (return_call $0
   (call $0
    (local.get $1)
    (local.get $0)
   )
   (i32.const 1)
  )
 )
 (func $7 (result i32)
  (return_call $0
   (call $0
    (i32.const 0)
    (i32.const 2)
   )
   (i32.const 0)
  )
 )
 (func $8 (result i32)
  (return_call $0
   (call $0
    (i32.const 0)
    (i32.const 2)
   )
   (i32.const 1)
  )
 )
 (func $11 (result i32)
  (local $0 i32)
  (local $1 i32)
  (local $2 i32)
  (local $3 i32)
  (local.set $0
   (call $7)
  )
  (local.set $1
   (call $8)
  )
  (local.set $2
   (call $0
    (call $7)
    (i32.const 1)
   )
  )
  (local.set $3
   (call $0
    (call $8)
    (i32.const 1)
   )
  )
  (return_call $6
   (call $6
    (call $6
     (call $6
      (call $5)
      (local.get $0)
     )
     (local.get $1)
    )
    (local.get $2)
   )
   (local.get $3)
  )
 )
 (func $12 (param $0 i32) (param $1 i32) (result i32)
  (local $2 i32)
  (local $3 i32)
  (local.set $2
   (i32.load
    (local.get $0)
   )
  )
  (local.set $3
   (i32.load offset=4
    (local.get $0)
   )
  )
  (call $1
   (local.get $0)
   (i32.const 2)
  )
  (if
   (i32.ne
    (local.get $3)
    (i32.const 1)
   )
   (then
    (return
     (local.get $1)
    )
   )
  )
  (local.set $0
   (call $12
    (i32.load offset=4
     (local.get $2)
    )
    (local.get $1)
   )
  )
  (return_call $0
   (call $0
    (i32.load
     (local.get $2)
    )
    (local.get $0)
   )
   (i32.const 1)
  )
 )
 (func $13 (result i32)
  (local $0 i32)
  (local $1 i32)
  (local $2 i32)
  (local $3 i32)
  (local.set $0
   (call $11)
  )
  (local.set $1
   (call $11)
  )
  (local.set $2
   (call $11)
  )
  (local.set $3
   (call $11)
  )
  (call $2
   (call $4
    (call $12
     (call $12
      (local.get $0)
      (local.get $1)
     )
     (call $12
      (local.get $2)
      (local.get $3)
     )
    )
    (call $5)
   )
   (i32.const 2)
  )
  (call $1
   (call $0
    (i32.const 0)
    (i32.const 0)
   )
   (i32.const 0)
  )
  (i32.const 0)
 )
)
