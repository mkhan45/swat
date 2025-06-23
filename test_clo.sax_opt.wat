(module
 (rec
  (type $5 (sub (struct (field (ref $6)))))
  (type $6 (func (param (ref $5) i32) (result i32)))
 )
 (type $8 (sub $5 (struct (field (ref $6)) (field i32))))
 (type $3 (func (result i32)))
 (type $0 (func (param i32 i32) (result i32)))
 (rec
  (type $10 (sub (struct (field (ref $11)))))
  (type $11 (func (param (ref $10) i32) (result (ref $5))))
 )
 (type $2 (func (param i32 i32)))
 (type $1 (func (param i32)))
 (type $4 (func (param i32) (result i32)))
 (import "sax" "mem" (memory $0 1))
 (import "sax" "print_val" (func $0 (type $2) (param i32 i32)))
 (global $0 (mut i32) (i32.const 0))
 (data $0 "{\"int\":\"int\",\"nat\":{\"\'zero\":null,\"\'succ\":\"nat\"}}")
 (elem declare func $15 $16 $17 $18)
 (export "serialize_types" (func $3))
 (export "main" (func $14))
 (func $1 (type $0) (param $0 i32) (param $1 i32) (result i32)
  (local $2 i32)
  (local $3 i32)
  (local.set $3
   (i32.load
    (local.tee $2
     (global.get $0)
    )
   )
  )
  (i32.store
   (local.get $2)
   (local.get $0)
  )
  (i32.store
   (i32.add
    (local.get $2)
    (i32.const 4)
   )
   (local.get $1)
  )
  (global.set $0
   (i32.add
    (local.get $2)
    (local.get $3)
   )
  )
  (local.get $2)
 )
 (func $2 (type $1) (param $0 i32)
  (i32.store
   (local.get $0)
   (i32.sub
    (global.get $0)
    (local.get $0)
   )
  )
  (global.set $0
   (local.get $0)
  )
 )
 (func $3 (type $3) (result i32)
  (memory.init $0
   (i32.const 0)
   (i32.const 0)
   (i32.const 48)
  )
  (i32.const 48)
 )
 (func $5 (type $0) (param $0 i32) (param $1 i32) (result i32)
  (local $2 i32)
  (local $3 i32)
  (local.set $2
   (i32.load
    (local.get $1)
   )
  )
  (local.set $3
   (i32.load offset=4
    (local.get $1)
   )
  )
  (call $2
   (local.get $1)
  )
  (if
   (i32.ne
    (local.get $3)
    (i32.const 1)
   )
   (then
    (return
     (local.get $0)
    )
   )
  )
  (return_call $7
   (call $5
    (local.get $0)
    (local.get $2)
   )
  )
 )
 (func $6 (type $3) (result i32)
  (return_call $7
   (call $1
    (i32.const 0)
    (i32.const 0)
   )
  )
 )
 (func $7 (type $4) (param $0 i32) (result i32)
  (return_call $1
   (local.get $0)
   (i32.const 1)
  )
 )
 (func $14 (type $3) (result i32)
  (local $0 i32)
  (local $1 (ref $8))
  (local $2 i32)
  (local $3 (ref $5))
  (local.set $0
   (call $15
    (struct.new $5
     (ref.func $15)
    )
    (call $6)
   )
  )
  (local.set $2
   (call_ref $6
    (local.tee $1
     (struct.new $8
      (ref.func $16)
      (call $7
       (call $6)
      )
     )
    )
    (call $6)
    (struct.get $8 0
     (local.get $1)
    )
   )
  )
  (call $0
   (call_ref $6
    (local.tee $3
     (call $18
      (struct.new $10
       (ref.func $18)
      )
      (local.get $0)
     )
    )
    (local.get $2)
    (struct.get $5 0
     (local.get $3)
    )
   )
   (i32.const 1)
  )
  (call $2
   (call $1
    (i32.const 0)
    (i32.const 0)
   )
  )
  (i32.const 0)
 )
 (func $15 (type $6) (param $0 (ref $5)) (param $1 i32) (result i32)
  (return_call $5
   (local.get $1)
   (call $6)
  )
 )
 (func $16 (type $6) (param $0 (ref $5)) (param $1 i32) (result i32)
  (return_call $5
   (struct.get $8 1
    (ref.cast (ref $8)
     (local.get $0)
    )
   )
   (local.get $1)
  )
 )
 (func $17 (type $6) (param $0 (ref $5)) (param $1 i32) (result i32)
  (return_call $5
   (local.get $1)
   (struct.get $8 1
    (ref.cast (ref $8)
     (local.get $0)
    )
   )
  )
 )
 (func $18 (type $11) (param $0 (ref $10)) (param $1 i32) (result (ref $5))
  (struct.new $8
   (ref.func $17)
   (local.get $1)
  )
 )
)
