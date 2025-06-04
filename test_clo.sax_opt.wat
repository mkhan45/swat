(module
 (rec
  (type $5 (sub (struct (field (ref $6)))))
  (type $6 (func (param (ref $5) i32) (result i32)))
 )
 (type $8 (sub $5 (struct (field (ref $6)) (field i32))))
 (type $3 (func (result i32)))
 (type $0 (func (param i32 i32) (result i32)))
 (type $1 (func (param i32)))
 (type $2 (func (param i32 i32)))
 (type $4 (func (param i32) (result i32)))
 (import "sax" "mem" (memory $0 1))
 (import "sax" "alloc" (func $0 (type $0) (param i32 i32) (result i32)))
 (import "sax" "free" (func $1 (type $1) (param i32)))
 (import "sax" "print_val" (func $2 (type $2) (param i32 i32)))
 (data $0 "{\"int\":\"int\",\"nat\":{\"\'zero\":null,\"\'succ\":\"nat\"}}")
 (elem declare func $14 $15)
 (export "serialize_types" (func $3))
 (export "main" (func $13))
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
  (call $1
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
   (call $0
    (i32.const 0)
    (i32.const 0)
   )
  )
 )
 (func $7 (type $4) (param $0 i32) (result i32)
  (return_call $0
   (local.get $0)
   (i32.const 1)
  )
 )
 (func $13 (type $3) (result i32)
  (local $0 (ref $8))
  (call $2
   (call $5
    (call $14
     (struct.new $5
      (ref.func $14)
     )
     (call $6)
    )
    (call_ref $6
     (local.tee $0
      (struct.new $8
       (ref.func $15)
       (call $7
        (call $6)
       )
      )
     )
     (call $6)
     (struct.get $8 0
      (local.get $0)
     )
    )
   )
   (i32.const 1)
  )
  (call $1
   (call $0
    (i32.const 0)
    (i32.const 0)
   )
  )
  (i32.const 0)
 )
 (func $14 (type $6) (param $0 (ref $5)) (param $1 i32) (result i32)
  (return_call $5
   (local.get $1)
   (call $6)
  )
 )
 (func $15 (type $6) (param $0 (ref $5)) (param $1 i32) (result i32)
  (return_call $5
   (struct.get $8 1
    (ref.cast (ref $8)
     (local.get $0)
    )
   )
   (local.get $1)
  )
 )
)
