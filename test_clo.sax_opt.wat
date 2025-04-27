(module
 (type $4 (func (result i32)))
 (type $6 (func (param i32 i32) (result i32)))
 (rec
  (type $7 (func (param (ref $10) i32) (result i32)))
  (type $8 (func (result (ref $10))))
  (type $9 (func (param i32) (result (ref $10))))
  (type $10 (struct (field (ref $7))))
 )
 (type $1 (func (param i32)))
 (type $5 (func (param i32 i32)))
 (type $2 (func (param i32) (result i32)))
 (import "sax" "mem" (memory $0 1))
 (import "sax" "alloc" (func $0 (type $6) (param i32 i32) (result i32)))
 (import "sax" "free" (func $1 (type $1) (param i32)))
 (import "sax" "print_val" (func $2 (type $5) (param i32 i32)))
 (data $0 "{\"int\":\"int\",\"nat\":{\"\'zero\":null,\"\'succ\":\"nat\"}}")
 (elem declare func $12)
 (export "serialize_types" (func $3))
 (export "main" (func $11))
 (func $3 (type $4) (result i32)
  (memory.init $0
   (i32.const 0)
   (i32.const 0)
   (i32.const 48)
  )
  (i32.const 48)
 )
 (func $5 (type $6) (param $0 i32) (param $1 i32) (result i32)
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
 (func $6 (type $4) (result i32)
  (return_call $7
   (call $0
    (i32.const 0)
    (i32.const 0)
   )
  )
 )
 (func $7 (type $2) (param $0 i32) (result i32)
  (return_call $0
   (local.get $0)
   (i32.const 1)
  )
 )
 (func $11 (type $4) (result i32)
  (call $2
   (call $12
    (struct.new $10
     (ref.func $12)
    )
    (call $6)
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
 (func $12 (type $7) (param $0 (ref $10)) (param $1 i32) (result i32)
  (return_call $5
   (local.get $1)
   (block (result i32)
    (call $6)
   )
  )
 )
)
