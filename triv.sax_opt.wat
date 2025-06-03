(module
 (rec
  (type $0 (func (param i32 i32) (result i32)))
  (type $1 (func (param i32)))
  (type $2 (func (param i32 i32)))
  (type $3 (func (result i32)))
 )
 (import "sax" "mem" (memory $0 1))
 (import "sax" "alloc" (func $0 (type $0) (param i32 i32) (result i32)))
 (import "sax" "free" (func $1 (type $1) (param i32)))
 (import "sax" "print_val" (func $2 (type $2) (param i32 i32)))
 (data $0 "{\"int\":\"int\",\"nat\":{\"\'zero\":null,\"\'succ\":\"nat\"}}")
 (export "serialize_types" (func $3))
 (export "main" (func $5))
 (func $3 (type $3) (result i32)
  (memory.init $0
   (i32.const 0)
   (i32.const 0)
   (i32.const 48)
  )
  (i32.const 48)
 )
 (func $5 (type $3) (result i32)
  (call $2
   (call $0
    (call $0
     (i32.const 0)
     (i32.const 0)
    )
    (i32.const 1)
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
)
