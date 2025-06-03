(module
  (rec
    (type $0 (func (param i32 i32) (result i32)))
    (type $1 (func (param i32)))
    (type $2 (func (param i32 i32)))
    (type $3 (func (result i32)))
  )
  (import "sax" "mem" (memory $0 i32 1))
  (import "sax" "alloc" (func $0 (type 0)))
  (import "sax" "free" (func $1 (type 1)))
  (import "sax" "print_val" (func $2 (type 2)))
  (export "serialize_types" (func 3))
  (export "main" (func 5))
  (func $3 (type 3) (i32.const 0) (i32.const 0) (i32.const 48) (memory.init 0 0) (i32.const 48))
  (func $4 (type 3) (i32.const 0) (return))
  (func $5
    (type 3)
    (local i32)
    (i32.const 0)
    (i32.const 0)
    (call 0)
    (local.set 0)
    (local.get 0)
    (i32.const 1)
    (call 0)
    (i32.const 1)
    (call 2)
    (i32.const 0)
    (i32.const 0)
    (call 0)
    (call 1)
    (i32.const 0)
  )
  (data $0
    "\7b\22\69\6e\74\22\3a\22\69\6e\74\22\2c\22\6e\61"
    "\74\22\3a\7b\22\27\7a\65\72\6f\22\3a\6e\75\6c\6c"
    "\2c\22\27\73\75\63\63\22\3a\22\6e\61\74\22\7d\7d"
  )
)
