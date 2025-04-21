(module
  (type $0 (func))
  (type $1 (func (param i32)))
  (type $2 (func (param i32) (result i32)))
  (type $3 (func (param i32) (result i32 i32)))
  (type $4 (func (result i32)))
  (type $5 (func (param i32 i32)))
  (type $6 (func (param i32 i32) (result i32)))
  (import "" "mem" (memory $0 i32 1))
  (import "" "alloc" (func $0 (type 6)))
  (import "" "free" (func $1 (type 1)))
  (import "" "print_val" (func $2 (type 5)))
  (export "serialize_types" (func 3))
  (func $3 (type 4) (i32.const 0) (i32.const 0) (i32.const 87) (memory.init 0 0) (i32.const 87))
  (func $empty (type 4) (i32.const 0) (i32.const 0) (call 0))
  (func $zero (type 4) (i32.const 0) (i32.const 2) (call 0))
  (func $prepend (type 6) (local.get 0) (local.get 1) (call 0) (i32.const 1) (call 0))
  (func (export "main") (param i32 i32) (result i32)
        (call $prepend (call $prepend (call $empty) (call $zero)) (call $zero))
        (i32.const 1)
        (call 2)
        (i32.const 0)
  )
  (data $0
    "\7b\22\62\69\6e\22\3a\7b\22\27\62\30\22\3a\22\62"
    "\69\6e\22\2c\22\27\62\31\22\3a\22\62\69\6e\22\2c"
    "\22\27\65\22\3a\6e\75\6c\6c\7d\2c\22\6c\69\73\74"
    "\22\3a\7b\22\27\6e\69\6c\22\3a\6e\75\6c\6c\2c\22"
    "\27\63\6f\6e\73\22\3a\5b\22\62\69\6e\22\2c\22\6c"
    "\69\73\74\22\5d\7d\7d"
  )
)
