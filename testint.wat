proc zero:
PushInt 0 d
proc five:
PushInt 5 d
proc ten:
needs local: f1
needs local: f2
Call five () into f1
Init f1
Call five () into f2
Init f2
Get f1
Get f2
Call _add_ (f1, f2) into d
proc sum:
needs local: n
needs local: one
needs local: n
Get n
Call _eqz_ (n) into tst
Init tst
Switch (tst)
Case 0:
Alias u to tst.inj
PushInt 1 one
Init one
Get n
Get one
Call _sub_ (n, one) into n1
Init n1
Get n1
Call sum (n1) into nxt
Init nxt
Get nxt
Get n
Call _add_ (nxt, n) into d
Case 1:
Alias u to tst.inj
PushInt 0 d
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
  (func $3 (type 4) (i32.const 0) (i32.const 0) (i32.const 37) (memory.init 0 0) (i32.const 37))
  (func $4 (type 4) (i32.const 0))
  (func $5 (type 4) (i32.const 5))
  (func $6 (type 4) (local i32 i32) (call 5) (local.set 0) (call 5) (local.set 1) (local.get 0) (local.get 1) (i32.add))
  (func $7
    (type 2)
    (local i32)
    (local.get 0)
    (i32.eqz)
    (block
      (block
        (block (i32.const 0) (i32.load 0 offset=4) (br_table 0 1 0))
        (i32.const 1)
        (local.set 1)
        (local.get 0)
        (local.get 1)
        (i32.sub)
        (call 7)
        (local.get 0)
        (i32.add)
        (return)
      )
      (i32.const 0)
      (return)
    )
    (i32.const 0)
    (i32.const 0)
  )
  (data $0
    "\7b\22\62\6f\6f\6c\22\3a\7b\22\27\66\61\6c\73\65"
    "\22\3a\6e\75\6c\6c\2c\22\27\74\72\75\65\22\3a\6e"
    "\75\6c\6c\7d\7d"
  )
)
