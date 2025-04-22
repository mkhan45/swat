(module
 (type $0 (func (param i32 i32) (result i32)))
 (type $1 (func (result i32)))
 (type $2 (func (param i32)))
 (type $3 (func (param i32) (result i32)))
 (import "" "" (memory $mem 1))
 (import "" "" (func $print_i32 (param i32)))
 (import "" "" (func $alloc (param i32 i32) (result i32)))
 (export "empty" (func $empty))
 (export "prepend" (func $prepend))
 (export "listrev" (func $listrev))
 (export "add" (func $add))
 (export "two" (func $two))
 (export "one" (func $one))
 (export "succ" (func $succ))
 (export "zero" (func $empty))
 (export "main" (func $0))
 (func $empty (result i32)
  (call $alloc
   (i32.const 0)
   (i32.const 0)
  )
 )
 (func $prepend (param $0 i32) (param $1 i32) (result i32)
  (call $alloc
   (call $alloc
    (local.get $0)
    (local.get $1)
   )
   (i32.const 1)
  )
 )
 (func $listrev (param $0 i32) (param $1 i32) (result i32)
  (block $block2 (result i32)
   (drop
    (block $block1 (result i32)
     (drop
      (block $block (result i32)
       (br_table $block1 $block
        (i32.const 0)
        (i32.sub
         (i32.load offset=4
          (local.get $0)
         )
         (i32.const 1)
        )
       )
      )
     )
     (br $block2
      (local.get $1)
     )
    )
   )
   (local.set $1
    (call $alloc
     (call $alloc
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
   (local.set $0
    (i32.load offset=4
     (i32.load
      (local.get $0)
     )
    )
   )
   (call $print_i32
    (local.get $1)
   )
   (local.get $0)
  )
 )
 (func $add (param $0 i32) (param $1 i32) (result i32)
  (block $block2 (result i32)
   (drop
    (block $block1 (result i32)
     (drop
      (block $block (result i32)
       (br_table $block1 $block
        (i32.const 0)
        (i32.sub
         (i32.load offset=4
          (local.get $1)
         )
         (i32.const 1)
        )
       )
      )
     )
     (br $block2
      (local.get $0)
     )
    )
   )
   (call $succ
    (call $add
     (local.get $0)
     (i32.load
      (local.get $1)
     )
    )
   )
  )
 )
 (func $two (result i32)
  (call $succ
   (call $succ
    (call $empty)
   )
  )
 )
 (func $one (result i32)
  (call $succ
   (call $empty)
  )
 )
 (func $succ (param $0 i32) (result i32)
  (call $alloc
   (local.get $0)
   (i32.const 1)
  )
 )
 (func $0 (param $0 i32) (param $1 i32) (result i32)
  (call $listrev
   (call $prepend
    (call $prepend
     (call $empty)
     (call $succ
      (call $empty)
     )
    )
    (call $two)
   )
   (call $empty)
  )
 )
)
