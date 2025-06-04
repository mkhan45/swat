(module
    (elem declare func $s)
    (rec
      (type $5 (sub (struct (field i32))))
      (type $6 (func (param (ref $5) i32) (result i32)))
    )
    (type $8 (sub $5 (struct (field i32 i32))))
    (func $s (type $6) (local.get 1))
    (func $test (result (ref $5))
        (struct.new $8 (i32.const 0) (i32.const 0)))
)
