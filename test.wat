(module
  (import "" "" (func $print_i32 (param i32)))

  (func (export "main") (param i32 i32) (result i32)
    ;; fn x => sum(0..x)
    (block
    	(loop
	    (i32.eqz (local.get 0))
	    (br_if 1)
	    (local.set 1 (i32.add (local.get 1) (local.get 0)))
	    (local.set 0 (i32.sub (local.get 0) (i32.const 1)))
	    (br 0)
	)
    )
    (local.get 1)
  )
  (func (export "test1") (param i32 i32) (result i32)
    ;; fn (x, y) => if (x == 0) then y else y * 2
    (block
	    (i32.eqz (local.get 0))
	    (br_if 0)
	    (local.set 1 (i32.add (local.get 1) (local.get 1)))
    )
    (local.get 1)
  )
)
