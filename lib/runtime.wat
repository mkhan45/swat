(module
  ;; leave the first byte empty so that we can use 0 as a sentinel value for the 
  ;; freelist being uninitialized
  (global $freelist_head (mut i32) (i32.const 1))
  (func $alloc (param $v1 i32) (param $v2 i32) (result i32)
    (local $current_offset i32)
    (local $next_free_absolute i32)
    (local $memory_size_bytes i32)
    
    ;; Get current freelist head (absolute address)
    (local.set $current_offset (global.get $freelist_head))
    
    ;; Read the value in first spot of current cell
    (local.set $next_free_absolute (i32.load (local.get $current_offset)))
    
    ;; Check if the value is zero
    (if (i32.eqz (local.get $next_free_absolute))
      (then
        ;; If zero, next free cell is current cell + 8 bytes
        (local.set $next_free_absolute 
          (i32.add (local.get $current_offset) (i32.const 8)))
        
        ;; Check if we need to grow memory
        (local.set $memory_size_bytes 
          (i32.mul (memory.size) (i32.const 65536)))
        
        ;; If next allocation would exceed memory, grow by 1 page
        (if (i32.ge_u (local.get $next_free_absolute) (local.get $memory_size_bytes))
          (then
            (drop (memory.grow (i32.const 1)))
          )
        )
      )
    )
    
    ;; Store v1 at current offset
    (i32.store (local.get $current_offset) (local.get $v1))
    
    ;; Store v2 at current offset + 4
    (i32.store 
      (i32.add (local.get $current_offset) (i32.const 4)) 
      (local.get $v2))
    
    ;; Update freelist head to the absolute address of next free cell
    (global.set $freelist_head (local.get $next_free_absolute))
    
    ;; Return the allocated offset
    (local.get $current_offset)
  )
  (func $free (param $offset i32)
    ;; Store absolute address of current freelist head in the freed cell
    (i32.store (local.get $offset) (global.get $freelist_head))
    
    ;; Update freelist head to the newly freed cell
    (global.set $freelist_head (local.get $offset))
  )
)
