(module
  (global $freelist_head (mut i32) (i32.const 0))
  (func $alloc (param $v1 i32) (param $v2 i32) (result i32)
    (local $current_offset i32)
    (local $next_free_relative i32)
    
    ;; Get current freelist head (absolute offset)
    (local.set $current_offset (global.get $freelist_head))
    
    ;; Read the relative offset to next free cell
    (local.set $next_free_relative (i32.load (local.get $current_offset)))
    
    ;; Store v1 at current offset
    (i32.store (local.get $current_offset) (local.get $v1))
    
    ;; Store v2 at current offset + 4
    (i32.store 
      (i32.add (local.get $current_offset) (i32.const 4)) 
      (local.get $v2))
    
    ;; Update freelist head by adding the relative offset
    (global.set $freelist_head 
      (i32.add (local.get $current_offset) (local.get $next_free_relative)))
    
    ;; Return the allocated offset
    (local.get $current_offset)
  )
  (func $free (param $offset i32)
    (local $relative_offset i32)
    
    ;; Calculate relative offset from freed cell to current freelist head
    (local.set $relative_offset 
      (i32.sub (global.get $freelist_head) (local.get $offset)))
    
    ;; Store relative offset in the freed cell
    (i32.store (local.get $offset) (local.get $relative_offset))
    
    ;; Update freelist head to the newly freed cell
    (global.set $freelist_head (local.get $offset))
  )
)
