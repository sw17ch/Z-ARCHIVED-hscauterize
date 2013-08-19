(cauterize "example" "0.0.1"

  (scalar foo uint8_t)
  (scalar bar uint32_t)
  (scalar some_float32 float32_t)
  (scalar some_float64 float64_t)
  
  (enumeration meep
               (value a)
               (value b)
               (value c 40))

  (fixed fixed_meeps meep 64)
  (bounded bounded_meeps meeps 64)
  (composite some_arrays
             (field fixed_field fixed_array)
             (field bounded_field bounded_array))
  (group some_types
         (field an_enum meep)
         (field a_scalar foo)))
