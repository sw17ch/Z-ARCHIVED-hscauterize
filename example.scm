(cauterize "example" "0.0.1"

  (scalar foo uint8)
  (scalar bar uint32)
  (scalar some_float32 float32)
  (scalar some_float64 float64)
  
  (enumeration meep
               (value a)
               (value b)
               (value c 40))

  (fixed fixed_meeps meep 64)
  (bounded bounded_meeps meep 64)
  (composite some_arrays
             (field fixed_field fixed_meeps)
             (field bounded_field bounded_meeps))
  (group some_types
         (field an_enum meep)
         (field a_scalar foo)))
