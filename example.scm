(cauterize
  (name "example")
  (version "0.0.1")

  (scalar foo uint8)
  (scalar bar uint32)
  
  (enumeration meep
               (value a)
               (value b)
               (value c 40))

  (fixed fixed_array meep 64)
  (bounded bounded_array fixed_array 64)
  (composite some_arrays
             (field fixed_field fixed_array)
             (field bounded_field bounded_array))
  (group some_types
         (field an_enum meep)
         (field a_scalar foo)))
