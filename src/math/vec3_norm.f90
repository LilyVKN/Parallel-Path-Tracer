FUNCTION vec3_norm(a)
    USE vec
    TYPE(VECTOR3), INTENT(IN) :: a
    TYPE(VECTOR3) :: vec3_norm
    
    REAL :: length

    length = vec3_dot(a,a)
    vec3_norm = a / length

END FUNCTION