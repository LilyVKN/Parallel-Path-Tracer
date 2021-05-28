! returns the normalized 3-dimensional vector of the given vector
FUNCTION vec3_norm(a)
    USE vec
    TYPE(VECTOR3), INTENT(IN) :: a
    TYPE(VECTOR3) :: vec3_norm
    
    REAL :: length

    length = SQRT(vec3_dot(a,a))
    vec3_norm = a / length

END FUNCTION