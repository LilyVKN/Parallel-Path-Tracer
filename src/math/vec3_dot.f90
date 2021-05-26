FUNCTION vec3_dot(a, b)
    USE vec
    TYPE(VECTOR3), INTENT(IN) :: a, b
    REAL :: vec3_dot

    vec3_dot = (a%x * b%x) + (a%y * b%y) + (a%z * b%z)

END FUNCTION vec3_dot