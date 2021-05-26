FUNCTION vec3_cross(a, b)
    USE vec
    TYPE(VECTOR3), INTENT(IN) :: a, b
    TYPE(VECTOR3) :: vec3_cross

    vec3_cross%x = (a%y * b%z) - (a%z * b%y)
    vec3_cross%y = (a%z * b%x) - (a%x * b%z)
    vec3_cross%z = (a%x * b%y) - (a%y * b%x)

END FUNCTION vec3_cross