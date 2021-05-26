FUNCTION vec2_dot(a, b)
    USE vec
    TYPE(VECTOR2), INTENT(IN) :: a, b
    REAL :: vec2_dot

    vec2_dot = (a%x * b%x) + (a%y * b%y)

END FUNCTION vec2_dot