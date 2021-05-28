! calculates the dot product of two 2-dimensional vectors and returns a REAL
FUNCTION vec2_dot(a, b)
    USE vec
    TYPE(VECTOR2), INTENT(IN) :: a, b
    REAL :: vec2_dot

    vec2_dot = (a%x * b%x) + (a%y * b%y)

END FUNCTION vec2_dot