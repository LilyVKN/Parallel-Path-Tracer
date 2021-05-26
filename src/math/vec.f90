MODULE vec

    TYPE, PUBLIC :: VECTOR2
        REAL :: x, y
    END TYPE VECTOR2

    TYPE, PUBLIC :: VECTOR3
        REAL :: x, y, z
    CONTAINS
        PROCEDURE, PASS(this) :: vec3_op_add
        PROCEDURE, PASS(this) :: vec3_op_div
        PROCEDURE, PASS(this) :: vec3_op_mult_left
        PROCEDURE, PASS(this) :: vec3_op_mult_right
        GENERIC, PUBLIC :: OPERATOR(+) => vec3_op_add
        GENERIC, PUBLIC :: OPERATOR(/) => vec3_op_div
        GENERIC, PUBLIC :: OPERATOR(*) => vec3_op_mult_left
        GENERIC, PUBLIC :: OPERATOR(*) => vec3_op_mult_right
    END TYPE VECTOR3

CONTAINS

! vector2 procedures ==========================================================

    FUNCTION vec2_dot(a, b)
        TYPE(VECTOR2), INTENT(IN) :: a, b
        REAL :: vec2_dot

        vec2_dot = (a%x * b%x) + (a%y * b%y)

    END FUNCTION vec2_dot

! vector3 procedures ==========================================================

    PURE FUNCTION vec3_op_add(lhs,this)
        TYPE(VECTOR3), INTENT(IN) :: lhs
        CLASS(VECTOR3), INTENT(IN) :: this
        TYPE(VECTOR3) :: vec3_op_add

        vec3_op_add = VECTOR3(lhs%x+this%x,lhs%y+this%y,lhs%z+this%z)
        
    END FUNCTION vec3_op_add

    PURE FUNCTION vec3_op_div(this,rhs)
        CLASS(VECTOR3), INTENT(IN) :: this
        REAL, INTENT(IN) :: rhs
        TYPE(VECTOR3) :: vec3_op_div

        vec3_op_div = VECTOR3(this%x/rhs,this%y/rhs,this%z/rhs)

    END FUNCTION vec3_op_div

    PURE FUNCTION vec3_op_mult_left(lhs,this)
        REAL, INTENT(IN) :: lhs
        CLASS(VECTOR3), INTENT(IN) :: this
        TYPE(VECTOR3) :: vec3_op_mult_left

        vec3_op_mult_left = VECTOR3(lhs*this%x,lhs*this%y,lhs*this%z)

    END FUNCTION vec3_op_mult_left

    PURE FUNCTION vec3_op_mult_right(this,rhs)
        CLASS(VECTOR3), INTENT(IN) :: this
        REAL, INTENT(IN) :: rhs
        TYPE(VECTOR3) :: vec3_op_mult_right

        vec3_op_mult_right = VECTOR3(this%x*rhs,this%y*rhs,this%z*rhs)

    END FUNCTION vec3_op_mult_right

    FUNCTION vec3_op_dot(this,a)
        CLASS(VECTOR3), INTENT(IN) :: this
        TYPE(VECTOR3), INTENT(IN) :: a
        REAL :: vec3_op_dot

        vec3_op_dot = vec3_dot(this,a)

    END FUNCTION vec3_op_dot

    FUNCTION vec3_dot(a, b)
        TYPE(VECTOR3), INTENT(IN) :: a, b
        REAL :: vec3_dot

        vec3_dot = (a%x * b%x) + (a%y * b%y) + (a%z * b%z)

    END FUNCTION vec3_dot

    FUNCTION vec3_cross(a, b)
        TYPE(VECTOR3), INTENT(IN) :: a, b
        TYPE(VECTOR3) :: vec3_cross

        vec3_cross%x = (a%y * b%z) - (a%z * b%y)
        vec3_cross%y = (a%z * b%x) - (a%x * b%z)
        vec3_cross%z = (a%x * b%y) - (a%y * b%x)

    END FUNCTION vec3_cross

END MODULE vec