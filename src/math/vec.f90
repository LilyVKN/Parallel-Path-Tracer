MODULE vec
    REAL, PARAMETER :: PI = 4.0 * ATAN(1.0)

    TYPE, PUBLIC :: VECTOR2
        REAL :: x, y
    CONTAINS
        PROCEDURE, PASS(this) :: vec2_op_add
        PROCEDURE, PASS(this) :: vec2_op_div
        PROCEDURE, PASS(this) :: vec2_op_mult_left
        PROCEDURE, PASS(this) :: vec2_op_mult_right
        PROCEDURE, PASS(this) :: vec2_op_subtract
        GENERIC, PUBLIC :: OPERATOR(+) => vec2_op_add
        GENERIC, PUBLIC :: OPERATOR(-) => vec2_op_subtract
        GENERIC, PUBLIC :: OPERATOR(/) => vec2_op_div
        GENERIC, PUBLIC :: OPERATOR(*) => vec2_op_mult_left, vec2_op_mult_right
    END TYPE VECTOR2

    TYPE, PUBLIC :: VECTOR3
        REAL :: x, y, z
    CONTAINS
        PROCEDURE, PASS(this) :: vec3_op_add
        PROCEDURE, PASS(this) :: vec3_op_div
        PROCEDURE, PASS(this) :: vec3_op_mult_left
        PROCEDURE, PASS(this) :: vec3_op_mult_right
        PROCEDURE, PASS(this) :: vec3_op_subtract
        GENERIC, PUBLIC :: OPERATOR(+) => vec3_op_add
        GENERIC, PUBLIC :: OPERATOR(-) => vec3_op_subtract
        GENERIC, PUBLIC :: OPERATOR(/) => vec3_op_div
        GENERIC, PUBLIC :: OPERATOR(*) => vec3_op_mult_left, vec3_op_mult_right
    END TYPE VECTOR3

CONTAINS
! vector2 procedures ==========================================================

    PURE FUNCTION vec2_op_add(lhs,this)
        TYPE(VECTOR2), INTENT(IN) :: lhs
        CLASS(VECTOR2), INTENT(IN) :: this
        TYPE(VECTOR2) :: vec2_op_add

        vec2_op_add = VECTOR2(lhs%x+this%x,lhs%y+this%y)

    END FUNCTION vec2_op_add

    PURE FUNCTION vec2_op_subtract(lhs,this)
        TYPE(VECTOR2), INTENT(IN) :: lhs
        CLASS(VECTOR2), INTENT(IN) :: this
        TYPE(VECTOR2) :: vec2_op_subtract

        vec2_op_subtract = VECTOR2(lhs%x-this%x,lhs%y-this%y)

    END FUNCTION vec2_op_subtract

    PURE FUNCTION vec2_op_div(this,rhs)
        CLASS(VECTOR2), INTENT(IN) :: this
        REAL, INTENT(IN) :: rhs
        TYPE(VECTOR2) :: vec2_op_div

        vec2_op_div = VECTOR2(this%x/rhs,this%y/rhs)

    END FUNCTION vec2_op_div

    PURE FUNCTION vec2_op_mult_left(lhs,this)
        REAL, INTENT(IN) :: lhs
        CLASS(VECTOR2), INTENT(IN) :: this
        TYPE(VECTOR2) :: vec2_op_mult_left

        vec2_op_mult_left = VECTOR2(lhs*this%x,lhs*this%y)

    END FUNCTION vec2_op_mult_left

    PURE FUNCTION vec2_op_mult_right(this,rhs)
        CLASS(VECTOR2), INTENT(IN) :: this
        REAL, INTENT(IN) :: rhs
        TYPE(VECTOR2) :: vec2_op_mult_right

        vec2_op_mult_right = VECTOR2(this%x*rhs,this%y*rhs)

    END FUNCTION vec2_op_mult_right

! vector3 procedures ==========================================================

    PURE FUNCTION vec3_op_add(lhs,this)
        TYPE(VECTOR3), INTENT(IN) :: lhs
        CLASS(VECTOR3), INTENT(IN) :: this
        TYPE(VECTOR3) :: vec3_op_add

        vec3_op_add = VECTOR3(lhs%x+this%x,lhs%y+this%y,lhs%z+this%z)
        
    END FUNCTION vec3_op_add

    PURE FUNCTION vec3_op_subtract(lhs,this)
        TYPE(VECTOR3), INTENT(IN) :: lhs
        CLASS(VECTOR3), INTENT(IN) :: this
        TYPE(VECTOR3) :: vec3_op_subtract

        vec3_op_subtract = VECTOR3(lhs%x-this%x,lhs%y-this%y,lhs%z-this%z)

    END FUNCTION vec3_op_subtract

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

END MODULE vec