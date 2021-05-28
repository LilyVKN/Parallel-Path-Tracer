MODULE ray
    USE color

    TYPE, PUBLIC :: ray_t
        TYPE(VECTOR3) :: start, end, dir
        REAL :: lambda, intensity
        TYPE(COLOR3) :: color
    CONTAINS
    END TYPE ray_t

CONTAINS

END MODULE ray