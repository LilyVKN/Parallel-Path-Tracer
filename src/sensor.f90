MODULE sensor
    USE ray
    TYPE(VECTOR3), EXTERNAL :: vec3_norm
    PRIVATE

    TYPE, PUBLIC :: sensor_t
        ! sample variables
        TYPE(ray_t), DIMENSION(:), ALLOCATABLE :: samples

        INTEGER, PRIVATE :: samples_per_pixel
        INTEGER, PRIVATE :: width, height
        INTEGER, PRIVATE :: sample_size
        
        ! view variables
        REAL, PUBLIC           :: near_field, fov
        TYPE(VECTOR3), PUBLIC  :: pos, dir
    CONTAINS
        ! workflow procedures
        PROCEDURE, PUBLIC :: init => sensor_init
        PROCEDURE, PUBLIC :: cleanup => sensor_cleanup

        ! get-set procedures
        PROCEDURE, PUBLIC :: set_screen => sensor_set_screen

        ! destructor override
        FINAL :: sensor_destructor
    END TYPE sensor_t

CONTAINS

    ! safely deallocates the sensor sample array
    SUBROUTINE sensor_cleanup(this)
        CLASS(sensor_t), INTENT(INOUT) :: this

        IF (ALLOCATED(this%samples)) DEALLOCATE(this%samples)

    END SUBROUTINE sensor_cleanup

    ! makes sure to call the cleanup function on deletion
    SUBROUTINE sensor_destructor(this)
        TYPE(sensor_t), INTENT(INOUT) :: this

        call this%cleanup()

    END SUBROUTINE sensor_destructor

    ! safely allocates the sample array and fills them with random values
    SUBROUTINE sensor_init(this)
        CLASS(sensor_t), INTENT(INOUT) :: this

        TYPE(VECTOR3) :: sensor_pos, x_axis, y_axis, pixel_pos

        ! make sure to freshly allocate the samples
        call this%cleanup()
        ALLOCATE(this%samples(this%sample_size))
        
        call RANDOM_SEED()

        ! get the axes for calculating the sensors position in world space
        sensor_pos = this%pos + (this%near_field * this%dir)
        x_axis = vec3_cross(this%dir,VECTOR3(0.0,1.0,0.0))
        y_axis = vec3_cross(x_axis,this%dir)
        pixel_size = TAN(this%fov) * this%near_field / (this%width / 2.0)

        ! iterate over all the sample set
        isample = 1
        WRITE(*,*) "pixel size: ", pixel_size
        DO i = 1, this%width
            DO j = 1, this%height
                DO k = 1, this%samples_per_pixel
                    
                    ! get a random pixel-space coordinate
                    call RANDOM_NUMBER(x)
                    call RANDOM_NUMBER(y)

                    ! convert to screen-space
                    x = (pixel_size * (x - 0.5))
                    x = x + ((i - this%width / 2.0) * pixel_size)
                    y = (pixel_size * (y - 0.5))
                    y = y + ((j - this%height / 2.0) * pixel_size)

                    ! convert to world-space
                    pixel_pos = sensor_pos + (x * x_axis) + (y * y_axis)
                    this%samples(isample)%start = pixel_pos

                    ! use the camera's view to calculate the sample's direction
                    this%samples(isample)%dir = vec3_norm(pixel_pos - this%pos)

                    isample = isample + 1
                END DO
            END DO
        END DO

    END SUBROUTINE sensor_init

    ! set the screen values for the sensor
    SUBROUTINE sensor_set_screen(this,samples,width,height)
        CLASS(sensor_t), INTENT(INOUT) :: this
        INTEGER, INTENT(IN) :: samples, width, height

        this%samples_per_pixel = samples
        this%width = width
        this%height = height
        
        this%sample_size = samples * width * height

    END SUBROUTINE sensor_set_screen

END MODULE sensor