MODULE color
    USE vec
    TYPE(VECTOR3), EXTERNAL :: vec3_cross

    ! RGB color type
    TYPE, PUBLIC :: COLOR3
        REAL :: r, g, b
    END TYPE COLOR3

    ! color system type defined using the CIE gamut method
    TYPE, PUBLIC :: COLORSYSTEM
        REAL :: xR, yR      ! red CIE observation standard
        REAL :: xG, yG      ! green CIE observation standard
        REAL :: xB, yB      ! blue CIE observation standard
        REAL :: xW, yW      ! CIE white point
        REAL :: gamma       ! gamma
    END TYPE COLORSYSTEM
    ! some common standardized color systems
    TYPE(COLORSYSTEM), PARAMETER :: NTSC = COLORSYSTEM(0.67,0.33,0.21,0.71, &
        0.14,0.08,0.3101,0.3162,0)
    TYPE(COLORSYSTEM), PARAMETER :: PAL = COLORSYSTEM(0.64,0.33,0.29,0.6,0.15, &
        0.06,0.3127,0.3291,0)
    TYPE(COLORSYSTEM), PARAMETER :: HDTV = COLORSYSTEM(0.67,0.33,0.21,0.71, &
        0.15,0.06,0.3127,0.329,0)
    TYPE(COLORSYSTEM), PARAMETER :: CIE = COLORSYSTEM(0.7355,0.2645,0.2658, &
        0.7243,0.1669,0.0085,1.0/3.0,1.0/3.0,0)
    TYPE(COLORSYSTEM), PARAMETER :: sRGB = COLORSYSTEM(0.64,0.33,0.3,0.6,0.15, &
        0.06,0.3127,0.329,0)
    TYPE(COLORSYSTEM), PARAMETER :: opRGB = COLORSYSTEM(0.64,0.33,0.21,0.71, &
        0.15,0.06,0.3127,0.329,0)

CONTAINS

! color utilities =============================================================

    ! calculates the emittance at the given wavelength and temperature
    !
    !   arguments (IN) --------------------------------------------------------
    !     lambda : REAL
    !       the wavelength of light to measure in nanometers
    !     temp : REAL
    !       the blackbody temperature in Kelvin
    !
    !   result : REAL ---------------------------------------------------------
    !       the spectral radiance of the blackbody as emissive power per unit 
    !       area per unit solid angle in W.sr^-1.m^-2.Hz^-1
    FUNCTION blackbody_emittance(lambda,temp) RESULT(B)
        REAL, INTENT(IN) :: lambda, temp
        REAL :: B

        B = lambda * 1E-9
        B = (B**5.0) * (EXP(1.4388E-2 / (B * temp)) - 1.0)
        B = 1.19104E-16 / B

    END FUNCTION blackbody_emittance

    ! calculates the RGB color of a blackbody at a given temperature with a
    ! specific precision
    !
    !   arguments (in) --------------------------------------------------------
    !       the colorsystem to translate the color to for RGB representation
    !     temp : REAL
    !       the temperature of the blackbody in Kelvin
    !     precision : REAL
    !       the stepsize for the wavelength in nanometers
    !     
    !   result : TYPE(COLOR3) -------------------------------------------------
    !       the RGB color closest representing the spectrum emitted by the body
    FUNCTION blackbody_to_rgb(system,temp,precision) RESULT(col)
        TYPE(COLORSYSTEM), INTENT(IN) :: system
        REAL, INTENT(IN) :: temp, precision
        TYPE(COLOR3) :: col

        REAL :: lambda, step
        TYPE(VECTOR3) :: color_match, XYZ

        step = precision
        IF (precision.LT.0) step = 5

        lambda = 380
        XYZ = VECTOR3(0,0,0)
        DO
            IF (lambda.GT.780) EXIT

            factor = blackbody_emittance(lambda,temp)
            color_match = cie_color_match(lambda)
            XYZ = XYZ + (factor * color_match)

            lambda = lambda + step
        END DO
        XYZ = XYZ / (XYZ%x + XYZ%y + XYZ%z)

        col = xyz_to_rgb(system,XYZ)

    END FUNCTION blackbody_to_rgb

    ! calculates the CIE observer standard values (x,y,z) represented by the
    ! given wavelength of light
    !
    !   arguments (in) --------------------------------------------------------
    !     lambda : REAL
    !       the wavelength of the light in nanometers
    !
    !   result : TYPE(VECTOR3) ------------------------------------------------
    !       the observer standard values (x,y,z)
    FUNCTION cie_color_match(lambda)
        REAL, INTENT(IN) :: lambda
        TYPE(VECTOR3) :: cie_color_match

        REAL :: x, y, z

        ! the color matching here is approximated by Gaussians rather than the
        ! colortables defined by the CIE standards
        x = gaussian(lambda,1.056,5998.0,379.0,310.0) + &
            gaussian(lambda,0.362,4420.0,160.0,267.0) + &
            gaussian(lambda,-0.065,5011.0,204.0,262.0)
        y = gaussian(lambda,0.821,5688.0,469.0,405.0) + &
            gaussian(lambda,0.286,5309.0,163.0,311.0)
        z = gaussian(lambda,1.270,4370.0,118.0,360.0) + &
            gaussian(lambda,0.681,4590.0,260.0,138.0)

        cie_color_match = VECTOR3(x,y,z)

    END FUNCTION cie_color_match

    ! calculates the RGB values for a given wavelength of light
    !
    !   arguments (in) --------------------------------------------------------
    !     system : TYPE(COLORSYSTEM)
    !       the color system which converts CIE to a specific gamut
    !     lambda : REAL
    !       the wavelength of light in nanometers
    !
    !   result : TYPE(COLOR3)
    !       the closest RGB value of the wavelength
    FUNCTION wavelength_to_color(system,lambda) RESULT(color)
        TYPE(COLORSYSTEM), INTENT(IN) :: system
        REAL, INTENT(IN) :: lambda
        TYPE(COLOR3) :: color

        TYPE(VECTOR3) :: cie_values

        cie_values = cie_color_match(lambda)
        color = xyz_to_rgb(system,cie_values)

    END FUNCTION wavelength_to_color

    ! converts CIE observer standard (x,y,z) to a specific color system's RGB
    !
    !   arguments (IN) --------------------------------------------------------
    !     system : TYPE(COLORSYSTEM)
    !       the color system which determines the gamut in CIE space
    !     xyz : TYPE(VECTOR3)
    !       the CIE observer standard values
    !
    !   result : TYPE(COLOR3)
    !       the RGB output based on the color system's gamut
    FUNCTION xyz_to_rgb(system,xyz) RESULT(color)
        TYPE(COLORSYSTEM), INTENT(IN) :: system
        TYPE(VECTOR3), INTENT(IN) :: xyz
        TYPE(COLOR3) :: color

        TYPE(VECTOR3) :: sysX, sysY, sysZ, sysW
        TYPE(VECTOR3) :: matX, matY, matZ, matW
        
        sysX = VECTOR3(system%xR,system%xG,system%xB)
        sysY = VECTOR3(system%yR,system%yG,system%yB)
        sysZ = VECTOR3(1.0-(system%xR+system%yR),1.0-(system%xG+system%yG), &
            1.0-(system%xB+system%yB))
        sysW = VECTOR3(system%xW,system%yW,1.0-(system%xW+system%yW))

        matX = vec3_cross(sysY,sysZ)
        matY = vec3_cross(sysX,sysZ)
        matZ = vec3_cross(sysX,sysY)
        matW%x = ((matX%x*sysW%x) + (matY%x*sysW%y) + (matZ%x*sysW%z)) / sysW%y
        matW%y = ((matX%y*sysW%x) + (matY%y*sysW%y) + (matZ%y*sysW%z)) / sysW%y
        matW%z = ((matX%z*sysW%x) + (matY%z*sysW%y) + (matZ%z*sysW%z)) / sysW%y

        matX = matX / matW%x
        matY = matY / matW%y
        matZ = matZ / matW%z

        color%r = (matX%x * xyz%x) + (matY%x * xyz%y) + (matZ%x * xyz%z)
        color%g = (matX%y * xyz%x) + (matY%y * xyz%y) + (matZ%y * xyz%z)
        color%b = (matX%z * xyz%x) + (matY%z * xyz%y) + (matZ%z * xyz%z)

    END FUNCTION xyz_to_rgb

END MODULE color