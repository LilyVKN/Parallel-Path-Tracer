! calculates the value on the Gaussian (non-normalized) distribution with upper
! and lower standard deviations separately defined. if the value is below the
! average, sigma_1 is used; otherwise, sigma_2 is used for the standard
! deviation.
!
!   arguments (in) ------------------------------------------------------------
!     x : REAL
!       the value at which to determine the Gaussian value
!     alpha : REAL
!       the amplitude factor which gets multiplied to the non-normalized
!       Gaussian result
!     mu : REAL
!       the average value (i.e. the peak of the distribution)
!     sigma_1 : REAL
!       the standard deviation for values below the average
!     sigma_2 : REAL
!       the standard deviation for values above the average
!
!   returns : REAL ------------------------------------------------------------
!       the gaussian value times alpha
!
!   notes ---------------------------------------------------------------------
!       for the normal distribution, sigma_1 and sigma_2 should be the same and
!       alpha should be 1 / (SIGMA * SQRT(2 * PI))
FUNCTION gaussian(x,alpha,mu,sigma_1,sigma_2)
    REAL, INTENT(IN) :: x, alpha, mu, sigma_1, sigma_2
    REAL :: gaussian

    REAL :: sigma
    
    sigma = sigma_2
    IF (x.LT.mu) sigma = sigma_1
    gaussian = alpha * EXP(((x - mu)**2) / (-2 * (sigma**2)))

END FUNCTION gaussian