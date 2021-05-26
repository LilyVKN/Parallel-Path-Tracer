FUNCTION gaussian(x,alpha,mu,sigma_1,sigma_2)
    REAL, INTENT(IN) :: x, alpha, mu, sigma_1, sigma_2
    REAL :: gaussian

    REAL :: sigma
    
    sigma = sigma_2
    IF (x.LT.mu) sigma = sigma_1
    gaussian = alpha * EXP(((x - mu)**2) / (-2 * (sigma**2)))

END FUNCTION gaussian