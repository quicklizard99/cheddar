TestPredationMatrixToLinks <- function()
{
    n <- paste('S',1:10)   # Names for testing

    m <- matrix(0, ncol=10, nrow=10, dimnames=list(n,n))
    stopifnot(0==nrow(PredationMatrixToLinks(m)))

    m <- matrix(0, ncol=10, nrow=10, dimnames=list(n,n))
    m[1,1] <- 1
    stopifnot(all(c('S 1','S 1') == PredationMatrixToLinks(m)))

    m <- matrix(0, ncol=10, nrow=10, dimnames=list(n,n))
    m[1,1] <- 1
    m[10,10] <- 1
    stopifnot(all(c('S 1','S 10','S 1','S 10') == 
                  PredationMatrixToLinks(m)))

    m <- matrix(0, ncol=10, nrow=10, dimnames=list(n,n))
    m[1,1] <- 1
    m[10,10] <- 1
    m[1,10] <- 1
    stopifnot(all(c('S 1','S 1','S 10', 'S 1','S 10','S 10') == 
                  PredationMatrixToLinks(m)))

    t1 <- PredationMatrixToLinks(PredationMatrix(TL84))
    t2 <- TLPS(TL84)[,c('resource', 'consumer')]
    stopifnot(identical(t1, t2))

    # Illegal values
    m <- matrix(-1, ncol=10, nrow=10, dimnames=list(n,n))
    F(PredationMatrixToLinks(m))
    m <- matrix(NA, ncol=10, nrow=10, dimnames=list(n,n))
    F(PredationMatrixToLinks(m))
    # No names
    F(PredationMatrixToLinks(matrix(0, ncol=10, nrow=10)))
}

