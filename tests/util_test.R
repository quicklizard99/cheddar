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

TestStripWhiteSpace <- function()
{
    stopifnot(''==cheddar:::.StripWhitespace(''))
    stopifnot(''==cheddar:::.StripWhitespace(' '))
    stopifnot(''==cheddar:::.StripWhitespace('   '))
    stopifnot('a'==cheddar:::.StripWhitespace('a'))
    stopifnot('a'==cheddar:::.StripWhitespace('a '))
    stopifnot('a'==cheddar:::.StripWhitespace('a  '))
    stopifnot('a'==cheddar:::.StripWhitespace(' a'))
    stopifnot('a'==cheddar:::.StripWhitespace('  a'))
    stopifnot('a'==cheddar:::.StripWhitespace(' a '))
    stopifnot('a'==cheddar:::.StripWhitespace('  a  '))
    stopifnot('a b c'==cheddar:::.StripWhitespace('a b c'))
    stopifnot('a b c'==cheddar:::.StripWhitespace(' a b c'))
    stopifnot('a b c'==cheddar:::.StripWhitespace(' a b c '))
    stopifnot('\\.[]a b c.$^-+.;/"' == 
              cheddar:::.StripWhitespace(' \\.[]a b c.$^-+.;/" '))
}

TestFormatLM <- function()
{
    # Values to 5 dp, no r squared
    models <- NvMLinearRegressions(TL84)
    res <- sapply(models, FormatLM, dp=5, r.squared=FALSE)
    expected <- expression(all = "y" == -2.68628 ~ "-" ~ 0.82711 * "x" * "" * 
    "" * "", producer = "y" == 2.55834 ~ "-" ~ 0.40715 * "x" * 
    "" * "" * "", invertebrate = "y" == 1.46561 ~ "-" ~ 0.32432 * 
    "x" * "" * "" * "", vert.ecto = "y" == -34.66097 ~ "-" ~ 
    11.62787 * "x" * "" * "" * "")
    stopifnot(res[[1]] == expected[[1]])
    stopifnot(res[[2]] == expected[[2]])
    stopifnot(res[[3]] == expected[[3]])
    stopifnot(res[[4]] == expected[[4]])

    # Values to 2 dp, lots of info
    models <- NvMLinearRegressions(TL84)
    res <- sapply(models, FormatLM, r=TRUE, slope.95.ci=TRUE, 
                  ci.plus.minus.style=TRUE)
    expected <- expression(all = "y" == -2.69 ~ "-" ~ 0.83 * "x" * ("" %+-% 0.1 ~ 
    "(95% CI, n=56)") * ", r = -0.92" * ("," ~ r^2 == 0.84), 
    producer = "y" == 2.56 ~ "-" ~ 0.41 * "x" * ("" %+-% 0.23 ~ 
        "(95% CI, n=31)") * ", r = -0.56" * ("," ~ r^2 == 0.32), 
    invertebrate = "y" == 1.47 ~ "-" ~ 0.32 * "x" * ("" %+-% 
        0.24 ~ "(95% CI, n=22)") * ", r = -0.54" * ("," ~ r^2 == 
        0.29), vert.ecto = "y" == -34.66 ~ "-" ~ 11.63 * "x" * 
        ("" %+-% 63.36 ~ "(95% CI, n=3)") * ", r = -0.92" * ("," ~ 
        r^2 == 0.84))
    stopifnot(res[[1]] == expected[[1]])
    stopifnot(res[[2]] == expected[[2]])
    stopifnot(res[[3]] == expected[[3]])
    stopifnot(res[[4]] == expected[[4]])

    # names other than x and y
    m <- lm(Log10N(TL84) ~ Log10M(TL84))
    res <- FormatLM(m, r=TRUE, slope.95.ci=TRUE, ci.plus.minus.style=TRUE)
    expected <- expression("Log10N(TL84)" == -2.69 ~ "-" ~ 0.83 * "Log10M(TL84)" * 
                        ("" %+-% 0.1 ~ "(95% CI, n=56)") * ", r = -0.92" * ("," ~ 
                        r^2 == 0.84))
    stopifnot(res[[1]] == expected[[1]])
}

