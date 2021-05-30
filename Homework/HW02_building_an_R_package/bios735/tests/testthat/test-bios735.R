test_that("same output as getT0", {
  m <- 5
  n <- 50
  little.n <- n/2
  set.seed(1)
  x <- matrix(rnorm(m*n),nrow=m,ncol=n)
  f <- gl(2,little.n)
  getT0 <- function(x, f) {
    ts <- sapply(seq_len(m), function(i) t.test(x[i,] ~ f, var.equal=TRUE)$statistic)
    unname(ts)
  }
  expect_equal(getT0(x,f), getT(x,f))

})

test_that("simple errors for bad input", {

  m <- 5
  n <- 50
  little.n <- n/2
  set.seed(1)
  x <- matrix(rnorm(m*n),nrow=m,ncol=n)
  f1 <- gl(4,(n/4))
  f2 <- gl(2, (little.n+2))
  f3 <- factor(rep(1:2, times=c((n/2+2),(n/2-2))) )

  expect_error(getT(x, f1))
  expect_error(getT(x, f2))
  expect_error(getT(x, f3))

})

