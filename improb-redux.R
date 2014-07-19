makepmfs = function(possib, values) {
  stopifnot(is.numeric(values))
  result = matrix(values, nrow=possib)
  # all rows are normalised?
  stopifnot(apply(result, 2, sum) == 1)
  # all entries are non-negative?
  stopifnot(result >= 0)
  result
}

makervars = function(possib, values) {
  stopifnot(is.numeric(values))
  matrix(values, ncol=possib, byrow=TRUE)
}

getexpectations = function(rvars, pmfs) {
  rvars %*% pmfs
}

################################################################################
# example
################################################################################

"
pmf = makepmfs(3, c(0.1, 0.5, 0.4))
rvar = makervars(3, c(2, 4, 1))
getexpectations(pmf, rvar)
"

################################################################################
# tests
################################################################################

# helper function to check that two matrices are equal within tolerance
.stopifnotalmostequal = function(a, b, tol=1e-10) {
  stopifnot(a + tol >= b)
  stopifnot(b + tol >= a)
}

# simple usage: expectation of a single random variable
test.expectation.1 = function() {
  pmf = makepmfs(2, c(0.4, 0.6))
  rvar = makervars(2, c(3, 9))
  .stopifnotalmostequal(getexpectations(rvar, pmf), 6.6)
}

# more complicated: expectation of three random variables
test.expectation.2 = function() {
  pmf = makepmfs(2, c(0.4, 0.6))
  rvars = makervars(2,
    c(3, 9,
      4, 4,
      0, 3))
  .stopifnotalmostequal(getexpectations(rvars, pmf), c(6.6, 4, 1.8))
}

# more complicated: expectation of multiple random variables with
# respect to multiple pmfs
test.expectation.3 = function() {
  pmfs = makepmfs(3,
    c(0.4, 0.5, 0.1,
      0.1, 0.8, 0.1))
  rvars = makervars(3,
    c(3, 9, 2,
      4, 4, 4,
      0, 3, 6,
      6, 2, 1))
  .stopifnotalmostequal(
    getexpectations(rvars, pmfs),
    matrix(
      c(5.9, 7.7,
        4, 4,
        2.1, 3.0,
        3.5, 2.3),
      byrow=TRUE, nrow=4)
    )
}

test = function() {
  test.expectation.1()
  test.expectation.2()
  test.expectation.3()
}

test()
