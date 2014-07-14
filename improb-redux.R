make.pmfs = function(possib, values) {
  stopifnot(is.numeric(values))
  result = matrix(values, nrow=possib)
  # all rows are normalised?
  stopifnot(apply(result, 2, sum) == 1)
  # all entries are non-negative?
  stopifnot(result >= 0)
  result
}

make.rvars = function(possib, values) {
  stopifnot(is.numeric(values))
  matrix(values, ncol=possib, byrow=TRUE)
}

get.expectations = function(rvars, pmfs) {
  rvars %*% pmfs
}

################################################################################
# example
################################################################################

"
pmf = make.pmfs(3, c(0.1, 0.5, 0.4))
rvar = make.rvars(3, c(2, 4, 1))
get.expectations(pmf, rvar)
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
  pmf = make.pmfs(2, c(0.4, 0.6))
  rvar = make.rvars(2, c(3, 9))
  .stopifnotalmostequal(get.expectations(rvar, pmf), 6.6)
}

# more complicated: expectation of three random variables
test.expectation.2 = function() {
  pmf = make.pmfs(2, c(0.4, 0.6))
  rvars = make.rvars(2,
    c(3, 9,
      4, 4,
      0, 3))
  .stopifnotalmostequal(get.expectations(rvars, pmf), c(6.6, 4, 1.8))
}

# more complicated: expectation of multiple random variables with
# respect to multiple pmfs
test.expectation.3 = function() {
  pmfs = make.pmfs(2,
    c(0.4, 0.6,
      0.1, 0.9))
  rvars = make.rvars(2,
    c(3, 9,
      4, 4,
      0, 3))
  .stopifnotalmostequal(
    get.expectations(rvars, pmfs),
    matrix(
      c(6.6, 4, 1.8,
        8.4, 4, 2.7),
      byrow=TRUE, nrow=2)
    )
}

test = function() {
  test.expectation.1()
  test.expectation.2()
  test.expectation.3()
}

test()
