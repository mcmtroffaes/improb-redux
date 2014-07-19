# helper function to convert list of pmf values to a matrix
.pmfstomatrix = function(possib, pmfsvalues) {
  stopifnot(is.numeric(pmfsvalues))
  result = matrix(pmfsvalues, nrow=possib)
  # all rows are normalised?
  stopifnot(apply(result, 2, sum) == 1)
  # all entries are non-negative?
  stopifnot(result >= 0)
  result
}

# helper function to convert list of random variable values to a matrix
.rvarstomatrix = function(possib, values) {
  stopifnot(is.numeric(values))
  matrix(values, ncol=possib, byrow=TRUE)
}

getexpectationsfunc = function(possibsize, pmfsvalues) {
  pmfmatrix = .pmfstomatrix(possibsize, pmfsvalues)
  function(rvarsvalues) {
    .rvarstomatrix(possibsize, rvarsvalues) %*% pmfmatrix
  }
}

# helper function to apply a function on expectations
.getexpectationsapplyfunc = function(getexpectations, func) {
  function(rvarsvalues) {
    apply(getexpectations(rvarsvalues), 1, func)
  }
}

getlowerprevisionsfunc = function(getexpectations) {
  .getexpectationsapplyfunc(getexpectations, min)
}

getupperprevisionsfunc = function(getexpectations) {
  .getexpectationsapplyfunc(getexpectations, max)
}

.isgammamaxixxxfunc = function(getxxxsfunc, tol=1e-10) {
  function(rvarsvalues) {
    xxxs = getxxxsfunc(rvarsvalues)
    xxxs >= (max(xxxs) - tol)
  }
}

isgammamaximinfunc = function(getexpectations) {
  .isgammamaxixxxfunc(getlowerprevisionsfunc(getexpectations))
}

isgammamaximaxfunc = function(getexpectations, tol=1e-10) {
  .isgammamaxixxxfunc(getupperprevisionsfunc(getexpectations))
}

################################################################################
# example
################################################################################

"
getexpectations = getexpectationsfunc(3, c(0.1, 0.5, 0.4))
getexpectations(c(2, 4, 1))
"

################################################################################
# tests
################################################################################

# helper function to check that two matrices are equal within tolerance
.stopifnotalmostequal = function(a, b, tol=1e-10) {
  #print("checking")
  #print(a)
  #print(b)
  stopifnot(a + tol >= b)
  stopifnot(b + tol >= a)
}

# simple usage: expectation of a single random variable
test.expectation.1 = function() {
  getexpectations = getexpectationsfunc(2, c(0.4, 0.6))
  .stopifnotalmostequal(getexpectations(c(3, 9)), 6.6)
}

# more complicated: expectation of three random variables
test.expectation.2 = function() {
  getexpectations = getexpectationsfunc(2, c(0.4, 0.6))
  rvars = 
    c(3, 9,
      4, 4,
      0, 3)
  .stopifnotalmostequal(getexpectations(rvars), c(6.6, 4, 1.8))
}

# more complicated: expectation of multiple random variables with
# respect to multiple pmfs
test.expectation.3 = function() {
  getexpectations = getexpectationsfunc(3,
    c(0.4, 0.5, 0.1,
      0.1, 0.8, 0.1))
  rvars = 
    c(3, 9, 2,
      4, 4, 4,
      0, 3, 6,
      6, 2, 1)
  .stopifnotalmostequal(
    getexpectations(rvars),
    matrix(
      c(5.9, 7.7,
        4, 4,
        2.1, 3.0,
        3.5, 2.3),
      byrow=TRUE, nrow=4)
    )
}

test.expectation.4 = function() {
  pmfs = c(
    0.4, 0.5, 0.1, # first probability mass function
    0.1, 0.8, 0.1, # second ...
    0.6, 0.2, 0.2) # third ...
  getexpectations = getexpectationsfunc(3, pmfs) # 3 = size of possibility space
  getlowerprevisions = getlowerprevisionsfunc(getexpectations)
  getupperprevisions = getupperprevisionsfunc(getexpectations)
  isgammamaximin = isgammamaximinfunc(getexpectations)
  isgammamaximax = isgammamaximaxfunc(getexpectations)
  rvars = c(
    3, 9, 2,
    4, 4, 4,
    0, 3, 6,
    6, 2, 1)
  .stopifnotalmostequal(getlowerprevisions(rvars), c(4, 4, 1.8, 2.3))
  .stopifnotalmostequal(getupperprevisions(rvars), c(7.7, 4, 3, 4.2))
  stopifnot(isgammamaximin(rvars) == c(TRUE, TRUE, FALSE, FALSE))
  stopifnot(isgammamaximax(rvars) == c(TRUE, FALSE, FALSE, FALSE))
}

test = function() {
  test.expectation.1()
  test.expectation.2()
  test.expectation.3()
  test.expectation.4()
}

test()
