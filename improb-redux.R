# Return a function that calculates the expectation of random variables
# with respect to a set of probability mass functions.
# The function returned takes any set of random variables (specified
# as a simple vector), and will return a matrix, with one row per
# random variable, and one column per probability mass function.
getexpectationsfunc = function(possibsize, pmfvalues) {
  # helper function to convert list of pmf values to a matrix
  .pmfstomatrix = function(possib, pmfvalues) {
    stopifnot(is.numeric(pmfvalues))
    result = matrix(pmfvalues, nrow=possib)
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

  # main function
  pmfmatrix = .pmfstomatrix(possibsize, pmfvalues)
  function(rvarvalues) {
    .rvarstomatrix(possibsize, rvarvalues) %*% pmfmatrix
  }
}

# Return a function which calls getexpectations, and then applies a
# vector function on each row (i.e. to calculate minimum expectation,
# maximum expectation, etc.).
getexpectationsapplyfunc = function(getexpectations, func) {
  function(rvarvalues) {
    apply(getexpectations(rvarvalues), 1, func)
  }
}

# Return a function which evaluates the lower prevision of random variables.
getlowerprevisionsfunc = function(getexpectations) {
  getexpectationsapplyfunc(getexpectations, min)
}

# Return a function which evaluates the upper prevision of random variables.
getupperprevisionsfunc = function(getexpectations) {
  getexpectationsapplyfunc(getexpectations, max)
}

# Return a function which evaluate the "Hurwicz" prevision of random variables.
gethurwiczprevisionsfunc = function(getexpectations, optimism) {
  .hurwicz = function(expectations) {
    optimism * max(expectations) + (1 - optimism) * min(expectations)
  }
  getexpectationsapplyfunc(getexpectations, .hurwicz)
}

# Return a function which tells you which random variables are
# Gamma-maxi-"something", where something is any function of a list of
# expectations (e.g. minimum, maximum, or something in between).
isgammamaxisomethingfunc = function(getsomethingsfunc, tol=1e-10) {
  function(rvarvalues) {
    somethings = getsomethingsfunc(rvarvalues)
    somethings >= (max(somethings) - tol)
  }
}

# Return a function which tells you which random variables are Gamma-maximin.
isgammamaximinfunc = function(getexpectations) {
  isgammamaxisomethingfunc(getlowerprevisionsfunc(getexpectations))
}

# Return a function which tells you which random variables are Gamma-maximax.
isgammamaximaxfunc = function(getexpectations) {
  isgammamaxisomethingfunc(getupperprevisionsfunc(getexpectations))
}

# Return a function which tells you which random variables are Hurwicz optimal.
isgammamaxihurwiczfunc = function(getexpectations, optimism) {
  isgammamaxisomethingfunc(gethurwiczprevisionsfunc(getexpectations, optimism))
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
  gethurwiczprevisions = gethurwiczprevisionsfunc(getexpectations, 0.5)
  isgammamaximin = isgammamaximinfunc(getexpectations)
  isgammamaximax = isgammamaximaxfunc(getexpectations)
  isgammamaxihurwicz = isgammamaxihurwiczfunc(getexpectations, 0.5)
  rvars = c(
    3, 9, 2,
    4, 4, 4,
    0, 3, 6,
    6, 2, 1)
  .stopifnotalmostequal(getlowerprevisions(rvars), c(4, 4, 1.8, 2.3))
  .stopifnotalmostequal(getupperprevisions(rvars), c(7.7, 4, 3, 4.2))
  .stopifnotalmostequal(gethurwiczprevisions(rvars), c(5.85, 4, 2.4, 3.25))
  stopifnot(isgammamaximin(rvars) == c(TRUE, TRUE, FALSE, FALSE))
  stopifnot(isgammamaximax(rvars) == c(TRUE, FALSE, FALSE, FALSE))
  stopifnot(isgammamaxihurwicz(rvars) == c(TRUE, FALSE, FALSE, FALSE))
}

test = function() {
  test.expectation.1()
  test.expectation.2()
  test.expectation.3()
  test.expectation.4()
}

test()
