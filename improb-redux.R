# helper function to check that two matrices are equal within tolerance
.stopifnotalmostequal = function(a, b, tol=1e-10) {
  stopifnot(a + tol >= b)
  stopifnot(b + tol >= a)
}

###############################################################################
# calculating expectations and functions of expectations
###############################################################################

# Return a function that calculates the expectation of random variables
# with respect to a set of probability mass functions.
# The function returned takes any set of random variables (specified
# as a simple vector), and will return a matrix, with one row per
# random variable, and one column per probability mass function.
getexpectationsfunc = function(possibsize, pmfvalues, tol=1e-10) {
  pmfmatrix = matrix(pmfvalues, nrow=possibsize)
  # all rows are normalised?
  .stopifnotalmostequal(apply(pmfmatrix, 2, sum), 1, tol=tol)
  # all entries are non-negative?
  stopifnot(pmfmatrix >= 0)
  # return function that multiplies the matrices
  function(rvarvalues) {
    matrix(rvarvalues, ncol=possibsize, byrow=TRUE) %*% pmfmatrix
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

# Return a function which applies a pairwise vector function to
# expectations (e.g. to determine maximality, interval dominance,
# etc.). The result is a square matrix, with an entry for each pair of
# random variables.
getexpectationsapplyfunc2 = function(getexpectations, func) {
  function(rvarvalues) {
    expectations = getexpectations(rvarvalues)
    apply(expectations, 1, function(exp1) {
      apply(expectations, 1, function(exp2) { func(exp1, exp2) }) })
  }
}

# Return a function which evaluates the lower prevision.
getlowerprevisionsfunc = function(getexpectations) {
  getexpectationsapplyfunc(getexpectations, min)
}

# Return a function which evaluates the upper prevision.
getupperprevisionsfunc = function(getexpectations) {
  getexpectationsapplyfunc(getexpectations, max)
}

# Return a function which evaluates the "Hurwicz" prevision.
gethurwiczprevisionsfunc = function(getexpectations, optimism) {
  .hurwicz = function(expectations) {
    optimism * max(expectations) + (1 - optimism) * min(expectations)
  }
  getexpectationsapplyfunc(getexpectations, .hurwicz)
}

###############################################################################
# optimality criteria based on the full distribution (no data)
###############################################################################

# Return a function which tells you which random variables are
# Gamma-maxi-"something", where "something" is any function from random
# variables to values (for example a function created with
# getlowerprevisionsfunc, getupperprevisionsfunc, or
# gethurwiczprevisionsfunc).
isgammamaxisomethingfunc = function(getsomethings, tol=1e-10) {
  function(rvarvalues) {
    somethings = getsomethings(rvarvalues)
    somethings >= (max(somethings) - tol)
  }
}

# Partial orders based on expectation values.
bayescompare = function(exp1, exp2, tol=1e-10) {
  all(exp1 > exp2 + tol)
}

waldcompare = function(exp1, exp2, tol=1e-10) {
  all(exp1 + tol >= exp2) && any(exp1 > exp2 + tol)
}

intervalcompare = function(exp1, exp2, tol=1e-10) {
  min(exp1) > max(exp2) + tol
}

# Return maximal elements based on partial order.
# This is *not* the most efficient implementation, but it is simple.
ismaximalfunc = function(getexpectations, compare) {
  function(rvarvalues) {
    getcomparisonmatrix = getexpectationsapplyfunc2(
      getexpectations, compare)
    apply(getcomparisonmatrix(rvarvalues), 1, function(row) { !any(row) })
  }
}

# Return a function which tells you which random variables are Bayes
# maximal (i.e. Bayes maximal with respect to some element of the
# credal set specified).
isrobustbayesfunc = function(getexpectations, tol=1e-10) {
  function(rvarvalues) {
    expectations = getexpectations(rvarvalues)
    # for each probability mass function (column), identify the random
    # variables (rows) that have maximal expectation
    maxexpectations = apply(expectations, 2, function(col) { col >= (max(col) - tol) })
    # for each random variable (row), check if it had maximal
    # expectation with respect to any probability mass function
    apply(maxexpectations, 1, function(row) { any(row) })
  }
}

################################################################################
# Bayes optimality
################################################################################

# Apply Bayes's theorem.
getposteriorpmf = function(paramsize, priorpmf, likelihoodpmf) {
  likelihoodmat = matrix(likelihoodpmf, nrow=paramsize, byrow=TRUE)
  posteriormat = apply(likelihoodmat, 2, function(col) { col * priorpmf })
  c(apply(posteriormat, 2, function(col) { col / sum(col) }))
}

isposteriorbayes = function(paramsize, posteriorpmf, utility) {
  stopifnot(FALSE)
}

################################################################################
# Wald's optimality
################################################################################

# list all strategies in a matrix (one row per strategy, enumerating a
# decision for each possible data value)
getstrategies = function(datasize, decisionsize) {
  args = lapply(1:datasize, function(x) { 1:decisionsize })
  result = as.matrix(expand.grid(args))
  dimnames(result) <- NULL
  result
}

# Get Wald's expected utility for any given strategy.
getwaldutilityfunc = function(datasize, likelihoodpmf, utility) {
  paramsize = length(likelihoodpmf) %/% datasize
  decisionsize = length(utility) %/% paramsize
  utilitymat = matrix(utility, nrow=decisionsize, byrow=TRUE)
  likelihoodmat = matrix(likelihoodpmf, nrow=paramsize, byrow=TRUE)
  getwaldexpectations = sapply(
    1:paramsize, function(param) {
      getexpectationsfunc(datasize, likelihoodmat[param,])
    })

  function(strategy) {
    stopifnot(datasize == length(strategy))
    sapply(
      1:paramsize,
      function(param) {
        rvar = sapply(strategy, function(decision) { utilitymat[decision,param] })
        getwaldexpectations[[param]](rvar)[1,1]
      })
  }
}

getwaldutilities = function(datasize, likelihoodpmf, utility) {
  paramsize = length(likelihoodpmf) %/% datasize
  decisionsize = length(utility) %/% paramsize
  getwaldutility = getwaldutilityfunc(datasize, likelihoodpmf, utility)
  strategies = getstrategies(datasize, decisionsize)
  t(apply(strategies, 1, getwaldutility))
}

iswaldadmissible = function(datasize, likelihoodpmf, utility) {
  paramsize = length(likelihoodpmf) %/% datasize
  decisionsize = length(utility) %/% paramsize
  getexpectations = function(rvarvalues) {
    matrix(rvarvalues, ncol=paramsize, byrow=TRUE)
  }
  ismaximal = ismaximalfunc(getexpectations, waldcompare)
  ismaximal(c(t(getwaldutilities(datasize, likelihoodpmf, utility))))
}

################################################################################
# tests
################################################################################

# simple usage: expectation of a single random variable
test.expectation.1 = function() {
  pmf = c(0.4, 0.6)
  rvar = c(3, 9)
  getexpectations = getexpectationsfunc(2, pmf) # 2 = size of possibility space
  .stopifnotalmostequal(getexpectations(rvar), 6.6)
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
  isgammamaximin = isgammamaxisomethingfunc(getlowerprevisions)
  isgammamaximax = isgammamaxisomethingfunc(getupperprevisions)
  isgammamaxihurwicz = isgammamaxisomethingfunc(gethurwiczprevisions)
  isbayesmaximal = ismaximalfunc(getexpectations, bayescompare)
  isintervalmaximal = ismaximalfunc(getexpectations, intervalcompare)
  isrobustbayes = isrobustbayesfunc(getexpectations)
  rvars = c(
    3, 9, 2,
    4, 4, 4,
    0, 3, 6,
    6, 2, 1)
  .stopifnotalmostequal(
    getexpectations(rvars),
    matrix(
      c(5.9, 7.7, 4,
        4, 4, 4,
        2.1, 3.0, 1.8,
        3.5, 2.3, 4.2),
      byrow=TRUE, nrow=4)
    )
  .stopifnotalmostequal(getlowerprevisions(rvars), c(4, 4, 1.8, 2.3))
  .stopifnotalmostequal(getupperprevisions(rvars), c(7.7, 4, 3, 4.2))
  .stopifnotalmostequal(gethurwiczprevisions(rvars), c(5.85, 4, 2.4, 3.25))
  stopifnot(isgammamaximin(rvars) == c(TRUE, TRUE, FALSE, FALSE))
  stopifnot(isgammamaximax(rvars) == c(TRUE, FALSE, FALSE, FALSE))
  stopifnot(isgammamaxihurwicz(rvars) == c(TRUE, FALSE, FALSE, FALSE))
  stopifnot(isbayesmaximal(rvars) == c(TRUE, TRUE, FALSE, TRUE))
  stopifnot(isintervalmaximal(rvars) == c(TRUE, TRUE, FALSE, TRUE))
  stopifnot(isrobustbayes(rvars) == c(TRUE, FALSE, FALSE, TRUE))
}

test.expectation.5 = function() {
  # from the 2007 paper
  pmfs = c(
    0.28, 0.72,
    0.5, 0.5, # need this convex combination for E-admissibility to be the same as robust Bayes in this problem
    0.7, 0.3)
  getexpectations = getexpectationsfunc(2, pmfs)
  getlowerprevisions = getlowerprevisionsfunc(getexpectations)
  getupperprevisions = getupperprevisionsfunc(getexpectations)
  gethurwiczprevisions = gethurwiczprevisionsfunc(getexpectations, 0.5)
  isgammamaximin = isgammamaxisomethingfunc(getlowerprevisions)
  isgammamaximax = isgammamaxisomethingfunc(getupperprevisions)
  isbayesmaximal = ismaximalfunc(getexpectations, bayescompare)
  isintervalmaximal = ismaximalfunc(getexpectations, intervalcompare)
  isrobustbayes = isrobustbayesfunc(getexpectations)
  rvars = c(
    4, 0,
    0, 4,
    3, 2,
    1/2, 3,
    47/20, 47/20,
    41/10, -3/10)
  stopifnot(isgammamaximin(rvars) == c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
  stopifnot(isgammamaximax(rvars) == c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))
  stopifnot(isbayesmaximal(rvars) == c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE))
  stopifnot(isintervalmaximal(rvars) == c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE))
  stopifnot(isrobustbayes(rvars) == c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
}

test.expectation.6 = function() {
  likelihoodpmf = c(
    0.9, 0.1,
    0.3, 0.7)
  utility = c(
    3, -1,
    0, 0)
  stopifnot(
    getstrategies(datasize=2, decisionsize=2)
    ==
    matrix(c(
      1,1,
      2,1,
      1,2,
      2,2), byrow=TRUE, nrow=4))
  .stopifnotalmostequal(
    getwaldutilities(datasize=2, likelihoodpmf=likelihoodpmf, utility=utility)
    ,
    matrix(c(
      3, -1,
      0.3, -0.7,
      2.7, -0.3,
      0, 0
      ), byrow=TRUE, nrow=4))
  stopifnot(
    iswaldadmissible(datasize=2, likelihoodpmf=likelihoodpmf, utility=utility)
    ==
    c(TRUE, FALSE, TRUE, TRUE)
    )
}

test.expectation.7 = function() {
  priorpmf = c(0.4, 0.6)
  likelihoodpmf = c(
    0.9, 0.1,
    0.3, 0.7)
  .stopifnotalmostequal(
    getposteriorpmf(2, priorpmf, likelihoodpmf),
    c(2/3, 1/3, 2/23, 21/23))
}

test = function() {
  test.expectation.1()
  test.expectation.2()
  test.expectation.3()
  test.expectation.4()
  test.expectation.5()
  test.expectation.6()
  test.expectation.7()
}

test()
