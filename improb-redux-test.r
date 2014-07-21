source("improb-redux.r")

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
      c(5.9, 7.7,  # 0.4 * 3 + 0.5 * 9 + 0.1 * 2 = 5.9
                   # 0.1 * 3 + 0.8 * 9 + 0.1 * 2 = 7.7
        4, 4,      # constant random variable, expectation equals the constant
        2.1, 3.0,  # etc.
        3.5, 2.3), # etc.
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
    getwaldutilities(datasize=2, likelihoodpmf=likelihoodpmf, utility=utility),
    c(3, -1,
      0.3, -0.7,
      2.7, -0.3,
      0, 0))
  stopifnot(
    iswaldstrategy(datasize=2, likelihoodpmf=likelihoodpmf, utility=utility)
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

test.expectation.8.detail = function(priorpmf, outcome) {
  likelihoodpmf = c(
    0.9, 0.1,
    0.3, 0.7)
  posteriorpmf = getposteriorpmf(2, priorpmf, likelihoodpmf)
  utility = c(
    3, -1,
    0, 0)
  stopifnot(
    isbayesstrategy(2, posteriorpmf, utility) ==
    matrix(outcome, byrow=TRUE, nrow=2))
}

test.expectation.8 = function() {
  # outcome:
  #  d(y=0.5) == 1   d(y==2) == 1
  #  d(y=0.5) == 2   d(y==2) == 2
  test.expectation.8.detail(
    c(0.4, 0.6),
    c(TRUE, FALSE,
      FALSE, TRUE))
  test.expectation.8.detail(
    c(1, 0),
    # always take d=1
    c(TRUE, TRUE,
      FALSE, FALSE))
  test.expectation.8.detail(
    c(0, 1),
    # always take d=2
    c(FALSE, FALSE,
      TRUE, TRUE))
}

test = function() {
  test.expectation.1()
  test.expectation.2()
  test.expectation.3()
  test.expectation.4()
  test.expectation.5()
  test.expectation.6()
  test.expectation.7()
  test.expectation.8()
}

test()
