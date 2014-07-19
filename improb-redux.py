import numpy as np

# Return a function that calculates the expectation of random variables
# with respect to a set of probability mass functions.
# The function returned takes any set of random variables (specified
# as a simple vector), and will return a matrix, with one row per
# random variable, and one column per probability mass function.
def getexpectationsfunc(pmfvalues):
  pmfmatrix = np.array(pmfvalues).T
  # all rows are normalised?
  np.testing.assert_allclose(
    np.apply_along_axis(np.sum, 0, pmfmatrix), np.ones(pmfmatrix.shape[1]))
  # all entries are non-negative?
  assert(np.all(pmfmatrix >= 0))
  # return function that multiplies the matrices
  return lambda rvarvalues: np.dot(rvarvalues, pmfmatrix)

# Return a function which calls getexpectations, and then applies a
# vector function on each row (i.e. to calculate minimum expectation,
# maximum expectation, etc.).
def getexpectationsapplyfunc(getexpectations, func):
  return lambda rvarvalues: np.apply_along_axis(
    func, 1, getexpectations(rvarvalues))

"""
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
"""

# Return a function which evaluates the lower prevision.
def getlowerprevisionsfunc(getexpectations):
  return getexpectationsapplyfunc(getexpectations, np.min)

# Return a function which evaluates the upper prevision.
def getupperprevisionsfunc(getexpectations):
  return getexpectationsapplyfunc(getexpectations, np.max)

# Return a function which evaluates the "Hurwicz" prevision.
def gethurwiczprevisionsfunc(getexpectations, optimism):
  def hurwicz(expectations):
    return optimism * max(expectations) + (1 - optimism) * min(expectations)
  return getexpectationsapplyfunc(getexpectations, hurwicz)

# Return a function which tells you which random variables are
# Gamma-maxi-"something", where "something" is any function from random
# variables to values (for example a function created with
# getlowerprevisionsfunc, getupperprevisionsfunc, or
# gethurwiczprevisionsfunc).
def isgammamaxisomethingfunc(getsomethings, tol=1e-10):
  def result(rvarvalues):
    somethings = getsomethings(rvarvalues)
    return somethings >= (np.max(somethings) - tol)
  return result

# Partial orders based on expectation values.
def bayescompare(exp1, exp2, tol=1e-10): return np.all(exp1 > exp2 + tol)
def intervalcompare(exp1, exp2, tol=1e-10): return np.min(exp1) > np.max(exp2) + tol

"""
# Return maximal elements based on partial order.
# This is *not* the most efficient implementation, but it is simple.
def ismaximalfunc(getexpectations, comparefunc):
  function(rvarvalues) {
    getcomparisonmatrix = getexpectationsapplyfunc2(
      getexpectations, comparefunc)
    apply(getcomparisonmatrix(rvarvalues), 1, function(row) { !any(row) })

# Return a function which tells you which random variables are Bayes
# maximal (i.e. Bayes maximal with respect to some element of the
# credal set specified).
isrobustbayesfunc = function(getexpectations, tol=1e-10) {
  function(rvarvalues) {
    expectations = getexpectations(rvarvalues)
    maxexpectations = apply(expectations, 2, function(col) { col >= (max(col) - tol) })
    apply(maxexpectations, 1, function(row) { any(row) })
  }
}
"""

################################################################################
# tests
################################################################################

# simple usage: expectation of a single random variable
def test_expectation_1():
  pmf = [[0.4, 0.6]]
  rvar = [[3, 9]]
  getexpectations = getexpectationsfunc(pmf)
  np.testing.assert_allclose(getexpectations(rvar), 6.6)

# more complicated: expectation of three random variables
def test_expectation_2():
  getexpectations = getexpectationsfunc([[0.4, 0.6]])
  rvars = [
    [3, 9],
    [4, 4],
    [0, 3]]
  np.testing.assert_allclose(getexpectations(rvars), [[6.6], [4], [1.8]])

# more complicated: expectation of multiple random variables with
# respect to multiple pmfs
def test_expectation_3():
  getexpectations = getexpectationsfunc([
    [0.4, 0.5, 0.1],
    [0.1, 0.8, 0.1]])
  rvars = [
    [3, 9, 2],
    [4, 4, 4],
    [0, 3, 6],
    [6, 2, 1]]
  np.testing.assert_allclose(
    getexpectations(rvars),
    [[5.9, 7.7],
     [4, 4],
     [2.1, 3.0],
     [3.5, 2.3]])

def test_expectation_4():
  pmfs = [
    [0.4, 0.5, 0.1], # first probability mass function
    [0.1, 0.8, 0.1], # second ...
    [0.6, 0.2, 0.2]] # third ...
  getexpectations = getexpectationsfunc(pmfs)
  getlowerprevisions = getlowerprevisionsfunc(getexpectations)
  getupperprevisions = getupperprevisionsfunc(getexpectations)
  gethurwiczprevisions = gethurwiczprevisionsfunc(getexpectations, 0.5)
  isgammamaximin = isgammamaxisomethingfunc(getlowerprevisions)
  isgammamaximax = isgammamaxisomethingfunc(getupperprevisions)
  isgammamaxihurwicz = isgammamaxisomethingfunc(gethurwiczprevisions)
  #isbayesmaximal = ismaximalfunc(getexpectations, bayescompare)
  #isintervalmaximal = ismaximalfunc(getexpectations, intervalcompare)
  #isrobustbayes = isrobustbayesfunc(getexpectations)
  rvars = [
    [3, 9, 2],
    [4, 4, 4],
    [0, 3, 6],
    [6, 2, 1]]
  np.testing.assert_allclose(
    getexpectations(rvars),
    [[5.9, 7.7, 4],
     [4, 4, 4],
     [2.1, 3.0, 1.8],
     [3.5, 2.3, 4.2]])
  np.testing.assert_allclose(getlowerprevisions(rvars), [4, 4, 1.8, 2.3])
  np.testing.assert_allclose(getupperprevisions(rvars), [7.7, 4, 3, 4.2])
  np.testing.assert_allclose(gethurwiczprevisions(rvars), [5.85, 4, 2.4, 3.25])
  np.testing.assert_equal(isgammamaximin(rvars), [True, True, False, False])
  np.testing.assert_equal(isgammamaximax(rvars), [True, False, False, False])
  np.testing.assert_equal(isgammamaxihurwicz(rvars), [True, False, False, False])
  #np.testing.assert_equal(isbayesmaximal(rvars) == c(True, True, False, True))
  #np.testing.assert_equal(isintervalmaximal(rvars) == c(True, True, False, True))
  #np.testing.assert_equal(isrobustbayes(rvars) == c(True, False, False, True))

"""
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
    1.0/2, 3,
    47.0/20, 47.0/20,
    41.0/10, -3.0/10)
  stopifnot(isgammamaximin(rvars) == [False, False, False, False, True, False])
  stopifnot(isgammamaximax(rvars) == [False, True, False, False, False, False])
  stopifnot(isbayesmaximal(rvars) == [True, True, True, False, True, False])
  stopifnot(isintervalmaximal(rvars) == [True, True, True, False, True, True])
  stopifnot(isrobustbayes(rvars) == [True, True, True, False, False, False])
}
"""
