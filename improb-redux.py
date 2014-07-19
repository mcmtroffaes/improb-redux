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
    func, 0, getexpectations(rvarvalues))

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
bayescompare = function(exp1, exp2, tol=1e-10) { all(exp1 > exp2 + tol) }
intervalcompare = function(exp1, exp2, tol=1e-10) { min(exp1) > max(exp2) + tol }

# Return maximal elements based on partial order.
# This is *not* the most efficient implementation, but it is simple.
ismaximalfunc = function(getexpectations, comparefunc) {
  function(rvarvalues) {
    getcomparisonmatrix = getexpectationsapplyfunc2(
      getexpectations, comparefunc)
    apply(getcomparisonmatrix(rvarvalues), 1, function(row) { !any(row) })
  }
}

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
