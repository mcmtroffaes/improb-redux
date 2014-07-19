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

################################################################################
# tests
################################################################################

# simple usage: expectation of a single random variable
def test_expectation_1():
  pmf = [[0.4, 0.6]]
  rvar = [[3, 9]]
  getexpectations = getexpectationsfunc(pmf)
  np.testing.assert_allclose(getexpectations(rvar), 6.6)
