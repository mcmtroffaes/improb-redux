source("improb-redux.r")
pmfs = c(
  0.3, 0.1, 0.1, 0.5,
  0.3, 0.1, 0.2, 0.4,
  0.4, 0.2, 0.1, 0.3,
  0.5, 0.1, 0.1, 0.3)
# normal form solution
getexpectations = getexpectationsfunc(4, pmfs) # 4 = size of possibility space
getlowerprevisions = getlowerprevisionsfunc(getexpectations)
getupperprevisions = getupperprevisionsfunc(getexpectations)
isgammamaximin = isgammamaxisomethingfunc(getlowerprevisions)
isgammamaximax = isgammamaxisomethingfunc(getupperprevisions)
isbayesmaximal = ismaximalfunc(getexpectations, bayescompare)
isintervalmaximal = ismaximalfunc(getexpectations, intervalcompare)
isrobustbayes = isrobustbayesfunc(getexpectations)
rvars = c(
  -1, -1, -1, -1,
  -1, -1, -6, 10,
  -6, 10, -1, -1,
  -6, 10, -6, 10,
  0, 0, 0, 0,
  -5, 11, -5, 11)
getexpectations(rvars)
getlowerprevisions(rvars)
getupperprevisions(rvars)
isgammamaximin(rvars)
isgammamaximax(rvars)
isintervalmaximal(rvars)
isbayesmaximal(rvars)
isrobustbayes(rvars)
# backward induction
getconditionalexpectations = getconditionalexpectationsfunc(4, pmfs)
t1 = c(TRUE, TRUE, FALSE, FALSE)
t2 = c(FALSE, FALSE, TRUE, TRUE)
s1 = c(TRUE, FALSE, TRUE, FALSE)
s2 = c(FALSE, TRUE, FALSE, TRUE)
getexpectations.t1 = getconditionalexpectations(t1)
getexpectations.t2 = getconditionalexpectations(t2)
isbayesmaximal.t1 = ismaximalfunc(getexpectations.t1, bayescompare)
isbayesmaximal.t2 = ismaximalfunc(getexpectations.t2, bayescompare)
isbayesmaximal.t1(
  c(-1, -1,
    -6, 10)) 
isbayesmaximal.t2(
  c(-1, -1,  # not optimal
    -6, 10))
isbayesmaximal(
  c(0, 0, 0, 0, # not optimal
    -5, 11, -5, 11))
isbayesmaximal(
  c(-1, -1, -6, 10,
    -6, 10, -6, 10, # not optimal
    -5, 11, -5, 11))
