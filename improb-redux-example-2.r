source("improb-redux.r")
pmfs = c(
  0.3, 0.1, 0.1, 0.5,
  0.3, 0.1, 0.2, 0.4,
  0.4, 0.2, 0.1, 0.3,
  0.5, 0.1, 0.1, 0.3)
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
