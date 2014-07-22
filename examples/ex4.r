source("../improb-redux.r")
pmfs = c(
  0.1, 0.45, 0.45,
  0.4, 0.3, 0.3,
  0.3, 0.2, 0.5)
getexpectations = getexpectationsfunc(3, pmfs) # 3 = size of possibility space
getlowerprevisions = getlowerprevisionsfunc(getexpectations)
getupperprevisions = getupperprevisionsfunc(getexpectations)
isgammamaximin = isgammamaxisomethingfunc(getlowerprevisions)
isgammamaximax = isgammamaxisomethingfunc(getupperprevisions)
isbayesmaximal = ismaximalfunc(getexpectations, rbayescompare)
isintervalmaximal = ismaximalfunc(getexpectations, intervalcompare)
isrobustbayes = isrobustbayesfunc(getexpectations)
rvars = c(
  -10, -5, 10,
  1, 1, 1)
getexpectations(rvars)
getlowerprevisions(rvars)
getupperprevisions(rvars)
isgammamaximin(rvars)
isgammamaximax(rvars)
isintervalmaximal(rvars)
isbayesmaximal(rvars)
isrobustbayes(rvars)
