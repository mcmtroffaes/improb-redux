source("../improb-redux.r")
pmfs = c(
  0, 0.6, 0.4,
  0.3, 0.3, 0.4)
getexpectations = getexpectationsfunc(3, pmfs) # 3 = size of possibility space
getlowerprevisions = getlowerprevisionsfunc(getexpectations)
getupperprevisions = getupperprevisionsfunc(getexpectations)
isgammamaximin = isgammamaxisomethingfunc(getlowerprevisions)
isgammamaximax = isgammamaxisomethingfunc(getupperprevisions)
isbayesmaximal = ismaximalfunc(getexpectations, rbayescompare)
isintervalmaximal = ismaximalfunc(getexpectations, intervalcompare)
isrobustbayes = isrobustbayesfunc(getexpectations)
rvars = c(
  100, 50, -25,
  75, 50, 0,
  60, 55, 10,
  35, 35, 35)
getexpectations(rvars)
getlowerprevisions(rvars)
getupperprevisions(rvars)
isgammamaximin(rvars)
isgammamaximax(rvars)
isintervalmaximal(rvars)
isbayesmaximal(rvars)
isrobustbayes(rvars)
