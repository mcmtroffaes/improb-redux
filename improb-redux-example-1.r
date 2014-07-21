source("improb-redux.r")
pmfs = c(
  0.5, 0.5,
  0.8, 0.2)
getexpectations = getexpectationsfunc(2, pmfs) # 2 = size of possibility space
getlowerprevisions = getlowerprevisionsfunc(getexpectations)
getupperprevisions = getupperprevisionsfunc(getexpectations)
isgammamaximin = isgammamaxisomethingfunc(getlowerprevisions)
isgammamaximax = isgammamaxisomethingfunc(getupperprevisions)
isbayesmaximal = ismaximalfunc(getexpectations, bayescompare)
isintervalmaximal = ismaximalfunc(getexpectations, intervalcompare)
isrobustbayes = isrobustbayesfunc(getexpectations)
rvars = c(
  440, 260,
  420, 300,
  370, 370)
isgammamaximin(rvars)
isgammamaximax(rvars)
isintervalmaximal(rvars)
isbayesmaximal(rvars)
isrobustbayes(rvars)
