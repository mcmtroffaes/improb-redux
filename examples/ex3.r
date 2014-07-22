source("../improb-redux.r")
pmfs = c(
  0.6, 0.4,
  0.65, 0.35)
getexpectations = getexpectationsfunc(2, pmfs) # 2 = size of possibility space
getlowerprevisions = getlowerprevisionsfunc(getexpectations)
getupperprevisions = getupperprevisionsfunc(getexpectations)
isgammamaximin = isgammamaxisomethingfunc(getlowerprevisions)
isgammamaximax = isgammamaxisomethingfunc(getupperprevisions)
isbayesmaximal = ismaximalfunc(getexpectations, rbayescompare)
isintervalmaximal = ismaximalfunc(getexpectations, intervalcompare)
isrbayesadmissible = isrbayesadmissiblefunc(getexpectations)
rvars = c(
  440, 260,
  420, 300,
  370, 370)
getexpectations(rvars)
getlowerprevisions(rvars)
getupperprevisions(rvars)
isgammamaximin(rvars)
isgammamaximax(rvars)
isintervalmaximal(rvars)
isbayesmaximal(rvars)
isrbayesadmissible(rvars)
