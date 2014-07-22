source("improb-redux.r")
pmfs = c(
  0.5, 0.5,
  0.8, 0.2)
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

pmfs2 = c(
  0.5, 0.5,
  0.8, 0.2,
  0.65, 0.35)
getexpectations2 = getexpectationsfunc(2, pmfs2) # 2 = size of possibility space
isrbayesadmissible2 = isrbayesadmissiblefunc(getexpectations2)
getexpectations2(rvars)
isrbayesadmissible2(rvars)
