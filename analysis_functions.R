glmfun<-function(data,y,buffer,x, ndvi_measure){
  # format function call 
  xname = paste(c(paste0("ndvi_",as.character(buffer), "_", as.character(ndvi_measure),"_iqr"),x) , collapse = " + ")
  formula1<-as.formula(paste(y,"~",xname))
  glm.fit<-do.call("glm",list(data=quote(data),family = 'binomial', formula1))
  #format estimate and CI for NDVI as Odds ratio with CI
  out <- exp(cbind(OR = coef(glm.fit), confint(glm.fit)))[2,]
  return(out)
}