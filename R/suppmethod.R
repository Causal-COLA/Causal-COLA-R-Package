
IPW<-function(e,Z){
  Z/e+(1-Z)/(1-e)
}

glm_si=function(data){
  S=glm(A_all~X1_all+X2_all+X3_all+X4_all+X5_all,data=data,family=binomial(link=logit)) # fitting a propensity score model
  S_ps=S$fitted.values
  coff_S = S$coefficients
  w_S_ps=IPW(S_ps,data$A_all)# getting PS scores
  y=data[,1]*w_S_ps
  #treatment X
  X=cbind(data[,2])
  y_treated_sum = sum(y[which(X==1)])
  #ps weight sum treated
  n_treated = sum(w_S_ps[which(data$A_all==1)])
  #ps weight sum control
  n_control = sum(w_S_ps[which(data$A_all==0)])
  y_control_sum = sum(y[which(X==0)])
  
  ATE_ps123_av=log(((y_treated_sum/n_treated)/(1-y_treated_sum/n_treated)) /((y_control_sum/n_control) /(1-(y_control_sum/n_control))))
  ATE_ps123_av_b1=log((y_control_sum/n_control) /(1-(y_control_sum/n_control)))
  ate_coeff_S=c(ATE_ps123_av_b1,ATE_ps123_av)
  est=c(coff_S,ate_coeff_S)
  ATE.var_S1=Godambe(est,data$A_all,data$Y_all,data[X_columns],PS.formula,"PS")
  return(c("ATE"=ate_coeff_S[2],"ATE.var"=ATE.var_S1))
}
U.PS = function(par,A,Y,X,PS.formula){
  A_X = as.matrix(cbind(1,X))
  A_1 = as.matrix(cbind(1,A))
  par.A_X = par[1:ncol(A_X)] #ps COEFF
  ATE = par[(ncol(A_X)+1):(ncol(A_X)+2)]
  ps = plogis(A_X %*% par.A_X)
  U.ps = c(A - plogis(A_X %*% par.A_X)) * (A_X)
  U.DR.ATE = c(A*1/ps+(1-A)*1/(1-ps))*A_1*c(Y-plogis(A_1 %*% ATE))
  return(cbind(PS=U.ps,ATE=U.DR.ATE))
}
G = function(par,m,A,Y,X,formula,type){
  if(type=="PS"){
    return(apply(U.PS(par,A,Y,X,formula),2,sum))
  }
}

Godambe = function(est,A,Y,X,formula,type){
  if(type=="PS"){
    meat.half=U.PS(par=est,A,Y,X,formula)
  }
  bread=numDeriv::jacobian(func=G,x=est,A=A,Y=Y,X=X,formula=formula,type=type)
  
  IF = meat.half%*%t(solve(-bread))
  
  ATE.var = sum(IF[,ncol(IF)]^2)
  
  return(ATE.var)
}
drawForestplot<-function(label,beta_hat,var){
  lb=beta_hat- qnorm(0.975)*(sqrt(var))
  up=beta_hat+qnorm(0.975)*(sqrt(var))
  df <- data.frame(label, beta_hat, lb, up)
  df$label <- factor(df$label, levels=rev(df$label))
  forest.plot <- ggplot2::ggplot(data=df, aes(x=label, y=beta_hat, ymin=lb, ymax=up)) +
    geom_pointrange() + 
    geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab("Method") + ylab("Mean (95% CI)") +
    theme_bw()  # use a white background
  return(forest.plot)
  
}
Godambe_renew = function(est,A,Y,X,formula,type){
  if(type=="PS"){
    meat.half=U.PS(par=est,A,Y,X,formula)
  }

  sample_size=length(A)
  bread=numDeriv::jacobian(func=G,x=est,A=A,Y=Y,X=X,formula=formula,type=type)
  #colSums(meat.half[,ncol(meat.half)]*meat.half)
  meat=matrix(rowSums(apply(meat.half,1,outer_pro)),dim(bread)[1],dim(bread)[1])

  return(list(bread=bread,meat=meat,sample_size=sample_size))
}
outer_pro=function(row){
  return(outer(row,row,"*"))
}
