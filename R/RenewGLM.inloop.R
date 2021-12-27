
Update_beta = function(X,y,betahat,sum2,type){
        tol=1e-6;
        max_iter=100;
        X<-as.matrix(cbind(1,X))
        betahat_old=betahat
        W<-invlinkdiv(X,betahat_old,type=type)
        H<-cp(X,y,W)
        
        U=chol(sum2+H)
        L=t(U)
        for (r in 1:max_iter){
          
          g_0=t(crossprod((y-invlink(X, betahat, type)),X));
          g_1=-t(crossprod((betahat-betahat_old),sum2));
          g=g_0+g_1;
          
          d_beta=backsolve(U,forwardsolve(L,g))
          
          df_beta=crossprod(g,d_beta);
          if (abs(df_beta)<tol){
            break
          }else {
            betahat=betahat+d_beta;
          }
        }
        W<-invlinkdiv(X,betahat,type=type)
        H_new<-cp(X,y,W)
        sum2<-sum2+H_new
        return(list(betahat, sum2))
}


Update_ate<-function(beta,A_all,y,X,y_treated_sum,y_control_sum,n_treated,n_control,lastsite = FALSE ){
  ps123_S=1/(exp(-as.matrix(cbind(1,X))%*%beta)+1)
  w_ps123_S=IPW(ps123_S,A_all)
  y=y*w_ps123_S
  y_treated_sum=y_treated_sum+sum(y[which(A_all==1)])
  y_control_sum=y_control_sum+sum(y[which(A_all==0)])
  #ps weight sum treated
  n_treated=n_treated+sum(w_ps123_S[which(A_all==1)])
  #ps weight sum control
  n_control=n_control+sum(w_ps123_S[which(A_all==0)])
  ATE_ps123_av=log(((y_treated_sum/n_treated)/(1-y_treated_sum/n_treated)) /((y_control_sum/n_control) /(1-(y_control_sum/n_control))))
  ATE_ps123_av_b1=log((y_control_sum/n_control) /(1-(y_control_sum/n_control)))
  ATE_ps123_av_r=c(ATE_ps123_av_b1,ATE_ps123_av)
  est=c(beta,ATE_ps123_av_r)
  ATE.var_S1=Godambe_renew(est,A_all,y,X,PS.formula,"PS")
  meat=meat+ATE.var_S1$meat
  bread=bread+ATE.var_S1$bread
  # save(y, X, file = paste(tempdatadir, "/Simdata",k, ".RData", sep="")
 
  if(lastsite==T){
    b=solve(bread)
    var=(b%*%meat%*%t(b))[dim(b)[1],dim(b)[2]]
  return(c("ATE"=ATE_ps123_av,"var"=(b%*%meat%*%t(b))[dim(b)[1],dim(b)[2]]))
  }
  return(list(bread=bread,meat=meat,y_treated_sum=y_treated_sum,
              y_control_sum=y_control_sum,n_treated=n_treated,n_control=n_control,beta_ps = beta,log_odds_ratio = ATE_ps123_av))
}








cp <-
  function(X, y, w){
    if (length(y)==1){
      H<-w*tcrossprod(X,X)
    }else{
      H<-crossprod(sqrt(w)*X)
    }
  }

invlink <-
  function(X, beta, type){
    if (length(beta)==1){
      eta<-X*beta
    }else{
      eta<-drop(as.matrix(X)%*%as.matrix(beta))
    }
    if(type=="gaussian"){ out <- eta }
    if(type=="binomial"){ out <- exp(eta)/(1 + exp(eta)) }
    if(type=="poisson") { out <- exp(eta) }
    out
  }

invlinkdiv <-
  function(X, beta, type){
    if (length(beta)==1){
      eta<-X*beta
    }else{
      eta<-drop(as.matrix(X)%*%as.matrix(beta))
    }
    if(type=="gaussian"){ out <- 1 }
    if(type=="binomial"){ out <- exp(eta)/(1 + exp(eta))^2 }
    if(type=="poisson") { out <- exp(eta) }
    out
  }

