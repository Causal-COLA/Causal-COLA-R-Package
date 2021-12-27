#' Generate example data
#' 
#' This method generates data for 4 hospitals and save each dataset
#' in its own directory locally.
#' @param None
#'
#' @return output A list of four datasets
#'
#' @keywords simulate data
#'
#' @export
#' 
#' @examples generateData()
#' 
generateData <-function(){
          filepath = getwd()
          tempdatadir = paste0(filepath,"/Simdata")
          if(!dir.exists(tempdatadir)){
            dir.create(tempdatadir)
          }
          X_columns=c("X1_all","X2_all","X3_all","X4_all","X5_all")
          
          xmu=c(1,1,1,1)
          n = c(100,80,80,50)
          prop2=0.5
          K = length(n)
          prop3=0.6
          X1_all=rnorm(sum(n),mean=xmu[1])#E(X1) = 0
          X2_all=rbinom(sum(n),1,prop2)
          X3_all=rnorm(sum(n),mean=xmu[3])
          X4_all=rnorm(sum(n),mean=xmu[4])
          X5_all=rbinom(sum(n),1,prop3)
          X_all = cbind(1,X1_all,X2_all,X3_all,X4_all,X5_all)
          X_all_only=cbind(X1_all,X2_all,X3_all,X4_all,X5_all)
          gamma= c(-1,0.3,0.5,0.3,0.5,0.3)
          beta = c(-2.8,0.4,0.3,0.3,0.5,0.3,0.5)
          beta_x = beta[3:(3+dim(X_all_only)[2]-1)]#mutable

          a=1; Y1 = rbinom(n=sum(n),size=1,prob=plogis(beta[1]+beta[2]*a+X_all_only%*%beta_x))
          a=0; Y0 = rbinom(n=sum(n),size=1,prob=plogis(beta[1]+beta[2]*a+X_all_only%*%beta_x))
          ### 1=treated, 0=control
          A_all = rbinom(n=sum(n),size=1,p=plogis(X_all %*%gamma))
          ### The observed outcome
          prob1=sum(A_all)/sum(n)
          Y_all = Y1*A_all + Y0*(1-A_all)
          data_all_generated = data.frame(cbind(Y_all,A_all,X_all_only,Y1,Y0))
          data_new=c()
          data_all=c()
          all_idx=c(1:(sum(n)))
          propa=sum(A_all)/sum(n)
          for(i in c(1:(K))){
            set.seed(i)
            idx <- sample(all_idx,size=n[i],replace = FALSE)#which row you want to use from the data
            data_new[[i]] <- data_all_generated[idx,]#save the data #you are not copying the data which is efficient
            data_all=rbind(data_all,data_new[[i]])
            all_idx=setdiff(all_idx,idx)
          }

          p1=mean(data_all_generated$Y1)
          #population probability of P(Y=1|X=0)
          p0=mean(data_all_generated$Y0)
          #OR
          (true.ATE = (p1/(1-p1))/(p0/(1-p0)))
          #RD
          #(true.ATE = p1-p0)
          #true_odds_ratio=log(true.ATE)
          X2_p = mean(data_all_generated$X2_all)

          for (i in c(1:K)){
            subdata= data_new[[i]]
            X=subdata[X_columns]
            A=subdata$A_all
            filepath = getwd()
            tempdatadir = paste0(filepath,"/Simdata/hospital",i)
            tempdatadir1 = paste0(filepath,"/tempdatadir1")
            if(!dir.exists(tempdatadir)){
              dir.create(tempdatadir)
            }
            if(!dir.exists(tempdatadir1)){
              dir.create(tempdatadir1)
            }
            y=subdata$Y_all
            save(A, X,y, file = paste(tempdatadir, "/Simdata.RData", sep=""))
            
            X=subdata[c("A_all",X_columns)]
          }
          return(data_new)

}
#' Using local data to obtain log odds ratio and variance estimates for each hospital
#' 
#' This function calculates a local log odds ratio and their variance estimates.
#' With the local log odds ratio and their variance estimates, the function also
#' compute a variance-inverted meta-analytical log odds ratio and variance estimates
#' by any spaces. 
#'
#' @param None
#'
#' @return A list that includes local odds ratio for each hospital and their variances.
#' Meta log odds ratio and its variance.
#'
#' @keywords Local calculation
#'
#' @export
#' 
#' @examples
#' getLocalodds()
getLocalodds<-function(){
  K=4
  log_ATE_ps=c()  
  log_ATE_ps_var=c()
  #obtain local ps and oracle ps
  for (i in 1:K){
    res=glm_si(data_new[[i]])
    log_ATE_ps[i]=res[1]
    log_ATE_ps_var[i]=res[2]

  }
  ATE_log_ps_meta <- sum(log_ATE_ps * (1/log_ATE_ps_var))/sum(1/(log_ATE_ps_var))
  ATE_log_ps_meta_var<-1/sum(1/log_ATE_ps_var)
  res =list("local.odds"=log_ATE_ps,"local.odds.var"=log_ATE_ps_var,'meta.odds' =ATE_log_ps_meta,'meta.odds.var' = ATE_log_ps_meta_var )
  return(res)
}

#' Plot local log odds ratios and their variances on a forest plot.
#' 
#' @param none.

#' @return A forest plot that shows local odds ratios and their variances and meta-analytical results 
#' @examples
#' getLocalodds.plot()
#'
getLocalodds.plot<-function(){
  drawForestplot(c("local 1","local 2","local 3",'local 4',"Meta"),
                 c(getLocalodds()$local.odds,getLocalodds()$meta.odds),c(getLocalodds()$local.odds.var,getLocalodds()$meta.odds.var))
  
}

#' Update propensity score estimates
#' 
#' This function updates estimates for propensity scores and their vairances.
#' @param X local covariates data.
#' @param y local outcome data.
#' @param betahat current estimates for propensity scores which is usually passed down from the preceding site.
#' @param sum2 current Hessian matrix which is usually passed down from the preceding site.
#' @param type
#' @return the updated \code{betahat} and \code{sum2}.
#'
#' @keywords First round update
#'
#' @export
#' 
#' @examples
#' Update_beta(X,y, betahat, sum2,type)
Update_beta = function(X,y,betahat,sum2,type ="binomial" ){
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

#' Update causal log odds ratio estimates
#' 
#' This function updates estimates for the collaborative causal log odds ratio and its variance.
#' @param beta causal log odds ratio from the previous site
#' @param A_all local treatment assignment 
#' @param X local covariates data.
#' @param y local outcome data.
#' @param y_treated_sum
#' @param y_control_sum
#' @param n_treated 
#' @param n_control 
#' @param lastsite defult is false which indicates whether the current site is the \code{lastsite}
#' @return a vector of intermediate quantites that will be passed down to the next site when \code{lastsite} is FALSE;
#' a vector of causal log odds ratio and its variance when \code{lastsite} is TRUE
#'
#' @keywords Second round ATE update
#'
#' @export
#' 
#' @examples
#' Update_ate(beta,A_all,y,X,y_treated_sum,y_control_sum,n_treated,n_control,TRUE)
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











# reverses the factor level ordering for labels after coord_flip()


