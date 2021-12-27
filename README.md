      causalCOLA   code{white-space: pre-wrap;} span.smallcaps{font-variant: small-caps;} span.underline{text-decoration: underline;} div.column{display: inline-block; vertical-align: top; width: 50%;} div.hanging-indent{margin-left: 1.5em; text-indent: -1.5em;} ul.task-list{list-style: none;}   code { white-space: pre; } .sourceCode { overflow: visible; }   pre > code.sourceCode { white-space: pre; position: relative; } pre > code.sourceCode > span { display: inline-block; line-height: 1.25; } pre > code.sourceCode > span:empty { height: 1.2em; } .sourceCode { overflow: visible; } code.sourceCode > span { color: inherit; text-decoration: inherit; } div.sourceCode { margin: 1em 0; } pre.sourceCode { margin: 0; } @media screen { div.sourceCode { overflow: auto; } } @media print { pre > code.sourceCode { white-space: pre-wrap; } pre > code.sourceCode > span { text-indent: -5em; padding-left: 5em; } } pre.numberSource code { counter-reset: source-line 0; } pre.numberSource code > span { position: relative; left: -4em; counter-increment: source-line; } pre.numberSource code > span > a:first-child::before { content: counter(source-line); position: relative; left: -1em; text-align: right; vertical-align: baseline; border: none; display: inline-block; -webkit-touch-callout: none; -webkit-user-select: none; -khtml-user-select: none; -moz-user-select: none; -ms-user-select: none; user-select: none; padding: 0 4px; width: 4em; color: #aaaaaa; } pre.numberSource { margin-left: 3em; border-left: 1px solid #aaaaaa; padding-left: 4px; } div.sourceCode { } @media screen { pre > code.sourceCode > span > a:first-child::before { text-decoration: underline; } } code span.al { color: #ff0000; font-weight: bold; } /\* Alert \*/ code span.an { color: #60a0b0; font-weight: bold; font-style: italic; } /\* Annotation \*/ code span.at { color: #7d9029; } /\* Attribute \*/ code span.bn { color: #40a070; } /\* BaseN \*/ code span.bu { } /\* BuiltIn \*/ code span.cf { color: #007020; font-weight: bold; } /\* ControlFlow \*/ code span.ch { color: #4070a0; } /\* Char \*/ code span.cn { color: #880000; } /\* Constant \*/ code span.co { color: #60a0b0; font-style: italic; } /\* Comment \*/ code span.cv { color: #60a0b0; font-weight: bold; font-style: italic; } /\* CommentVar \*/ code span.do { color: #ba2121; font-style: italic; } /\* Documentation \*/ code span.dt { color: #902000; } /\* DataType \*/ code span.dv { color: #40a070; } /\* DecVal \*/ code span.er { color: #ff0000; font-weight: bold; } /\* Error \*/ code span.ex { } /\* Extension \*/ code span.fl { color: #40a070; } /\* Float \*/ code span.fu { color: #06287e; } /\* Function \*/ code span.im { } /\* Import \*/ code span.in { color: #60a0b0; font-weight: bold; font-style: italic; } /\* Information \*/ code span.kw { color: #007020; font-weight: bold; } /\* Keyword \*/ code span.op { color: #666666; } /\* Operator \*/ code span.ot { color: #007020; } /\* Other \*/ code span.pp { color: #bc7a00; } /\* Preprocessor \*/ code span.sc { color: #4070a0; } /\* SpecialChar \*/ code span.ss { color: #bb6688; } /\* SpecialString \*/ code span.st { color: #4070a0; } /\* String \*/ code span.va { color: #19177c; } /\* Variable \*/ code span.vs { color: #4070a0; } /\* VerbatimString \*/ code span.wa { color: #60a0b0; font-weight: bold; font-style: italic; } /\* Warning \*/   // apply pandoc div.sourceCode style to pre.sourceCode instead (function() { var sheets = document.styleSheets; for (var i = 0; i < sheets.length; i++) { if (sheets\[i\].ownerNode.dataset\["origin"\] !== "pandoc") continue; try { var rules = sheets\[i\].cssRules; } catch (e) { continue; } for (var j = 0; j < rules.length; j++) { var rule = rules\[j\]; // check if there is a div.sourceCode rule if (rule.type !== rule.STYLE\_RULE || rule.selectorText !== "div.sourceCode") continue; var style = rule.style.cssText; // check if color or background-color is set if (rule.style.color === '' && rule.style.backgroundColor === '') continue; // replace div.sourceCode by a pre.sourceCode rule sheets\[i\].deleteRule(j); sheets\[i\].insertRule('pre.sourceCode{' + style + '}', j); } } })();  

causalCOLA
==========

#### Mengtong Hu

#### 2021-12-27

*   [1 generateData(): simulate datasets](#generatedata-simulate-datasets)
*   [2 Update\_beta(): First round of communication for propensity score estimates update:](#update_beta-first-round-of-communication-for-propensity-score-estimates-update)
*   [3 Update\_ate(): the second round updates causal log odds ratio of the treatment and the variance for the casual log odds ratio](#update_ate-the-second-round-updates-causal-log-odds-ratio-of-the-treatment-and-the-variance-for-the-casual-log-odds-ratio)
    *   [3.1 Starting again at hospital 1:](#starting-again-at-hospital-1)
    *   [3.2 Hospital 4 finishes the second round of updates and output causal log odds ratio and its inference matrices, and output the final updates to all sites:](#hospital-4-finishes-the-second-round-of-updates-and-output-causal-log-odds-ratio-and-its-inference-matrices-and-output-the-final-updates-to-all-sites)

This is a tutorial for the casual-COLA platform. Causal-COLA is a collaborative platform for institutions to make causal inferences on their sensitive data (such as medical data) without sharing them.

The causal-COLA platform consists of two parts :

1.  Interactive Interface: a website that serves as secure data-hub for data transmissions between sites. [https://github.com/Causal-COLA/Causal-COLA-FrontEnd](https://github.com/Causal-COLA/Causal-COLA-FrontEnd)
2.  R package causalCOLA: a r package that allows users from different sites to run the analysis on their local computers. Built With
* [R](https://www.r-project.org/) 

The tutorial illustrates how to conduct a two-round causal-COLA analysis by working through a simulated example involving four hospitals.

1 generateData(): simulate datasets
===================================

Change your directory to where you want to save the simulated datasets. The simulated datasets are saved into five separate folders `hospital1`,`hospital2`,`hospital3`, and `hospital4` to mimic the scenario that each local sites store data at their own facilities. Each dataset includes vectors of incidents(Y), treatment status(A) and covariates(X). To simplify the rest of the demonstration, the `generateData()` method will also save the dataset as a list of local datasets through the r object `hospital_data` which is automatically saved in the current environment.

    
    devtools::install_github("https://github.com/Causal-COLA/Causal-COLA-R-Package")
    #> Skipping install of 'causalCOLA' from a github remote, the SHA1 (2bfa6bbd) has not changed since last install.
    #>   Use `force = TRUE` to force installation
    library(causalCOLA)

2 Update\_beta(): First round of communication for propensity score estimates update:
=====================================================================================

We use b to denote the site that we currently have access to. ## At hospital 1: When b = 1 which is the starting site, we can initialize a betahat and a sum2

    tempdatadir = getwd()
    b = 1
    # Load Data so X is supposed to be a covariates vector and y is supposed to be the treatment vector
        X<-y<-A<-NULL;
        load(paste(tempdatadir,"/Simdata/hospital",b,"/Simdata.RData",sep=""))
    #Site 1 specifies the number of covariates p. Here we use 5.
        p = dim(X)[2]+1
    #Initialize betehat using an empty vector or some other initial value init
        betahat = rep(0,p) 
    #Initialize sum2
        sum2<-diag(0,p,p)
        output= Update_beta(X,A,betahat,sum2,"binomial")
    # The intermediate output from site 1 in round 1 invovles beta coefficient estimates and Hessian matrices
        print(output)
    #> [[1]]
    #>             [,1]
    #> [1,] -0.28402150
    #> [2,]  0.36118110
    #> [3,]  0.45555729
    #> [4,] -0.05081609
    #> [5,]  0.47563415
    #> [6,] -0.33163140
    #> 
    #> [[2]]
    #>               1   X1_all    X2_all   X3_all   X4_all    X5_all
    #> 1      20.93789 21.78285 11.553201 23.42310 22.19748 13.064873
    #> X1_all 21.78285 43.80061 11.220944 26.56848 20.52154 14.437830
    #> X2_all 11.55320 11.22094 11.553201 13.00086 11.82413  7.732463
    #> X3_all 23.42310 26.56848 13.000855 44.26446 29.24566 14.690996
    #> X4_all 22.19748 20.52154 11.824127 29.24566 43.76209 13.113792
    #> X5_all 13.06487 14.43783  7.732463 14.69100 13.11379 13.064873

Once site 1 finishes analysis, site 1 will upload the results to the Interactive Interface using the pre-assgined username and password. The second hospital will log onto the Interactive Interface using its assigned username and password and download the output uploaded by hospital 1 and proceed the analysis.

Here we directly save the local result from hospital 1 into the local folder `/Simdata/hospital2` where the raw data of hospital 2 are stored.

    #Site 1 finishes analysis and save the results into where the second hospital has access to
        save(output, file = paste(tempdatadir,"/Simdata/hospital",b+1, "/output_r1.RData", sep=""))
        
    #Analysis Start from site 2
        b=2
    #Site 2 loads output results from site 1
        load(paste(tempdatadir,"/Simdata/hospital",b, "/output_r1.RData", sep=""))
    #Site 2 prepares its local data
        X<-y<-A<-NULL;
        load(paste(tempdatadir,"/Simdata/hospital",b,"/Simdata.RData",sep=""))
    #Site 2 updates statistics
        betahat = output[[1]]
        sum2 = output[[2]]
        output= Update_beta(X,y,betahat,sum2,"binomial")
    #Site 2 finishes analysis and save the results in the designated directory
        save(output, file = paste(tempdatadir,"/Simdata/hospital",b+1, "/output_r1.RData", sep=""))

Now Site 2 has turn the updated results over to site 3 who also updates the results.

    #Analysis Start from site 3
        b=3
    #Site 3 loads output results from site 1
      load(paste(tempdatadir,"/Simdata/hospital",b, "/output_r1.RData", sep=""))
    #Site 3 prepares its local data
      X<-y<-A<-NULL;
      load(paste(tempdatadir,"/Simdata/hospital",b,"/Simdata.RData",sep=""))
    #Site 3 updates statistics
      betahat = output[[1]]
      sum2 = output[[2]]
      output= Update_beta(X,y,betahat,sum2,"binomial")
    #Site 3 finishes analysis and save the results in the Site 4 directory
       save(output, file = paste(tempdatadir,"/Simdata/hospital",b+1, "/output_r1.RData", sep=""))   

Site 4 receives data from site 3 and finishes the first round update #Site 4 completes first-round update (PS). The output from Site 4 will be uploaded to the interactive interface where Site 1 has access to download. Here, we save the output results from Site 4 to the into the local folder `/Simdata/hospital1`

      load(paste(tempdatadir,"/Simdata/hospital",b, "/output_r1.RData", sep=""))
       X<-y<-A<-NULL;
      load(paste(tempdatadir,"/Simdata/hospital",b,"/Simdata.RData",sep=""))
      betahat = output[[1]]
      sum2 = output[[2]]
      output= Update_beta(X,y,betahat,sum2,"binomial")
      save(output, file = paste(tempdatadir,"/Simdata/hospital",1, "/output_r1.RData", sep=""))     
      
      

3 Update\_ate(): the second round updates causal log odds ratio of the treatment and the variance for the casual log odds ratio
===============================================================================================================================

3.1 Starting again at hospital 1:
---------------------------------

Read in the output data from hospital 4 and hospital 1’s own raw data.

    b = 1
    
    load(paste(tempdatadir,"/Simdata/hospital",b, "/output_r1.RData", sep=""))
    betahat = output[[1]]
    X<-y<-A<-NULL;
    load(paste(tempdatadir,"/Simdata/hospital",b,"/Simdata.RData",sep=""))

`Update_ate()` is the function that update causal odds ratio as well as the inferences matrices. There are eight inputs needed for the `Update_ate()` function.

1.  The first input is betahat which corresponds to the estimated propensity score estimates.
2.  The second to fourth arguments correspond to the data.
3.  The fifth to eighth arguments correspond to the intermediate quantities used in calculating the log odds ratio.
4.  The last argument “lastsite” is optional and is default to be “FALSE”. When the last site which is hospital 4in our example, does the final update, the “lastsite” will be TRUE.

    y_treated_sum=0;y_control_sum = 0 ; n_treated =0; n_control= 0; meat = 0; bread =0
    output = Update_ate(betahat,A,y,X,y_treated_sum,y_control_sum,n_treated,n_control,meat,bread)
    save(output, file = paste(tempdatadir,"/Simdata/hospital",b+1, "/output_r2.RData", sep=""))     
    
    
    #Analysis continue at site 2
    b=2
    #Site 2 loads output results from site 1
    load(paste(tempdatadir,"/Simdata/hospital",b, "/output_r2.RData", sep=""))
    #Site 2 prepares its local data
    X<-y<-A<-NULL;
    load(paste(tempdatadir,"/Simdata/hospital",b,"/Simdata.RData",sep=""))
    #Site 2 updates statistics. The outputs from site 1 are used as input for `Update_ate()`.
    betahat = output$beta_ps
    y_treated_sum =output$y_treated_sum; y_control_sum=output$y_control_sum; n_treated = output$n_treated;n_control = output$n_control
    meat = output$meat; bread =output$bread
    output = Update_ate(betahat,A,y,X,y_treated_sum,y_control_sum,n_treated,n_control,meat,bread)
    #Site 2 finishes analysis and save the results in the designated directory
    save(output, file = paste(tempdatadir,"/Simdata/hospital",b+1, "/output_r2.RData", sep=""))

    #Analysis continue at site 3
    b=3
    #Site 3 loads output results from site 2
    load(paste(tempdatadir,"/Simdata/hospital",b, "/output_r2.RData", sep=""))
    #Site 3 prepares its local data
    X<-y<-A<-NULL;
    load(paste(tempdatadir,"/Simdata/hospital",b,"/Simdata.RData",sep=""))
    #Site 3 updates statistics. The outputs from site 2 are used as input for `Update_ate()`.
    betahat = output$beta_ps
    y_treated_sum =output$y_treated_sum; y_control_sum=output$y_control_sum; n_treated = output$n_treated;n_control = output$n_control
    meat = output$meat; bread =output$bread
    output = Update_ate(betahat,A,y,X,y_treated_sum,y_control_sum,n_treated,n_control,meat,bread)
    #Site 3 finishes analysis and save the results in the designated directory
    save(output, file = paste(tempdatadir,"/Simdata/hospital",b+1, "/output_r2.RData", sep=""))

3.2 Hospital 4 finishes the second round of updates and output causal log odds ratio and its inference matrices, and output the final updates to all sites:
-----------------------------------------------------------------------------------------------------------------------------------------------------------

When we update causal log odds ratio at hospital 4, we need to make sure that we pass in “lastsite = TRUE” in `Update_ate`

    b=4
    #Site 4 loads output results from site 3
    load(paste(tempdatadir,"/Simdata/hospital",b, "/output_r2.RData", sep=""))
    #Site 4 prepares its local data
    X<-y<-A<-NULL;
    load(paste(tempdatadir,"/Simdata/hospital",b,"/Simdata.RData",sep=""))
    #Site 4 updates statistics. The outputs from site 3 are used as input for `Update_ate()`.
    betahat = output$beta_ps
    y_treated_sum =output$y_treated_sum; y_control_sum=output$y_control_sum; n_treated = output$n_treated;n_control = output$n_control
    meat = output$meat; bread =output$bread
    output = Update_ate(betahat,A,y,X,y_treated_sum,y_control_sum,n_treated,n_control,meat,bread,TRUE)
    #Site 4 finishes analysis and save the results in the designated directory where every group will have access to.
    print(output)
    #>        ATE        var 
    #> 0.08776381 0.88152685
    save(output, file = paste(tempdatadir,"/Simdata/hospital",b, "/finalouput.RData", sep=""))

(function () { var script = document.createElement("script"); script.type = "text/javascript"; script.src = "https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML\_HTMLorMML"; document.getElementsByTagName("head")\[0\].appendChild(script); })();