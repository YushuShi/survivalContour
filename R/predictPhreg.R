#' @import prodlim
#' @importFrom mets cumsumstrata squareintHdM readPhreg
#' @importFrom stats coef qnorm

predictPhreg <- function(object,newdata,times=NULL,individual.time=FALSE,tminus=FALSE,se=TRUE,robust=FALSE,conf.type="log",conf.int=0.95,km=FALSE,...)
{# {{{ default is all time-points from the object

  ### take baseline and strata from object# {{{
  strata <- object$strata[object$jumps]
  nstrata <- object$nstrata
  jumptimes <- object$cumhaz[,1]
  chaz <- object$cumhaz[,2]
  if (se) {
    if (!robust) {
      se.chaz <- object$se.cumhaz[,2]
      #varbeta <- object$ihessian
      varbeta <- object$II
      Pt <- apply(object$E/c(object$S0),2,cumsumstrata,strata,nstrata)
    } else {
      if (is.null(object$opt) | is.null(object$coef)) fixbeta<- 1 else fixbeta <- 0
      IsdM <- squareintHdM(object,ft=NULL,fixbeta=fixbeta,...)
      ###
      se.chaz <-   IsdM$varInt[object$jumps]^.5
      covv <- IsdM$covv[object$jumps,,drop=FALSE]
      varbeta <- IsdM$vbeta
      Pt <- IsdM$Ht[object$jumps,,drop=FALSE]
    }
  } # }}}


  ### setting up newdata with factors and strata
  desX <- readPhreg(object,newdata)
  X <- desX$X
  strataNew <- desX$strata

  if (is.null(times)) times <- sort(unique(c(object$exit)))
  if (individual.time & is.null(times)) times <- c(object$exit)
  if (individual.time & length(times)==1) times <- rep(times,length(object$exit))

  se.cumhaz <- NULL
  if (!individual.time) {
    pcumhaz <- surv <- matrix(0,nrow(X),length(times))
    if (se) se.cumhaz <- matrix(0,nrow(X),length(times))
  } else {
    pcumhaz <- surv <- matrix(0,nrow(X),1)
    if (se) se.cumhaz <- matrix(0,nrow(X),1)
  }
  hazt <- length(times)

  for (j in unique(strataNew)) {
    where <- sindex.prodlim(c(0,jumptimes[strata==j]),times,strict=tminus)
    plhazt <- hazt <- c(0,chaz[strata==j])
    if (km) { plhazt <- c(1,exp(cumsum(log(1-diff(hazt)))));  plhazt[is.na(hazt)] <- 0 }
    if (se) se.hazt <- c(0,se.chaz[strata==j])
    Xs <- X[strataNew==j,,drop=FALSE]
    ###	offs <- object$offsets[object$strata==j]
    if (object$p==0) RR <- rep(1,nrow(Xs)) else RR <- c(exp( Xs %*% coef(object)))
    if (se)  { # {{{ based on Hazard's
      if (object$p>0) {
        Ps <- Pt[strata==j,,drop=FALSE]
        Ps <- rbind(0,Ps)[where,,drop=FALSE]
        #  print(Xs); print(varbeta); print(dim(Ps)); print((Xs %*% varbeta))
        Xbeta <- Xs %*% varbeta
        seXbeta <- rowSums(Xbeta*Xs)^.5
        cov2 <- cov1 <- Xbeta %*% t(Ps*hazt[where])
        if (robust)	{
          covvs <- covv[strata==j,,drop=FALSE]
          covvs <- rbind(0,covvs)[where,,drop=FALSE]
          covv1 <- Xs %*% t((covvs*hazt[where]))
          cov1 <- cov1-covv1
        }
      } else cov1 <- 0
    }# }}}
    haztw <- hazt[where]
    if (se) se.haztw <- se.hazt[where]
    if (is.null(object$propodds)) {
      plhaztw <- plhazt[where]
      if (!individual.time) pcumhaz[strataNew==j,]  <- RR%o%haztw else pcumhaz[strataNew==j,] <- RR*haztw[strataNew==j]
      if (!km) {
        if (!individual.time) surv[strataNew==j,]  <- exp(- RR%o%haztw)
        else surv[strataNew==j,]  <- exp(-RR*haztw[strataNew==j])
      } else {
        if (!individual.time) surv[strataNew==j,]  <- exp( RR %o% log(plhaztw))
        else surv[strataNew==j,]  <- plhaztw[strataNew==j]^RR
      }
    } else {
      if (!individual.time) surv[strataNew==j,]  <- 1/(1+RR%o%haztw)
      else surv[strataNew==j,]  <- 1/(1+RR*haztw[strataNew==j])
    }
    if (se) {# {{{
      if (object$p>0)  {
        if (!individual.time) se.cumhaz[strataNew==j,]  <-
            ((RR %o% se.haztw)^2+(c(RR*seXbeta) %o% haztw)^2-2*RR^2*cov1)^.5
        else se.cumhaz[strataNew==j,]  <- RR* (se.haztw^2+(c(seXbeta)*haztw)^2-2*diag(cov1))^.5
      } else {
        if (!individual.time) se.cumhaz[strataNew==j,]  <- RR %o% (se.haztw)
        else se.cumhaz[strataNew==j,]  <- RR* se.haztw[strataNew==j]
      }
    }# }}}
  }


  zval <- qnorm(1 - (1 - conf.int)/2, 0, 1)
  std.err <- se.cumhaz
  cisurv  <- list()
  cisurv$upper <- NULL
  cisurv$lower <- NULL

  ### different conf-types for surv
  if (se) {# {{{
    if (conf.type == "plain") {# {{{
      temp1 <- surv + zval * std.err * surv
      temp2 <- surv - zval * std.err * surv
      cisurv <- list(upper = pmin(temp1, 1), lower = pmax(temp2,
                                                          0), conf.type = "plain", conf.int = conf.int)
    }
    if (conf.type == "log") {
      xx <- ifelse(surv == 0, 1, surv)
      temp1 <- ifelse(surv == 0, NA, exp(log(xx) + zval * std.err))
      temp2 <- ifelse(surv == 0, NA, exp(log(xx) - zval * std.err))
      cisurv <- list(upper = pmin(temp1, 1), lower = temp2,
                     conf.type = "log", conf.int = conf.int)
    }
    if (conf.type == "log-log") {
      who <- (surv == 0 | surv == 1)
      temp3 <- ifelse(surv == 0, NA, 1)
      xx <- ifelse(who, 0.1, surv)
      temp1 <- exp(-exp(log(-log(xx)) + zval * std.err/log(xx)))
      temp1 <- ifelse(who, temp3, temp1)
      temp2 <- exp(-exp(log(-log(xx)) - zval * std.err/log(xx)))
      temp2 <- ifelse(who, temp3, temp2)
      cisurv <- list(upper = temp1, lower = temp2,
                     conf.type = "log-log", conf.int = conf.int)
    }# }}}
  }# }}}

  if (object$p>0) RR <-  exp(X %*% coef(object)) else RR <- rep(1,nrow(X))

  ### non-cox setting
  if (!is.null(object$propodds)) pcumhaz <- -log(surv)

  out <- list(surv=surv,times=times,
              upper=cisurv$upper,lower=cisurv$lower,cumhaz=pcumhaz,se.cumhaz=se.cumhaz,strata=strataNew,X=X, RR=RR)
  if (length(class(object))==2 && substr(class(object)[2],1,3)=="cif") {
    out <- c(out,list(cif=1-out$surv,cif.lower=1-out$upper, cif.upper=1-out$lower))
  }
  class(out) <- c("predictphreg")
  if (length(class(object))==2) class(out) <- c("predictphreg",class(object)[2])
  return(out)
}# }}}
