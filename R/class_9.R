#' Moving Average
#'
#' @description We created a function that calculates a moving average of a
#'   specified window width across a vector.
#' @param x a vector for which you will be calculating the moving average.
#' @param side whether you want to average elements on the left, right, or both
#'   sides; should accept the following values: "left","right","both".
#' @param numbertoside the number of elements to the side of your index element
#'   that you want to include in the moving average.
#' @details To create this function, we only used methods that were
#'   introduced in either Lecture 1 or Lecture 2.
#' @return A vector of the moving average
#' \item{Input}{input vector}
#' \item{Output}{output vector}
#' @examples
#' inputdata<-c(0,2.457,3.878,3.663,1.90,-0.658,-2.943,-3.986,-3.349,-1.299,
#' 1.299,3.349,3.986,2.943,0.658,-1.904,-3.663,-3.878,-2.457,0)
#' movingaverage(x=inputdata,numbertoside=3,side="left")

movingaverage<-function(x=x,side="both",numbertoside=NULL){

  returnma<-NULL
  for(i in 1:length(x)){

    if (side=="both"){
      start<-i-numbertoside
      stop<-i+numbertoside
    } else if (side=="left"){
      start<-i-numbertoside
      stop<-i
    } else if (side=="right"){
      start<-i
      stop<-i+numbertoside
    }
    start<-max(start,1)
    tempdata<-x[start:stop]
    outaverage<-mean(tempdata,na.rm=TRUE)
    returnma<-c(returnma,outaverage)
  }
  malist<-list(Input=x,Output=returnma)
  return(malist)
}


#' Two-Sample Power Calculation Based on Simulation
#'
#' @description Power is the probability we reject the null hypothesis given it
#'   is false. Building on this definition, create a function that uses
#'   simulations to estimate power for a two-sample T-test.
#' @param Var1mean Variable 1 mean
#' @param Var2mean Variable 2 mean
#' @param Var1sd Variable 1 standard deviation
#' @param Var2sd Variable 2 standard deviation
#' @param Var1samplesize Variable 1 sample size
#' @param Var2samplesize Variable 2 sample size
#' @param nsim Number of simulations
#' @param alphalevel alpha-level (default=0.05)
#' @details First, you will need to simulate two normally distributed variables,
#'   each with a distinct sample size, mean, and standard deviation, and perform
#'   a T-test. For that single simulation, evaluate if we would reject the null
#'   hypothesis given a specific alpha-level. Now repeat this simulation many
#'   times. Power can then be estimated as the proportion of simulations for
#'   which we rejected the null hypothesis.
#' @return Empirical power calculation
#' @examples
#' SimTtestPower(Var1mean=20,Var2mean=22,Var1sd=4,Var2sd=6,
#' Var1samplesize=40,Var2samplesize=40,nsim=10000,alphalevel=0.05)

SimTtestPower<-function(Var1mean=NULL,Var2mean=NULL,Var1sd=NULL,
                        Var2sd=NULL,Var1samplesize=NULL,Var2samplesize=NULL,
                        nsim=100,alphalevel=0.05){

  sigyesorno<-replicate(nsim,{
    Var1<-rnorm(Var1samplesize,mean=Var1mean,sd=Var1sd)
    Var2<-rnorm(Var2samplesize,mean=Var2mean,sd=Var2sd)
    tempttestpval=t.test(Var1,Var2)$p.value
    output<-if(tempttestpval<alphalevel) 1 else 0
    return(output)
  })
  temppower<-sum(sigyesorno)/nsim
  return(temppower)
}
