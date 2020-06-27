# Functions used for CPT simulation
# Xtophe Bontemps 
# April 2020


# Generates the random numbers (states of nature) to compute the integral with respect to Normal distribution
# Returns a dataframe with Neps couples (point, probability) for discrete approximation of a Normal distribution  

GenEpsilon <- function(Neps){
  set.seed(2512)
  eps <- sort(rnorm(Neps, mean=0, sd=1))
  
  # Data.frame
  df.eps <- data.frame(eps)
  df.eps <-df.eps %>%
    mutate(
      halfpoint =  lag(eps) + 1/2*(eps - lag(eps)),
      probeps = pnorm(lead(halfpoint)) - pnorm(halfpoint)
    )
  
  # Completing the endpoints 
  df.eps$probeps[1] <- pnorm(df.eps$halfpoint[2])
  df.eps$probeps[Neps] <- 1-pnorm(df.eps$halfpoint[Neps])
  
  # Cleaning
  df.eps <- select(df.eps, -halfpoint)
  
  # Checking the sum is equal to 1
  # print(colSums(df.eps))
  
  return(df.eps)
}

# CRRA Utility function
U.CRRA <- function(w, r)
{
  if (r == 1) {
    # for both cases: If w <=0 , we impute _NA_real_ which is compatible with real operations
    u <- ifelse(w>0, log(w), NA_real_) 
  }
  else {
    u <- ifelse(w>0, (1/(1-r)) * w^(1-r), NA_real_)
  }
  return(u)
}

# Kahneman and Tversky Utility function 

U.KT <-function(w,
                rrminus,
                rrplus,
                lambda)
{
  if (rrplus == 1) {
    u <- ifelse(w>0, log(w), NA_real_)
  }
  else if (rrminus == -1) {
    u <- ifelse(w>0, (-lambda) * log(w), NA_real_)  
  }
  
  else {  
    u <- ifelse(w > 0, (1/(1-rrplus)) * w^(1-rrplus) ,
                ifelse(w <= 0, (-lambda/(1-rrminus))*((-w)^(1- rrminus)),NA_real_))
  }
  return(u)
}



## Computing the CPT function
# Needs functions: 
#        - U.KT (KT utility function)
## returning x* and the RP 

resultsKT <- function(df.profit, 
                      wref, 
                      rrplus , 
                      rrminus ,
                      lambda , 
                      # Weight function parameters
                      delta ,  
                      gamma 
)
{
  
  # Substracting wref, the reference profit 
  df.profMat <- df.profit - wref
  
  df.ProbProfMat <- data.frame(mapply(`*` , df.profMat, Epsilons$probeps))

  # Expected profit 
  df.ExpectedProf = rowSums(df.ProbProfMat)
  
  ### ---  Computing Pi's [Notations refers to BABCOCK (2015)] 
  
  # Gain and loss dummy probabilities 
  df.Iloss <- as.data.frame(ifelse(df.profMat < 0,1,0 ))
  df.Igain <- as.data.frame(ifelse(df.profMat > 0,1,0 ))
  
  df.probloss <- data.frame(mapply(`*` , df.Iloss, Epsilons$probeps))
  df.probgain <- data.frame(mapply(`*` , df.Igain, Epsilons$probeps))
  
  # computing Piplus and Piminus 
  df.cumprobloss <- as.data.frame(t(apply(df.probloss, 1, cumsum)))
  
  # Here we need to reverse cumsum
  df.cumprobgain <- as.data.frame(t(apply(df.probgain, 1, revcumsum)))
  
  # Weighting Function (applies to both delta and gamma)
  
  Fweight <-function(x, theta)
    #Theta can be  either delta or gamma 
  {
    w  <-  (x^theta)/ (x^theta + (1-x)^theta )^(1/theta) 
    return(w)
  }
  
  df.weightloss <- map_df(df.cumprobloss, Fweight, theta = delta)
  df.weightgain <- map_df(df.cumprobgain, Fweight, theta = gamma)
  
  # Computing PiGain and PiLoss
  
  pi.loss <- as.data.frame(t(diff(t(df.weightloss))))
  
  # we need to add the first element
  probeps1 <- Epsilons$probeps[1]
  pi.loss1 <- Fweight(probeps1, theta = delta ) 
  pi.loss <- cbind( pi.loss1, pi.loss)  
  
  ## Gains
  pi.gain <- rev(as.data.frame(t(diff(t(rev(df.weightgain))))))
  
  # we need to add the last element
  probepsEnd <- Epsilons$probeps[N.eps]  
  pi.gainEnd <- Fweight(probepsEnd, theta = gamma ) #  <<< pi.loss1 est un scalaire... 
  
  pi.gain <- cbind( pi.gain, pi.gainEnd)
  
  ###  Applying probablities of gains and losses to resp. cells (matrix of dummies)
  df.pi <- (pi.loss * df.Iloss) + (pi.gain * df.Igain)  # term to term multiplication with zero-one matrices

  # Applying KT Utility to profits
  df.Util <- purrr::map_df(df.profMat, U.KT, 
                           rrplus = rrplus,
                           rrminus = rrminus,
                           lambda = lambda )
  
  
  ## CPT value function 
  df.EUCPTMAT <-  df.Util * df.pi
  
  # Putting everything in df.final
  df.final <- cbind(df, df.ExpectedProf)   
  colnames(df.final) <- c("x", "EProf")
  
  df.final$EUKT <-  rowSums(df.EUCPTMAT,  na.rm=T)
  ##  very important to remove the NA, otherwise the rowsum is not working
  
  ## Retrieving the index of the maximum;  
  i.star <- which.max(df.final$EUKT)
  x.star  <- df.final[i.star,"x"]
  EU.star <- df.final[i.star, "EUKT"]
  EProf.star <- df.final[i.star, "EProf"]
  
  # Risk premium
  ## -- Defining the optimization function
  fopti.KT <- function(RP,  ...){
    abs(EU.star - U.KT(EProf.star - RP,
                       rrplus = rrplus,
                       rrminus = rrminus,
                       lambda = lambda)
    )
  }
  
  # Solution (Risk Premium)
  RP.sol <- optimize(fopti.KT, interval = c(-100,500), maximum = FALSE)
  RP <- round(RP.sol$minimum, digits = 3)
  
  ###  Certainty equivalent 
  CE <- EProf.star - RP
  
  ## -- Return list --
  ReturnList <- list("X.star" = x.star, "RP" = round(RP, 2))
  return(ReturnList )
}

