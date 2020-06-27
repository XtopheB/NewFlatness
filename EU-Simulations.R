# Generating TABLE 1 
# Using MainCPT-KT to simplify the program
# 09/04/2020 : Results of table 1 verified (only delta xstar change because optimal is 264 here)
# 11/04/2020: Functions removed and copied toi GlobalFunctions.R 
# 11/04/2020: Loops and output tables (4 and 5) created.  



library(dplyr)
library(ggplot2)
library(stargazer)
library(purrr)


source("GlobalFunctions.R")

## Step 0:  Nature's distribution (epsilon generation)

N.eps <- 1000  ####  <<<--- Change to 1000  here
Simu.Case <- "C"

# Computing Epsilons data.frame 
Epsilons <- GenEpsilon(Case = Simu.Case, Neps = N.eps)

# Simulation parameter info
Epsilon.Case <- ifelse(Simu.Case == "A", "Normally drawn", 
                       ifelse(Simu.Case == "B",  "Uniformly drawn",
                              "equaly spaced" ))


##  Step 1 : Generation of equally distributed Xs
suppmax <- 400  #defines the support of inputs in [0, suppmax ]

Ns <- 400     #### <<<-----  change to 400 points 
x <- as.data.frame(seq(from = 0, to = suppmax, length.out = Ns+1 ))
x<- x[-1,]  #  to avoid start at x=0

df<- data.frame(x)
df.final <- df

## Step 2: Production function 

# Production function parameters (Benchmark)  
beta0 = 0
beta1 = 15
a1 = 0.30
beta2 = 30
a2 = -0.10

text.param.prod <-  paste("Production function y =",beta1," * x^", a1, " +",beta2,"* x^",a2," * eps" )

# Profit function parameters
px <- 11   
initial.wealth <- 500


#Individal profit
df.profMat <- data.frame(mapply(function(x, y) (initial.wealth + px*(beta0 + beta1 * x^a1 + beta2 * (x ^a2) *y) - x),
                   df,Epsilons$eps))


# Computing the integral  (Expected profit)
df.EprofMat <- data.frame(mapply(`*` , df.profMat, Epsilons$probeps))

# Expected Profit
df.final$EProf <- rowSums(df.EprofMat)


####################  TABLE 1   ##################
##################################################
r.list <- c(0,1,2,3,4)
res.CRRA<- data.frame()

for (i.r in seq_along(r.list))  {
  res.CRRA [1, "price ratio"] <- px
  
  #Expected Utility -- Case CRRA 
  
  ## computing the risk neutral reference 
  df.final0 <- df    # initialisation
  
  df.UMat0 <- map_df(df.profMat, U.CRRA, r = 0)
  # Computing the integral 
  
  df.EUMat0 <-  data.frame(mapply(`*` , df.UMat0, Epsilons$probeps))
  df.final0$EU <- rowSums(df.EUMat0)
  
  
  # Maximums (whatever the model)
  i.star0 <- which.max(df.final0$EU)
  x.star0  <- df.final0[i.star0,"x"]
  EU.star0 <- df.final0[i.star0, "EU"]
  EProf.star0 <- df.final0[i.star0, "EProf"]
  
  
  ## Computing for all  r.list values 
  df.UMat <- map_df(df.profMat, U.CRRA, r = r.list[i.r])
  # Computing the integral 
  
  df.EUMat <-  data.frame(mapply(`*` , df.UMat, Epsilons$probeps))
  df.final$EU <- rowSums(df.EUMat)
  
 
  # Maximums (whatever the model)
  i.star <- which.max(df.final$EU)
  x.star  <- df.final[i.star,"x"]
  EU.star <- df.final[i.star, "EU"]
  EProf.star <- df.final[i.star, "EProf"]
  
  
  # # Step 4 : Risk premium
  # Optimisation fucntion defiend for each value of r 
  fopti.CRRA.r <- function(RP, ...){
    abs(EU.star - U.CRRA(df.final[i.star,]$EProf - RP,r = r.list[i.r]))
  }
  
  # Input used for self‐insurance
  
  delta.x <- 100*(x.star - x.star0)/x.star
  
  # Solution and Risk premium CRRA 
  RP.sol <- optimize( fopti.CRRA.r, interval = c(-100,500), maximum = FALSE)
  RP <- round(RP.sol$minimum, digits = 3)
  
  ###  Certainty equivalent
  CE <- df.final[i.star,"EProf"] - RP
  
  ## Puting that in table 
  
  res.CRRA[ 1, paste0("r = ",r.list[i.r])] <- round(x.star, digits = 0)
  res.CRRA[ 2, paste0("r = ",r.list[i.r])] <-  round(delta.x, digits = 2)
  res.CRRA[ 3, paste0("r = ",r.list[i.r])] <-  round(RP, digits = 0)
 # res.CRRA[ 3, paste0("r = ",r.list[i.r])] <-  round(EProf.star, digits = 2)
  res.CRRA[ 4, paste0("r = ",r.list[i.r])] <-  round(CE, digits = 0)
  res.CRRA[ 5, paste0("r = ",r.list[i.r])] <-  round(-100*RP/EProf.star, digits = 2)
 # res.CRRA[6,] <- c(NA, r.list)
}

rownames(res.CRRA) <- c( "X*","Input change", "RP",  "CE","RP/Prof*" )

# Results

print(paste("CRRA with epsilon", Epsilon.Case, ",(N.eps =", N.eps,"), Initial wealth = ", initial.wealth))
print(text.param.prod)
print(res.CRRA)

## formatted output 
stargazer(res.CRRA,
          summary = FALSE,
          title = paste("CRRA (N.eps =", N.eps,"), Initial wealth = ", initial.wealth),
          notes = paste0(text.param.prod,"  (Last run:", format(Sys.time(), "%Y-%m-%d"), ")"),
          digits = 1,
         # type = 'latex', out = 'results/Table1.tex')
          type = 'text', out = 'results/Table1.txt')



#######################
######## Table  2  ####
#######################


# simulation parameters for px and r levels
r.list <- c(0,1,2,3,4)
px.list <- c(13, 11, 9,7)

initial.wealth <- 500

# Initializing 
Simu.px <- data.frame()


for (j in seq_along(px.list))  {
    #initializing
    res.CRRA<- data.frame()
    
    # Profit function parameters
    px <- px.list[j]   
    
    #Individal profit
    df.profMat <- data.frame(mapply(function(x, y) (initial.wealth + px*(beta0 + beta1 * x^a1 + beta2 * (x ^a2) *y) - x),
                                    df,Epsilons$eps))
    
    
    # Computing the integral  (Expected profit)
    df.EprofMat <- data.frame(mapply(`*` , df.profMat, Epsilons$probeps))
    
    # Expected Profit
    df.final$EProf <- rowSums(df.EprofMat)
    
    for (i.r in seq_along(r.list))  {
      res.CRRA [1, "price ratio"] <- px
      
      #Expected Utility -- Case CRRA 
      
      ## computing the risk neutral reference 
      df.final0 <- df    # initialisation
      
      df.UMat0 <- map_df(df.profMat, U.CRRA, r = 0)
      # Computing the integral 
      
      df.EUMat0 <-  data.frame(mapply(`*` , df.UMat0, Epsilons$probeps))
      df.final0$EU <- rowSums(df.EUMat0)
      
      
      # Maximums (whatever the model)
      i.star0 <- which.max(df.final0$EU)
      x.star0  <- df.final0[i.star0,"x"]
      EU.star0 <- df.final0[i.star0, "EU"]
      EProf.star0 <- df.final0[i.star0, "EProf"]
      
      
      ## Computing for all  r.list values 
      df.UMat <- map_df(df.profMat, U.CRRA, r = r.list[i.r])
      # Computing the integral 
      
      df.EUMat <-  data.frame(mapply(`*` , df.UMat, Epsilons$probeps))
      df.final$EU <- rowSums(df.EUMat)
      
      
      # Maximums (whatever the model)
      i.star <- which.max(df.final$EU)
      x.star  <- df.final[i.star,"x"]
      EU.star <- df.final[i.star, "EU"]
      EProf.star <- df.final[i.star, "EProf"]
      
      
      # # Step 4 : Risk premium
      # Optimisation fucntion defiend for each value of r 
      fopti.CRRA.r <- function(RP, ...){
        abs(EU.star - U.CRRA(df.final[i.star,]$EProf - RP,r = r.list[i.r]))
      }
      
      # Input used for self‐insurance
      
      delta.x <- 100*(x.star - x.star0)/x.star
      
      # Solution and Risk premium CRRA 
      RP.sol <- optimize( fopti.CRRA.r, interval = c(-100,500), maximum = FALSE)
      RP <- round(RP.sol$minimum, digits = 3)
      
      ###  Certainty equivalent
      CE <- df.final[i.star,"EProf"] - RP
      
      ## Puting that in table 
      
      res.CRRA[ 1, paste0("r = ",r.list[i.r])] <- round(x.star, digits = 0)
      res.CRRA[ 2, paste0("r = ",r.list[i.r])] <-  round(delta.x, digits = 2)
      res.CRRA[ 3, paste0("r = ",r.list[i.r])] <-  round(RP, digits = 0)
      #res.CRRA[ 4, paste0("r = ",r.list[i.r])] <-  round(CE, digits = 0)
      res.CRRA[ 4, paste0("r = ",r.list[i.r])] <-  round(-100*RP/EProf.star, digits = 2)
      res.CRRA[5, ] <- NA
      
      rownames(res.CRRA) <- c( "X*","Input change", "RP","RP/Prof*", " " )
      
    } #end loop over values of r
     
    
    # On screen results
    
    print(paste("CRRA with epsilon", Epsilon.Case, ",(N.eps =", N.eps,"), Initial wealth = ", initial.wealth))
    print(text.param.prod)
    print(res.CRRA)
    
    # Binding results
    Simu.px <-  rbind( Simu.px, res.CRRA)
    
}  #end loop over price ratio, px    

    
print(Simu.px)


## formatted output 
stargazer(Simu.px,
          summary = FALSE,
          title = paste("CRRA (N.eps =", N.eps,"), Initial wealth = ", initial.wealth),
          notes = paste0(text.param.prod,"  (Last run:", format(Sys.time(), "%Y-%m-%d"), ")"),
          digits = 1,
          type = 'text', out = 'results/Table2.txt')



#######################
######## Table  3  ####
#######################


# Simulation parameters 
r.list <- c(0,1,2,3,4)

# Cases proposed for parameters  in production function 
case.prod.f <-        data.frame(c(15, 0.20))  # Case1
case.prod.f <- cbind(case.prod.f,c(15, 0.25))  # Case 2
case.prod.f <- cbind(case.prod.f,c(15, 0.30 )) # Benchmark
case.prod.f <- cbind(case.prod.f,c(10, 0.30 )) # Case4
case.prod.f <- cbind(case.prod.f,c(18, 0.30 )) # Case5

row.names(case.prod.f ) <- c("beta1", "a1") 
print(case.prod.f)

# Global (invariant)  parameters
initial.wealth <- 500
px <- 11

# Initializing 
Simu.prod.f <- data.frame()

for (j in 1:ncol(case.prod.f) )  {
  
  # Production function parameters  
  beta0 = 0
  beta1 = case.prod.f[1,j]
  a1 = case.prod.f[2,j]
  beta2 = 30
  a2 = -0.10
  
 # For output presentation 
  param.prod.f <- paste0(beta1,"* x**", a1 )
  param.prod.g <- paste0(beta2,"* x**",a2)
  text.param.prod <-  paste0("Production function y = ",param.prod.f," + ",param.prod.g,"* eps"  )
  print(text.param.prod)
  
  #Individal profit
  df.profMat <- data.frame(mapply(function(x, y) (initial.wealth + px*(beta0 + beta1 * x^a1 + beta2 * (x ^a2) *y) - x),
                                  df,Epsilons$eps))
  
  # Computing the integral  (Expected profit)
  df.EprofMat <- data.frame(mapply(`*` , df.profMat, Epsilons$probeps))
  
  # Expected Profit
  df.final$EProf <- rowSums(df.EprofMat)
  
  res.CRRA<- data.frame()
  for (i.r in seq_along(r.list))  {
    res.CRRA [1, "Production function"] <- paste0("f(x) = ",param.prod.f)
    
    #Expected Utility -- Case CRRA 
    
    ## computing the risk neutral reference 
    df.final0 <- df    # initialisation
    
    df.UMat0 <- map_df(df.profMat, U.CRRA, r = 0)
    # Computing the integral 
    
    df.EUMat0 <-  data.frame(mapply(`*` , df.UMat0, Epsilons$probeps))
    df.final0$EU <- rowSums(df.EUMat0)
    
    
    # Maximums (whatever the model)
    i.star0 <- which.max(df.final0$EU)
    x.star0  <- df.final0[i.star0,"x"]
    EU.star0 <- df.final0[i.star0, "EU"]
    EProf.star0 <- df.final0[i.star0, "EProf"]
    
    
    ## Computing for all  r.list values 
    df.UMat <- map_df(df.profMat, U.CRRA, r = r.list[i.r])
    # Computing the integral 
    
    df.EUMat <-  data.frame(mapply(`*` , df.UMat, Epsilons$probeps))
    df.final$EU <- rowSums(df.EUMat)
    
    
    # Maximums (whatever the model)
    i.star <- which.max(df.final$EU)
    x.star  <- df.final[i.star,"x"]
    EU.star <- df.final[i.star, "EU"]
    EProf.star <- df.final[i.star, "EProf"]
    
    
    # # Step 4 : Risk premium
    # Optimisation fucntion defiend for each value of r 
    fopti.CRRA.r <- function(RP, ...){
      abs(EU.star - U.CRRA(df.final[i.star,]$EProf - RP,r = r.list[i.r]))
    }
    
    # Input used for self‐insurance
    
    delta.x <- 100*(x.star - x.star0)/x.star
    
    # Solution and Risk premium CRRA 
    RP.sol <- optimize( fopti.CRRA.r, interval = c(-100,500), maximum = FALSE)
    RP <- round(RP.sol$minimum, digits = 3)
    
    ###  Certainty equivalent
    CE <- df.final[i.star,"EProf"] - RP
    
    ## Puting that in table 
    
    res.CRRA[ 1, paste0("r = ",r.list[i.r])] <- round(x.star, digits = 0)
    res.CRRA[ 2, paste0("r = ",r.list[i.r])] <-  round(delta.x, digits = 2)
    res.CRRA[ 3, paste0("r = ",r.list[i.r])] <-  round(RP, digits = 0)
    res.CRRA[ 4, paste0("r = ",r.list[i.r])] <-  round(-100*RP/EProf.star, digits = 2)
    res.CRRA[5, ] <- NA
    
  } #end loop over values of r
  
  rownames(res.CRRA) <- c( "X*","Input change", "RP","RP/Prof*", " " )
  # On screen results
  
  print(paste("Price ratio =", px,", Initial wealth = ", initial.wealth))
  print(text.param.prod)
  print("-- ")
  print(res.CRRA)
  
  # Binding results
  Simu.prod.f  <-  rbind( Simu.prod.f , res.CRRA)
  
}  #end loop over production function parameters 

# on screen results
print(Simu.prod.f )


## formatted output 
stargazer(Simu.prod.f,
          summary = FALSE,
          title = paste("Price ratio =", px,", Initial wealth = ", initial.wealth),
          notes = paste0("production function = f(x) + ", param.prod.g, " (Last run:", format(Sys.time(), "%Y-%m-%d"), ")"),
          digits = 1,
          type = 'text', out = 'results/Table3.txt')



################################
######## Table  Appendix 4  ####
################################


# Simulation parameters 
r.list <- c(0,1,2,3,4)

# Cases proposed for parameters  in production function 
case.prod.g <-        data.frame(c(20,-0.1))  # Case1
case.prod.g<- cbind(case.prod.g,c(40, -0.1))  # Case 2
case.prod.g<- cbind(case.prod.g,c(30, -0.1 )) # Benchmark
case.prod.g<- cbind(case.prod.g,c(30, -0.2 )) # Case4

row.names(case.prod.g) <- c("beta2", "a2") 
print(case.prod.g)

# Global (invariant)  parameters
initial.wealth <- 500
px <- 11

# Initializing 
Simu.prod.g <- data.frame()

for (j in 1:ncol(case.prod.g) )  {
  
  # Production function parameters  
  beta0 = 0
  beta1 = 15
  a1 = 0.30
  beta2 = case.prod.g[1,j]
  a2 = case.prod.g[2,j]
  
  # For output presentation 
  param.prod.f <- paste0(beta1,"* x**", a1 )
  param.prod.g <- paste0(beta2,"* x**",a2)
  text.param.prod <-  paste0("Production function y = ",param.prod.f," + ",param.prod.g,"* eps"  )
  print(text.param.prod)
  
  #Individal profit
  df.profMat <- data.frame(mapply(function(x, y) (initial.wealth + px*(beta0 + beta1 * x^a1 + beta2 * (x ^a2) *y) - x),
                                  df,Epsilons$eps))
  
  # Computing the integral  (Expected profit)
  df.EprofMat <- data.frame(mapply(`*` , df.profMat, Epsilons$probeps))
  
  # Expected Profit
  df.final$EProf <- rowSums(df.EprofMat)
  
  res.CRRA<- data.frame()
  for (i.r in seq_along(r.list))  {
    res.CRRA [1, "Production function"] <- paste0("g(x) = ",param.prod.g)
    
    #Expected Utility -- Case CRRA 
    
    ## computing the risk neutral reference 
    df.final0 <- df    # initialisation
    
    df.UMat0 <- map_df(df.profMat, U.CRRA, r = 0)
    # Computing the integral 
    
    df.EUMat0 <-  data.frame(mapply(`*` , df.UMat0, Epsilons$probeps))
    df.final0$EU <- rowSums(df.EUMat0)
    
    
    # Maximums (whatever the model)
    i.star0 <- which.max(df.final0$EU)
    x.star0  <- df.final0[i.star0,"x"]
    EU.star0 <- df.final0[i.star0, "EU"]
    EProf.star0 <- df.final0[i.star0, "EProf"]
    
    
    ## Computing for all  r.list values 
    df.UMat <- map_df(df.profMat, U.CRRA, r = r.list[i.r])
    # Computing the integral 
    
    df.EUMat <-  data.frame(mapply(`*` , df.UMat, Epsilons$probeps))
    df.final$EU <- rowSums(df.EUMat)
    
    
    # Maximums (whatever the model)
    i.star <- which.max(df.final$EU)
    x.star  <- df.final[i.star,"x"]
    EU.star <- df.final[i.star, "EU"]
    EProf.star <- df.final[i.star, "EProf"]
    
    
    # # Step 4 : Risk premium
    # Optimisation fucntion defiend for each value of r 
    fopti.CRRA.r <- function(RP, ...){
      abs(EU.star - U.CRRA(df.final[i.star,]$EProf - RP,r = r.list[i.r]))
    }
    
    # Input used for self‐insurance
    
    delta.x <- 100*(x.star - x.star0)/x.star
    
    # Solution and Risk premium CRRA 
    RP.sol <- optimize( fopti.CRRA.r, interval = c(-100,500), maximum = FALSE)
    RP <- round(RP.sol$minimum, digits = 3)
    
    ###  Certainty equivalent
    CE <- df.final[i.star,"EProf"] - RP
    
    ## Puting that in table 
    
    res.CRRA[ 1, paste0("r = ",r.list[i.r])] <- round(x.star, digits = 0)
    res.CRRA[ 2, paste0("r = ",r.list[i.r])] <-  round(delta.x, digits = 2)
    res.CRRA[ 3, paste0("r = ",r.list[i.r])] <-  round(RP, digits = 0)
    res.CRRA[ 4, paste0("r = ",r.list[i.r])] <-  round(-100*RP/EProf.star, digits = 2)
    res.CRRA[5, ] <- NA
    
  } #end loop over values of r
  
  rownames(res.CRRA) <- c( "X*","Input change", "RP","RP/Prof*", " " )
 
   # On screen results
  print(paste("Price ratio =",px , "Initial wealth = ", initial.wealth))
  print(text.param.prod)
  print("-- ")
  print(res.CRRA)
  
  # Binding results
  Simu.prod.g  <-  rbind( Simu.prod.g , res.CRRA)
  
}  #end loop over production function parameters 


print(Simu.prod.g )


## formatted output 
stargazer(Simu.prod.g,
          summary = FALSE,
          title = paste("Price ratio =", px,", Initial wealth = ", initial.wealth),
          notes = paste0("production function =", param.prod.f,"+ g(x) *eps (Last run:", format(Sys.time(), "%Y-%m-%d"), ")"),
          digits = 1,
          type = 'text', out = 'results/TableA4.txt')







################################
######## NEw TABLE WEALTH   ####
################################


# simulation parameters for Wealth and r levels
r.list <- c(0,1,2,3,4)
wealth.list <- c(500, 1000, 1500, 5000)

#Invariant parameter
px <- 11

# Initializing 
Simu.wealth <- data.frame()


for (j in seq_along(wealth.list))  {
  #initializing
  res.CRRA<- data.frame()
  
  # Profit function parameters
  initial.wealth <- wealth.list[j]   
  
  #Individal profit
  df.profMat <- data.frame(mapply(function(x, y) (initial.wealth + px*(beta0 + beta1 * x^a1 + beta2 * (x ^a2) *y) - x),
                                  df,Epsilons$eps))
  
  
  # Computing the integral  (Expected profit)
  df.EprofMat <- data.frame(mapply(`*` , df.profMat, Epsilons$probeps))
  
  # Expected Profit
  df.final$EProf <- rowSums(df.EprofMat)
  
  for (i.r in seq_along(r.list))  {
    res.CRRA [1, "Initial Wealth"] <- initial.wealth
    
    #Expected Utility -- Case CRRA 
    
    ## computing the risk neutral reference 
    df.final0 <- df    # initialisation
    
    df.UMat0 <- map_df(df.profMat, U.CRRA, r = 0)
    # Computing the integral 
    
    df.EUMat0 <-  data.frame(mapply(`*` , df.UMat0, Epsilons$probeps))
    df.final0$EU <- rowSums(df.EUMat0)
    
    
    # Maximums (whatever the model)
    i.star0 <- which.max(df.final0$EU)
    x.star0  <- df.final0[i.star0,"x"]
    EU.star0 <- df.final0[i.star0, "EU"]
    EProf.star0 <- df.final0[i.star0, "EProf"]
    
    
    ## Computing for all  r.list values 
    df.UMat <- map_df(df.profMat, U.CRRA, r = r.list[i.r])
    # Computing the integral 
    
    df.EUMat <-  data.frame(mapply(`*` , df.UMat, Epsilons$probeps))
    df.final$EU <- rowSums(df.EUMat)
    
    
    # Maximums (whatever the model)
    i.star <- which.max(df.final$EU)
    x.star  <- df.final[i.star,"x"]
    EU.star <- df.final[i.star, "EU"]
    EProf.star <- df.final[i.star, "EProf"]
    
    
    # # Step 4 : Risk premium
    # Optimisation fucntion defiend for each value of r 
    fopti.CRRA.r <- function(RP, ...){
      abs(EU.star - U.CRRA(df.final[i.star,]$EProf - RP,r = r.list[i.r]))
    }
    
    # Input used for self‐insurance
    
    delta.x <- 100*(x.star - x.star0)/x.star
    
    # Solution and Risk premium CRRA 
    RP.sol <- optimize( fopti.CRRA.r, interval = c(-100,500), maximum = FALSE)
    RP <- round(RP.sol$minimum, digits = 3)
    
    ###  Certainty equivalent
    CE <- df.final[i.star,"EProf"] - RP
    
    ## Puting that in table 
    
    res.CRRA[ 1, paste0("r = ",r.list[i.r])] <- round(x.star, digits = 0)
    res.CRRA[ 2, paste0("r = ",r.list[i.r])] <-  round(delta.x, digits = 2)
    res.CRRA[ 3, paste0("r = ",r.list[i.r])] <-  round(RP, digits = 0)
    #res.CRRA[ 4, paste0("r = ",r.list[i.r])] <-  round(CE, digits = 0)
    res.CRRA[ 4, paste0("r = ",r.list[i.r])] <-  round(-100*RP/EProf.star, digits = 2)
    res.CRRA[5, ] <- NA
    
    rownames(res.CRRA) <- c( "X*","Input change", "RP","RP/Prof*", " " )
    
  } #end loop over values of r
  
  
  # On screen results
  
  print(paste("CRRA with epsilon", Epsilon.Case, ",(N.eps =", N.eps,"), Price ratio = ", px))
  print(text.param.prod)
  print(res.CRRA)
  
  # Binding results
  Simu.wealth <-  rbind( Simu.wealth, res.CRRA)
  
}  #end loop over initial wealth    


print(Simu.wealth)


## formatted output 
stargazer(Simu.wealth,
          summary = FALSE,
          title = paste("CRRA (N.eps =", N.eps,"), Price ratio = ", px),
          notes = paste0(text.param.prod,"  (Last run:", format(Sys.time(), "%Y-%m-%d"), ")"),
          digits = 1,
          type = 'text', out = 'results/TableWealth.txt')



