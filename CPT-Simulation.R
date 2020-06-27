###  CPT   version or MainCRRA.R #####
# Computes the RP and Expected profit for various  parameters of KT CPT definition
# Uses U.CPT function in the global.R file
# 13/02/2019: Bugs corRected, results OK with Celine
# 13/02/2019 : Functional version of MainCPT.R
# 20/03/2019: Tests using Chavas test to compute with KT utility
# 10/04/2020: Code lifting and checks (OK with paper)
# 11/04/2020: Functions removed and copied toi GlobalFunctions.R 
# 12/04/2020: Loops and output tables (4 and 5) created.  

library(dplyr)
library(ggplot2)
library(purrr)
library(spatstat.utils) # for revcumsum
library(stargazer)
source("GlobalFunctions.R")

## Step 0:  Nature's distribution (epsilon generation)
N.eps <- 1000  
Simu.Case <- "C"  

# Generating Epsilons 
Epsilons <- GenEpsilon(Case = Simu.Case, Neps = N.eps)

# Recording simulation parameter info
Epsilon.Case <- ifelse(Simu.Case == "A", "Normally drawn", 
                            ifelse(Simu.Case == "B",  "Uniformly drawn",
                                   "equaly spaced" ))


##  Step 1 : Generation of equally distributed Xs
suppmax <- 400  #defines the support of inputs in [0, suppmax ]
Ns <- 400     #### <<<-----  change to 400 points 
x <- as.data.frame(seq(from = 0, to = suppmax, length.out = Ns+1 ))
x<- x[-1,]  #  to avoid start at x=0
df<- data.frame(x)

# Recording simulation parameter info
text.param.simu <- paste(" Nb ind =", Ns, ",  epsilons", N.eps, "(case", Epsilon.Case, ")")

## Step 2: Production function 

#### Benchmark ####
px <- 11 
# Initial wealth (Could be changed )
initial.wealth <- 500  

# Production  parameters

beta0 = 0
beta1 = 15
a1 = 0.30
beta2 = 30
a2 = -0.10

# Recording production function parameters info
text.param.prod <-  paste0("Production function y =",beta1," * x**", a1, " + ",beta2,"* x**",a2," * eps" )
text.param.prod2 <- paste("Price ratio =",px,", Initial wealth =", initial.wealth )

####  Individal profit  ----

df.profMat0 <- data.frame(mapply(function(x, y) 
  (initial.wealth + px*(beta0 + beta1 * x^a1 + beta2 * (x ^a2) *y) - x), 
  df,Epsilons$eps))

## Computing the probalistic profit 

df.ProbProfMat0 <- data.frame(mapply(`*` , df.profMat0, Epsilons$probeps))
# Expected profit 
df.ExpectedProf0 = rowSums(df.ProbProfMat0)

# Computing some reference values for base profit (to be used later)
prof.25 <- round(quantile(df.ExpectedProf0, probs = 0.25),0)
prof.50 <- round(quantile(df.ExpectedProf0, probs = 0.50),0)
prof.max <- round(max(df.ExpectedProf0),0)

#####   --- Choice of parameters for the simulation  #### 

# Scenarios definition
scenarios <-      data.frame(c(0.12,0.12,2.25, 0.61,0.69 )) #Scenario 0 (KT)
scenarios <- cbind(scenarios,c(0.12,0.12,2.25, 0.61,0.30 )) #Scenario 1
scenarios <- cbind(scenarios,c(0.88,0.12,2.25, 0.61,0.69 )) #Scenario 2
scenarios <- cbind(scenarios,c(0.88,0.12,2.25, 0.61,0.30 )) #Scenario 3
scenarios <- cbind(scenarios,c(0.88,0.12,3.50, 0.61,0.30 )) #Scenario 4

names(scenarios) <- c("KT", "Sc.1", "Sc.2", "Sc.3", "Sc.4")
row.names(scenarios) <- c("rr+", "rr-", "lambda", "gamma", "delta") 


################# TABLE 4  #######

# List of cases ( reference points, w0, in rows)  for each scenario
w0.list <- c(0, prof.50, prof.max)


# Initializing results
results <- data.frame(matrix(ncol = ncol(scenarios),nrow = 2*length(w0.list),
                             dimnames=list(NULL, names(scenarios))))

##looping over the scenarios (in column)
for (j in 1:ncol(scenarios) )  {   
    # Current scenario number 
    sc.num <- j
    
    # Inputing parameters for each scenario
    
    rrplus.input <-  scenarios[c("rr+"), sc.num]   # Scenario 0 is in column 1
    rrmoins.input <- scenarios[c("rr-"), sc.num]  
    lambda.input <-  scenarios[c("lambda"), sc.num]
    gamma.input <-   scenarios[c("gamma"), sc.num]
    delta.input <-   scenarios[c("delta"), sc.num]
    
    
    # looping over the cases for the reference point w0
    for (i in seq_along(w0.list))  {
      
      
        
      ###  Reference point 
      w0.input <-   w0.list[i]  #prof.50  # prof.max  # prof.50    # 0  Reference profit w0 !!
      text.param.KT <-  paste0("Ref. Point w0 = ", w0.input)
      
      ######   calling the function  #####
      
      
      results.KT <- resultsKT(df.profit = df.profMat0, 
                                     w0.ref = w0.input, 
                                     rrplus = rrplus.input, 
                                     rrmoins = rrmoins.input,
                                     lambda = lambda.input, 
                                    # Weight function parameters
                                     delta = delta.input,  
                                     gamma = gamma.input
                          )
      
      
      # Computing difference with EU
      delta.x <- round(100*(results.KT$X.star - 264)/results.KT$X.star, 1)
      
      ###  -- On screen printing of  parameters --- ####
      
      text.param.WeightCPT <-  paste0( "UKT: rr+ = ",rrplus.input,
                                       ", rr- = ",rrmoins.input,
                                       ", lambda = ", lambda.input,
                                       ", gamma = ", gamma.input,
                                       ", delta = ", delta.input)
      
      cat(paste0("----- \n", 
                text.param.prod2, "\n",
                text.param.KT, "\n",
                text.param.WeightCPT, "\n",
                "x* = ", results.KT$X.star,
                " (", delta.x, "%) \n", 
                "RP = ",results.KT$RP, "\n" ))
      
      k <- 2*i -1
      results[k, sc.num ] <- paste0(results.KT$X.star," (",delta.x, "%)") 
      results[k+1, sc.num ] <- paste0("[RP = ", round(results.KT$RP, 0), "]") 
      
     # results[i, sc.num ] <- paste0(results.KT$X.star," (RP = ", results.KT$RP, ")") 
    }  # end loop cases (w0 values)

} #end loop scenarios

#### Results (in a data.frame) ###

Simu.Output <- rbind(scenarios, results)
row.names(Simu.Output) <- c(row.names(scenarios) , 
                            "x* (Ref = 0)", " -",
                            "x*(Ref = Median)", "- ",
                            "x* (Ref = Max)", " - ")

## formatted output 
values.profits <- paste(unlist(w0.list), collapse=', ')
stargazer(Simu.Output,
          summary = FALSE,
          title = paste ("CPT", text.param.prod2, ", Refs (0, med., max) =", values.profits),
          notes =  paste0(text.param.prod,"  (Last run:", format(Sys.time(), "%Y-%m-%d"), ")"),
          digits = 1,
          # type = 'latex', out = 'results/Table1.tex')
          type = 'text', out = paste0("results/Table4-", format(Sys.time(), "%m-%d")," .txt")
          )



##################### TABLE 5 ####################
# ( Assuming it is runned after TABLE 4 --> scenarios and production parameters are known)



px.list <- c(11,13,7)

# Carefull : References are different when px changes)
x.star.px <- c(264, 335, 138)   # indicate that somewhere

# Initializing results
results.px <- data.frame(matrix(ncol = ncol(scenarios),nrow = 2*length(px.list),
                             dimnames=list(NULL, names(scenarios))))
prof.med.list <- ""


##looping over the scenarios (in column)
for (j in 1:ncol(scenarios) )  {   
  # Current scenario number 
  sc.num <- j
  
  # Inputing parameters for each scenario
  
  rrplus.input <-  scenarios[c("rr+"), sc.num]   # Scenario 0 is in column 1
  rrmoins.input <- scenarios[c("rr-"), sc.num]  
  lambda.input <-  scenarios[c("lambda"), sc.num]
  gamma.input <-   scenarios[c("gamma"), sc.num]
  delta.input <-   scenarios[c("delta"), sc.num]
  
  
# List of cases (different price ratios)  for each scenario

  # looping over the cases for the price ratio px
  for (i in seq_along(px.list)) { 

    # price ratio 
    px <- px.list[i]
    text.param.KT <-  paste0("Price ratio = ", px, " -RefX0 = ", x.star.px[i] )
    ####  Individal profit  ----
    
    df.profMat0 <- data.frame(mapply(function(x, y) 
      (initial.wealth + px*(beta0 + beta1 * x^a1 + beta2 * (x ^a2) *y) - x), 
      df,Epsilons$eps))
    
    ## Computing the probalistic profit 
    
    df.ProbProfMat0 <- data.frame(mapply(`*` , df.profMat0, Epsilons$probeps))
    # Expected profit 
    df.ExpectedProf0 = rowSums(df.ProbProfMat0)
    
    # Computing the reference point (set at the median profit) 
    prof.med <- round(quantile(df.ExpectedProf0, probs = 0.50),0)
    
    #recording the values for output
    prof.med.list <- ifelse(j == ncol(scenarios),paste0(prof.med.list, " ", prof.med), " ")
    
    ######   calling the KT function  #####
    
    results.KT <- resultsKT(df.profit = df.profMat0, 
                            w0.ref = prof.med, 
                            rrplus = rrplus.input, 
                            rrmoins = rrmoins.input,
                            lambda = lambda.input, 
                            # Weight function parameters
                            delta = delta.input,  
                            gamma = gamma.input
    )
    
    
    # Computing difference with EU (beware : References are different when px changes)

    delta.x <- round(100*(results.KT$X.star - x.star.px[i])/results.KT$X.star, 1)
    
    ###  -- On screen printing of  parameters --- ####
    text.param.prod3 <- paste("Reference Point (median prof) =",prof.med,", Initial wealth =", initial.wealth )
    text.param.WeightCPT <-  paste0( "UKT: rr+ = ",rrplus.input,
                                     ", rr- = ",rrmoins.input,
                                     ", lambda = ", lambda.input,
                                     ", gamma = ", gamma.input,
                                     ", delta = ", delta.input)
    
    cat(paste0("----- \n", 
               text.param.prod3, "\n",
               text.param.KT, "\n",
               text.param.WeightCPT, "\n",
               "x* = ", results.KT$X.star,
               " (", delta.x, "%) \n", 
               "RP = ",results.KT$RP, "\n" ))
    
    # results.px[i, sc.num ] <- paste(results.KT$X.star," (", delta.x, "%)")  
    # New version with RP
    # Index for two lines of results
    k <- 2*i -1
    results.px[k, sc.num ] <- paste0(results.KT$X.star," (",delta.x, "%)") 
    results.px[k+1, sc.num ] <- paste0("[RP = ", results.KT$RP, "]") 
    
  }  # end loop cases (px values)
} #end loop scenarios

#### Results (in a data.frame) ###

Simu.Output.px <- rbind(scenarios, results.px)
row.names(Simu.Output.px) <- c(row.names(scenarios) ,
                               paste("x* (ratio 1-",px.list[1],")" ) , paste(" X0* ref. =", x.star.px[1]),
                               paste("x* (ratio 1-",px.list[2],")" ) , paste(" X0* ref. =", x.star.px[2]),
                               paste("x* (ratio 1-",px.list[3],")" ) , paste(" X0* ref.=", x.star.px[3]))




## formatted output 
px.list.values <- paste(unlist(px.list), collapse=', ')

stargazer(Simu.Output.px,
          summary = FALSE,
          title = paste0("CPT: Initial wealth =", initial.wealth, 
                         ", Ref.Point = med. profit (px = ", px.list.values,") = resp.",
                         prof.med.list ),
          notes =  paste0(text.param.prod," (Last run:", format(Sys.time(), "%Y-%m-%d"), ")"),
          digits = 1,
          # type = 'latex', out = 'results/Table1.tex')
          type = 'text', out = paste0("results/Table5-", format(Sys.time(), "%m-%d")," .txt")
)




