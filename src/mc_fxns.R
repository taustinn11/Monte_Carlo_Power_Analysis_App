### Monte carlo functions

library(tidyverse)
library(shiny)

#theme_set(new = theme_bw)

n_sims = 1000   #This is the number of simulations to run the MC for. Higher n_sims will require more time 
n_reps = 14    #This is the number of independent replicates being considered for the experiment
fx = 0.10       #This is the minimum effect size that the user may want to detect
sd = 0.10       #This is the standard deviation expected expressed in terms of a percentage of the total effect. This needs to be determined by the user.

#Data Randomizer

data_randomizer <- function(n_reps, fx, sd) {

  control <- rnorm(n_reps, 1, sd)
  treat <- rnorm(n_reps, (1+1*fx), sd)
  
  outcomes <- cbind(control, treat)
  return(outcomes)

}

p.val <- replicate(n_sims,
          {
            df <- data_randomizer(n_reps, fx, sd)

            p.val <- t.test(df[,2], df[,1], alternative = "greater", )$p.value
            
            return(p.val)
          }
          )
p.val

pwr.pct <- sum(p.val<0.05)/n_sims*100
pwr.pct

#Inputs for this: n_sims, n_reps, fx, sd, alternative, 

### Make a plot version instead

n_sims = 100   #This is the number of simulations to run the MC for. Higher n_sims will require more time 
fx = 0.10       #This is the minimum effect size that the user may want to detect
sd = 0.10       #This is the standard deviation expected expressed in terms of a percentage of the total effect. This needs to be determined by the user.
n_reps = c(2:50) #sample sizes


#For loop method

l <- list()     #Create and empty list to store values in

for(i in c(n_reps)) {
  p.val <- replicate(n_sims,
                     {
                       df <- data_randomizer(i, fx, sd)
                       
                       p.val <- t.test(df[,2], df[,1], alternative = "greater", )$p.value
                       
                       return(p.val)
                     }
  )
  
  l[[i-1]] <- sum(p.val<0.05)/n_sims*100
}

l %>% unlist(T, T) %>% 
  as.data.frame() %>% 
  cbind(., n_reps) %>% 
  rename(pwr = 1) %>% 
  ggplot(aes(n_reps, pwr))+
  geom_line()+
  ggtitle("Cumulative Power Distribution")+
  xlab("Number of Independent Replicates")+
  ylab("Power")+
  theme_bw()

## sapply method
  
data.frame(n_reps, 
           pwr = sapply(n_reps, function(x) {

            p.val <- replicate(n_sims,
                      {
                        df <- data_randomizer(x, fx, sd)
                        
                        p.val <- t.test(df[,2], df[,1], alternative = "greater", )$p.value
                        
                        return(p.val)
                      }
                      )
            sum(p.val<0.05)/n_sims*100
            } 
          )
) %>% 
  ggplot(aes(n_reps, pwr))+
  geom_line(size = 2, color = "Red")+
  geom_hline(yintercept = 80, linetype = 2)+
  ggtitle("Cumulative Power Distribution")+
  xlab("Number of Independent Replicates")+
  ylab("Power")+
  theme_bw()

#Timing the different methods

library(microbenchmark)

#The below code takes a long time to run, and I'm not sure if it is accurate or not. 
microbenchmark::microbenchmark(floop = for(i in c(n_reps)) {
  p.val <- replicate(n_sims,
                     {
                       df <- data_randomizer(i, fx, sd)
                       
                       p.val <- t.test(df[,2], df[,1], alternative = "greater", )$p.value
                       
                       return(p.val)
                     }
  )
  
  l[[i-1]] <- sum(p.val<0.05)/n_sims*100
},
sapp = data.frame(n_reps, 
                  pwr = sapply(n_reps, function(x) {
                    
                    p.val <- replicate(n_sims,
                                       {
                                         df <- data_randomizer(x, fx, sd)
                                         
                                         p.val <- t.test(df[,2], df[,1], alternative = "greater", )$p.value
                                         
                                         return(p.val)
                                       }
                    )
                    sum(p.val<0.05)/n_sims*100
                  } 
                  )
)
)
