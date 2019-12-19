library(ggplot2)



Number.Of.Synapses <- 10000

Numb.Trials <- 10000


#Creating data frame inital synaptic strengths 

Our.Synapses<- data.frame( "Synapse" = sample(x = 0:1, size = Number.Of.Synapses, replace = TRUE), 
                           "State" = sample(x = 1:1, size = Number.Of.Synapses, replace = TRUE))


# Synapse  <- sample(x = 0:1, size = 1000, replace = TRUE)
# State <- sample(x = 1:10, size = 1000, replace = TRUE)




x <- 1/2


# Defining function that creates the inital trace 

Inital.Trace <- function(Synapse.Input){
 
  
  New.Synapse.Input <- Synapse.Input
  #New.State <- Synapse.Input['Synapse']
  #New.Synapse <- Synapse['State']
 
   for (i in 1:length(Synapse.Input$State)){
    
    Potentiate.Or.Weaken <- sample(1:2,1)
    
    if (Potentiate.Or.Weaken == 2 && Synapse.Input$Synapse[i] == 1){
      Probability.of.MetaPlastic.Event  <- x^Synapse.Input$State[i]/(1-x)
      
      Updated.State <- Synapse.Input$State[i] + sample(0:1, 1, prob = c(1-Probability.of.MetaPlastic.Event, Probability.of.MetaPlastic.Event))
      New.Synapse.Input$State[i] <- Updated.State   
      
    }else if (Potentiate.Or.Weaken == 1 && Synapse.Input$Synapse[i] == 1){
      
      Probability.of.Plastic.Event <- x^(Synapse.Input$State[i]-1)
      
      Updated.Synapse <- Synapse.Input$Synapse[i] - sample(0:1,1, prob = c(1- Probability.of.Plastic.Event, Probability.of.Plastic.Event))
      
      if (Updated.Synapse == 0){
        
        New.Synapse.Input$State[i] <- 1
      }
      
      New.Synapse.Input$Synapse[i] <- Updated.Synapse 
   
    }else if(Potentiate.Or.Weaken == 1 && Synapse.Input$Synapse[i] == 0){
      
      Probability.of.MetaPlastic.Event <- x^Synapse.Input$State[i]/(1-x)
      
      Updated.State <- Synapse.Input$State[i] + sample(0:1, 1, prob = c(1-Probability.of.MetaPlastic.Event, Probability.of.MetaPlastic.Event))
      
      New.Synapse.Input$State[i] <- Updated.State
    }else if(Potentiate.Or.Weaken == 2 && Synapse.Input$Synapse[i] == 0){
      
      Probability.of.Plastic.Event <-x^(Synapse.Input$State[i]-1)
      
      Updated.Synapse <- Synapse.Input$Synapse[i] + sample(0:1,1, prob = c(1- Probability.of.Plastic.Event, Probability.of.Plastic.Event))
      
      if(Updated.Synapse == 1){
        
        New.Synapse.Input$State[i] <- 1
      }
      
      New.Synapse.Input$Synapse[i] <- Updated.Synapse 
      
    }
   }
  return(New.Synapse.Input)
}


# Defining the function that continues plastic modification 

Random.Plastic.Events <- function(Synapse.Input){
  
  
  New.Synapse.Input <- Synapse.Input
  #New.State <- Synapse.Input['Synapse']
  #New.Synapse <- Synapse['State']
  
  Potentiate.Or.Weaken <- sample(1:2,1)
  
  if (Potentiate.Or.Weaken == 2 && Synapse.Input$Synapse == 1){
    Probability.of.MetaPlastic.Event  <- x^Synapse.Input$State/(1-x)
    
    Updated.State <- Synapse.Input$State + sample(0:1, 1, prob = c(1-Probability.of.MetaPlastic.Event, Probability.of.MetaPlastic.Event))
    
    New.Synapse.Input$State <- Updated.State   
    
  }else if (Potentiate.Or.Weaken == 1 && Synapse.Input$Synapse == 1){
    
    Probability.of.Plastic.Event <- x^(Synapse.Input$State-1)
    
    Updated.Synapse <- Synapse.Input$Synapse - sample(0:1,1, prob = c(1- Probability.of.Plastic.Event, Probability.of.Plastic.Event))
    
    if (Updated.Synapse == 0){
      
      New.Synapse.Input$State <- 1
    }
    
    New.Synapse.Input$Synapse <- Updated.Synapse 
    
  }else if(Potentiate.Or.Weaken == 1 && Synapse.Input$Synapse == 0){
    
    Probability.of.MetaPlastic.Event <- x^Synapse.Input$State/(1-x)
    
    Updated.State <- Synapse.Input$State + sample(0:1, 1, prob = c(1-Probability.of.MetaPlastic.Event, Probability.of.MetaPlastic.Event))
    
    New.Synapse.Input$State <- Updated.State
  }else if(Potentiate.Or.Weaken == 2 && Synapse.Input$Synapse == 0){
    
    Probability.of.Plastic.Event <-x^(Synapse.Input$State-1)
    
    Updated.Synapse <- Synapse.Input$Synapse + sample(0:1,1, prob = c(1- Probability.of.Plastic.Event, Probability.of.Plastic.Event))
    
    if(Updated.Synapse == 1){
      
      New.Synapse.Input$State <- 1
    }
    
    New.Synapse.Input$Synapse <- Updated.Synapse 
    
  }
  
  return(New.Synapse.Input)
}

#Function That Does Random Updating 


Random.Mod.Function <- function(Memory.Trace){
  
  Memory.Trace.Synapse.Coloumns <- which(Memory.Trace$Synapse != Our.Synapses$Synapse)
  
  Memory.Trace.Synapse.Coloumns.Values <- Memory.Trace$Synapse[c(Memory.Trace.Synapse.Coloumns)]
  
  Count.Signal <- 0
  Updated.Signal <- 0
  
  Updated.Memory.State <- Memory.Trace
  
  for (i in 1:Numb.Trials){
    
    Pick.Synapse <- (sample(1:Number.Of.Synapses, 1))
    
    Selected.Synapse <-  (Updated.Memory.State[Pick.Synapse,])
    
    New.State <-  (Random.Plastic.Events(Selected.Synapse))
    
    Updated.Memory.State$Synapse[Pick.Synapse] <- New.State$Synapse
    
    Updated.Memory.State$State[Pick.Synapse] <- New.State$State
    
    Updated.Memory.Trace.Coloumns.Values <- Updated.Memory.State$Synapse[c(Memory.Trace.Synapse.Coloumns)]
    
    Updated.Signal[i] <- (Signal.Inital - sum(Updated.Memory.Trace.Coloumns.Values != Memory.Trace.Synapse.Coloumns.Values))
    
    Count.Signal[i] <- Updated.Signal[i]
    
    
    # Need code that tracks signal and stores it so that a plot of trials$signal can be made 
    
    # Need code that makes a new memory trace and "Reinforces the memory" 
  }
  
  return(Count.Signal)
}



#Creating inital trace 


Memory.Trace <- Inital.Trace(Our.Synapses)

Signal.Inital <-  (Number.Of.Synapses - sum(Memory.Trace$Synapse == Our.Synapses$Synapse))

Signal.Inital


# Randomly selects a synapse for modification n amount of times 

Random.Mod.Function(Memory.Trace)


# Creates data for plotting

X.Axis <- (1:Numb.Trials)


Model.Data <- data.frame(Time = X.Axis, Signal = Random.Mod.Function(Memory.Trace))

Model.Data.4 <- data.frame(Time = X.Axis, Signal.2 = Random.Mod.Function(Memory.Trace))

# Plot.Data <- rbind(Model.Data,Model.Data.4)

# Makes Plot 

library(ggplot2)

ggplot(Model.Data, aes(x = Time, y = Signal)) + 
  
  geom_line()


ggplot() + 
  geom_line(data=Model.Data, aes(x = Time, y = Signal), color='green') + 
  geom_line(data=Model.Data.4, aes(x = Time, y = Signal.2  ), color='red')



ggplot(Model.Data, Model.Data.4, aes(x = Time)) + 
  geom_line(aes(y = Signal), color = "darkred") + 
  geom_line(aes(y = Signal.2), color="steelblue", linetype="twodash") 


# plot(X.Axis, Random.Mod.Function(Memory.Trace))







 # ----This was test code not nessecary to run model---- 

Memory.Trace.Synapse.Coloumns <- which(Memory.Trace$Synapse != Our.Synapses$Synapse)

Memory.Trace.Synapse.Coloumns


Updated.Memory.Trace.Coloumns.Values <-  Updated.Memory.State$Synapse[c(Memory.Trace.Synapse.Coloumns)]


Memory.Trace.Synapse.Coloumns.Values <- Memory.Trace$Synapse[c(Memory.Trace.Synapse.Coloumns)]


# Creating the second memory trace 

Second.Memory.Trace <- Inital.Trace(Updated.Memory.State)

Second.Updated.Memory.Trace.Coloumns <- which(Second.Memory.Trace$Synapse != Updated.Memory.State)























