# Strengths and definitions of different variables 
library(ggplot2)




Number.Of.Synapses <- 10000


# Sets the amount of trials between each memory trace 

Numb.Trials <- 100000

Numb.Trials.2 <- 100000

Numb.Trials.3 <- 100000

#Creating data frame inital synaptic strengths 

Our.Synapses<- data.frame( "Synapse" = sample(x = 0:1, size = Number.Of.Synapses, replace = TRUE), 
                           "State" = sample(x = 1:10, size = Number.Of.Synapses, replace = TRUE))


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
  Ultimate.Data.Frame <- data.frame("Signal" = numeric(),
                                    "Synapse" = numeric(), 
                                    "State" = numeric())
  
  for (i in 1:Numb.Trials){
    
    Pick.Synapse <- (sample(1:Number.Of.Synapses, 1, replace = TRUE))
    
    Selected.Synapse <-  (Updated.Memory.State[Pick.Synapse,])
    
    New.State <-  (Random.Plastic.Events(Selected.Synapse))
    
    Updated.Memory.State$Synapse[Pick.Synapse] <- New.State$Synapse
    
    Updated.Memory.State$State[Pick.Synapse] <- New.State$State
    
    Updated.Memory.Trace.Coloumns.Values <- Updated.Memory.State$Synapse[c(Memory.Trace.Synapse.Coloumns)]
    
    Updated.Signal[i] <- (Signal.Inital - sum(Updated.Memory.Trace.Coloumns.Values != Memory.Trace.Synapse.Coloumns.Values))
    
    
    #Ultimate.Data.Frame <- rbind(Ultimate.Data.Frame, data.frame("Signal" = Updated.Signal[i],
                                 #"Synapse" = Updated.Memory.State$Synapse[i], 
                                 #"State" = Updated.Memory.State$State[i]))
    
    Count.Signal[i] <- Updated.Signal[i]
    
    
    # Need code that tracks signal and stores it so that a plot of trials$signal can be made 
    
    # Need code that makes a new memory trace and "Reinforces the memory" 
  }
  
  return(Count.Signal)
}



# Creates the second random plastic events after second memory encoding 

Random.Mod.Function.2 <- function(Memory.Trace){
  
  Memory.Trace.Synapse.Coloumns <- which(Memory.Trace$Synapse != Our.Synapses$Synapse)
  
  Second.Memory.Trace.Coloumns <- which(Memory.Trace.2$Synapse != Our.Synapses$Synapse)
  
  Memory.Trace.Synapse.Coloumns.Values <- Memory.Trace$Synapse[c(Memory.Trace.Synapse.Coloumns, Second.Memory.Trace.Coloumns)]
  
  Count.Signal <- 0 
  
  Updated.Signal <- 0
  
  Updated.Memory.State <- Memory.Trace
  Ultimate.Data.Frame <- data.frame("Signal" = numeric(),
                                    "Synapse" = numeric(), 
                                    "State" = numeric())
  
  for (i in 1:Numb.Trials.2){
    
    Pick.Synapse <- (sample(1:Number.Of.Synapses, 1, replace = TRUE))
    
    Selected.Synapse <-  (Updated.Memory.State[Pick.Synapse,])
    
    New.State <-  (Random.Plastic.Events(Selected.Synapse))
    
    Updated.Memory.State$Synapse[Pick.Synapse] <- New.State$Synapse
    
    Updated.Memory.State$State[Pick.Synapse] <- New.State$State
    
    Updated.Memory.Trace.Coloumns.Values <- Updated.Memory.State$Synapse[c(Memory.Trace.Synapse.Coloumns, Second.Memory.Trace.Coloumns)]
    
    Updated.Signal[i] <- (Added.Signal - sum(Updated.Memory.Trace.Coloumns.Values != Memory.Trace.Synapse.Coloumns.Values))
    
    Count.Signal[i] <-Updated.Signal[i]
    
    #Ultimate.Data.Frame <- rbind(Ultimate.Data.Frame, data.frame("Signal" = Updated.Signal[i],
                                                                # "Synapse" = Updated.Memory.State$Synapse[i], 
                                                                # "State" = Updated.Memory.State$State[i]))
    
    
    # Need code that tracks signal and stores it so that a plot of trials$signal can be made 
    
    # Need code that makes a new memory trace and "Reinforces the memory" 
  }
  
  return(Count.Signal)
}

# Random Mod Function 3 after third memory encoding signal 

 Random.Mod.Function.3 <- function(Memory.Trace){
  
  Memory.Trace.Synapse.Coloumns <- which(Memory.Trace$Synapse != Our.Synapses$Synapse)
  
  Second.Memory.Trace.Coloumns <- which(Memory.Trace.2$Synapse != Our.Synapses$Synapse)
  
  Third.Memory.Trace.Coloumns <- which(Memory.Trace.3$Synapse != Our.Synapses$Synapse)
  
  Memory.Trace.Synapse.Coloumns.Values <- Memory.Trace$Synapse[c(Memory.Trace.Synapse.Coloumns, Second.Memory.Trace.Coloumns, Third.Memory.Trace.Coloumns)]
  
  Count.Signal <- 0
  
  Updated.Signal <- 0
  
  Updated.Memory.State <- Memory.Trace
  Ultimate.Data.Frame <- data.frame("Signal" = numeric(),
                                    "Synapse" = numeric(), 
                                    "State" = numeric())
  
  for (i in 1:Numb.Trials.3){
    
    Pick.Synapse <- (sample(1:Number.Of.Synapses, 1, replace = TRUE))
    
    Selected.Synapse <-  (Updated.Memory.State[Pick.Synapse,])
    
    New.State <-  (Random.Plastic.Events(Selected.Synapse))
    
    Updated.Memory.State$Synapse[Pick.Synapse] <- New.State$Synapse
    
    Updated.Memory.State$State[Pick.Synapse] <- New.State$State
    
    Updated.Memory.Trace.Coloumns.Values <- Updated.Memory.State$Synapse[c(Memory.Trace.Synapse.Coloumns, Second.Memory.Trace.Coloumns, Third.Memory.Trace.Coloumns)]
    
    Updated.Signal[i] <- (Added.Signal.2 - sum(Updated.Memory.Trace.Coloumns.Values != Memory.Trace.Synapse.Coloumns.Values))
    
    #Ultimate.Data.Frame <- rbind(Ultimate.Data.Frame, data.frame("Signal" = Updated.Signal[i],
                                                                # "Synapse" = Updated.Memory.State$Synapse[i], 
                                                                # "State" = Updated.Memory.State$State[i]))
    Count.Signal[i] <- Updated.Signal[i]
    
    
  }
  
  return(Count.Signal)
}


#Creating inital trace 


Memory.Trace <- Inital.Trace(Our.Synapses)

Signal.Inital <-  sum(Memory.Trace$Synapse != Our.Synapses$Synapse)

Signal.Inital

# Creates Second Memory Trace 

Memory.Trace.2 <- Inital.Trace(Our.Synapses)

Added.Signal <- (sum(Memory.Trace.2$Synapse != Our.Synapses$Synapse) + Model.Data$Signal[Numb.Trials])

Added.Signal

# Creates Third Memory Trace 

Memory.Trace.3 <- Inital.Trace(Our.Synapses)

Added.Signal.2 <- (sum(Memory.Trace.3$Synapse != Our.Synapses$Synapse) + (Model.Data.2$Signal[Numb.Trials.2] + Model.Data$Signal[Numb.Trials]))

Added.Signal.2

# Randomly selects a synapse for modification n amount of times 

Random.Mod.Function(Memory.Trace)


# Creates data for plotting

X.Axis <- (1:Numb.Trials)

X.Axis.2 <- (1:Numb.Trials + Numb.Trials.2)

X.Axis.3 <- (1:Numb.Trials + Numb.Trials.2 + Numb.Trials.3)

Random.Mod.Function(Memory.Trace)

Model.Data <- data.frame(Time = X.Axis, Signal = Random.Mod.Function(Memory.Trace))

Model.Data.2 <- data.frame(Time = X.Axis.2, Signal = Random.Mod.Function.2(Memory.Trace))

Model.Data.3 <- data_frame(Time = X.Axis.3, Signal = Random.Mod.Function.3(Memory.Trace))

Combined.Model.data.1 <- rbind(Model.Data, Model.Data.2, Model.Data.3)

#Combined.Model.data.Final <- bind_cols(Combined.Model.data.1, Model.Data.3)



# Makes Plot 

library(ggplot2)

ggplot(Combined.Model.data.1, aes(x = Time, y = Signal)) + 
  
  geom_line()





#    -----Extra Code Not Nessecary for implementation-------



# plot(X.Axis, Random.Mod.Function(Memory.Trace))


# This Tells Us Which Columns are modifyed 

Memory.Trace.Synapse.Coloumns <- which(Memory.Trace$Synapse != Our.Synapses$Synapse)

Memory.Trace.Synapse.Coloumns


Updated.Memory.Trace.Coloumns.Values <-  Updated.Memory.State$Synapse[c(Memory.Trace.Synapse.Coloumns)]


Memory.Trace.Synapse.Coloumns.Values <- Memory.Trace$Synapse[c(Memory.Trace.Synapse.Coloumns)]


# Creating the second memory trace 

Second.Memory.Trace <- Inital.Trace(Updated.Memory.State)

Second.Updated.Memory.Trace.Coloumns <- which(Second.Memory.Trace$Synapse != Updated.Memory.State)


