# egoSim.R: evolutionary models of interacting agents ####
#' @author Matt Jaquiery, \email{matt.jaquiery@@psy.ox.ac.uk}
#' 
#' @description egoSim allows for evolutionary models where the agents in any given
#' generation can interact with other agents via a connectivity matrix. This
#' allows, for example, agents to give one another advice on the solution to a
#' common problem.
#'
#' @keywords evolution, genetic algorithm, social network


# Utility functions ####

#' Return the number of non-zero connections in row i of aMatrix
#'
#' @param i row to inspect
#' @param aMatrix matrix containing at least \code{i} rows
#' @param threshold value whose magnitude connections in \code{i} must exceed to be
#'   counted
#'
#' @return number of super-threshold values in row \code{i}
#'
#' @export
getDegree <- function(i, aMatrix, threshold = 0) {
  length(which(abs(aMatrix[i,])>abs(threshold)))
}

#' Clamp \code{x} to be between \code{minVal} and \code{maxVal}
#'
#' @param x value or vector of values to clamp
#' @param maxVal maximum value elements can take
#' @param minVal = 0 minimum value elements can take
#'
#' @return \code{x} if \code{x} is between \code{minVal} and \code{maxVal},
#'   otherwise \code{minVal} if \code{x} is lower or \code{maxVal} if \code{x}
#'   is higher
#'   
#' @export
clamp <- function(x, maxVal, minVal = 0) {
  if(length(x)>1) {
    out <- vector(length=length(x))
    for(i in 1:length(out))
      out[i] <- clamp(x[i],maxVal,minVal)
  }
  return(max(minVal, min(maxVal, x)))
}

# Model functions ####

#' Return an agent created from parents
#'
#' @param modelParams parameters for the model as a named list
#' @param parents data.frame of parent agents who will have some say in
#'   determining the properties of the agent
#'   
#' @return \code{data.frame} row(s) representing the spawned agent
#' 
#' @export
makeAgent <- function(modelParams, parents = NULL) {
  # Inherit egoBias if there's a previous generation
  if(!is.null(parents))
    agent <- data.frame(egoBias = parents$egoBias[1],
                        sensitivity = runif(1))
  else
    agent <- data.frame(egoBias = runif(1),
                        sensitivity = runif(1))
  return(agent)
}

#' Return the initial strength of a connection between \code{sender} and \code{receiver}
#' 
#' @param receiver data.frame row of the agent receiving information over the connection
#' @param sender data.frame row of the agent sending information over the connection
#' 
#' @return the strength of the connection
#' 
#' @export
initalConnectionStrength <- function(receiver, sender) {
  return(1)
}

#' Return a connectivity matrix containing the connection strengths of the links
#' between each sender (cols) and receiver (rows) pair of \code{agents}
#'
#' @param modelParams parameters for the model as a named list
#' @param agents data.frame of agents to be connected
#'
#' @description This default function establishes reciprocal connections with a
#'   strength determined by the \code{\link{initalConnectionStrength}} function in
#'   \code{modelParams}
#'
#' @return n*n connectivity matrix where n is the number of rows in
#'   \code{agents}
#'
#' @export
connectAgents <- function(modelParams, agents) {
  n <- dim(agents)[1]
  ties <- matrix(0, nrow = n, ncol = n)
  for(a in 1:n) {
    while(getDegree(a, ties) < modelParams$degreeCount) {
      t <- sample(1:n, 1)
      if(t==a | getDegree(t, ties) >= (modelParams$degreeCount * 1.5))
        next()
      ties[a,t] <- modelParams$initialConnectionStrength(agents[a,], agents[t,])
      ties[t,a] <- modelParams$initialConnectionStrength(agents[t,], agents[a,])
    }
  }
  return(ties)
}

#' Default function to define agents in the model
#'
#' @param modelParams parameters for the model as a named list
#' @param previousGeneration previous generation list containing data frame
#'   $agents and connectivity matrix $ties. NULL for instantiating the first
#'   generation
#' @param parents list of vectors containing indices of previousGeneration
#'   containing the parents to be used to produce each new agent.
#'
#' @description This function is called each generation and returns a list with
#'   two components, a data frame of agents and a matrix of their connections.
#'   Replacements for this function should follow this format.
#'
#' @return list with two components: \describe{
#'   \item $agents \describe{
#'     \item \code{id} global agent id
#'     \item \code{genId} agent id within this generation
#'     \item \code{generation} agent's generation
#'     \item \code{fitness} agent's fitness score
#'     \item ... other fields defining the agent's specific properties
#'   }
#'   \item $ties \code{n}*\code{n} matrix of connections between senders
#'    (column) and receivers (row) for each agent-agent pairing
#' }
#'
#' @export
makeAgents <- function(modelParams, previousGeneration = NULL, parents = NULL) {
  # Population size and generation extracted from current population or by
  # modelParams$agentCount
  if(is.null(previousGeneration)) {
    n <- modelParams$agentCount
    g <- 0
  }
  else {
    n <- dim(previousGeneration)[1]
    g <- previousGeneration$generation[1]
  }
  
  # Create the agents by inheritance from parents where applicable
  for(i in 1:n) {
    if(!is.null(parents[[i]]))
      a <- modelParams$makeAgent(modelParams, previousGeneration[parents[[i]]])
    else
      a <- modelParams$makeAgent(modelParams)
    # add in the metadata for the agent
    a <- cbind(data.frame(id = (n*g+i), genId = i, generation = g, fitness = 0), a)
    if(!exists('makeAgents.agents'))
      makeAgents.agents <- a
    else
      makeAgents.agents <- rbind(makeAgents.agents, a)
  }
  
  # Connect agents together
  ties <- connectAgents(makeAgents.agents)
  makeAgents.agents$degree <- sapply(1:dim(makeAgents.agents)[1], getDegree, ties)
  return(list(agents = makeAgents.agents, ties = ties))
}


fitFun <- function(agent, world) {
  return(agent)
}

selectParents <- function(modelParams, agents, ties) {
  return(agents$genId)
}

egoSim <- function(agentCount, 
                   agentDegree, 
                   decisionCount, 
                   generationCount, 
                   mutationChance, 
                   learnRate = 0.00,
                   makeAgentsFun = NULL,
                   makeAgentFun = NULL,
                   connectAgentsFun = NULL,
                   initialConnectionStrengthFun = NULL) {
  
  # Generate the model parameters
  modelParams <- list(agentCount = agentCount,
                      agentDegree = agentDegree,
                      decisionCount = decisionCount,
                      generationCount = generationCount,
                      mutationChance = mutationChance,
                      learnRate = learnRate,
                      makeAgent = ifelse(!is.null(makeAgentFun),makeAgentFun,makeAgent),
                      makeAgents = ifelse(!is.null(makeAgentsFun),makeAgentsFun,makeAgents),
                      connectAgents = ifelse(!is.null(connectAgentsFun),connectAgentsFun,connectAgents),
                      initialConnectionStrength = ifelse(!is.null(initialConnectionStrengthFun),
                                                         initialConnectionStrengthFun,
                                                         initalConnectionStrength))
  
  tStart <- Sys.time()
  tmp <- makeAgents(n = agentCount, degreeCount = agentDegree)
  agents <- tmp$agents
  ties <- tmp$ties
  
  for(g in 1:generationCount) {
    #print(paste0('Generation ',g,'. Mean egoBias = ',round(mean(agents$egoBias[which(agents$generation==g-1)]),3)))
    # Make decisions
    for(d in 1:decisionCount) {
      # Correct answer (same for all agents)
      worldState <- runif(1)
      answer <- worldState > .5
      # Precache agents pre-advice decisions (i.e. their advice to other agents)
      tmp <- agents[which(agents$generation == g-1),]
      tmp$decision <- vector(length = dim(tmp)[1])
      for(a in 1:dim(tmp)[1]) {
        tmp$decision[a] <- worldState + rnorm(1, mean = worldState, sd = tmp$sensitivity[a])
        tmp$decision[a] <- clamp(tmp$decision[a],0,1)
      }
      tmp$advisor <- vector(length = dim(tmp)[1])
      tmp$adjustment <- tmp$advisor
      for(a in 1:length(tmp$genId)) {
        myAnswer <- tmp$decision[a]
        if(sum(ties[a,]>0)) {
          tmp$advisor[a] <- sample(which(ties[a,]!=0),1)
          # Advice is either wholly negative or wholly positive (i.e. not graded)
          #advice <- ifelse(tmp$decision[tmp$advisor[a]] > .5, .5, -.5)
          # Advice is weighted by opinion of the advisor
          advice <- advice * ties[a,tmp$advisor[a]]
          # And then combined with own initial decision via egocentric bias
          myFinalAnswer <- (tmp$decision[a] * tmp$egoBias[a]) + ((1-tmp$egoBias[a]) * advice)
          myFinalAnswer <- clamp(myFinalAnswer, 0, 1)
          # Connections are updated after advice taking on the basis of agreement between initial decision and advice
          confidence <- abs(myAnswer - 0.5)
          if((myAnswer > .5 & advice > 0) 
             | (myAnswer < .5 & advice < 0)) 
            # Agreement, boost connection by confidence
            tmp$adjustment[a] <- ties[a, tmp$advisor[a]] + (confidence * learnRate)
          else
            tmp$adjustment[a] <- ties[a, tmp$advisor[a]] - (confidence * learnRate)
          tmp$adjustment[a] <- clamp(tmp$adjustment[a], 0, 1)
        } else {
          # No advisors connected
          myFinalAnswer <- myAnswer
          tmp$adjustment[a] <- 0
          tmp$advisor[a] <- a
        }
        # Tally the decision result
        if((myFinalAnswer > .5) == answer)
          tmp$successes[a] <- tmp$successes[a] + 1
      }
      # update agents
      agents$successes[which(agents$generation==g-1)] <- tmp$successes
      for(a in 1:length(tmp$genId))
        ties[a,tmp$advisor[a]] <- tmp$adjustment[a]
    }
    if(g==generationCount)
      next()
    # Evolve
    tmp <- agents[which(agents$generation == g-1),]
    tickets <- vector(length = sum(tmp$successes)) # each success buys you a ticket in the draw
    tmp <- tmp[order(tmp$successes, decreasing = T),]
    i <- 0
    for(a in 1:dim(tmp)[1]) {
      tickets[(i+1):(i+1+tmp$successes[a])] <- a
      i <- i + 1 + tmp$successes[a]
    }
    winners <- sample(tickets, agentCount, replace = T)
    # The winners clone their egocentric discounting
    winners <- tmp[winners,]
    # Some winners mutate
    winners$egoBias[which(runif(dim(winners)[1]) < mutationChance)] <- NA
    tmp <- makeAgents(previousGeneration = winners, generation = g, degreeCount = agentDegree)
    agents <- rbind(agents, tmp$agents)
    ties <- tmp$ties
  }
  
  tEnd <- Sys.time()
  tElapsed <- as.numeric(format(tEnd,"%s")) - as.numeric(format(tStart,"%s"))
  # Return an output containing inputs and the agents data frame
  modelData <- list(agentCount = agentCount,
                    agentDegree = agentDegree,
                    decisionCount = decisionCount,
                    generationCount = generationCount,
                    mutationChance = mutationChance,
                    learnRate = learnRate,
                    initialConnectionStrength = initialConnectionStrength,
                    agents = agents,
                    duration = tElapsed)
  return(modelData)
}