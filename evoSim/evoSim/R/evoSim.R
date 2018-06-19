# evoSim.R: evolutionary models of interacting agents ####
#' @author Matt Jaquiery, \email{matt.jaquiery@@psy.ox.ac.uk}
#'
#' @description egoSim allows for evolutionary models where the agents in any given
#' generation can interact with other agents via a connectivity matrix. This
#' allows, for example, agents to give one another advice on the solution to a
#' common problem.
#'
#' @keywords evolution, genetic algorithm, social network
#'
#' @section Functions
#'
#' @docType package
#' @name evoSim
#' @title evolutionary models of interacting agents
NULL

## Utility functions ####

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

## Model functions ####
### Agent Creation functions ####

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
#' @inheritParams makeAgent
#' @param agents data.frame of agents to be connected
#' @param oldTies the previous connectivity matrix used by parents
#'
#' @description This default function establishes reciprocal connections with a
#'   strength determined by the \code{\link{initalConnectionStrength}} function in
#'   \code{modelParams}
#'
#' @return n*n connectivity matrix where n is the number of rows in
#'   \code{agents}
#'
#' @export
connectAgents <- function(modelParams, agents, parents, oldTies) {
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
#' @section
#' returns list with two components: \describe{
#'   \item{$agents}{\describe{
#'     \item{\code{id}}{global agent id}
#'     \item{\code{genId}}{agent id within this generation}
#'     \item{\code{generation}}{agent's generation}
#'     \item{\code{fitness}}{agent's fitness score}
#'     \item{...}{other fields defining the agent's specific properties}
#'   }}
#'   \item{$ties}{\code{n}*\code{n} matrix of connections between senders
#'    (column) and receivers (row) for each agent-agent pairing
#'   }}
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

### Decision-making functions ####

#' Return a world state for the given \code{generation} and \code{decision}
#' numbers
#'
#' @inheritParams getDecision
#'
#' @description By default this function returns \code{runif(1)}, and agents'
#'   task is to reproduce the value
#'
#' @return a world state to be passed to other decision functions
#'   \code{\link{getDecision}}, \code{\link{getAdvisor}},
#'   \code{\link{updateConnections}} as \code{world$state}
#'
#' @export
getWorldState <- function(modelParams,world) {
  return(runif(1))
}

#' Return the decisions made by agents
#'
#' @param modelParams parameters for the model as a named list
#' @param agents data.frame of agents
#' @param world list of world properties (\code{state} obtained from
#'   \code{\link{getWorldState}}, and \code{generation}, and \code{decision}
#'   numbers.)
#' @param ties square matrix of connections between agents
#'
#' @description This function uses the \code{agents$advisor} column to determine
#'   whether to produce an initial or final decision. Initial decisions are
#'   obtained by having an agent make a noisy assessment of the
#'   \code{world$state}, with a sensitivity based on its
#'   \code{agents$sensitivity}.
#'
#'   Final decisions are made by taking a weighted average of the agent's
#'   initial decision and the initial decision of the selected advisor.
#'
#' @return a vector of decisions which can be bound to the \code{agents} data
#'   frame
#' @export
getDecision <- function(modelParams, agents, world, ties) {
  mask <- which(agents$generation == world$generation)
  out <- NULL
  if(any(!is.null(agents$advisor[mask]))) {
    # initial decision - look and see
    for(a in agents$id[mask]) {
      out <- c(out, world$state + rnorm(1, mean = world$state, sd = agents$sensitivity[which(agents$id==a)]))
    }
  } else {
    # final decision - take advice
    for(a in agents$id[mask]) {
      agent <- agents[which(agents$id==a),]
      # Check there is advice to be had
      if(is.null(agent$advisor))
        out <- c(out,agent$initialDecision)
      else {
        advice <- agents$initialDecision[which(agents$id == agent$advisor)]
        out <- c(out, (agent$initialDecision * agent$egoBias) + ((1-agent$egoBias) * advice))
      }
    }
  }
#  return(clamp(out,1,0))
  return(out)
}

#' Return the advisor selected for each agent
#'
#' @inheritParams getDecision
#'
#' @description Agents are selected randomly from among the connections
#'   available to an agent. Custom replacements for this function may return
#'   more than one advisor, provided \code{\link{getDecision}} is replaced with
#'   a function capable of handling the input
#'
#' @return a vector of agent ids identifying advisors which can be cound to the
#'   \code{agents} data frame.
#' @export
getAdvisor <- function(modelParams, agents, ties, world) {
  out <- NULL
  for(i in 1:dim(ties)[1]){
    advisorGenId <- sample(which(ties[i,]>0),1)
    out <- c(out, agents$id[which(agents$genId==advisorGenId & agents$generation==world$generation)])
  }
  return(out)
}

#' Update the strength of connections between agents
#'
#' @inheritParams getDecision
#'
#' @description Agents' connection strengths can update dynamically. Customising
#'   this function will allow this to happen. This function is called as the
#'   last step in the decision process, after final decisions have been made and
#'   evaluated by \code{\link{getFitness}}.
#'
#' @return a square matrix similar to \code{ties} containing updated connection strengths
#' @export
updateConnections <- function(modelParams, agents, ties, world) {
  return(ties)
}

### Selection functions ####

#' Calculate the cumulative fitness (higher is better) for each agent based on
#' the current decision
#'
#' @inheritParams getDecision
#'
#' @description Agent's fitness is \code{agents$fitness[i]-1} if incorrect,
#'   otherwise unchanged. Negative values are used so that higher fitness is
#'   better, but the actual value tracked can be thought of as an error score.
#'   This allows for arbitrarily high error values (low fitness scores) while
#'   still keeping the intuitive label 'fitness'.
#'
#' @return a vector of fitness values to replace \code{agents$fitness}
#' @export
getFitness <- function(modelParams, agents, ties, world) {
  mask <- which(agents$generation==world$generation)
  # binary answer
  correct <- rep((world$state < .5),length(mask))
  correct <- correct == (agents$finalDecision[mask] < .5)
  return(correct-1)
}

#' Select the parents for each slot in the next generation
#'
#' @inheritParams getDecision
#'
#' @description Process the fitness of agents in a generation and select the
#'   ones which will reporoduce in the next generation. This implementation uses
#'   clones - only one parent exists for each agent, but other implementations
#'   can return a vector containing vectors of any length, allowing any number
#'   of parents for each agent. The output of this function is passed as a
#'   parameter to \code{\link{makeAgents}}
#'
#' @return a vector of agent ids for the parent of each to-be-spawned agent
#'
#' @export
selectParents <- function(modelParams, agents, ties, world) {
  tmp <- agents[which(agents$generation == g-1),]
  tmp <- tmp[order(tmp$fitness, decreasing = T),]
  # drop the worst half of the population
  half <- quantile(tmp$fitness, .5)
  tmp <- tmp[which(tmp$fitness > half),]
  # the others get weighted by relative fitness which are transformed to +ve values
  tmp$fitness <- tmp$fitness - half
  tickets <- vector(length = sum(tmp$fitness)) # each success buys a ticket in the draw
  i <- 0
  for(a in 1:dim(tmp)[1]) {
    tickets[(i+1):(i+1+tmp$fitness[a])] <- a
    i <- i + 1 + tmp$fitness[a]
  }
  winners <- sample(tickets, agentCount, replace = T)
  # The winners clone their egocentric discounting
  winners <- tmp[winners,'id']
  return(winners)
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
                   initialConnectionStrengthFun = NULL,
                   getWorldStateFun = NULL,
                   getDecisionFun = NULL,
                   getAdvisorFun = NULL,
                   updateConnectionsFun = NULL) {

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
                                                         initalConnectionStrength),
                      getWorldState = ifelse(!is.null(getWorldStateFun),getWorldStateFun,getWorldState),
                      getDecision = ifelse(!is.null(getDecisionFun),getDecisionFun,getDecision),
                      getAdvisor = ifelse(!is.null(getAdvisorFun),getAdvisorFun,getAdvisor),
                      updateConnections = ifelse(!is.null(updateConnectionsFun),
                                                 updateConnectionsFun,
                                                 updateConnections),
                      getFitness = ifelse(!is.null(getFitnessFun),getFitnessFun,getFitness),
                      selectParents = ifelse(!is.null(selectParentsFun),selectParentsFun,selectParents))

  tStart <- Sys.time()
  tmp <- makeAgents(n = agentCount, degreeCount = agentDegree)
  agents <- tmp$agents
  ties <- tmp$ties

  for(g in 1:modelParams$generationCount) {
    # Make decisions
    for(d in 1:modelParams$decisionCount) {
      # Correct answer (same for all agents)
      world <- list(state = modelParams$getWorldState(modelParams,g,d),
                    generation = g,
                    decision = d)
      mask <- which(agents$generation == g-1)
      # Initial decisions
      agents$initialDecision[mask] <- modelParams$getDecision(modelParams, agents, world, ties)
      # Advice
      agents$advisor[mask] <- modelParams$getAdvisor(modelParams, agents, ties, world)
      # Final decision
      agents$finalDecision[mask] <- modelParams$getDecision(modelParams, agents, world, ties)
      # Evaluate decision
      agents$fitness[mask] <- modelParams$getFitness(agents, world)
      # Update connections
      ties <- modelParams$updateConnections(modelParams, agents, ties, world)
    }
    if(g==modelParams$generationCount)
      next()
    # Evolve
    parents <- modelParams$selectParents(modelParams, agents, ties, world)
    tmp <- makeAgents(modelParams, agents, parents)
    agents <- rbind(agents, tmp$agents)
    ties <- tmp$ties
  }

  tEnd <- Sys.time()
  tElapsed <- as.numeric(format(tEnd,"%s")) - as.numeric(format(tStart,"%s"))
  # Return an output containing inputs and the agents data frame
  modelData <- list(model = modelParams,
                    agents = agents,
                    duration = tElapsed)
  return(modelData)
}
