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
  x[x>maxVal] <- maxVal
  x[x<minVal] <- minVal
  return(x)
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
  # Inherit egoBias if there's a previous generation and we're not mutating
  if(!is.null(parents) & runif(1) > modelParams$mutationChance)
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
  n <- nrow(agents)
  ties <- matrix(0, nrow = n, ncol = n)
  tieCount <- rep(0, n)
  # picking proceeds in rounds to avoid total isolates
  for(i in 1:modelParams$agentDegree) {
    for(a in 1:n) {
      # skip if we've been picked this round
      if(tieCount[a]>=i)
        next()
      # find potential targets
      targets <- which(tieCount < i)
      targets <- targets[targets != a & !(targets %in% ties[a,])]
      # pick a target
      if(length(targets) <= 0)
        break()
      if(length(targets) == 1)
        b <- targets
      else
        b <- sample(targets, 1)
      # form the ties
      ties[a,b] <- modelParams$initialConnectionStrength(agents[a,], agents[b,])
      ties[b,a] <- modelParams$initialConnectionStrength(agents[b,], agents[a,])
      # update the counters
      tieCount[c(a,b)] <- tieCount[c(a,b)] + 1
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

  g <- g + 1 # increment generation

  # Create the agents by inheritance from parents where applicable
  for(i in 1:n) {
    if(!is.null(parents[[i]]))
      a <- modelParams$makeAgent(modelParams,
                                 previousGeneration[previousGeneration$id == parents[[i]], ])
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
  ties <- modelParams$connectAgents(modelParams, makeAgents.agents)
  makeAgents.agents$degree <- sapply(1:nrow(makeAgents.agents), getDegree, ties)
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
getWorldState <- function(modelParams, world) {
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
#' @return an adjusted version of the \code{agents} data frame with decisions
#'   included.
#' @export
getDecision <- function(modelParams, agents, world, ties, initial = F) {
  mask <- which(agents$generation == world$generation)
  if(initial) {
    # initial decision - look and see
    n <- length(mask)
    agents$initialDecision[mask] <- rnorm(n, rep(world$state, n), clamp(agents$sensitivity[mask],Inf))
  } else {
    # Final decision - take advice
    # Use vector math to do the advice taking
    out <- NULL
    out <- (agents$initialDecision[mask] * agents$egoBias[mask]) + ((1-agents$egoBias[mask]) * agents$advice[mask])
    out[is.na(out)] <- agents$initialDecision[mask][is.na(out)]
    agents$finalDecision[mask] <- out
  }
  return(agents)
}

#' Return the advice selected for each agent
#'
#' @inheritParams getDecision
#'
#' @description Advisors are selected randomly from among the connections
#'   available to an agent. Custom replacements for this function may return
#'   more than one value for advice, provided \code{\link{getDecision}} is replaced with
#'   a function capable of handling the input
#'
#' @return a version of the \code{agents} data frame including advice values.
#' @export
getAdvice <- function(modelParams, agents, world, ties) {
  mask <- which(agents$generation == world$generation)
  agents$advisor[mask] <- apply(ties, 1, function(x) sample(which(x != 0),1))
  # Fetch advice as a vector
  agents$advice[mask] <- agents$initialDecision[agents$advisor[mask]]
  return(agents)
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
updateConnections <- function(modelParams, agents, world, ties) {
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
#' @return an updated version of the \code{agents} data frame with updated fitness values
#' @export
getFitness <- function(modelParams, agents, world, ties) {
  mask <- which(agents$generation==world$generation)
  # fitness increases by error vs world state
  agents$fitness[mask] <- agents$fitness[mask] + (-1*abs(agents$finalDecision[mask] - world$state))
  return(agents)
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
selectParents <- function(modelParams, agents, world, ties) {
  tmp <- agents[which(agents$generation == world$generation),]
  tmp <- tmp[order(tmp$fitness, decreasing = T),]
  # drop the worst half of the population
  tmp <- tmp[1:(floor(nrow(tmp)/2)), ]
  # the others get weighted by relative fitness which are transformed to +ve values
  tmp$fitness <- tmp$fitness - min(tmp$fitness) + 1
  # scale appropriately
  while(any(tmp$fitness < 10))
    tmp$fitness <- tmp$fitness * 10
  # and round off
  tmp$fitness <- round(tmp$fitness)
  tickets <- vector(length = sum(tmp$fitness)) # each success buys a ticket in the draw
  i <- 0
  for(a in 1:nrow(tmp)) {
    tickets[(i+1):(i+1+tmp$fitness[a])] <- a
    i <- i + 1 + tmp$fitness[a]
  }
  winners <- sample(tickets, modelParams$agentCount, replace = T)
  # The winners clone their egocentric discounting
  winners <- tmp[winners,'id']
  return(winners)
}

#' Run an evolutionary model of interacting agents
#'
#' @param agentCount number of agents in each generation
#' @param agentDegree (average) number of connections each agent has
#' @param decisionCount number of decisions made each generation
#' @param generationCount number of generations (duration of model)
#' @param mutationChance probability of a mutation occuring
#' @param learnRate scaling factor for updating connection weights
#' @param other list used for passing other arguments to custom functions.
#'   Accessed by \code{modelParams$other}
#' @param makeAgentsFun function for producing agents. Should follow format of
#'   \code{\link{makeAgents}}
#' @param makeAgentFun function for producing an individual agent. Typically
#'   called from \code{makeAgents}. Should follow format of
#'   \code{\link{makeAgent}}
#' @param connectAgentsFun function for connecting agents. Should follow format
#'   of \code{\link{connectAgents}}
#' @param initialConnectionStrengthFun function for determining the initial
#'   strength of connections. Should follow format of
#'   \code{\link{initialConnectionStrength}}
#' @param getWorldStateFun function for determining the initial world state.
#'   Should follow format of \code{\link{getWorldState}}
#' @param getDecisionFun decision function for the agents. Should follow format
#'   of \code{\link{getDecision}}
#' @param getAdvisorFun function determining which agent acts as advisor. Should
#'   follow format of \code{\link{getAdvisor}}
#' @param updateconnectionsFun update function for agent connections. Should
#'   follow format of \code{\link{updateConnections}}
#' @param getFitnessFun fitness function for agent evaluation. Should follow
#'   format of \code{\link{getFitness}}
#' @param selectParentsFun Select which agents get to be parents for the next
#'   generation. Should follow format of \code{\link{selectParents}}
#'
#' @description This function is the specifier for the interactive agent-based
#'   models enabled by evoSim. It is called with some simple parameters
#'   specifying the number of agents, etc., and further parameters may be used
#'   to fine-tune the performance. Almost all behaviour of the model can be
#'   adjusted by specifying an appropriate replacement for the default
#'   functions.
#'
#' @export
evoSim <- function(agentCount,
                   agentDegree,
                   decisionCount,
                   generationCount,
                   mutationChance,
                   learnRate = 0.00,
                   other = NULL,
                   makeAgentsFun = NULL,
                   makeAgentFun = NULL,
                   connectAgentsFun = NULL,
                   initialConnectionStrengthFun = NULL,
                   getWorldStateFun = NULL,
                   getDecisionFun = NULL,
                   getAdviceFun = NULL,
                   updateConnectionsFun = NULL,
                   getFitnessFun = NULL,
                   selectParentsFun = NULL) {

  # Generate the model parameters
  modelParams <- list(agentCount = agentCount,
                      agentDegree = agentDegree,
                      decisionCount = decisionCount,
                      generationCount = generationCount,
                      mutationChance = mutationChance,
                      other = other,
                      learnRate = learnRate,
                      makeAgent = ifelse(!is.null(makeAgentFun),makeAgentFun,makeAgent),
                      makeAgents = ifelse(!is.null(makeAgentsFun),makeAgentsFun,makeAgents),
                      connectAgents = ifelse(!is.null(connectAgentsFun),connectAgentsFun,connectAgents),
                      initialConnectionStrength = ifelse(!is.null(initialConnectionStrengthFun),
                                                         initialConnectionStrengthFun,
                                                         initalConnectionStrength),
                      getWorldState = ifelse(!is.null(getWorldStateFun),getWorldStateFun,getWorldState),
                      getDecision = ifelse(!is.null(getDecisionFun),getDecisionFun,getDecision),
                      getAdvice = ifelse(!is.null(getAdviceFun),getAdviceFun,getAdvice),
                      updateConnections = ifelse(!is.null(updateConnectionsFun),
                                                 updateConnectionsFun,
                                                 updateConnections),
                      getFitness = ifelse(!is.null(getFitnessFun),getFitnessFun,getFitness),
                      selectParents = ifelse(!is.null(selectParentsFun),selectParentsFun,selectParents))

  tStart <- Sys.time()
  tmp <- makeAgents(modelParams)
  agents <- tmp$agents
  ties <- tmp$ties
  decisions <- NULL

  for(g in 1:modelParams$generationCount) {
    # Make decisions
    for(d in 1:modelParams$decisionCount) {
      # Correct answer (same for all agents)
      world <- list(generation = g, decision = d)
      world$state <- modelParams$getWorldState(modelParams, world)
      # Initial decisions
      agents <- modelParams$getDecision(modelParams, agents, world, ties, initial = T)
      # Advice
      agents <- modelParams$getAdvice(modelParams, agents, world, ties)
      # Final decision
      agents <- modelParams$getDecision(modelParams, agents, world, ties)
      # Evaluate decision
      agents <- modelParams$getFitness(modelParams, agents, world, ties)
      # Update connections
      ties <- modelParams$updateConnections(modelParams, agents, world, ties)
      # Record decision
      mask <- which(agents$generation==g)
      agents$decision[mask] <- d
      decisions <- rbind(decisions, agents[mask, c('id', 'genId', 'generation',
                                                   'decision', 'fitness', 'egoBias', 
                                                   'initialDecision', 'advisor', 'advice', 'finalDecision')])
    }
    if(g==modelParams$generationCount)
      next()
    # Evolve
    parents <- modelParams$selectParents(modelParams, agents, world, ties)
    tmp <- makeAgents(modelParams, agents[agents$generation == g, ], parents)
    for(n in names(agents)[which(!(names(agents) %in% names(tmp$agents)))])
      tmp$agents[,n] <- NA
    agents <- rbind(agents, tmp$agents)
    ties <- tmp$ties
  }

  tEnd <- Sys.time()
  tElapsed <- as.numeric(format(tEnd,"%s")) - as.numeric(format(tStart,"%s"))
  # Return an output containing inputs and the agents data frame
  modelData <- list(model = modelParams,
                    # to avoid duplicating info we reduce agents dataframe to remove columns about decisions
                    agents = agents[,-which(names(agents) %in% 
                                              c('initialDecision', 'advisor', 'advice', 'finalDecsision'))], 
                    decisions = decisions,
                    duration = tElapsed)
  return(modelData)
}
