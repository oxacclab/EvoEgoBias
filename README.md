# EvoEgoBias

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/132620633.svg)](https://zenodo.org/badge/latestdoi/132620633)
<!-- badges: end -->

Evolutionary models exploring when egocentric discounting is sensible.

## Egocentric discounting

Egocentric discounting refers to the tendency of individuals receiving advice to assign less weight to that advice than would be expected if they were following a normative 'averaging' strategy. The averaging strategy predicts that an individual ought to weigh the advice of a single advisor equally to their own, and it can be demonstrated that for many kinds of decision task this strategy is optimal (or at least an improvement over systematic assymetric weighting).

Given its disadvantages, egocentric discounting stands in need of explanation: why should this sub-optimal information gathering strategy persist? Several possible explanations can be suggested:

* Egocentric discounting is an artefact of other necessary or useful processes (spandrel)

* Egocentric discounting may preserve variability and be retained for its capacity to avoid overexploitation and to explore new solutions

* Egocentric discounting may protect against predictability/vulnerability to mal-intentioned advice

The first explanation is difficult to test. The other two will be considered here. First, however, we try to get a sense of how much of an efficency cost is imposed by egocentric discounting. Discounting typically means that, in a two-person situation, the advisee apportions 20-40% weight to the advice of the advisor, and the remaining 60-80% weight to their own initial opinion. Where advice comes from a group of advisors, the weight accorded tends to increase with the size of the group, though it is still stacked such that the group's collective opinion is weighed less heavily than the advisee's initial opinion.

## Model structure

A typical advice-taking task is an estimation problem wherein a decision-maker is tasked with guessing some value. The decision-maker's guess will contain the correct value plus some error, typically drawn from a normal distribution (with a variance inversely equal to the decision-maker's sensitivity). An advisor's advice will take the same form, and, on average, the mean of the decision-maker's estimate and the advisor's estimate will be the best approximation of the true answer. 

The agents in this model operate in this fashion, and act both as advisors and decision-makers (indeed the advice and the decision are usually the same). Advice disseminates over a network graph specifying the ties between agents. During the course of their lives, agents make a series of decision for which the answer varies, and after they have made those decisions the agents undergo an evolutionary process which probabalistically selects the better-performing agents for reproduction.

Environment properties:
* Discrimination vs Estimation task
    * Discrimination task
        * Answers are discrete
        * Advice is discrete
        * Either or both can be graded by confidence
    * Estimation task
        * Answers are necessarily continuous
        * Confidence, if included, is on a different scale to answer
* Decisions per generation
* Reproduction method and rules
    * A/sexual reproduction
    * Probabilistic vs determinisitic selection
    * Fitness function
        * The inverse of the total error accumulated across decisions made during individual's lifetime
    * Which properties are inheritable
* Number of agents

Connectivity:
* Number of advisors per decision-maker
    * Fixed or allowed to vary?
        * Gregariousness would be an interesting property to allow to vary; presumably there's a trade-off between number of advisors recruited and ability to estimate the accuracy of those advisors under some conditions
    * Also number of these consulted on each decision
    * Where multiple advisors are encountered, the scaling of bias by advisor count becomes important
* Static vs dynamic networks
* Estimates of advisors' accuracy
    * Encoded as link strength (discounting applied on top)

Agent (inheritable) properties:  
* Egocentric bias
    * Agents differ in how heavily they weigh the advice of others
* Sensitivity
    * Agents differ in their ability to perform the task
    * Sensitivity is the narrowness of the gaussian distribution from which error is drawn for each decision
* Network ties
    * Children may be more likely to tie with parents' ties...

Extensions:
* Metacognitive accuracy
    * Agents may differ on how well confidence tracks accuracy
* Advice seeking
    * Agents may preferentially choose to get advice from high vs low weighted sources
* Advice != Decision
    * Agents may give advice on decisions they do not face (or give different advice to their decision for themselves), and may put in less effort (perform more poorly) when they do so.
* Objective feedback
    * On some/all decisions
    
## Thesis work

For those readers wishing to reproduce work in the DPhil thesis [Exploring Social Metacognition](https://github.com/mjaquiery/oxforddown), you can do so following the steps below.
Please be aware that the thesis code runs a very large number of models, and will take several tens of hours of processing and around 15GB of storage to run.
Reducing the `runCount` variable at the top of the script will allow the principles to be demonstrated more quickly.

1. Run the code in `thesis.R` to create the raw data.  
2. Run the code in `thesis_reduce.R` to boil this data down to just the dataframes that are archived online and that are loaded by the thesis.
3. The resulting `thesis.Rdata` file can be loaded by the script in the thesis to produce the overall averaged graph.
