# Model 11 output  

## 19/09/2018  
Results of an initial run from model11.R show that the un/capped decisions are working well, and demonstrating the anticipated effect with noisy advice, noisy communication, and bad advice.  

The results from categorical decisions are extreme, suggesting a mistake in the code. This is possibly due to not ensuring the components are appropriately scaled before combination into a categorical decision. They also appear to be reversed, suggesting fitness is being calculated in an inverted manner. 

## 20/09/2018  

Results show consistency for un/capped decisions. Results show that categorical decisions are fixed for noisy advice and bad advice, but remain undifferentiated at 100% egocentric bias for noisy communication. Again, suggestive of a bug which should be fixed before investigating different use of confidence scales as a potential source of noise.  

## 27/09/2018  

Removed seldom-used results summary, and added recording of every decision in an attempt to detect the issue with categorical decisions under noisy communication. Also switched the default decision type underlying categorical decisions to be capped decisions rather than uncapped decisions. Models have been taking too long to run; something that happens infrequently. Perhaps an infinite loop somewhere, or a bug in the way the code is processed on ARC.  

## 02/10/2018   

Significantly restructured core files to support saving of each decision and to improve runtime. Results should duplicate those from before, including failure to demonstrate averaging for categorical decisions under noisy communication.  

## 04/10/2018  

Fixed bug making agents use the initial decisions of the first generation as advice rather than the initial decisions of their own generation. This has fixed the problem with categorical decisions under noisy communication. Next step is to implement different use of confidence scale and observe whether it produces similar pressure towards egocentric discounting.  

## 06/10/2018  

Made decision recording optional due to its massive time increase.  

## 08/10/2018  

Values for categorical decisions hover around egoBias .5 regardless of sensitivity or manipulation, plausibly because the task is too easy for the agents. Trying again with the world value being any real value drawn from a normal distribution with mean 50 and SD 1.  

## 19/11/2018  

Model 17 shows a curious result when sensitivity is allowed to evolve along with egocentric bias: despite the fact that the sensitivity hits the minimum within a few generations and stays there, egocentric bias still rises to well over .75.  

###Sensitivity:  

![Sensitivity graph](results/d3a4_2018-11-19_13-05-32 sensitivity.png "manipulation = sensitivity evolution")  

###EgoBias:

![EgoBias graph](results/d3a4_2018-11-19_13-05-32 graph.png "manipulation = sensitivity evolution")  



## 14/02/2019

Decisionmaking in humans seems to be more driven by a bimodal choose-self or average approach. Updated the model implementation to use a probability of averaging vs choosing own original estimate rather than a weighting of the average as the outcome. It's not clear whether the bias exceeds 0 using the sensitivity manipulation. 

The model should be run on the more basic manipulations to begin with.

## 25/02/2019

Using the choose-self-or-average strategy under the basic manipulations of advice noise and bad advice, the agents show an extreme sensitivity to the parameter values. Whether they remain stable at any parameter value with an outcome between 0 and 1, or whether only 0 and 1 are stable points at all parameter values. There's potentially a saddle effect where parameter values below some critical number tend eventually to stability at 0, and parameter values above it tend eventually to stability at 1.

Note that 0 and 1 refer to probabilities of choosing self rather than averaging, not to the resultant mean advice-weight. Because simple averaging always occurs where the probability is 0, the own-advice-weight ranges from .5 to 1 for these models.

![Noisy advice](results/2019-02-21_23-50-17_d1-a2_graph.png "Note the remarkable sensitivity to parameter values")

![Bad advice](results/2019-02-23_15-15-17_Uncapped_bad-advice_graph.png "An even closer examination of the potential saddle point.")

## 01/02/2019

Futher exploration in model 19 demonstrates that the relationship is not as clear as previously thought. Over 3000 generations it is plausible that the stability-at-extremes effect emerges, but simulation over more generations is required. 

![Weighted-average strategy](results/2019-03-01_01-57-22_Weighted-Average-with bad advice_graph.png "The existence of a fairly stable mid-point solution seems to be detectable at .98 weighting")

![Pick-or-average strategy](results/2019-03-01_05-55-08_Pick-or-Average-with bad advice_graph.png "Plausibly even the most midrange value (.98) is tending towards a stable extreme value.")

For computational reasons this needs to be broken up into multiple rounds. Each generation acts as a save point for the models, so they can be resumed from previous saves without compromise. The core package needs to be updated to handle taking an existing dataframe as input. 

## 04/03/2019

A **critical** mistake in Model 20 means previous result all used the pick-or-average strategy. The models must be rerun now that Model 20 has been fixed to use the correct weighted-average function where applicable. 

Nick mentioned that the bad advice was catestrophically bad, and that less terrible advice would be better. Also, more decisions/generation may lead to a more even distribution of exposure to bad advice. Consequently, in Model 21:  
* bad advice will be changed to be initial estimate + 3SD using the advisor's sensitivity SD.   
* Decisions/generation will be doubled to 60.   
* Initially the models will be run for a few generations to find the sensitive badAdviceProb values.

The results are pretty surprising: there really does seem to be a split in stability between the weighted-average and pick-or-average models.

![Weighted-average strategy](results/2019-03-05_02-09-39_Weighted-Average-with bad advice_graph.png "Across a range of values which increasingly favour discounting there attractors move in a stable manner.")

![Pick-or-average strategy](results/2019-03-05_00-03-10_Pick-or-Average-with bad advice_graph.png "Strategies converge fast to 1 or 0 rather than midrange values.")

## 18/03/2019

It's plausible from the various human datasets that the human strategy is not simply pick-or-average, but is instead a pick-or-weighted-average/something-else. So this can be modelled simply by allowing a model to include parameters for both the p(pick vs average) and the weighting of the average. 

