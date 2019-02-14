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

