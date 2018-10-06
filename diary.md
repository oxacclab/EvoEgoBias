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