# Model 11 output  
## 19/09/2018  
Results of an initial run from model11.R show that the un/capped decisions are working well, and demonstrating the anticipated effect with noisy advice, noisy communication, and bad advice.  
The results from categorical decisions are extreme, suggesting a mistake in the code. This is possibly due to not ensuring the components are appropriately scaled before combination into a categorical decision. They also appear to be reversed, suggesting fitness is being calculated in an inverted manner.  
## 20/09/2018  
Results show consistency for un/capped decisions. Results show that categorical decisions are fixed for noisy advice and bad advice, but remain undifferentiated at 100% egocentric bias for noisy communication. Again, suggestive of a bug which should be fixed before investigating different use of confidence scales as a potential source of noise.  
