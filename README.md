# A visualization of "Algorithmic Price Discrimination - Possible Welfare Outcomes of 3rd order price discrimination with noisy measure of buyers valuation".
## About
This small project simulates the welfare implications of 3rd order price discrimination with uncertainty about the buyers' product valuations.
The code implements an easy example of Cummings et al. (2020) to provide an inuition for their theoretical results.
The paper and the specific example analyzed here is described in this [Presentation](haglund_senn_stalder__cummings_et_al.pdf), Section "Noisy Signal".
The project was a part of a Master-level class in Industrial Organization by Prof. Stefan Buehler and Dr. Nicolas Eschenbaum at University of St. Gallen.

## Related Literature
Cummings, Rachel Devanur, Nikhil R. and Huang, Zhiyi and  Wang, Xiangning. "Algorithmic Price Discrimination". 31st Annual ACM-SIAM Symposium on Discrete Algorithms (SODA 2020), Salt Lake City, UT, USA, 5-8 January 2020.

Bergemann, Dirk and Brooks, Benjamin and Morris, Stephen. “The Limits of Price Discrimination.” The American Economic Review, vol. 105, no. 3, 2015
 
 
## Contributors
Erik Senn (erik.senn[at]gmx.de), Katharina Haglund, Jeremia Stalder

## Code Files
[main.R](main.R) - set input parameters, generate segmentations, plot results

[functions.R](functions.R) - functions to get welfare outcome for given segmentation

## Main File Description

Idea: Compute possible welfare outcomes of market segmentations with only a noisy measure of buyers valuation.
  
Output: Dataframe with optimal outcome for each tested segmentation, plots for welfare outcomes with and without segmentations
  
Current Setting: Example of Cummings et al. Simple random noise and 3 valuations / types.
  
Limitations: Only supports length (value set) = length (type set). Special points (red) in plots are hard-coded, might not fit for every noise level.
  
## Future ToDOs
- Compute optimal segmentation via linear programm (Cummings et al., Theorem 2.1).	
- Define other noise functions.


