# Algorithmic Price Discrimintation - Possible Welfare Outcomes of 3rd order price discrimination with noisy measure of buyers valuation.
Contributors: Erik Senn (erik.senn[at]gmx.de), Katharina Haglund, Jeremia Stalder

Background Info: Code used for Presentation of Cummings et al. (2020) in Industrial Organizations Class, University of St.Gallen

## Related Literature
Cummings, Rachel, et al. "Algorithmic Price Discrimination". 31st Annual ACM-SIAM Symposium on Discrete Algorithms (SODA 2020), Salt Lake City, UT, USA, 5-8 January 2020.

Bergemann, Dirk, et al. “The Limits of Price Discrimination.” The American Economic Review, vol. 105, no. 3, 2015

## Code Files:
main.R - set input parameters, generate segmentations, plot results

functions.R - functions to get welfare outcome for given segmentation

## Main File Description

  Idea: Compute possible welfare outcomes of market segmentations with only a noisy measure of buyers valuation.
  
  Output: Dataframe with optimal outcome for each tested segmentation, Plots for welfare outcomes with and without segmentations
  
  Current Setting: Example of Cummings et al. Simple random noise and 3 valuations / types
  
  Limitations: only tested for length value set = length type set. Special Points (red) in plots are customized, might not suit for every noise level
  
  Optional ToDOs:
  
    Compute optimal segmentation via linear programm (Cummings et al., Theorem 2.1)
	
    Define new noise functions
	
  Theoretical background: Bergemann et al. (2015) / Cummings et al (2020)
