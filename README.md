# model-selection

This is the repository for Optimized Model Selection for Estimating Treatment Effects from Costly Simulations of the US Opioid Epidemic paper accepted in ANNSIM'24

## Abstract
Agent-based simulation with a synthetic population can help us compare different treatment conditions while everything else is constant within the same population (i.e., digital twin). FRED (A Framework for Reconstructing Epidemiological Dynamics) is an agent-based simulation software with a geospatial aspect containing a synthetic population based on the US Census data. This population scale simulation requires large computational power to get accurate estimates for treatment effects (i.e., small confidence interval width).
Selecting a model at a specific sample size is a crucial problem. Depending on the sample size, the ability of the method to estimate accurately can change significantly. In this paper, we discuss different methods to explore which works on a specific sample size. In addition to the empirical results, we provide a mathematical analysis of the MSE equation and how its components decide which model to select and why a specific method behaves that way in a range of sample sizes.
