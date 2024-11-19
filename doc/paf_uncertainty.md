# Propagating Uncertainty from Risk Ratios (RR) to Population Attributable Fraction (PAF)

For a single exposure level, Risk ratios (RRs) are retrieved with a 90% confidence interval. In our implementation, the Population Attributable Fraction (PAF) is calculated using the formula:

```math
\text{PAF} = \frac{\text{RR}_{\text{perm}}}{\text{RR}_{\text{base}}} - 1
```

where `RR_base` is the baseline RR and `RR_perm` is the permutation RR. This is done for multiple age groups and pixels, before aggregating the PAF across age groups and averaging across pixels using population density rasters to obtain population-weighted estimates.


To propagate the uncertainty from RRs to PAF, two primary methodologies have been implemented:

1. **Bootstrapping**
2. **Delta Method**


Both methods rely assume that the RRs are log-normally distributed (See [Burnett et al. 2014, Supplemental Material p. 28](https://pmc.ncbi.nlm.nih.gov/articles/PMC3984213/) and [Lee et al. 2024](https://pmc.ncbi.nlm.nih.gov/articles/PMC11471335/)).


The resulting confidence intervals are then aggregated across age groups and pixels to obtain population-weighted PAF estimates.


By default, the Delta Method is used to propagate uncertainty from RRs to PAF. The bootstrapping method can quickly run out of memory when the number of bootstrap samples is large or the number of pixels is high. [Lee et al. 2024](https://pmc.ncbi.nlm.nih.gov/articles/PMC11471335/) have shown that both methods provide similar results.


## Methodologies

To propagate the uncertainty from RRs to PAF, two primary methodologies have been implemented:

### 1. Delta Method

The **Delta Method** is an analytical approach that approximates the variance of a function of random variables using a Taylor series expansion.

#### Process:
- **Log Transformation:** Utilize the log-transformed RRs (`log(RR_base)` and `log(RR_perm)`) to stabilize variance and get closer to normality.
- **Variance Estimation:** Calculate the variances of the log-transformed RRs based on their confidence intervals.
- **PAF Formula:** Apply the PAF formula in terms of log-transformed RRs:
    ```math
    \text{PAF} = e^{\log(\text{RR}_{\text{perm}}) - \log(\text{RR}_{\text{base}})} - 1
    ```
- **Variance Propagation:** Use the Delta Method to approximate the variance of PAF by propagating the variances of the log-transformed RRs:
    ```math
    \text{Var}(\text{PAF}) \approx e^{2(X - Y)} \left( \text{Var}(X) + \text{Var}(Y) \right) + e^{2(X - Y)} \left( e^{\text{Var}(X) + \text{Var}(Y)} - 1 - \left( \text{Var}(X) + \text{Var}(Y) \right) \right )
    ```
  where:
  - $X = \log(\text{RR}_{\text{perm}})$
  - $Y = \log(\text{RR}_{\text{base}})$

  
  *This approximation assumes independence between `RR_base` and `RR_perm` and relies on a second-order Taylor series expansion to linearize the relationship.*
  
- **Confidence Intervals:** Derive confidence intervals for PAF using the estimated variance and standard normal distribution:
  ```math
  \text{CI} = \text{PAF} \pm z \times \text{SE}_{\text{PAF}}
  ```
- **Population Weighting:** Aggregate the PAF estimates across age groups and pixels using population weights to obtain population-weighted PAF estimates.


### 2. Bootstrapping

**Bootstrapping** is a resampling technique used to estimate the distribution of a statistic by repeatedly sampling with replacement from the observed data.

#### Process:
- **Log Transformation:** Utilize the log-transformed RRs (`log(RR_base)` and `log(RR_perm)`) to stabilize variance and get closer to normality.
- **Resampling:** Generate a large number of bootstrap samples from the transformed data and the associated confidence intervals.
- **PAF Calculation:** For each bootstrap sample, compute the PAF using the formula:
  ```math
  \text{PAF} = \frac{\text{RR}_{\text{perm}}}{\text{RR}_{\text{base}}} - 1
  ```
- **Aggregation:** Aggregate the PAF estimates across all bootstraps to derive confidence intervals.
- **Population Weighting:** Combine the pixel-level PAF estimates using population density rasters to obtain population-weighted PAF estimates.

