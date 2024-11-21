## Combining PAF with Epidemiological Data from GBD

After calculating the **Population Attributable Fraction (PAF)** and its associated confidence intervals (CIs), the next step involves integrating these PAF estimates with epidemiological data obtained from the **Global Burden of Disease (GBD)** study.

To combine the associated uncertainty from both PAF and GBD estimates, we employ the **Delta Method** to propagate the variances and calculate the confidence intervals for the attributable burden metric.



The attributable burden ($A$) is calculated as:
```math
A = PAF \times EPI
```

#### **Propagating Uncertainty**

To propagate the uncertainty from both $PAF$ and $EPI$ to $A$, we employ the **Delta Method**, which uses first-order Taylor series approximations to estimate the variance of a function of random variables.

The Delta Method assumes the following:
- **Independence**: PAF and GBD estimates are independent.
- **Normality**: PAF and GBD estimates are approximately normally distributed.


The variance of $A$ is approximated as:
```math
\text{Var}(A) = \left( EPI \right)^2 \cdot \text{Var}(PAF) + \left( PAF \right)^2 \cdot \text{Var}(EPI)
```
   
where:
- $\text{Var}(PAF) = \left( \frac{\text{PAF}_{\text{upper}} - \text{PAF}_{\text{lower}}}{2  z} \right)^2$
- $\text{Var}(EPI) = \left( \frac{\text{EPI}_{\text{upper}} - \text{EPI}_{\text{lower}}}{2  z} \right)^2$

3. **Calculating Standard Error and Confidence Intervals:**

  - **Standard Error $\text{SE}_A$:**
```math
\text{SE}_A = \sqrt{\text{Var}(A)}
```
     
  - **Confidence Interval:**
```math
\text{CI}_A = A \pm z \times \text{SE}_A
```
  - $z$ = Z-score corresponding to the desired confidence level (e.g., 1.96 for 95% confidence interval)
