
# boundid

The **boundid** R package accompanies the paper “Going Through the Roof:
Difference-in-Differences Designs in Contexts of Natural Boundaries” by
Ludwig Schulze and Joris Frese. In this paper, we discuss parallel
trends violations in difference-in-differences estimations where treated
units are unable to follow the counterfactual control trend
post-treatment due to natural boundaries to the scale of the outcome
variable.

The goal of the **boundid** R package is to quantify the bias resulting
from this parallel trends violation and account for it with various
trimming- and weighting-based methods. The package is built around two
complementary functions: **boundid_test** and **boundid_adjust**.

## Installation

You can install the development version of boundid in R like so:

``` r
devtools::install_github("fresej/boundid")
```

## boundid_test

The **boundid_test** function is used to evaluate (the extent of)
boundary bias in a difference-in-differences setup. It contains the
following parameters:

**Data**: An R dataframe.

**X_var**: The outcome variable.

**threshold**: The natural boundary of the outcome.

**floor**: A logical statement. Should be set to TRUE if the boundary is
a floor, or to FALSE if the boundary is a ceiling. Default is TRUE.

**treatment**: The treatment variable.

**time**: The time variable.

**ID**: The unit ID.

**first_period**: A numeric value indicating the first time period in
which the treatment is administered.

**S**: A numeric value indicating the counterfactual fall. If NULL, the
function automatically computes the counterfactual fall of the control
group.

**more_info**: A logical statement. If TRUE, then additional information
is included in the output, such as the distance of the furthest unit
above or below the boundary (or the distance of the 1st percentile):

**stag**: A logical statement. Should be TRUE if the design is staggered
or FALSE for a basic 2x2 DiD.

The **output** of this function is a console output containing
information about the extent of boundary bias, including, but not
limited to, the pre-treatment means of the control and the treatment
group, the trend of the control group (which is the counterfactual trend
for the treatment group), the number of observations in the treatment
group going below the natural boundary if they follow the counterfactual
trend, the treatment group post-treatment means with and without natural
boundaries after following the counterfactual trend, and a t-test of the
difference between those two means.

## boundid_adjust

The **boundid_adjust** function is used to prepare weighting or trimming
parameters before the DiD estimation step to counteract boundary bias.
In the weighting approach, these weights take on values between 0 and 1.
In the trimming approach, these weights take on either the value 0 or
the value 1. The function contains the following parameters:

**Data**: An R dataframe.

**X_var**: The outcome variable.

**threshold**: The natural boundary of the outcome.

**floor**: A logical statement. Should be set to TRUE if the boundary is
a floor, or to FALSE if the boundary is a ceiling. Default is TRUE.

**treatment**: The treatment variable.

**time**: The time variable.

**ID**: The unit ID.

**group2_mean**: A numeric value indicating the pre-treatment mean of
the treatment group.

**ATU**: A logical statement. TRUE if the ATU is the target estimand.

**ATT**: A logical statement. TRUE if the ATT is the target estimand.

**S**: A numeric value indicating the counterfactual fall. If NULL, the
function automatically computes the counterfactual fall of the control
group.

**cut**: A logical statement. If TRUE, then the cutting/trimming
approach will be used. If FALSE, then the weighting approach will be
used.

**panel**: A logical statement. If TRUE, then the function generates
weights for the whole panel. If FALSE, then the function generates
weights for two time periods.

The **output** of this function is a vector containing the newly
generated weights to be included in the estimation step to counter-act
boundary bias.

## Example

This is a basic example using simulated data to demonstrate how to use
the boundid_test and boundid_adjust functions in the boundid package to
estimate boundary bias and adjust for it with the weighting and the
trimming approach in a classic 2x2 DiD setup:

``` r
invisible(suppressWarnings({
  library(boundid)
  library(tidyverse)
  library(fixest)
  library(ggpubr)
  library(truncnorm)
}))
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.1     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.0
#> ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
#> ✔ purrr     1.0.1     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors


# simulate bounded DiD dataset
set.seed(123)
n = 1000
df <- data.frame(treat = sample(c(1, 0), n, replace = TRUE, prob = c(0.5, 0.5))) %>%
  mutate(post = 0, id = row_number()) %>%
  bind_rows(mutate(., post = 1)) %>%
  group_by(id) %>%
  mutate(
    treat_effect = ifelse(treat == 1, -12 + rnorm(1, mean = 0, sd = 2),  
                          -12 + rnorm(1, mean = 0, sd = 2)),  
    base_outcome = ifelse(treat == 1, 
                          rtruncnorm(1, a = 0, mean = 5, sd = 5), 
                          rtruncnorm(1, a = 0, mean = 15, sd = 5)),
    outcome = ifelse(post == 0, base_outcome, base_outcome + treat_effect)
  ) %>%
  ungroup() %>%
  mutate(
    treat_effect = ifelse(post == 0, NA, treat_effect),
    outcome = pmax(outcome, 0)  # ensures non-negative values
  ) %>%
  select(-base_outcome)


# plot pre- and post-means
means <- df %>%
  group_by(post, treat) %>%
  summarise(mean_X = mean(outcome, na.rm = TRUE))
#> `summarise()` has grouped output by 'post'. You can override using the
#> `.groups` argument.
ggplot(means, aes(x = factor(post), y = mean_X, color = factor(treat))) +
  geom_point(size = 4) +
  labs(title = "Mean of X for Time Periods and Treatment Groups",
       x = "Time Period", y = "Mean of X", color = "Treatment Group") +
  scale_color_manual(values = c("blue", "red")) +
  theme_classic() +
  ylim(0,15)
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r

# boundid_test
boundid_test(df, "outcome", 0, floor = T, df$treat, df$post, 
             more_info = T, ID = "id")
#> Difference between restricted and unrestricted mean:
#> 
#> Control Group Pre-Treatment  Mean:  14.94954 
#> Treatment Group Pre-Treatment  Mean:  6.384563 
#> Counterfactual Trend:  -10.9861 
#> 
#> Values below boundary: 430 
#> Counterfactual Mean with boundary:  0.2579151 
#> Counterfactual Mean without boundary:  -4.601539 
#> Absolute Difference:  4.859454 
#> 
#> Two Sample t-test Results:
#> p-value:  1.68066e-126 
#> t-statistic:  -27.87427 
#> degrees of freedom:  984 
#> confidence interval:  -5.201565 -4.517343
#> [1] 1.68066e-126

# create weights
test_weights <- boundid_adjust(df, "outcome", 0, floor = T,treatment = df$treat, 
                               time = df$post, ID = df$id, ATT = T, 
                               cut = F, panel = F,orig_ID = df$id)
# cut observations
test_cuts <- boundid_adjust(df, "outcome", 0, floor = T,treatment = df$treat, 
                            time = df$post, ID = df$id, ATT = T, cut = T, 
                            panel = F,orig_ID = df$id)
summary(test_weights)
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> 0.0000034 0.0000034 0.0241736 0.4940000 1.0000000 1.0000000
summary(test_cuts)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   0.000   0.000   1.000   0.546   1.000   1.000


# estimate DiDs
## unadjusted
biased <- feols(outcome ~ treat*post|factor(post) + factor(id)  ,data=df, cluster = "id")
#> The variables 'treat' and 'post' have been removed because of collinearity (see $collin.var).

##weighted
weighted <- feols(outcome ~ treat*post|factor(post) + factor(id)  ,data=df, cluster = "id", 
                  weights = test_weights)
#> The variables 'treat' and 'post' have been removed because of collinearity (see $collin.var).

## trimmed
trimmed <- feols(outcome ~ treat*post|factor(post) + factor(id)  ,data=df, cluster = "id", 
                 weights = test_cuts)
#> NOTE: 908 observations removed because of 0-weight.
#> The variables 'treat' and 'post' have been removed because of collinearity (see $collin.var).

# plot DiD coefficients
coef_biased <- coef(biased)["treat:post"]
coef_weighted <- coef(weighted)["treat:post"]
coef_trimmed <- coef(trimmed)["treat:post"]
se_biased <- summary(biased)$coeftable["treat:post", "Std. Error"]
se_weighted <- summary(weighted)$coeftable["treat:post", "Std. Error"]
se_trimmed <- summary(trimmed)$coeftable["treat:post", "Std. Error"]

lower_biased <- coef_biased - 1.96 * se_biased
upper_biased <- coef_biased + 1.96 * se_biased
lower_weighted <- coef_weighted - 1.96 * se_weighted
upper_weighted <- coef_weighted + 1.96 * se_weighted
lower_trimmed <- coef_trimmed - 1.96 * se_trimmed
upper_trimmed <- coef_trimmed + 1.96 * se_trimmed

results <- data.frame(
  model = c("Biased (Unadjusted)", "Weighted (boundid)", "Trimmed (boundid)"),
  estimate = c(coef_biased, coef_weighted, coef_trimmed),
  lower = c(lower_biased, lower_weighted, lower_trimmed),
  upper = c(upper_biased, upper_weighted, upper_trimmed)
)

ggplot(results, aes(x = model, y = estimate, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, size = 1, color = "red", linetype = "dashed") +
  geom_pointrange(size = 1, linewidth = 1) +
  labs(x = "Model", y = "DiD Coefficient", 
       title = "DiD Coefficients with 95% Confidence Intervals") +
  theme_minimal()
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
```

<img src="man/figures/README-example-2.png" width="100%" />
