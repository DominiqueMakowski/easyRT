
# driftR

*Tools and examples for fitting (Hierarchical) Drift Diffusion Models in
R*

## Motivation

This repo is my attempt at understanding and implementing sequential
models, starting with DDMs for reaction times in R. **Please don’t
hesitate** to open an issue to discuss and suggest things that could be
improved or clarified.

I tried to use HDDM and PyDDM in Python, and had a look at R
alternatives, but I didn’t find any solution that was easy to use
out-of-the-box.

This wouldn’t have been possible without this blogpost
<http://singmann.org/wiener-model-analysis-with-brms-part-i/>

## Theory

![Something](man/figures/wiener_example.webp)

*A graphical illustration of the Wiener diffusion model for two-choice
reaction times. An evidence counter starts at value $\alpha*\beta$ and
evolves with random increments. The mean increment is $\delta$ . The
process terminates as soon as the accrued evidence exceeds $\alpha$ or
deceeds 0. The decision process starts at time $\tau$ from the stimulus
presentation and terminates at the reaction time. \[This figure and
caption are taken from Wabersich and Vandekerckhove (2014, The R
Journal, CC-BY license).\]*

DDms are based on Wiener distributions that contain several parameters:

- The **drift** rate (delta $\delta$) is the average slope of the
  accumulation process towards the boundaries. The larger the (absolute
  value of the) drift rate, the stronger the evidence for the
  corresponding response option.
- The **boundary** separation (alpha $\alpha$) is the distance between
  the two decision bounds and interpreted as a measure of response
  caution.
- The starting point (beta $\beta$) of the accumulation process is a
  measure of response **bias** towards one of the two response
  boundaries.
- The **non-decision time** (tau $\tau$) captures all non-decisional
  process such as stimulus encoding and response processes.

## Data

``` r
library(tidyverse)
library(driftR)

data <- ddm_data(n = c(200, 200),
                 drift = c(0, 2),
                 boundary = 1,
                 bias = 0.5,
                 ndt = 0.2)

head(data$data)
##      rt response condition
## 1 0.338    upper         1
## 2 0.423    upper         1
## 3 0.360    lower         1
## 4 0.275    upper         1
## 5 0.356    lower         1
## 6 0.332    lower         1
```

``` r
ddm_plot(data$data, density = data$density)
```
