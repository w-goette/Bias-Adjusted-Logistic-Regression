---
title: "Supplementary Materials"
description: | 
  Additional documentation of the simulation study on a bias-adjusted Bayesian logistic regression model.
date: 09/12/2022
author:
  - name: William Goette
    affiliations: 
      - id: CLP
        name: University of Texas Southwestern Medical Center
        department: Department of Pscyhiatry
        city: Dallas
        state: Texas
    orcid: 0000-0003-2909-1068
    email: William.Goette@UTSouthwestern.edu
abstract: |
      In the corresponding study, we presented general summaries describing the simulation conditions and methods as well as the results. The supplementary details provided here provide additional documentation on the simulation study as well as deeper exploration of all results from the study.
citation:
  type: document
title-block-banner: true
title-block-categories: true
format: 
  html:
    self-contained: true
    toc: true
    number-sections: true
    smooth-scroll: true
    reference-location: margin
    citation-location: margin
    link-external-icon: true
    link-external-newwindow: true
    code-fold: true
    code-tools:
      source: false
      toggle: true
      caption: "Reveal code"
    code-overflow: scroll
    code-block-border-left: true
    theme: 
      light: flatly
      dark: darkly
    mainfont: "Lato"
fig-cap-location: top
tbl-cap-location: top
knitr:
  opts_chunk: 
    R.options:
      knitr.graphics.auto_pdf: true
comments:
  hypothesis: true
execute: 
  echo: false
  warning: false
editor: visual
---

```{r}
#read in data
load(".RData")

#load libraries
library(tidyverse)
library(ggdist)
library(cmdstanr)
library(posterior)
library(easystats)
library(bayesplot)
library(gganimate)
library(patchwork)
library(knitr)
library(kableExtra)
```

# Purpose of Document

The number of analyses and conditions examined for this study extend beyond the scope of what can be summarized or described reasonably in a typical manuscript, but it is still important that readers have access to all relevant information for a statistical method that is being proposed. Toward this end, these material are provided to all curious readers alongside the annotated R script materials. The aim of those annotated materials was to provide readers familiar with R all the documentation needed to understand the code without having to run it while also providing anyone with access to R the ability to reproduce the simulation study results. In conjunction with those script files, this supplementary documentation should provide practical and more accessible summary of the methods for readers who may be unfamiliar with R.

An additional strength of this supplementary material over the R script files is that there is much greater space with these material for exploring the results of the study. There are potentially many figures and tables of interest for understanding how the model performs and how various study conditions affected these performances. Ultimately, this document is unlikely to provide all of the results and visualizations that a critical reader may want, but it will certainly do better than the manuscript was in terms of comprehensive summary. Additionally, those with more specific questions and desires from the results are encouraged to reproduce the simulation study and extract those results for themselves.

These material were created with the understanding that information may or may not be relevant to each individual reader. As such, every effort has been made to make the sections of this webpage independent of each other. A side effect of this is that there may be repetition of information for anyone who is reading from beginning to end. This being said, the general outline of this supplement is as follows:

1.  Mathematical Details
    i)  Provides general background on the model (includes overview of Bayesian methods)
    ii) Summarizes mathematical details of a typical logistic regression
    iii) Explains mathematical specification of the proposed adjusted model
2.  Simulation Study Details
    i)  Begins with a discussion of the purpose and uses of simulation studies
    ii) Provides explanations of how simulated data were generated
    iii) Concludes with detailed documentation of results from the simulation study
3.  Applied Data Study Details
    i)  Gives extra result documentation from the non-impaired versus Alzheimer's disease case example
    ii) Gives the same extra documentation but for the non-impaired versus mild cognitive impairment case example

## Using the Document

Several features of the document may be helpful for readers to know:

First, an option for viewing the page in "night" mode (i.e., with darker background colors) is provided via a slider in the top right-hand side of the page.

Second, by default, all R code referenced in the document is hidden, but this can be toggled on or off as desired. Individual sections of code may also be revealed as desired by clicking on the code option. Most of the code is either specific to generating plots or tables used for illustrative purposes in this document or are provided in the R script files in the case of any summaries of simulation results. Additionally, hovering over code breaks in the document should give an option to copy the code for convenience.

Third, a table of contents appears on the right of the page. Readers looking for specific sections are encouraged to navigate their via the table of contents. The table of contents will disappear in cases where there is margin content on the right-hand side. If this happens when the table of contents is needed, then scrolling up or down should make the table reappear.

Fourth, any links to external websites will be demarcated with an indicator (a small rectangle with an arrow pointing from the center through the top right corner). By default, clicking any of these links will open the linked material in a separate window/tab.

Fifth, a citation for this document is provided at the bottom of the page. This citation is not guaranteed to be accurate as this page was made public before peer-review and acceptance of the study, so depending on when this material is accessed, the citation may no longer be up-to-date. Additionally, once the manuscript is accepted for publication, the citation on this page will be for the accepted manuscript, not for this supplementary material. If the supplementary material needs to be cited independently of the published study for any reason, then readers are encouraged to identify the appropriate methods for this within their professional/journal guidelines.

# Overview of the Proposed Model

One of the advantages of Bayesian methods is their flexibility in estimation as they do no necessarily rely on strict assumptions of parametric forms or asymptotic sampling distributions for inference. This being said, Bayesian models and their frequentist counterparts do generally return similar results with the similarity increasing as the priors used in the Bayesian estimation become increasingly flat. While there may be some solace in the fact that a Bayesian model can be made to return a result that is similar to that of a frequentist equivalent, specifying priors for the sole aim of returning values that align with another estimation method undermines some of the significant strengths of Bayesian methods. To help unpack these subtleties, this first section aims to introduce some very general details of Bayesian models and the role of simulations in a Bayesian workflow.

## What are Bayesian Methods and Why Use Them?

### Bayesianism and Likelihoods

At the core of all Bayesian methods is an integration of **prior** information, the **likelihood** of the data, and the **posterior** distribution that is, more or less, a weighted average of the prior and likelihood. Likelihood functions are likely familiar to nearly all researchers as the most prevalent method for model estimation is that of maximum likelihood estimation wherein **parameter** values are taken to whatever values maximize the likelihood of the observed data. We can examine this property using a simple one-sample *z*-test. Say we have a sample of *n* = 15 individuals who have all been given the same intelligence test that has a mean of 100 and standard deviation of 15 in the population (i.e., $\mu$ = 100 and $\sigma$ = 15). This sample of individuals has a mean of 88 and standard deviation of 12 (i.e., $\bar{x}$ = 88 and $s_x$ = 12), and we want to know what the likelihood of these sample statistics would be given our known population parameters. In this case, there is no unknown information and no need to estimate the population parameters, so the likelihood of the sample's data is given by the *p*-value of the sample's *z*-statistic: $\frac{\bar{x} - \mu}{\sigma / \sqrt{n}} = \frac{88 - 100}{15 / \sqrt{15}} \approx -3.10, p = .001$. This example may not look too informative for what maximum likelihood estimation is really doing, so we can consider the next step to this question as it appears unlikely that the sample of *n* = 15, $\bar{x}$ = 88, and $s_x$ = 12 derives from the sample population with $\mu$ = 100 and $\sigma$ = 15. In this case, the natural question is, based on what is observed from the sample, what might we guess as being their respective population's mean and standard deviation?

::: column-margin

Prior

:   probability distribution summarizing the plausibility of parameter values before knowing anything about the data

Likelihood

:   a probability function capturing the probability of the observed data given possible parameter values

Posterior

:   the probability distribution of parameters that can be thought of as a weighted average of the prior and likelihood distributions

Parameter

:   an unknown numerical property belonging to a population that is estimated based on data sampled from that population -- statistical inference attempts to quantify its value using mathemetical models
:::

In order to answer this question, we need to consider what kind of shape we assume the population to have. As this is intelligence testing, a normal distribution is a reasonable assumption for the distribution of scores in a population. In most psychological phenomena, a normal distribution will be reasonable as the Gaussian curve emerges anytime that there is a population-level central tendency (i.e., values that observations center around) and there is random variation over all observations (e.g., genetic mutations, measurement error, sampling error). The formula for the **probability distribution function** of the normal (or Gaussian) distribution is

$$f(x_i) = \frac{1}{\sigma\sqrt{2\pi}}e^{-\frac{1}{2}(\frac{X_i - \mu}{\sigma})^2}$$

A deep mathematical understanding of this function is not needed; instead, it is important to note that the probability of a single observation ($x_i$) depends on only two unknown values: $\mu$ and $\sigma$ as all other terms in the equation are known. To put this another way, the probability of observing the same $x_i$ value will vary due only to what values we use for $\mu$ and $\sigma$, which should make intuitive sense. For example, if we administered a memory test to an individual and they obtain a standardized score of 102, then this is very probable in the general/healthy population but would be very rare in the Alzheimer's disease population, meaning that the score does not change but its probability will as a function of just the mean and standard deviation.

::: column-margin

Probability Distribution Function

:   mathematical formula that returns a curve over a range of values with the height of the curve at any given value corresponding its probability

Likelihood Function

:   the iteration of a probability distribution over all observations in a sample -- usually on the natural logarithm scale
:::

If the statistical interest is purely on estimating the probability of a single score, then the above formula is sufficient; however, likelihood functions are compound functions over a whole sample. Most of the time, we can assume that a sample consists of independent and identically distributed (commonly abbreviated as ***i.i.d.***) observations of a population, meaning that each observation is sampled from the same population (i.e., identically distributed) and that each new observation is not impacted by any previous observations (i.e., observations are independent of one another). Given the assumption that observations in a sample are i.i.d, we do not need to worry about how to extend the probability distribution function to the **likelihood function**. First, since the sampled data all come from the same population, we have only one set of parameters to estimate with each observation providing some information about the same parameters. Second, since the data are all independent of one another, combining all of the individual probabilities together is just a matter of multiplying them in the same way that we would multiply $\frac{1}{2}\times\frac{1}{2}\times\frac{1}{2}$ to get the probability of flipping a coin and getting heads three times. With the normal distribution, this can be written as the following:

$$\ln(\mathcal{L}_X) = 
\frac{1}{\sigma\sqrt{2\pi}}e^{-\frac{1}{2}(\frac{X_1 - \mu}{\sigma})^2} \times \frac{1}{\sigma\sqrt{2\pi}}e^{-\frac{1}{2}(\frac{X_2 - \mu}{\sigma})^2} \times \frac{1}{\sigma\sqrt{2\pi}}e^{-\frac{1}{2}(\frac{X_3 - \mu}{\sigma})^2} \times \dots \times
\frac{1}{\sigma\sqrt{2\pi}}e^{-\frac{1}{2}(\frac{X_N - \mu}{\sigma})^2}$$

or, if we use an operator to make the output slightly simpler, then it can be written as:

$$\ln(\mathcal{L}_X) = \prod_{i = 1}^{N} \frac{1}{\sigma\sqrt{2\pi}}e^{-\frac{1}{2}(\frac{X_i - \mu}{\sigma})^2}$$

As with the probability distribution function, a deep understanding of the math involved here is not too necessary as the important takeaway is that the likelihood function is just the product of the probabilities of each observation in a sample. While multiplication is straightforward, it is even easier to work with addition, so likelihoods are often used on the natural logarithm scale because multiplication becomes addition on the log-scale (e.g., $A \times B = C$ and $\log{A} + \log{B} = \log{C}$ are the same thing). The natural logarithm, $\log_e$ or $\ln$, is generally used in these cases as it also simplifies the math of equations with a power of $e$ because $\log_e{e^{x}} = x$. The normal distribution's log-probability distribution function and its corresponding log-likelihood function then become the following:

$$\ln(f(x_i)) = -\frac{1}{2}\ln(2\pi) - \frac{1}{2}\ln(\sigma^2) - \frac{1}{2}(\frac{x_i - \mu}{\sigma})^2$$ $$\ln(\mathcal{L}_X) = \frac{N}{2}\ln(2\pi) - \frac{N}{2}\ln(\sigma^2) - \frac{1}{2}\sum_{i = 1}^N(\frac{x_i - \mu}{\sigma})^2$$

Importantly, the formula in this form still depends solely on the unknowns of $\mu$ and $\sigma$, and in this particular format, the effect that the sample size ($N$) has on the resulting log-likelihood is relatively clear. Up to this point, however, we have not actually tried to solve the problem of what $\mu$ and $\sigma$ should be, but the next step is in the name: maximum likelihood estimation. Our estimate for these parameters will be whatever value gives the maximum possible value for the (log-)likelihood. Finding the maximum of a function can be done with calculus. While the objective here is not to teach the reader how to do calculus, it is helpful for the sake of completeness to know what is being done with maximum likelihood estimation. One way of finding a maximum in a function is to take the function's derivative with respect to whatever variable is of interest in the function (where function really just means an equation of some kind). The reason that the derivative can indicate the presence of a maximum point is that the derivative is the slope of the function at some point. If one imagines a function that produces some curve over some range of values, then the derivative is the slope of a line that runs tangential to the curve at every point along that range of values. This is perhaps easier to visualize, so consider the figure below:

```{r message = FALSE}
#create plot
data.frame(x = seq(-6, 6, 0.01)) %>%
  mutate(
    y = -(x^4 + 2*x^3 - 24*x^2 - 50*x - 25),
    dx = -(4*x^3 + 6*x^2 - 48*x - 50),
    ddx = -(12*x^2 + 12*x - 48),
    int = dx * x * -1 + y
    ) %>%
ggplot(
  aes(x = x, y = y)
  ) +
  geom_path() +
  geom_point() +
  geom_abline(
    aes(intercept = int, slope = dx),
    linetype = 2
    ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(-6, 6)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(-450, 275)
  ) +
  labs(
    title = "Base and Derivative Function"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  transition_reveal(
    x, keep_last = FALSE
    )
  
```

As the function is plotted, a line running tangent to the point is also shown. This tangent line is the derivative of the curve. When the curve is rising, the derivative has a positive slope, and the opposite is true when the curve is falling. Note that, at the points of the curve where its direction changes, the tangent line is actually flat. Thinking about the derivative as the rate of change in the curve, when the curve changes its direction, then the curve is either going from increasing to decreasing or vice versa. Since the tangent line is flat at these points, the derivative (i.e., the slope of that tangent line) is 0, so taking a derivative with respect to some variable and solving the derivative for 0 will return the points at which the function is changing from one direction to the next. When this change in direction is from increasing to decreasing, then that point is a maximum. This plot reveals a deeper issue with relying just on derivatives for finding a maximum. For starters, the first derivative does not differentiate between a maximum and minimum value nor does it tell how many maxima or minima there may be. In cases like this one, where there are multiple modes, then there is a local maximum (the smaller bump on the left of the plot) and a global maximum (the larger hump to the right). Anyone familiar with more complicated maximum likelihood estimations (e.g., mixture models, structural equation models) is aware that the detection of a maximum is not a guarantee that the maximum likelihood has been found, which is why various maximum likelihood estimation methods exist and provide various checks to avoid identification of local maxima. In the case of the normal distribution (and most other parametric distributions), this is not a concern as there is only one peak, meaning that there will ever only be one point in the function where the derivative is exactly zero.

Using the derivative method to find the maximum likelihood estimator of the normal distribution's unknown parameters, we end up needing to take two partial derivatives: one for the mean and one for the standard deviation. For the sake of brevity, we will skip why the following is the derivative of the likelihood function with respect to $\mu$, but we will still follow the problem for a couple of steps to demonstrate what the maximum likelihood estimator of the mean is from a sample. The (partial) derivative of the log-likelihood with respect to $\mu$ is the following:

$$\frac{\partial}{\partial\mu}\ln(\mathcal{L}_X) = \sum_{i=1}^{N}\frac{x_i - \mu}{\sigma}$$

The formula from here can be simplified one extra time and then some basic algebra will give the maximum likelihood estimator for the population mean. Since it can be hard to know exactly what to do with the summation operator, it helps to consider the fact that $\mu$ is going to be subtracted a total of $N$ times just like how the numerator of this equation will be divided by $\sigma$ a total of $N$ times. Since these are all fixed values, we can pull them out of the summation operator by simply multiplying them by $N$, which means that the only summation left is the sum of all the observed data. The other simplification that can be made before turning to algebra to solve the equation is to set the left-hand side equal to 0 as we know that the likelihood is maximized when this derivative equals zero. The maximum likelihood estimator for $\mu$ can thus be solved as follows:

$$0 = \frac{\sum_{i=1}^{N}x_i - N\mu}{N\sigma}$$

$$0 = \sum_{i=1}^{N}x_i - N\mu$$

$$N\mu = \sum_{i=1}^{N}x_i$$

$$\mu = \frac{\sum_{i=1}^{N}x_i}{N}$$

Most readers will probably recognize the final statement as the mathematical notation for computing the mean, so the maximum likelihood estimate for the population mean is actually just the sample mean. It is convenient that the estimation of the population mean, $\mu$, depends on no other information than the sample mean, which makes this an ideal demonstration of a relatively straightforward maximum likelihood estimation. Unfortunately, the derivation of the maximum likelihood estimate for $\sigma$ is slightly more complicated and involves the estimate for the population mean, $\hat{\mu}$. Sparing these details, the maximum likelihood estimate for the population standard deviation ends up being the unadjusted sample standard deviation (i.e., variance divided by N rather than N - 1). Since these sample statistics end up being the values that maximize the likelihood function for a set of observed, normally distributed data, they can be used directly in the *z*-test statistic (and many other statistics where a population mean or standard deviation must be estimated).

### Limitation of Maximum Likelihoods Alone

At this point, it's useful to reflect on exactly why these details were worked through. Superficially, the objective was to describe the likelihood function, which is part of the Bayesian estimation and how frequentist and Bayesian estimation typically produce similar results. More subtly, the goal was to identify a significant limitation of frequentist methods and lay the foundation for why Bayesian methods answer the questions that most researchers are interested in.

To identify this limitation, we need to consider how we went about maximizing the likelihood function earlier. Since this will apply to all parameters, instead of using traditional parameter notations like $\mu$ and $\sigma$, we will use $\theta$ to generically refer to any parameter or set of parameters. In essence, we solved the various likelihood equations by asking "what value of $\theta$ gives the greatest likelihood?" This seems reasonable, but it is important to stop and reflect on what information must be used in order to answer this question. In the case of any statistic, there will be a limited set of known quantities that are obtained from either mathematical constants (e.g., $\pi$, $e$, $\sqrt{2}$) or sample data (e.g., $N$, $\bar{x}$, $s_x$) that must then be used to estimate some value for the unknown parameters. In the case of previously worked examples, we leveraged the fact that the sample data are known so that the likelihood function was maximized with respect to this sample data. Since the likelihood function is just a probability associated with a set of values, we can write the likelihood as a probability statement where we have ultimately estimated $p(X| \theta)$, which is read as the probability of the observed data, $X$, given the estimation for the parameter(s), $\theta$.

The idea that statistical estimation ultimately estimates parameters in such a way as to maximize $p(X | \theta)$ (i.e., the probability of the data) makes sense for the context of typical *p*-values, which quantifies the probability of observing data as or more extreme relative to the implied parameter values. While sensible when a researcher cares about the *p*-value as interpreted appropriately, most researchers are more interested in being able to use sample data to estimate the probability of the parameters: $p(\theta | X)$ instead of $p(X | \theta)$. Though the difference may seem relatively superficial, it has important implications for the conclusions that can be drawn from statistical methods.

### Back to Bayes

The issue, fundamentally, is that $p(\theta | X) \neq p(X | \theta)$, so finding $\theta$ that maximizes the likelihood of the data does not give direct information about the probability of $\theta$ values. To illustrate how the two probability statements are not equal to one another, we can consider the case of medical screening tests. For simplicity, we can assume that there are only two possible conditions that any person can have: healthy or unhealthy. Similarly, we can assume that the test gives only two results: negative or positive. The probability that any single person selected at random from the population will be "unhealthy" is given by the baserate of the condition, so if a disease occurs in 30% of the population, then the probability that any single, random person from the population has the condition is 0.30. Most screening tests are also imperfect in their detection of the condition with most having greater sensitivity than specificity to the condition of interest. If a test has a sensitivity of 0.90, then this means that the probability that the test is positive for someone who has the condition of interest is 0.90. So far, we know the probability of having the condition with no other knowledge ($p(\theta) = 0.30$), and we will know the result of the test (either $X = 0$ for a negative test or $X = 1$ for a positive). In this scenario, there are 4 possible outcomes: $\theta = 1$ and $X = 1$ (true positive), $\theta = 1$ and $X = 0$ (false negative), $\theta = 0$ and $X = 1$ (false positive), and $\theta = 0$ and $X = 0$ (true negative). If a test is positive, then maximizing the likelihood for $p(X = 1 | \theta)$ means assuming $\theta = 1$ as this corresponds to the sensitivity of the test, which was 0.90. Note that this is not the same thing as being able to answer $p(\theta | X = 1)$, or the probability that someone has the condition given the fact that they tested positive for it.

In order to estimate the key quantity $p(\theta | X)$, we need to use Bayes' theorem:

$$p(\theta | X) = \frac{p(\theta) \times p(X|\theta)}{p(X)}$$

There are a few things to note about Bayes' theorem based on the content presented so far. First, we can relate some key quantities to the definitions of probabilities distributions earlier. To begin, there is the now familiar likelihood, $p(X | \theta)$ in the numerator, and the result of the formula is the posterior distribution, $p(\theta | X)$. Of the two remaining terms, $p(\theta)$ is the prior distribution and $p(X)$ is called the marginal, or sometimes the evidence as it quantifies what has been observed. The marginal normalizes the formula, meaning that it guarantees that the result is probability distribution that sums to unity (1). Unfortunately, this value normally hard to quantify, so most Bayesian estimations replace the marginal with a different normalizing factor. For this example, however, we will use the traditional marginal.

The information known so far is that $X = 1$, $p(\theta) = 0.30$ and that $p(X = 1 | \theta) = 0.90$ (when maximized by estimating $\theta = 1$). All that remains in this case is finding the marginal, which would be $p(X = 1)$. Since we are treating this problem as a traditional binary classification problem, let us assume that the test's sensitivity was derived from a confusion matrix. Since this is the case, we could identify the marginal by examining the rate of all positive tests (true and false positives) in that matrix. For convenience, let's say that the confusion matrix is the one that appears in the margin. In this case, a total of 27 + 14 = 41 test results are expected be positive test, and since there were 100 total individuals in this confusion matrix, this means that the probability of a positive result is simply $p(X = 1) = \frac{41}{100} = 0.41$.

```{r}
#| tbl-cap: Mock Confusion Matrix
#| column: margin

as.table(matrix(c(27, 14, 3, 56), nrow = 2, dimnames = list(c("True: +", "True: -"), c ("Test: +", "Test: -"))))
```

Finally, all of this information can be combined to derive the probability of interest, namely the probability that a person has the condition of interest given that they have tested postive for it. These values can be plugged into Bayes' theorem as follows:

$$p(\theta = 1 | X = 1) = \frac{p(\theta = 1) \times p(X = 1 | \theta = 1)}{p(X = 1)}$$ $$p(\theta = 1 | X = 1) = \frac{0.30 \times 0.90}{0.40} \approx 0.68$$

As previously alluded to, $p(\theta | X) \neq p(X | \theta)$ despite the fact that the two statements seem like simple translations of one another. In this case, the sensitivity of the test ($p(X = 1 | \theta = 1)$) would lead a clinician to be overly confident that a person has the condition of interest as the actual probability that the positive test result is indicative of the condition is just over $\frac{2}{3}$ compared to the more generous estimate of $\frac{9}{10}$ given by the test sensitivity. While Bayes' theorem is generally introduced in similar types of examples (e.g., some screening test that is highly accurate but a condition that has low baserate/prior probability), the same general methods are used for any Bayesian statistical analysis. As mentioned earlier, the marginal probability, $p(X)$, is challenging to quantify in most cases, so most Bayesian estimations will replace the denominator's $p(x)$ with the integral of the numerator along $\theta$. Under this amendment, Bayes' theorem becomes this:

$$p(\theta | X) = \frac{p(\theta) \times p(X | \theta)}{\int{p(\theta) \times p(X | \theta)d(\theta)}}$$

The integral in the denominator applies to continuous data and parameter spaces, which is typical of most statistical problems. In cases of discrete data, the integral is just replaced with a summation of the numerator over the range of possible discrete values. As this is easier to observe in the discrete case, we will consider this for now. The marginal value $p(X)$ can be calculated from summation of conditional probabilities through the law of total probability which states that $p(A) = \sum_np(A \cap B_n)$ when event $B_n$ is a partition of the same sampling space as $A$. To put this in simpler terms, the law of total probability indicates that the probability of event $A$ is the sum of the probabilities that both $A$ and $B_n$ occur as long as event $B_n$ is required for $A$ to occur as well. Say, for example, that one wants to know the probability that a particular set of batteries pulled randomly from a bag will be corroded. In this bag are 100 batteries from 3 different brands: 50% of the batteries are from Brand 1, 25% from Brand 2, and the remaining 25% from Brand 3. Brand 1 says that just 3% of its batteries corrode after a single year. Brand 2 advertises that 1.5% of its batteries corrode in the same time frame. Finally, Brand 3 has data suggesting that 5% of its batteries corrode a year after purchase. The probability that any single battery is corroded, let's call this $p(A)$, therefore depends on what brand of battery is pulled, or in stats terms is $p(A | B_n)$. In this case, not all batteries are equally likely, so $p(A | B_n)$ needs to be weighted by the probability of the specific battery brand, or $p(B_n)$ so that we can get $p(A \cap B_n)$. In other words, to find $p(A)$ we need to compute $\sum_np(A | B_n) \times p(B_n)$, which takes the same form as in Bayes' theorem. For completeness, this simple example produces the following: $p(A) = 0.03 \times 0.50 + 0.015 \times 0.25 + 0.05 \times 0.25 \approx 0.03$. If we wanted, then we could use this value and the rest of the information that we know (prior = $p(B_n)$, likelihood = $p(A | B_n)$, and the marginal $p(A)$) to derive the posterior ($p(B_n | A)$ with Bayes' theorem if so desired. As mentioned, this summation example in the case of discrete outcomes is extended to the continuous case via the integral.

Statistical programs like Stan, which was used for this project, utilize Bayesian estimation methods that do not rely on direct computation of a normalized posterior probability distribution. As a result, it is not uncommon to see Bayes' theorem reduced to $p(\theta | X) \propto p(\theta) \times p(X | \theta)$, which is to say that the posterior distribution is proportional to the product of the prior and likelihood distributions. More accurately, one can say that the joint probability distribution $p(X, \theta)$ is equal to the product of the prior and likelihood. Rather than utilizing pure calculus as is done in many other statistical estimations, many Bayesian programs, like Stan, will draw samples from the posterior distribution, $p(\theta | X)$, using the joint distribution of $p(X, \theta)$, though to be technically precise these are really done on the log-probabilities. Stan implements a specific type of sampler for the posterior estimation called a Hamiltonian Monte Carlo (HMC) sampler, which is similar in purpose but different in its methodological implementation from the more familiar Markov Chain Monte Carlo sampler of most Bayesian statistical programs. In HMC, the gradient of the parameter space is sampled over many different values of $\theta$ while holding $X$ fixed. The combination of the the gradient information with the joint probability distribution is sufficient to derive estimates of the posterior via the samples drawn from the fitting algorithm. Many technical details are omitted for the sake of both brevity and readability as it may not be relevant to many researchers as to how Bayesian estimators work in much the same way that knowing how to implement an Expectation-Maximization algorithm for (local) maximum likelihood estimation may not be relevant for deriving *p*-values in structural equation models.

### Putting It Together

At this point, the fundamental relationships between Bayesian and maximum likelihood estimation have been laid out to some degree of detail, so it is reasonable to then whether Bayesian methods provide any benefits that warrant the additional considerations. For most individuals first encountering Bayesian statistics, the idea of a researcher specifying a prior distribution over the plausible parameter values can raise suspicion about bias. As a simple example, consider the case of two opposing researchers. Researcher A is a proponent of a particular experimental diagnostic test for which they receive a small royalty while Researcher B has repeatedly criticized this test and is a consultant for a competing test. After extensive back-and-forth, both have laid out the best possible research design to definitively assess the accuracy of the test, and they want to utilize Bayesian statistical estimation on the results of this study. In order to calculate these results, they need to specify a prior for the accuracy of the test. Since accuracy must be between 0 and 1, a reasonable distribution to use for this prior is the beta distribution. The plot below shows the priors for the two researchers:

```{r}
#| layout-ncol: 2
#| column: page-inset
#| fig-cap: 
#|   - "Researcher A's Prior Belief (proponent)"
#|   - "Researcher B's Prior Belief (critic)"

prior_a <-
ggplot() + 
  geom_function(
    fun = dbeta, 
    args = list(
      shape1 = 20, 
      shape2 = 5
      )
    ) +
  scale_x_continuous(
    limits = c(0, 1)
  ) +
  labs(
    x = "Test Accuracy",
    y = "Probability Density"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_blank(),
        axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14)
  )

prior_b <- 
ggplot() +
  geom_function(
    fun = dbeta, 
    args = list(
      shape1 = 5, 
      shape2 = 20
      )
    ) +
  scale_x_continuous(
    limits = c(0, 1)
  ) +
  labs(
    x = "Test Accuracy",
    y = "Probability Density"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_blank(),
        axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14)
  )

prior_a
prior_b

```

These prior distributions strongly reflect both researcher's beliefs about what the accuracy of the test will be. In the case of Researcher A, there is essentially no probability given to any accuracy less than 50% while the most probable accuracies are believed to be aroung 80%. The opposite is true for Research B who places nearly no probability of an accuracy better than 50% and actually believes that the most probable accuracies would be around 20%. Since external factors may make it unlikely that these prior beliefs for either researcher are likely to change too much before any data is observed, we can add two additional reseachers (perhaps peer reviewers) who are skeptics. We can say that both Researcher C and D are skeptical in the sense that they believe the most likely accuracy estimate for the test is 50% or somewhere near this chance (or null) level. What we will vary over these two skeptical researchers is the degree to which they are skeptical: Researcher C will be "weakly" skeptical while Researcher D will be "strongly" skeptical. Their prior beliefs are shared below:

```{r}
#| layout-ncol: 2
#| column: page-inset
#| fig-cap: 
#|   - "Researcher C's Prior Belief (weak skeptic)"
#|   - "Researcher D's Prior Belief (strong skeptic)"

prior_c <-
ggplot() + 
  geom_function(
    fun = dbeta, 
    args = list(
      shape1 = 2, 
      shape2 = 2
      )
    ) +
  scale_x_continuous(
    limits = c(0, 1)
  ) +
  labs(
    x = "Test Accuracy",
    y = "Probability Density"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_blank(),
        axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14)
  )

prior_d <-
ggplot() +
  geom_function(
    fun = dbeta, 
    args = list(
      shape1 = 35, 
      shape2 = 35
      )
    ) +
  scale_x_continuous(
    limits = c(0, 1)
  ) +
  labs(
    x = "Test Accuracy",
    y = "Probability Density"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_blank(),
        axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14)
  )

prior_c
prior_d

```

Compared to the previous two researchers, Researchers C and D place the greatest probability on the fact that the test is either 50% accurate (i.e., chance level) or something close to this, but Researcher C is less "sure" of this as a wide range of possible values are considered somewhat probable compared to Researcher D. This thought experiment is clearly contrived in order to elicit these extreme priors, but there is still some area of truth in this scenario as it is possible that priors could be abused by researchers much as *p*-hacking can bias frequentistic statistics. Seeing these various prior distributions for the parameter of test accuracy may also help to build some intuition for how data can influence the final conclusions since the posterior distribution of interest to Bayesian statistics is essentially the weighted average of the prior and data likelihood. In cases where the prior is highly influential, then the posterior will look more like that prior, and then, when the data are many (or the prior is weakly informative), the posterior will look more like the likelihood. In order to see what happens when data are added to the estimation, we need to first figure out what the data should be. For the sake of this hypothetical, we will say that the disease of interest has a prevalence of 40% and the test really has a sensitivity of 80% and specificity of 70%. Given these true parameter values, the accuracy of the test can be computed as $0.80 \times 0.40 + 0.70 \times (1 - 0.40) = 0.74$, which is a respectable accuracy but certainly not phenomenal. Since this hypothetical study was purported to be definitive, we will also want to ensure that it has a high power to detect the true accuracy of test. To compute the sample size needed, we can use the following two power equations based on sensitivity and specificity:

$$TP + FN = z^2 \frac{sens \times (1 - sens)}{L^2}$$ $$TN + FP = z^2 \frac{spec \times (1 - spec)}{L^2}$$

Starting with the formula for sensitivity, the left hand side is the sum of individuals who have the condition of interest (i.e., those true positives and false negatives from the test results), which are the counts needed to compute sensitivity: $\frac{TP}{TP + FN}$. The same is true for specificity's left-hand side. The only other terms that need to be defined in these equations is *z*, which is simply the *z* statistic associated with the desired $\alpha$ rate of the analysis, and then L, which is the width of the desired 95% confidence interval for the value of interest. Since the study was intended to be highly accurate and informative, we can say that the desired $\alpha$ of the study is 0.001 (instead of the more traditional 0.05) and that we wanted to derive an estimate whose 95% confidence interval spanned no more than 0.10 points. Substituting these values in to the equations below gives the following:

$$TP + FN = 3.29 \frac{0.80 \times (1 - 0.80)}{0.10^2} = 52.64$$ $$TN + FP = 3.29 \frac{0.70 \times (1 - 0.70)}{0.10^2} = 69.09$$

These values only return the number of true positive (*n* = 52.64) or true negative (*n* = 69.09) cases needed to obtain estimates of either sensitivity or specificity to the desired accuracy. In order to know how many people we need to actually sample, we need to consider the prevalence of the condition since the expected number of individuals in a sample who have the condition would be $N \times P$, where $P$ is the prevalence rate. Correspondingly, the number of individuals in a given sample who would be expected to be without the condition would be $N \times (1 - P)$. Note that this assumes a purely random sample, which for the sake of the hypothetical we can say was done. In reality, there are likely sampling biases from convenience samples where certain conditions of interest may be experienced at higher rates (e.g., recruiting from health clinics where individuals are likely presenting because they have symptoms). To calculate the final sample size needed, we can use the following equations:

$$N = \frac{TP + FN}{P} = \frac{52.64}{0.40} = 131.60$$ $$N = \frac{TN + FP}{1 - P} = \frac{69.09}{1 - 0.40} = 115.15$$

In this case, the number needed to get the desired accuracy of the test's sensitivity is larger than the sample size needed for specificity, so we will assume that the hypothetical study targetted this value for their final sample size. We will also be generous and say that the study recruited 150 individuals in the end. Using the testing sensitivity, specificity, and prevalence, we can create the confusion matrix that this study would have observed (shown in margin).

```{r}
#| tbl-cap: Hypothetical Confusion Matrix
#| column: margin

as.table(matrix(c(48, 12, 27, 63), nrow = 2, dimnames = list(c("True: +", "True: -"), c ("Test: +", "Test: -"))))
```

Running the numbers, these cell values return sensitivity = 0.80, specificity = 0.70, and accuracy = 0.74 as intended. The confusion matrix is needed for the likelihood specification because it allows us to specify a beta likelihood that is scaled to the amount of information obtained in the sample. Generally speaking, the more appropriate likelihood for this scenario may be a Bernoulli distribution as we are interested in the likelihood of individual dichotomous predictions (i.e., correct classification versus incorrect classification), but since this is for illustrative purposes, it is a bit simpler to use the same distributions for the prior and likelihood. With beta distributions, it is possible to specify the shape parameters as counts of the observed data. In this case, 48 + 63 = 111 individuals were correctly classified while 12 + 27 = 39 were not, so these values can be entered as the shape parameters to return a likelihood with a mean directly over the accuracy of the test (i.e., $\mu_{\alpha = 111, \beta = 39} = 0.74$) and a standard deviation relative to the sample size (in this case, the standard deviation of the beta distribution is about 0.04). This likelihood is shown below.

```{r}
#| fig-cap: Hypothetical Study Likelihood Distribution
#| column: page-inset

lik_plot <-
ggplot() +
  geom_function(
    fun = dbeta, 
    args = list(
      shape1 = 111, 
      shape2 = 39
      )
    ) +
  scale_x_continuous(
    limits = c(0, 1)
  ) +
  labs(
    x = "Test Accuracy",
    y = "Probability Density"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_blank(),
        axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14)
  )

lik_plot
```

Using this information, we can finally integrate the data together to see how the posterior distributions for these 4 researchers compare given that they all observe the same data but held differing prior beliefs. The figures below show the priors, likelihood, and subsequent posterior distributions side-by-side for comparison purposes.

```{r}
#| fig-cap: Combined Prior, Likelihood, and Posterior Distributions
#| column: page-inset
#| fig-width: 10
#| fig-height: 8.5

#make posterior plots
post_a <- 
  data.frame(x = seq(0, 1, length.out = 101)) %>%
    mutate(n = dbeta(x, 20, 5) * dbeta(x, 111, 39)) %>%
    mutate(d = integrate(function(x) dbeta(x, 20, 5) * dbeta(x, 111, 39), lower = 0, upper = 1)$value) %>%
    mutate(y = n/d) %>%
  ggplot(aes(x = x, y = y)) +
    geom_line() +
  scale_x_continuous(
    limits = c(0, 1)
  ) +
  labs(
    x = "Test Accuracy",
    y = "Probability Density"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_blank(),
        axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14)
  )

post_b <- 
  data.frame(x = seq(0, 1, length.out = 101)) %>%
    mutate(n = dbeta(x, 5, 20) * dbeta(x, 111, 39)) %>%
    mutate(d = integrate(function(x) dbeta(x, 5, 20) * dbeta(x, 111, 39), lower = 0, upper = 1)$value) %>%
    mutate(y = n/d) %>%
  ggplot(aes(x = x, y = y)) +
    geom_line() +
  scale_x_continuous(
    limits = c(0, 1)
  ) +
  labs(
    x = "Test Accuracy",
    y = "Probability Density"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_blank(),
        axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14)
  )

post_c <- 
  data.frame(x = seq(0, 1, length.out = 101)) %>%
    mutate(n = dbeta(x, 2, 2) * dbeta(x, 111, 39)) %>%
    mutate(d = integrate(function(x) dbeta(x, 2, 2) * dbeta(x, 111, 39), lower = 0, upper = 1)$value) %>%
    mutate(y = n/d) %>%
  ggplot(aes(x = x, y = y)) +
    geom_line() +
  scale_x_continuous(
    limits = c(0, 1)
  ) +
  labs(
    x = "Test Accuracy",
    y = "Probability Density"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_blank(),
        axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14)
  )

post_d <- 
  data.frame(x = seq(0, 1, length.out = 101)) %>%
    mutate(n = dbeta(x, 35, 35) * dbeta(x, 111, 39)) %>%
    mutate(d = integrate(function(x) dbeta(x, 35, 35) * dbeta(x, 111, 39), lower = 0, upper = 1)$value) %>%
    mutate(y = n/d) %>%
  ggplot(aes(x = x, y = y)) +
    geom_line() +
  scale_x_continuous(
    limits = c(0, 1)
  ) +
  labs(
    x = "Test Accuracy",
    y = "Probability Density"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_blank(),
        axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14)
  )

(prior_a / prior_b / prior_c / prior_d & labs(x = NULL, y = NULL)) | (lik_plot) | (post_a / post_b / post_c / post_d & labs(x = NULL, y = NULL))
```

Despite such differing initial beliefs, the four researchers' posterior beliefs are generally similar after observing the data. For context, the mean of the posteriors are at about 0.75, 0.67, 0.74, and 0.67 for Researcher A, B, C, and D, respectively. Thus, while the exact value of the test's accuracy may not be uniformly agreed upon, none leave the study concluding that the test is worse than chance and there is more overlap of plausible values than not. Recall as well that the expected value for the likelihood was 0.74, so the Bayesian results here do not disagree with the estimate that would have been obtained from maximum likelihood estimation either. This outcome highlights an important feature of priors in Bayesian analyses: as more data is observed, priors have less influence on the posterior distribution. Thus, when data are sufficiently large, objections to the use of priors become less relevant as their overall effect on the conclusions become increasingly minimal.

With this in mind, one may wonder whether priors are needed at all as maximum likelihood estimates are more well-researched and familiar to the general audience and seem to not differ much from the conclusions of Bayesianism. While there are many reasons to potentially prefer Bayesian estimation over maximum likelihood estimates, we will focus on just a few practical points. First, the subjectivity of Bayesian priors can actually be a compelling argument for statistical results. Consdier for example what happened with Researcher B's priors after observing the data: this hypothetical individual when from strongly believing that the test was worse than chance in its accuracy to believing that it was at least better than chance and likely somewhere around 70% accurate. The fact that the data provided such strong evidence against starting beliefs speaks to Bayesian methods' ability to summarize evidence both for and against a null or alternative hypothesis, rather than just the latter in the case of standard *p*-value estimates. Likewise, if a researcher begins with skeptical priors (where most of the prior probability is placed over nil or very small effects) but then concludes that effects are present and of some appreciable size, then this is hardly an objectionable research practice as the theory was tested against a pessimistic starting condition.

Another reason to prefer methods that incorporate priors is that most researchers have priors to begin with. If we consider maximum likelihood estimation, then the goal is to find any parameter value that maximizes the likelihood. Let's return to the case of trying to estimate a population's IQ from a sample. There is likely no researcher who would reasonably believe that the IQ of this population was -500 or 76359, yet the parameters for the normal distribution mean can range from $-\infty$ to $\infty$. Indeed, if a uniform (or flat) prior over all possible parameter values were specified for a Bayesian model, then the resulting posterior distribution just the likelihood function with a mode at the maximum likelihood estimate. As a result, the similarity of a frequentist and Bayesian statistical result becomes smaller as the prior distribution increasingly approximates this flat prior. Ideally, a prior distribution can exert some pressure on the statistical estimation to avoid highly improbable values. In this way, priors are sometimes treated as soft constraints on parameters where the specification constrains the model to only certain ranges of values but not strictly, meaning that if the data are compelling enough then the model can venture beyond the initially plausible values suggested by the prior. This modeling flexibility allows a broader range of statistical models to be identified in Bayesian frameworks compared to frequentist methods where hard constraints are needed to identify certain models. There is a significant difference between forcing a parameter value to being positive compared to strongly encouraging that a parameter remain positive with a prior. As Bayesian models can utilize priors to identify models without as strict of assumptions, they also have an important role in conducting analyses when sample sizes are smaller than traditionally acceptable for identifying certain kinds of models, though investigation of statistical conclusions' sensitivity to the priors needs to be done in cases where sample sizes are small and priors can potentially exert greater influence on the posterior.

Another benefit of Bayesian methods and priors that may be harder to recognize is the manner in which assumptions must necessarily be made transparent. Certainly, priors can be abused, but anticipating potential concerns or questions about appropriate prior specification necessarily encourages researchers to communicate in studies the priors they assumed. In comparison, traditional statistical methods have their own assumptions, but whether assumptions are tested, what assumption tests were used, and the results of all assumption tests are rarely reported in research. Bayesian methods also benefit from the absence of a closed mathematical form. Historically, Bayesian statistical estimation required use of conjugate priors, which are distributions that -- when multiplied by particular likelihood functions -- return a specific kind of posterior distribution whose parameters are able to be computed directly. For example, the conjugate prior for a normal likelihood is a normal distribution, which produces a normal distribution as the posterior. Since these posterior distributions have known formulas, the posterior can be described using the typical mathematical tools. The limitation of specifying prior beliefs within these constraints, however, is no longer there are new estimation methods can sample posterior distributions of arbitrary shape. While the strength of this is that prior distributions can take any form necessary to accurately reflect prior beliefs, the downside is that this model flexibility makes the posterior distribution's shape and form potentially unknowable, meaning that only sampling methods like HMC or MCMC can be used since there is no closed form mathematical summary upon which to perform calculus or other operations. Again, this is a minor weakness as modern estimation methods have become better and faster, but it also requires explicitly testing assumptions of the model rather than being able to apply mathematical reasoning to prove certain behaviors of the model (e.g., as is done with the Central Limit Theorem or in asymptotic sampling behavior). Much like having to be transparent about priors, the need to test whether a Bayesian model performs and behaves as intended requires that researchers communicate clearly about the modeling assumptions and the way in which the data generation process is conceptualized. Ideally, models will be calibrated using simulation studies where theoretical data are generated under the assumed data generation process and the model is run iteratively on these data to confirm that the model recovers the simulated data's behaviors. This latter case will be discussed in greater depth in the simulation section of this document.

::: callout-tip
## Recommended Readings and Resources for Bayesian Methods

The information documented throughout this section is a product of multiple references on Bayesian statistical methods. Rather than independently and repeatedly reference these materials, we have opted to include this general notification of the recommended materials for interested readers.

[**Articles**]{.underline}

-   The primer on Bayesian statistics published in *Nature Reviews* by [van de Schoot et al. (2021)](https://www.nature.com/articles/s43586-020-00001-2 "Bayesian statistics and modelling") is accessible and detailed for readers wanting to know exactly what goes into the Bayesian workflow and methods.

-   An introduction on Bayesian statistics by [Kruschke & Liddell (2018a)](https://link.springer.com/article/10.3758/s13423-017-1272-1 "Bayesian data analysis for newcomers") was published in *Psychonomic Bulletin & Review* and provides a beginner-friendly introduction and orientation to Bayesian methods.

-   Related to the reference above, [Kruschke & Liddell (2018b)](https://link.springer.com/article/10.3758/s13423-016-1221-4 "The Bayesian New Statistics") also published a description of how Bayesian statistics can be related to [Cumming (2014)](https://journals.sagepub.com/doi/10.1177/0956797613504966 "The New Statistics") proposed "New Statistics" in place of traditional *p*-values.

-   [Kruschke (2021)](https://www.nature.com/articles/s41562-021-01177-7 "Bayesian analysis reporting guidelines") has also recently provided new reporting guidelines for Bayesian models, which can help to highlight ways in which Bayesian methods promote greater scientific transparency than traditional methods.

-   For readers more interested in clinical applications of statistical methods, [Huygelier et al (2021)](https://www.cambridge.org/core/journals/journal-of-the-international-neuropsychological-society/article/abs/value-of-bayesian-methods-for-accurate-and-efficient-neuropsychological-assessment/126A463F31A670AACEAD12C09C85A702 "The value of Bayesian methods") have described how Bayesian statistics can be applied to and improve clinical neuropsychology practice.

[**Books**]{.underline}

-   The standard reference text in Bayesian methods is [Gelman et al.'s (2013) *Bayesian Data Analysis*](https://www.routledge.com/Bayesian-Data-Analysis/Gelman-Carlin-Stern-Dunson-Vehtari-Rubin/p/book/9781439840955), which is in its 3rd edition. The text details many of the mathematical details of Bayesianism and models, so it is among the more technical references recommended.

-   The spiritual update to *BDA3* is now [Gelman et al.'s (2020) *Regression and Other Stories*](https://www.cambridge.org/highereducation/books/regression-and-other-stories/DD20DD6C9057118581076E54E40C372C#overview), which integrates greater emphasis on applied data analysis in a Bayesian framework rather than the technical aspects.

-   A similarly applied and accessible text on Bayesian methods and model-based statistical inference is \[McElreath's (2020) *Statistical Rethinking*\] (https://www.cambridge.org/highereducation/books/regression-and-other-stories/DD20DD6C9057118581076E54E40C372C#overview), which is accompanied by recorded YouTube lectures for each chapter by the author.
:::

## Traditional Logistic Regression

With these statistical methods in mind, we now turn to the typical estimation of logistic regressions. In practicality, the logistic regression is a standard linear regression with a logistic transformation to rescale the linear predictions to be between 0 and 1, making the predictions interpretable as a probability. The likelihood function of the logistic regression is thus a Bernoulli distribution defined as follows:

$$\mathcal{L}_X = \prod_{i=1}^{N}\theta^{x_i}(1 - \theta)^{1 - x_i}$$

The probability distribution mass (since data are discrete instead of continuous) of the Bernoulli distribution is defined by a single parameter, $\theta$, which is the probability that $x_i = 1$. There are thus two outcomes for the likelihood. The first occurs when $x_i = 1$ as follows:

$$Pr(x_i = 1 | \theta) = \theta^1(1 - \theta)^{(1-1)}$$ $$= \theta(1 - \theta)^0$$ $$= \theta \times 1$$ $$= \theta$$

And then the second outcome is what occurs when $x_i = 0$, which is as follows:

$$Pr(x_i = 0 | \theta) = \theta^0(1 - \theta)^{(1-0)}$$ $$= 1 \times (1 - \theta)^1$$ $$= 1 - \theta$$

These outcomes should be relatively sensible, but this is limited to a use case where we care only about a single dichotomous outcome with a fixed probability of being a 1. More often, we want to compute individual probabilities of an outcome being 1, or to translate this into the formula, we want to go from $Pr(x_i | \theta)$ to $Pr(x_i | \theta_i)$. In a logistic regression, the linear predictor terms return $\theta_i$ based on the linear combination of those terms. As mentioned, however, when wanting to predict discrete data that can be only 0 or 1, the predictions of linear models are not useful. Applying some simple mathematical transformations, however, can allow conversion of the linear predictors to the probability scale. To begin with, we will use a simple linear regression model with an intercept and a single predictor:

$$\hat{y_i} = \beta_0 + \beta_xx_{i}$$

For now, we will ignore error and stick to just predicting the observation since logistic regressions have different residual structures than continuous regression models. Ultimately, we want the formula to predict probabilities, so we will change $\hat{y}$ to $\hat{p}$ as wishful thinking and work on making the rest of the equation becoming a true predictor of probability. There are two notable features of probabilities compared to continuous outcomes: they are strictly positive and they must have range from 0 to 1. One common transformation to keep variables positive is to exponentiation them as shown here:

$$\hat{p_i} = e^{\beta_0 + \beta_xx_i}$$

This simple transformation ensures that the resulting value will be positive, but it does not constrain the model to be between 0 and 1. In order to limit the range of the prediction, we need to specify some kind of ratio and do so in such a way that it cannot exceed 1. The simplest way of doing this would be to divide the equation by itself and then add 1 to the denominator, ensuring that the denominator is always larger than the numerator and thus the result is always between 0 and 1:

$$\hat{p_i} = \frac{e^{\beta_0 + \beta_xx_i}}{1 + e^{\beta_0 + \beta_xx_i}}$$

The equation now satisfies the requirements we have for a probability, so it is looking OK; however, the right hand side is non-linear. In order to produce a linear model, we need some way of reducing the model to just the intercept and predictors on one side. We can do this by changing our focus from estimating the probability to estimating the odds. It is easy to convert between probabilities and odds:

$$ p = \frac{odds}{1 + odds}$$ $$odds = \frac{p}{1 - p}$$

Using these relationships, we can see that what we have estimated so far uses the odds to estimate probability, so the equation clears up quite a bit when rearranged as this:

$$\frac{\hat{p_i}}{1 - \hat{p_i}} = e^{\beta_0 + \beta_xx_i}$$

In this arrangement, it is much easier to see that a simple additive linear relationship can be estimated. The final step needed is to take the (natural) logarithm of both sides so that the exponentiation on the right-hand side is dropped:

$$\ln(\frac{\hat{p_i}}{1 - \hat{p_i}}) = \beta_0 + \beta_xx_i$$

Since we have taken the logarithm of the odds, we are estimating the change in probability in terms of log-odds units, or logits. In other words, the linear regression equation produces an estimate of the log-odds of a individual being classified as 1 versus 0, and we can then go through the steps needed to convert this log-odd to a probability that is used in the Bernoulli likelihood distribution:

$$\ln(\frac{\hat{p_i}}{1 - \hat{p_i}}) = \beta_0 + \beta_xx_i$$ $$\frac{\hat{p_i}}{1 - \hat{p_i}} = e^{\beta_0 + \beta_xx_i}$$ $$\hat{p_i} = \frac{e^{\beta_0 + \beta_xx_i}}{1 + e^{\beta_0 + \beta_xx_i}}$$ $$y_i \sim {\sf Bernoulli}(\hat{p_i})$$ $$y_i \sim \hat{p_i}^{y_i}(1 - \hat{p_i})^{(1 - y_i)}$$

Note that we can read the $y_i \sim {\sf Bernoulli}(\hat{p_i})$ as "the observation of y for person i is distributed as a Bernoulli distribution with estimated probability of $\hat{p}$ that y is 1." We can extend this notation further to a complete Bayesian specification of the model by incorporating the prior distributions to the likelihood statement. Before doing this, though, it is convenient to define the inverse-logit function so that we can have fewer steps than we just went through from the logit to probability. The inverse logit function can be defined as $logit^{-1}(x) = \frac{e^x}{1 + e^x}$. With this defined, the Bayesian logistic regression model can be written as follows:

$$\beta_0 \sim N(0, 1)$$ $$\beta_x \sim N(0, 1)$$ $$\eta_i = \beta_0 + \beta_xx_i$$ $$y_i \sim {\sf Bernoulli}(logit^{-1}(\eta_i))$$

Here, the prior distributions for the regression parameters are specified as the standard normal distribution (i.e., a normal distribution with mean of 0 and standard deviation of 1). This scale of prior may be inappropriate for certain situations, but for this particular study, all variables were standardized before being modeled. As a result, these priors indicate that the most likely values for effects are 0 or something close to 0 but that, overall, we are about 68% confident that the true standardized log-odds ratio value will be somewhere between -1 and 1 and 98% confident that it will be between -2 and 2. To convert the standardized log-odds effects into the more familiar Cohen's *d* metric, we can use the following: $d \approx \frac{\ln(OR)\times\sqrt3}{\pi}$. Applying these transformations, the priors indicate that we expect nearly all (98%) coefficients to correspond to Cohen's *d* effect sizes between -1.10 and 1.10, which would correspond to large effects.

## Bias-Adjusted Logistic Regression

Having worked through the standard logistic regression, it is prudent to reflect on why any adjustment would be needed for the model. The logistic regression model is well-defined in cases where there is no error in the definition of $y_i$. If someone wants train a logistic regression model that will predict whether an object either an elephant or an airplane. There is limited reason to think that the initial classification of objects was subject to any error in this case as there is essentially no ambiguity between elephants and airplanes, so while the regression model may not be perfectly accurate, we are confident that the observed classifications being used to fit the regression are assigned without any error. When there is no error in the outcome variable, the likelihood statement is perfectly fine, but if there is error in some of those initial observations, then the likelihood statement becomes biased. This problem arises when these models are applied to prediction problems where there is no definitive way of classifying the outcome. For example, these models are often applied to clinical diagnoses that are made in absence of gold standard diagnostic tests being done. An example of this is in Alzheimer's disease research were there is no definitive way of saying with complete certainty whether a person had Alzheimer's disease or not as there is likely more of a spectrum of the dementia syndrome rather than a concrete yes or no state.

### Motivation for an Adjusted Model

To address the potential impact of using data with initial classification error, we can consider some common metrics of model performance and fit. We can begin with a fundamental measure of logistic regression performance called the deviance. Deviance is analogous to the sum of squared residuals in the case of continuous linear regression in that it is the difference between predicted and observed values and is minimized by regression terms. The deviance is related directly to the log-likelihood distribution of the model and its saturated form, and these log-likelihood distributions are usually multiplied by -2 as this transformation causes the deviance to become chi-square distributed, which allows for statistical inference. The following formulas define the deviance and log-likelihoods of the model-of-interest and saturated model:

$$D = -2\times[\ln(\mathcal{L_M}) - \ln(\mathcal{L_S})]$$ $$\ln(\mathcal{L_M}) = \sum_{i = 1}^Ny_i\times\ln(\hat{p_i})+(1 - y_i)\times\ln(1-\hat{p_i})$$ $$\ln(\mathcal{L_S}) = \sum_{i = 1}^Ny_i\times\ln(\hat{y_i})+(1 - y_i)\times\ln(1-\hat{y_i})$$

There are two points to consider with the deviance as it relates to the observed classifications used in the model, $y_i$. The first is that the the likelihood of the model, $\mathcal{L_M}$, is computed based on whether $y_i$ is 0 or 1, meaning that, if a given $y_i = 0$ when it should be $y_i = 1$, then the log-likelihood of the result will be biased toward the inaccurate assignments, which will limit the model's generalizability. Recall that the minimization of the deviance is how the logistic regression coefficients are found -- just like how the linear regression line is the line that minimizes the residuals. If the observed data are wrong, then the coefficients minimize the deviance of those error-in-variable outcomes rather than true outcomes, so the resulting coefficients may not be accurate to those needed for the true outcome to begin with. As such, if there is interest in developing a model that will improve upon some diagnostic method, then using the error-in-variable outcomes will penalize any model that could be making predictions that are more accurate to the true classification but also inherently less accurate to the observed outcome.

The second consideration is in the specification of the likelihood for the saturated model, $\mathcal{L_S}$, which is used in the computation of deviance. In this model likelihood, the entire probability statement depends on the observed variables, so when there is error in this variable, the saturated model's likelihood will be biased toward the error-in-variable outcome compared to the true observation value. As mentioned, the deviance is a minimization function for the coefficients, meaning that ignoring this problem can fail to return meaningful or accurate effect estimates.

There are several goodness of fit indicators that are also impacted by the potential bias of favoring prediction of error-in-variable outcomes. Consider Pearson's chi-square goodness of fit test:

$$\chi^2 = \sum_{i = 1}^J\frac{(O_j - E_j)^2}{E_j}$$

Here, the chi-square test is based on cells of a 2-way contingency matrix with each row J corresponding to a particular profile/binning and then a column corresponding to the observed 0 or 1. In this case, when the model has the potential to predict more accurate classifications than those observed, that estimation is penalized such that a model that does better than the initial data generation in classifying correct outcomes could be determined as having poor model fit given this test. We can see that a similar issue would arise with the Hosmer-Lemeshow goodness of fit test as well:

$$\chi^2 = \sum_{i = 1}^J\frac{(O_j - E_j)^2}{E_j\times\frac{1 - E_j}{n_j}}$$

### The Adjustment

Fortunately, the area of diagnostic medicine has provided multiple ways for thinking about and quantifying the probability of misclassification. A model that can utilize these statistics, and potentially estimate them in the process, thus seems like a simple and direct solution to the issue of error-in-variable outcomes. Specifically, the sensitivity and specificity are the metrics of interest as they convey directly the probability of true positives and negatives, and they have the benefit of not being dependent on prevalence like positive and negative predictive values are. Specifically, we can define sensitivity and specificity in terms of true and false positives and negatives as follows:

$$\sf Sensitivity = \frac{TP}{TP + FN}$$ $$\sf Specificity = \frac{TN}{TN + FP}$$

Another way of thinking about sensitivity and specificity is as the true positive and negative rates, respectively. Specifically, the sensitivity is the proportion of true positive from all individuals with the condition of interest (i.e., the sum of true positive and false negatives), and the same is true of the specificity but for the proportion of true negatives. Using this information, it is possible to treat the observed values as mixtures of accurate and inaccurate classifications relative to the diagnostic method's sensitivity and specificity. The mixture model treats all observed outcomes of 1 as a combination of true and false positives while all observed outcomes of 0 are combinations of true and false negatives. This mixture is decomposed into a probability of a 1 given the test's sensitivity (or a 0 given its specificity) and then a logistic regression model predicting whether a 0 or 1 is more likely given the regression equation. Another way to think of a mixture model is to treat the true classification of a case as a discrete latent variable (either a 0 or 1) that must be estimated from a noisy indicator (the observed 0 or 1 classification). The goal of the mixture model then is to use the observed but noisy information to identify the unobserved (or latent), true classification. In Stan, the HMC estimator requires that the gradient of the joint probability distribution is continuous, meaning that discrete parameter values cannot be directly estimated; instead, Stan requires that these parameter values are marginalized out of the model so that only continuous probabilities remain in the joint probability space.

To put this in terms of the desired likelihood, we have the following:

$$\mathcal{L}_{Y=1} = \prod_{i = 1}^N[{\sf Bernoulli}(y_i | {\sf Sensitivity})\times{\sf Bernoulli}(1 | logit^{-1}(\eta_i))]$$ $$\mathcal{L}_{Y=0} = \prod_{i = 1}^N[{\sf Bernoulli}(1-y_i | {\sf Specificity})\times{\sf Bernoulli}(0 | logit^{-1}(\eta_i))]$$

We thus estimate for each individual, a likelihood that the true value is either a 0 or 1 as a function of sensitivity/specificity and the logistic regression prediction. In plain language, the likelihood that the true classification is a 1 is given by the probability of the observed value given the sensitivity of the initial diagnostic method times the probability of a 1 given the logistic regression equation. The same general relationships are true for the case of a true diagnosis of 0, but now it is the probability of a negative result given the specificity and the logistic regression equation. THe combined log-likelihood is thus:

$$\ln(\mathcal{L}_Y) = \sum_{i=1}^N \ln(\exp({\ln({\sf Bernoulli}(y_i | {\sf Sensitivity})) + \ln({\sf Bernoulli}(1 | logit^{-1}(\eta_i))) + \ln({\sf Bernoulli}(1-y_i | {\sf Specificity})) + \ln({\sf Bernoulli}(0 | logit^{-1}(\eta_i)))}))$$

For readability, the exponentiation is written as the function $exp(x)$ instead of $e^x$. The mixture model allows for estimation of the logistic equation in the presence of initial observation error via sensitivity and specificity of the diagnostic method. The issue, however, is that not every method has a known sensitivity or specificity, and in cases where there is no definitive diagnostic test, then sensitivity and specificity can't be formally be determined. As a result, the model also needs a way of estimating sensitivity and specificity. While it is possible to do this, there must be some constraint placed on the estimation for the model to be identified. The constraint made for this model is in the assumption that the diagnostic method is at least better than chance at the initial classification. This is specified in a constraint on the classification error with the false negative rate ($\theta$) and false positive rate ($\phi$) such that $\theta + \phi < 1$. This can be specified with a Dirichlet prior over the error terms. The Dirichlet distribution returns a simplex, which is a vector that sums to 1, so requesting a simplex of length 3 guarantees that the sum of just two of these terms will be less than one. These terms can then be converted to sensitivity, $1 - \theta$, and specificity, $1 - \phi$. Beyond these modifications, the model's specification remains the same as the traditional logistic regression.

# Overview of Simulation Study

Simulation studies serve a valuable purpose in statistical research as they give an opportunity to empirically test how statistics behave. They also serve an important role in Bayesian statistics as they can be used to calibrate models and verify that the data generation process is captured intended. For this reason, details on the simulation process are important to provide additional details.

## Goals of Simulation Studies

The goals of most simulation studies are to demonstrate that a statistical model is reliable in terms its ability to converge and also reliably recover parameters of interest. In the real world, parameters are never known, so the ability of a model to accurately estimate the parameter value has to be implied from mathematical details. A simulation study allows data to be generated with known parameters so that model estimates can be compared directly to those parameter values. Simulation studies can focus on other aspects of statistical models, but paramter recovery is the focus of this one.

Among the recovery statistics, there are three that focus on point estimate accuracy of the parameters and one on the quality of the 95% credible intervals. The three accuracy estimates center around estimate bias. Bias is computed as the average difference between the estimated and true value:

$$bias = \frac{\sum_{i=1}^S(\hat{\theta_s} - \theta)}{N_{s}}$$

The formula for bias is thus not dissimilar from any other mean with the exception that the average is being taken over the iterations of the simulation study rather than the size of a sample. The challenge of bias, however, is that the signs of estimates can cancel each other out. For example, if most estimates tend to be slightly greater than the true parameter value but a few estimates are much smaller, then these absolute differences may be missed. One solution to this issue is to adopt a method similar to that of the standard deviation where the differences are squared so that the sum of differences is always positive. Of course, squaring the numerator would exaggerate differences, so the square root is then taken of the mean in order to return the scale back to the raw data. This process gives the root mean square error (RMSE):

$$RMSE = \sqrt{\frac{\sum_{i=1}^S(\hat{\theta_s} - \theta)^2}{N_s}}$$

While the RMSE can be interpreted as the standard deviation of the point estimates, it can exaggerate the most extreme differences. For example, consider a scenario where the estimate is 4 points higher than the true value and another that is 8 point higher. When these differences are squared, they become 16 and 64, respectively, so instead of the difference between the two being a scale of 2 it is now a scale of 4. One way of avoiding this exaggeration of the most extreme differences is to take the mean of the absolute difference, or the mean absolute error (MAE):

$$MAE = \frac{\sum_{i=1}^S| \hat{\theta_s} - \theta |}{N_{s}}$$

The combination of bias, RMSE, and MAE can help to understand how point estimates of parameter values behave on average. Statistical summaries, however, often also accompany confidence intervals summarizing likely values. In Bayesian statistics, the confidence interval is called a credible interval as it provides an interval of values that are credible given the prior and likelihood. In the case of a 95% confidence interval, the values are all those that would not be rejected as significantly different ($\alpha = 0.05$) from the point estimate. The 95% Bayesian credible interval gives the range of values over which there is a 0.95 probability that the true parameter value is. The performance of these intervals can be formally tested in simulation studies via the empirical coverage rate (ECR), which is the proportion of true parameter values that are within the credible interval. When a 95% credible interval is used, the ECR should be around 0.95.

Simulation studies also have an important role in Bayesian model construction. In model-based statistical inference, the goal is to specify a model that captures a simplified data generation process. The data generation process refers to the various systems and factors that theory informs as being important in explaining data that is observed in researcher -- in other words, it is the variables and relationships of those variables that give rise to the data. A well-specified model would include these same variables and relationships and thus produce predictions that appear similar to and match observed data. In simulation studies, this means that data can be generated under a specific process and the model can be checked to ensure that it is calibrated correctly.

## Simulation Methods

Simulations work by specifying a specific data generation process with known parameter values and then varying these conditions to see how they affect model results. In this case, there is a need to simulate data according to a logistic regression model as this ensures that the simulated data follow a specific logistic regression. There also needs to be a way of introducing classification error based on sensitivity and specificity. This study uses a series of custom functions to generate data, run models, and extract results. The core of the data generation is reviewed in this section.

The first step of generating the data is to simulate a set of predictor values and their corresponding coefficients. In the `simDat()` function used to simulate the data (see R script files), the following lines correspond to this step:

```{r echo=TRUE, eval=FALSE}
# generate matrix of predictor data: N (sample size) x P (predictors)
x <- matrix(rnorm(N * P, mean = 0, sd = 1), nrow = N, ncol = P)

# generate logistic regression parameters
b_0 <- rnorm(1, mean = 0, sd = 0.25)       # intercept
b_x <- rnorm(P, mean = 1, sd = 0.25)       # coefficients

# since coefficients favor large effects, make ~1/4 negative
if ( P > 1 )
  b_x[seq.int(2, P, 4)] <- -1 * b_x[seq.int(2, P, 4)]
```

The first line generates a matrix with N rows corresponding to the simulation data's number of observations and P columns for the number of predictors. These values are drawn randomly from a normal distribution with a mean of 0 and standard deviation of 1, corresponding to fully standardized predictor values. The next two lines draw random values from normal distributions for the intercept and regression coefficients. For the intercept, a single value from a normal distribution with mean of 0 and standard deviation of 0.25 was used for each iteration. A similar process was used for each of the regression coefficients except that these used normal distributions with means of 1 and standard deviations of 0.25, meaning that the true parameter values used for each iteration were most likely to be of a large effect size. Since it was unlikely under these conditions for a coefficient to be assigned a negative value, some subset of values were selected at random to be negative. This is accomplished in the final line of code shown where one in every four coefficients were forced to be negative.

With a matrix of covariates and the coefficients for these predictors, a logistic regression model could then be used to generate binary classifications. This is conducted over a loop so that each individual gets their own probability of a 1 class and then their own realization of this probability via the binomial distribution, `rbinom()`. In a case where there is no error at all from the logistic regression model, the probabilities could be rounded directly to either 0 or 1, but using the random binomial observation generation permits incorporation of sampling error as well since the assignment of 0 and 1 is done probabilistically in this case. These steps are taken in the next lines of code:

```{r echo=TRUE, eval=FALSE}
# use logistic regression to create true observations and probabilities
y <- rep(0, N)
pr <- rep(0, N)

for ( n in 1 : N ) {
  pr[n] <- 1 / (1 + exp(-1 * (b_0 + (x[n, ] %*% b_x))))
  y[n] <- rbinom(1, 1, pr[n])
}
```

The application of a logistic model gives the true classifications plus some probabilistic error, but the interest of this simulation study is to incorporate more systematic error in the observations. To do this, the simulation is given a sensitivity and specificity to systematically readjust observations. The following code conditionally assigns each case a new 0 or 1 based on the true observation, so if the true value is 1, then sensitivity is used for the probability of 1 (true positive) versus 0 (false negative) and then when the true value is 0 then the same is done but with specificity.

```{r echo=TRUE, eval=FALSE}
# use sensitivity and specificity to create misdiagnoses
obs <- rep(0, N)

for ( n in 1 : N )
  obs[n] <- rbinom(1, 1, ifelse(y[n], Sn, 1 - Sp))
```

Simulation studies work by varying inputs in the data generation process and then running that process many times. For this study, all conditions were run for 1000 iterations, and the data generation variables that were adjusted across conditions were the number of predictors, sample size, and sensitivity and specificity. The other major contribution of simulation studies is to capitalize on probabilistic variability as this can mirror sampling error. In addition to classification probability through the `rbinom()` function, variation was also added to sensitivity and specificity so that the mean of each condition was entered but each individual iteration within conditions had some random deviation from this mean.

## Simulation Results

The detailed results of the simulation study are described in this section. Primarily, these results are shared via plots as they effectively summarize the variability of both the simulation conditions and the estimation results.

### Sensitivity and Specificity

Only the adjusted model estimates sensitivity and specificity, so the results are shown for this model alone. Plotted alongside the model's estimates of sensitivity and specificity are the sensitivities and specificities of each iteration in the condition.

```{r}
#| fig-cap: Sensitivity Estimates
#| column: page-inset
#| fig-width: 15
#| fig-height: 10

sns_spc_comb %>%
  mutate(SampleSizeN = as.numeric(SampleSize)*2,
         SensitivityN = as.numeric(levels(Sensitivity))[Sensitivity]) %>%
  ggplot() +
  stat_eye(
    aes(x = SampleSizeN, y = sens_dsm, group = SampleSize, fill = 'Model-based Estimate'),
    side = "right",
    width = 1,
    point_interval = NULL,
    justification = -0.15,
    point_color = NA
  ) +
  geom_boxplot(
    aes(x = SampleSizeN, y = sens_dsm, group = SampleSize),
    width = 0.20,
    outlier.shape = NA,
    position = position_nudge(0.15),
    col = RColorBrewer::brewer.pal(3, "Dark2")[2],
    size = 1
  ) +
  stat_eye(
    aes(x = SampleSizeN, y = sens_cond, group = SampleSize, fill = 'Simulation Condition'),
    side = "left",
    width = 1,
    point_interval = NULL,
    justification = 1.15,
    point_color = NA
  ) +
  geom_boxplot(
    aes(x = SampleSizeN, y = sens_cond, group = SampleSize),
    width = 0.20,
    outlier.shape = NA,
    position = position_nudge(-0.15),
    col = RColorBrewer::brewer.pal(3, "Dark2")[1],
    size = 1
  ) +
  geom_hline(
    aes(yintercept = SensitivityN),
    linetype = 2,
    size = 0.5
  ) +
  scale_x_continuous(
    breaks = c(2, 4, 6, 8), labels = levels(sns_spc_comb$SampleSize),
    name = "Sample Size",
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    n.breaks = 5
  ) +
  scale_fill_manual(
    name='Source of Sensitivity Values',
    breaks=c('Simulation Condition', 'Model-based Estimate'),
    values=c('Simulation Condition' = RColorBrewer::brewer.pal(3, "Set2")[1],
             'Model-based Estimate' = RColorBrewer::brewer.pal(3, "Set2")[2])
  ) +
  labs(
    x = "Sample Size",
    y = "Sensitivity",
    title = "Estimate Accuracy for Sensitivity"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.background = element_rect(fill = "darkgrey", linetype = "solid", size = 1.5),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    strip.text = element_text(family = "serif", face = "bold", size = 14, color = "white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    fill = guide_legend(title = "Source of Sensitivity Values", title.position = "top", title.hjust = 0.5,
                        title.theme = element_text(family = "serif", size = 14))
  ) +
  facet_grid(
    Parameters ~ Sensitivity,
    labeller = labeller(
      Parameters = labs_Pr,
      Sensitivity = labs_Sn
    ))
```
```{r}
#| fig-cap: Specificity Estimates
#| column: page-inset
#| fig-width: 15
#| fig-height: 10

sns_spc_comb %>%
  mutate(SampleSizeN = as.numeric(SampleSize)*2,
         SpecificityN = as.numeric(levels(Specificity))[Specificity]) %>%
  ggplot() +
  stat_eye(
    aes(x = SampleSizeN, y = spec_dsm, group = SampleSize, fill = 'Model-based Estimate'),
    side = "right",
    width = 1,
    point_interval = NULL,
    justification = -0.15,
    point_color = NA
  ) +
  geom_boxplot(
    aes(x = SampleSizeN, y = spec_dsm, group = SampleSize),
    width = 0.20,
    outlier.shape = NA,
    position = position_nudge(0.15),
    col = RColorBrewer::brewer.pal(3, "Dark2")[2],
    size = 1
  ) +
  stat_eye(
    aes(x = SampleSizeN, y = spec_cond, group = SampleSize, fill = 'Simulation Condition'),
    side = "left",
    width = 1,
    point_interval = NULL,
    justification = 1.15,
    point_colour = NA,
    fill = RColorBrewer::brewer.pal(3, "Dark2")[1]
  ) +
  geom_boxplot(
    aes(x = SampleSizeN, y = spec_cond, group = SampleSize),
    width = 0.20,
    outlier.shape = NA,
    position = position_nudge(-0.15),
    col = RColorBrewer::brewer.pal(3, "Dark2")[1],
    size = 1
  ) +
  geom_hline(
    aes(yintercept = SpecificityN),
    linetype = 2,
    size = 0.5
  ) +
  scale_x_continuous(
    breaks = c(2, 4, 6, 8), labels = levels(sns_spc_comb$SampleSize),
    name = "Sample Size",
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    n.breaks = 5
  ) +
  scale_fill_manual(
    name='Source of Specificity Values',
    breaks=c('Simulation Condition', 'Model-based Estimate'),
    values=c('Simulation Condition' = RColorBrewer::brewer.pal(3, "Set2")[1],
             'Model-based Estimate' = RColorBrewer::brewer.pal(3, "Set2")[2])
  ) +
  labs(
    x = "Sample Size",
    y = "Specificity",
    title = "Estimate Accuracy for Specificity"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.background = element_rect(fill = "darkgrey", linetype = "solid", size = 1.5),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    strip.text = element_text(family = "serif", face = "bold", size = 14, color = "white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    fill = guide_legend(title = "Source of Specificity Values", title.position = "top", title.hjust = 0.5,
                        title.theme = element_text(family = "serif", size = 14))
  ) +
  facet_grid(
    Parameters ~ Specificity,
    labeller = labeller(
      Parameters = labs_Pr,
      Specificity = labs_Sp
    ))
```

In these plots, the various conditions varying the sensitivity/specificity (plot columns), sample size (x-axis), and number of predictors (plot rows) are shown together so that it is visually easier to track the relative influence of various factors on estimation. Additionally, a horizontal dashed line at the conditions' central sensitivity/specificity is plotted to help evaluate how well the model estimates converge on the true values. A density plot and corresponding box-and-whiskers plot are then provided to visualize the variability of estimation. Overall, the model is able to accurately recover sensitivity and specificity except in cases where there are few predictors while more precise/less variable estimates are obtained at larger sample sizes.

### Classification Accuracy and Area Under the Curve

Since both models concern the prediction of true diagnostic status, the next plots correspond to comparisons of the traditional and adjusted logistic regression models. For contextualization of the results, expected accuracies and areas under the curve (AUCs) were computed from the simulation parameters. For accuracy, a combination of sensitivity, specificity, and prevalence from the iteration were used to generate the expected level of accuracy that a method could obtain. With the AUC, the probabilities estimated from the true logistic regression were used. These plots are provided below:

```{r}
#| fig-cap: Diagnostic Accuracy Estimates
#| column: page-inset
#| fig-width: 15
#| fig-height: 10

acc_auc_comb %>%
  mutate(SampleSizeN = as.numeric(SampleSize)*2) %>%
  group_by(SampleSize, Parameters) %>%
  mutate(mEA = mean(acc_tru_con)) %>%
  ungroup() %>%
  pivot_longer(cols = acc_tru_con:acc_dsm_tru) %>%
  mutate(name = factor(name, levels = c("acc_dsm_tru", "acc_log_tru", "acc_tru_con"),
                       labels = c("Adjusted Logistic", "Unadjusted Logistic", "Expected Accuracy"))) %>%
  ggplot() +
  stat_eye(
    aes(x = value, y = name, group = SampleSize, fill = name),
    side = "right",
    width = 0.6,
    point_interval = "mean_qi",
    alpha = 0.75
  ) +
  geom_vline(
    aes(xintercept = mEA),
    linetype = 2,
    size = 0.5
  ) +
  scale_x_continuous(
    n.breaks = 5
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_fill_discrete(breaks = c("Expected Accuracy", "Unadjusted Logistic", "Adjusted Logistic")) +
  labs(
    x = "Accuracy",
    y = NULL,
    title = "Obtained Diagnostic Accuracy from Simulations"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.background = element_rect(fill = "darkgrey", linetype = "solid", size = 1.5),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text.x = element_text(family = "serif", face = "bold", size = 12),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    strip.text = element_text(family = "serif", face = "bold", size = 14, color = "white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    fill = guide_legend(title = "Model Used for Classification", title.position = "top", title.hjust = 0.5,
                        title.theme = element_text(family = "serif", size = 14),
                        override.aes = list(linetype = 0, shape = NA))
  ) +
  facet_grid(
    SampleSize~ Parameters,
    labeller = labeller(
      SampleSize = labs_Ss,
      Parameters = labs_Pr
    ))
```
```{r}
#| fig-cap: Area Under the Curve Estimates
#| column: page-inset
#| fig-width: 15
#| fig-height: 10

acc_auc_comb %>%
  mutate(SampleSizeN = as.numeric(SampleSize)*2) %>%
  group_by(SampleSize, Parameters) %>%
  mutate(mEA = mean(auc_tru_tru)) %>%
  ungroup() %>%
  pivot_longer(cols = auc_tru_tru:auc_dsm_tru) %>%
  mutate(name = factor(name, levels = c("auc_dsm_tru", "auc_log_tru", "auc_tru_tru"),
                       labels = c("Adjusted Logistic", "Unadjusted Logistic", "Expected AUROC"))) %>%
  ggplot() + aes(mEA) +
  geom_vline(
    aes(xintercept = mEA),
    linetype = 2,
    size = 0.5
  ) +
  stat_eye(
    aes(x = value, y = name, group = SampleSize, fill = name),
    side = "right",
    width = 0.6,
    point_interval = "mean_qi",
    alpha = 0.75
  ) +
  scale_x_continuous(
    n.breaks = 5
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_fill_discrete(breaks = c("Expected AUROC", "Unadjusted Logistic", "Adjusted Logistic")) +
  labs(
    x = "Area Under Receiver Operator Characteristic Curve",
    y = NULL,
    title = "Obtained Areas Under the Curve from Simulations"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.background = element_rect(fill = "darkgrey", linetype = "solid", size = 1.5),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text.x = element_text(family = "serif", face = "bold", size = 12),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    strip.text = element_text(family = "serif", face = "bold", size = 14, color = "white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    fill = guide_legend(title = "Model Used for Classification", title.position = "top", title.hjust = 0.5,
                        title.theme = element_text(family = "serif", size = 14),
                        override.aes = list(linetype = 0, shape = NA))
  ) +
  facet_grid(
    SampleSize~ Parameters,
    labeller = labeller(
      SampleSize = labs_Ss,
      Parameters = labs_Pr
    ))
```

The adjusted logistic regression obtains greater accuracy than would be expected if limited to just the accuracy implied by sensitivity and specificity, which supports the fact that the model is able to adjust predictions to true values despite having error-in-observation values to begin with. In the case of AUC, however, it appears that there is less preference for the adjusted model. With larger sample sizes, the adjusted model does best, but it should also be noted that most of the AUC is wasted as any method better than chance will have an AUC greater than 0.50. As a result, most of the left-hand space of these graphs is given and thus exaggerates what appears to be minimal differences in AUCs. A solution to this problem is a partial AUC wherein the area is computed only for parts of the receiver operator characteristic curve (ROC) where some threshold is exceeded. For example, if one wanted to prefer sensitivity to specificity and was only concerned with the accuracy of a model when sensitivity is at least 0.70, then the partial AUC can be computed. Since there was no reason to prefer sensitivity or specificity in the simulation, the partial AUC was not computed even though it might help to capture true differences in the AUC of the models.

### Recovery of Parameter Values

Having demonstrated that the adjusted logistic regression model improves the accuracy of predicting true classifications, it is natural to ask whether the adjusted model has any implications for statistical inference in these models. Since it is clear that the adjusted model produces different predictions, it is also clear that the parameter estimates will vary between the two models as well. Since the true parameter values used to generate the data are known, we can compare the models' abilities to accurately estimate these values. We begin this with the RMSE and MAE of the model intercepts:

```{r}
#| fig-cap: Intercept Accuracy Estimates
#| column: page-inset
#| fig-width: 7

Rec_res %>%
  mutate(SampleSizeN = as.numeric(SampleSize)*2) %>%
  ggplot() +
  geom_point(
    aes(x = SampleSizeN, y = log_int_rms, color = 'Unadjusted Logistic', shape = 'Root Mean Square Error'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = log_int_rms, color = 'Unadjusted Logistic', linetype = 'Root Mean Square Error'),
    size = 1.15
  ) +
  geom_point(
    aes(x = SampleSizeN, y = log_int_mae, color = 'Unadjusted Logistic', shape = 'Mean Absolute Error'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = log_int_mae, color = 'Unadjusted Logistic', linetype = 'Mean Absolute Error'),
    size = 1.15
  ) +
  geom_point(
    aes(x = SampleSizeN, y = dsm_int_rms, color = 'Adjusted Logistic', shape = 'Root Mean Square Error'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = dsm_int_rms, color = 'Adjusted Logistic', linetype = 'Root Mean Square Error'),
    size = 1.15
  ) +
  geom_point(
    aes(x = SampleSizeN, y = dsm_int_mae, color = 'Adjusted Logistic', shape = 'Mean Absolute Error'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = dsm_int_mae, color = 'Adjusted Logistic', linetype = 'Mean Absolute Error'),
    size = 1.15
  ) +
  scale_x_continuous(
    breaks = c(2, 4, 6, 8), labels = levels(Rec_res$SampleSize),
    name = "Sample Size",
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    n.breaks = 10
  ) +
  scale_color_manual(
    name='Model',
    breaks=c('Unadjusted Logistic', 'Adjusted Logistic'),
    values=c('Unadjusted Logistic' = RColorBrewer::brewer.pal(3, "Set2")[1],
             'Adjusted Logistic' = RColorBrewer::brewer.pal(3, "Set2")[2])
  ) +
  scale_linetype_manual(
    name = 'Error Type',
    breaks = c('Root Mean Square Error', 'Mean Absolute Error'),
    values = c('Root Mean Square Error' = 1,
               'Mean Absolute Error' = 2)
  ) +
  scale_shape_manual(
    name = 'Error Type',
    breaks = c('Root Mean Square Error', 'Mean Absolute Error'),
    values = c('Root Mean Square Error' = 16,
               'Mean Absolute Error' = 18)
  ) +
  labs(
    x = "Sample Size",
    y = "Error",
    title = "Intercepts: Root-Square and Mean Absolute Error"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.spacing.x = unit(1.25, "lines"),
    strip.background = element_rect(fill = "darkgrey", linetype = "solid", size = 1.5),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    strip.text = element_text(family = "serif", face = "bold", size = 14, color = "white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    color = guide_legend(title = "Model", title.position = "top", title.hjust = 0.5,
                         title.theme = element_text(family = "serif", size = 14),
                         override.aes = list(shape = NA)),
    shape = guide_legend(title = "Error Type", title.position = "top", title.hjust = 0.5,
                         title.theme = element_text(family = "serif", size = 14)),
    linetype = guide_legend(title = "Error Type", title.position = "top", title.hjust = 0.5,
                         title.theme = element_text(family = "serif", size = 14))
  ) +
  facet_grid(
    ~ Parameters
  )
```

The adjusted logistic regression model prodcued less error in the estimates of the intercepts. The difference between the RMSE and MAE helps to indicate the extent to which extreme estimates occur since these values are squared first whereas the MAE takes just the absolute value of the difference. The fact that RMSE values are greater than MAE values is not surprising, and there is not a clear pattern to suggest that there are circumstances where extreme values are more common for either model.

The intercept is, of course, just one part of the logistic regression with the more influential part being the regression coefficient. This is particularly true in this simulation study as the intercepts all centered 0 and had very little probability of being particularly large effects. In contrast, the coefficients all had means of 1, meaning that they were more likely to have large effects.

```{r}
#| fig-cap: Coefficient Accuracy Estimates
#| column: page-inset
#| fig-width: 7

Rec_res %>%
  mutate(SampleSizeN = as.numeric(SampleSize)*2) %>%
  ggplot() +
  geom_point(
    aes(x = SampleSizeN, y = log_co1_rms, color = 'Unadjusted Logistic', shape = 'Root Mean Square Error'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = log_co1_rms, color = 'Unadjusted Logistic', linetype = 'Root Mean Square Error'),
    size = 1.15
  ) +
  geom_point(
    aes(x = SampleSizeN, y = log_co1_mae, color = 'Unadjusted Logistic', shape = 'Mean Absolute Error'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = log_co1_mae, color = 'Unadjusted Logistic', linetype = 'Mean Absolute Error'),
    size = 1.15
  ) +
  geom_point(
    aes(x = SampleSizeN, y = dsm_co1_rms, color = 'Adjusted Logistic', shape = 'Root Mean Square Error'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = dsm_co1_rms, color = 'Adjusted Logistic', linetype = 'Root Mean Square Error'),
    size = 1.15
  ) +
  geom_point(
    aes(x = SampleSizeN, y = dsm_co1_mae, color = 'Adjusted Logistic', shape = 'Mean Absolute Error'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = dsm_co1_mae, color = 'Adjusted Logistic', linetype = 'Mean Absolute Error'),
    size = 1.15
  ) +
  scale_x_continuous(
    breaks = c(2, 4, 6, 8), labels = levels(Rec_res$SampleSize),
    name = "Sample Size",
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    n.breaks = 10
  ) +
  scale_color_manual(
    name='Model',
    breaks=c('Unadjusted Logistic', 'Adjusted Logistic'),
    values=c('Unadjusted Logistic' = RColorBrewer::brewer.pal(3, "Set2")[1],
             'Adjusted Logistic' = RColorBrewer::brewer.pal(3, "Set2")[2])
  ) +
  scale_linetype_manual(
    name = 'Error Type',
    breaks = c('Root Mean Square Error', 'Mean Absolute Error'),
    values = c('Root Mean Square Error' = 1,
               'Mean Absolute Error' = 2)
  ) +
  scale_shape_manual(
    name = 'Error Type',
    breaks = c('Root Mean Square Error', 'Mean Absolute Error'),
    values = c('Root Mean Square Error' = 16,
               'Mean Absolute Error' = 18)
  ) +
  labs(
    x = "Sample Size",
    y = "Error",
    title = "Coefficients: Root-Square and Mean Absolute Error"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.spacing.x = unit(1.25, "lines"),
    strip.background = element_rect(fill = "darkgrey", linetype = "solid", size = 1.5),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    strip.text = element_text(family = "serif", face = "bold", size = 14, color = "white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    color = guide_legend(title = "Model", title.position = "top", title.hjust = 0.5,
                         title.theme = element_text(family = "serif", size = 14),
                         override.aes = list(shape = NA)),
    shape = guide_legend(title = "Error Type", title.position = "top", title.hjust = 0.5,
                         title.theme = element_text(family = "serif", size = 14)),
    linetype = guide_legend(title = "Error Type", title.position = "top", title.hjust = 0.5,
                            title.theme = element_text(family = "serif", size = 14))
  ) +
  facet_grid(
    ~ Parameters
  )
```

As was observed with the intercepts as well, the adjusted model produces more accurate estimates of the regression coefficients; however, unlike the intercepts, the difference between the two models' estimate accuracy is pronounced. There is also a notable pattern as sample size increases where the adjusted model becomes increasingly accurate while there is limited change in estimation accuracy for the unadjusted model. Additionally, there is a tendency for there to be more extreme estimates when sample size is 100 for both models as indicated by the greater RMSE to MAE difference at that sample size only.

The final estimation accuracy factor to consider is the ECR for the 95% credible intervals. The following plot is the 95% ECR for the unadjusted and adjusted logistic regressions for both the intercepts and coefficients.

```{r}
#| fig-cap: Empirical Coverage Rates
#| column: page-inset
#| fig-width: 7

Rec_res %>%
  mutate(SampleSizeN = as.numeric(SampleSize)*2) %>%
  ggplot() +
  geom_point(
    aes(x = SampleSizeN, y = log_int_ecr, color = 'Unadjusted Logistic', shape = 'Intercept'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = log_int_ecr, color = 'Unadjusted Logistic', linetype = 'Intercept'),
    size = 1.15
  ) +
  geom_point(
    aes(x = SampleSizeN, y = log_co1_ecr, color = 'Unadjusted Logistic', shape = 'Coefficients'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = log_co1_ecr, color = 'Unadjusted Logistic', linetype = 'Coefficients'),
    size = 1.15
  ) +
  geom_point(
    aes(x = SampleSizeN, y = dsm_int_ecr, color = 'Adjusted Logistic', shape = 'Intercept'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = dsm_int_ecr, color = 'Adjusted Logistic', linetype = 'Intercept'),
    size = 1.15
  ) +
  geom_point(
    aes(x = SampleSizeN, y = dsm_co1_ecr, color = 'Adjusted Logistic', shape = 'Coefficients'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = dsm_co1_ecr, color = 'Adjusted Logistic', linetype = 'Coefficients'),
    size = 1.15
  ) +
  geom_hline(
    aes(yintercept = 0.95),
    linetype = 2,
    size = 0.5
  ) +
  scale_x_continuous(
    breaks = c(2, 4, 6, 8), labels = levels(Rec_res$SampleSize),
    name = "Sample Size",
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    n.breaks = 10
  ) +
  scale_color_manual(
    name='Model',
    breaks=c('Unadjusted Logistic', 'Adjusted Logistic'),
    values=c('Unadjusted Logistic' = RColorBrewer::brewer.pal(3, "Set2")[1],
             'Adjusted Logistic' = RColorBrewer::brewer.pal(3, "Set2")[2])
  ) +
  scale_linetype_manual(
    name = 'Parameter',
    breaks = c('Intercept', 'Coefficients'),
    values = c('Intercept' = 1,
               'Coefficients' = 2)
  ) +
  scale_shape_manual(
    name = 'Parameter',
    breaks = c('Intercept', 'Coefficients'),
    values = c('Intercept' = 16,
               'Coefficients' = 18)
  ) +
  labs(
    x = "Sample Size",
    y = "Coverage Rate",
    title = "Empirical Coverage Rates of Parameters"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.spacing.x = unit(1.25, "lines"),
    strip.background = element_rect(fill = "darkgrey", linetype = "solid", size = 1.5),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    strip.text = element_text(family = "serif", face = "bold", size = 14, color = "white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    color = guide_legend(title = "Model", title.position = "top", title.hjust = 0.5,
                         title.theme = element_text(family = "serif", size = 14),
                         override.aes = list(shape = NA)),
    shape = guide_legend(title = "Parameter", title.position = "top", title.hjust = 0.5,
                         title.theme = element_text(family = "serif", size = 14)),
    linetype = guide_legend(title = "Parameter", title.position = "top", title.hjust = 0.5,
                            title.theme = element_text(family = "serif", size = 14))
  ) +
  facet_grid(
    ~ Parameters
  )
```

The ECRs of both models are problematic, though the adjusted model's credible intervals are much more accurate overall. The unadjusted model's ECRs are nearly 0, indicating that almost none of the time would the credible intervals contain the true parameter value. The ECR also has an important relationship with statistical inference as 1-ECR is the False Discovery Rate (FDR), so when the ECR = 0.95 as intended, then FDR = 0.05. When the ECR is as low as what is observed in unadjusted model, this means that nearly all significant results are false discoveries. Another important pattern to consider is that the adjusted model's ECR improves with additional predictors, though this may be somewhat impacted by the fact that all predictors are relevant in the simulation study because the parameter values were intended to be centered around 1. Compared to the intercept, the adjusted model does do better with the coefficients.

One potential reason that the intercept variable is captured poorly in the adjusted model is that the model is weakly identified. The intercept in a logistic regression often adjusts the estimated log-odds to their true value in cases where no other predictor variable is able to do so. In this case, when all of the standardized variables are at the mean, then the model's intercept gives the overall log-odds of being classified as a 1 or a 0. This is an acceptable adjustment, which is in some essence a type of offsetting for the model, when the only other factor that could account for a discrepancy between observed log-odds and estimated log-odds is some baseline risk. In the adjusted model, however, there is an extra complication as the observed log-odds may be biased from an imperfect class assignment, so the intercept and the estimated sensitivity and specificity both may need to be adjusted to account for the residual discrepancy. At the moment, this hypothesis is not tested, so it remains a speculation. It is possible that stricter priors over the intercept and sensitivity/specificity is needed to more strongly identify the model. One such prior may be to incorporate expected prevalence as this may help to inform the extent to which sensitivity/specificity versus the intercept needs to be adjusted.

### Implications for Statistical Inference

As mentioned before, biased estimation of parameter values can carry consequences for statistical inferences on model results. The example so far has been the relationship between ECR and FDR, which already has implications for erroneous inferences. Rather than focusing, however, on power and FDR, it can be more meaningful to consider M- and S-type errors. Where power and FDR are related to the null hypothesis testing, M- and S-type errors describe the kinds of mistakes that are made when a result is taken to be significant. The M-type error (magnitude) indicates a case when a significant result is found but is either under or overestimated, though traditionally the focus is on cases where an effect is overestimated. The S-type error (sign) occurs when a significant result is identified but has the wrong sign (i.e., a positive true effect but an estimated negative one or vice versa). These inferential errors have particularly important meanings for replication and decision-making as they specifically aim to quantify the consequences of inference rather than just focusing on whether or not a correct decision is made relative to some arbitrary null and alternative hypothesis. In the Bayesian inference, we are generally interested with the probability of some effect being greater, smaller, or equal to some threshold. In this particular case, we simplify the full posterior to a simple question: is the effect credibly different than 0? This is operationalized by testing whether the 95% credible interval includes zero or not. When the interval does not include zero, then we are able to conclude that there is at least a 95% probability that the true effect is not zero. It is important to note that this is an arbitrary threshold and decision for "significance," but it is selected as it is similar to standard inference standards used by researchers, giving it additional context to readers of all backgrounds.

The only plot shown for these errors are in the simulation conditions where just a single predictor was part of the model. The primary reason for this is that the plots become increasingly crowded when many parameters are included as there are many more results and thus much more to plot all at once. To avoid this, it could be possible to plot each predictor separately, but this would result in a 10 x 4 plot for the largest condition tested (10 rows corresponding to 10 fitted parameters and 4 columns for the 4 sample size conditions). The information for these larger conditions is thus better summarized with tables, which is done following these plots.

```{r}
#| layout-nrow: 2
#| layout-ncol: 1
#| fig-cap: 
#|   - "Unadjusted Model Inference Errors"
#|   - "Adjusted Model Inference Errors"
#| column: page-inset

C_1P_SM %>%
  filter(coef1_log_sig == "Significant") %>%
  arrange(coef1_log_raw) %>%
  group_by(SampleSize) %>%
  mutate(id = seq_len(n())) %>%
  ggplot() +
  geom_linerange(
    aes(x = id, ymin = coef1_log_cil, ymax = coef1_log_ciu),
    alpha = 0.25
  ) +
  geom_point(
    aes(x = id, y = coef1_tru_raw, color = coef1_log_tru),
    size = 0.75
  ) +
  geom_hline(
    aes(yintercept = 1.5),
    linetype = 2,
    size = 1.15
  ) +
  geom_hline(
    aes(yintercept = 0.5),
    linetype = 2,
    size = 1.15
  ) +
  scale_color_manual(
    name='coef1_log_tru',
    breaks=c('Under Estimated', 'Within Interval', 'Over Estimated'),
    values=c('Under Estimated' = RColorBrewer::brewer.pal(3, "Set2")[1],
             'Within Interval' = RColorBrewer::brewer.pal(3, "Set2")[2],
             'Over Estimated'  = RColorBrewer::brewer.pal(3, "Set2")[3])
  ) +
  labs(
    x = "Counts of Significant Results (Ordered from smallest to largest estimate)",
    y = "Coefficient Value",
    title = "Sign and Magnitude Errors of Unadjusted Logistic Regression Model"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.background = element_rect(fill = "darkgrey", linetype = "solid", size = 1.5),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    strip.text = element_text(family = "serif", face = "bold", size = 14, color = "white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    color = guide_legend(title = "Parameter Value", title.position = "top", title.hjust = 0.5,
                         title.theme = element_text(family = "serif", size = 14),
                         override.aes = list(size = 2))
  ) +
  facet_grid(
  ~ SampleSize,
  labeller = labeller(
    SampleSize = labs_Ss
  ),
  scales = "free"
  )

C_1P_SM %>%
  filter(coef1_dsm_sig == "Significant") %>%
  arrange(coef1_dsm_raw) %>%
  group_by(SampleSize) %>%
  mutate(id = seq_len(n())) %>%
  ggplot() +
  geom_linerange(
    aes(x = id, ymin = coef1_dsm_cil, ymax = coef1_dsm_ciu),
    alpha = 0.25
  ) +
  geom_point(
    aes(x = id, y = coef1_tru_raw, color = coef1_dsm_tru),
    size = 0.75
  ) +
  geom_hline(
    aes(yintercept = 1.5),
    linetype = 2,
    size = 1.15
  ) +
  geom_hline(
    aes(yintercept = 0.5),
    linetype = 2,
    size = 1.15
  ) +
  scale_color_manual(
    name='coef1_log_tru',
    breaks=c('Under Estimated', 'Within Interval', 'Over Estimated'),
    values=c('Under Estimated' = RColorBrewer::brewer.pal(3, "Set2")[1],
             'Within Interval' = RColorBrewer::brewer.pal(3, "Set2")[2],
             'Over Estimated'  = RColorBrewer::brewer.pal(3, "Set2")[3])
  ) +
  labs(
    x = "Counts of Significant Results (Ordered from smallest to largest estimate)",
    y = "Coefficient Value",
    title = "Sign and Magnitude Errors of Adjusted Logistic Regression Model"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.background = element_rect(fill = "darkgrey", linetype = "solid", size = 1.5),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    strip.text = element_text(family = "serif", face = "bold", size = 14, color = "white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    color = guide_legend(title = "Parameter Value", title.position = "top", title.hjust = 0.5,
                         title.theme = element_text(family = "serif", size = 14),
                         override.aes = list(size = 2))
  ) +
  facet_grid(
    ~ SampleSize,
    labeller = labeller(
      SampleSize = labs_Ss
    ),
    scales = "free"
  )
```

The plots demonstrated that the unadjusted model leads to highly problematic parameter estimates such that in nearly all cases effect sizes are underestimated. In contrast, the adjusted model produces estimates much more congruent with true values and has more balanced over and under estimation error rates. To help the plots connect more to the simulation conditions, dashed horizontal lines are plotted at +/- 2SDs of the true coefficient distribution (i.e., coefficients were drawn from the *N*(1, 0.25) distribution). It can be seen then that the unadjusted model becomes rapidly over optimistic about its estimation accuracy as the credible intervals for its effect estimates are much more narrow than for adjusted model, and the average effect size estimated by the unadjusted model is below even where nearly 99% of all true effects would be expected. One potential trade-off of the adjusted model is that there are fewer significant results compared to the unadjusted model, implying that there is less power in the adjusted model. Part of this is a favorable consequence of the skeptical priors used, which should draw posterior estimates away from "extreme" results and closer to "null" ones. The more conservative estimation is a potential benefit of Bayesian methods compared to the more liberal *p*-values of frequentist null hypothesis testing that have contributed to replication crises across multiple disciplines. Since Bayesian methods are generally less concerned with null hypothesis tests, power to Bayesians is more meaningfully captured in estimation accuracy. In this sense, the fact that the posterior distributions, as indicated by the 95% credible intervals, tend to be a little larger for the adjusted compared to unadjusted values may be considered a weakness until one also notes that those wider intervals guard against the magnitude errors of the traditional logistic model.

Additionally, there are better ways of potentially defining statistical significance in the Bayesian framework. As noted previously, the use of a 95% credible interval that excludes 0 is a relatively arbitrary decision as it treats an interval like \[14.50, -0.003\] the same as \[-5.50, 5.50\] despite one clearly favoring the probability of the effect being zero (the latter) while other gives very minimal probability to that (the former). Alternative points for inference against a null hypothesis are the probability of direction, or the proportion of the posterior distribution that has the same sign as the mean, and the region of practical equivalence, which is the proportion of the posterior that includes zero and values deemed to be practically equivalent to zero. Future simulation studies can examine these possibilities (or others like the p-MAP or Bayes Factors).

The remainder of the simulation results are the tables for the inference errors. Since the number of parameters increase dramatically, the tables are separated into tabbed boxes so that the results of the conditions with three parameters have 4 tabs (the intercept + 3 parameters) and so on. Each tab will show the results a different parameter within those conditions. Shown in each table is the rate of non-significant results, the overall error rate, the rate of sign errors, and then the rates of under and over estimation.

::: panel-tabset
```{r}
library(knitr)
library(kableExtra)
```

## Intercept

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_1P, param = "int", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Intercept Inferences in Models with 1 Predictor",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    full_width = FALSE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

## Coefficient

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_1P, param = "coef1", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Coefficient Inferences in Models with 1 Predictor",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```
:::

::: panel-tabset
## Int.

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_3P, param = "int", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Intercept Inferences in Models with 3 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

## Coef. 1

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_3P, param = "coef1", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Coefficient Inferences in Models with 3 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

## Coef. 2

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_3P, param = "coef2", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Coefficient Inferences in Models with 3 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

## Coef. 3

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_3P, param = "coef3", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Coefficient Inferences in Models with 3 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```
:::

::: panel-tabset
## Int.

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_5P, param = "int", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Intercept Inferences in Models with 5 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

## Coef. 1

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_5P, param = "coef1", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Coefficient Inferences in Models with 5 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

## Coef. 2

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_5P, param = "coef2", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Coefficient Inferences in Models with 5 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

## Coef. 3

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_5P, param = "coef3", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Coefficient Inferences in Models with 5 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

## Coef. 4

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_5P, param = "coef4", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Coefficient Inferences in Models with 5 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

## Coef. 5

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_5P, param = "coef5", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Coefficient Inferences in Models with 5 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```
:::

::: panel-tabset
## Int.

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_10P, param = "int", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Intercept Inferences in Models with 10 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

## Coef. 1

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_10P, param = "coef1", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Coefficient Inferences in Models with 10 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

## Coef. 2

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_10P, param = "coef2", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Coefficient Inferences in Models with 10 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

## Coef. 3

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_10P, param = "coef3", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Coefficient Inferences in Models with 10 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

## Coef. 4

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_10P, param = "coef4", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Coefficient Inferences in Models with 10 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

## Coef. 5

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_10P, param = "coef5", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Coefficient Inferences in Models with 10 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

## Coef. 6

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_10P, param = "coef6", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Coefficient Inferences in Models with 10 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

## Coef. 7

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_10P, param = "coef7", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Coefficient Inferences in Models with 10 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

## Coef. 8

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_10P, param = "coef8", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Coefficient Inferences in Models with 10 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

## Coef. 9

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_10P, param = "coef9", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Coefficient Inferences in Models with 10 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

## Coef. 10

```{r warning=FALSE, message=FALSE}
kable(
  snmErr(resData = C_10P, param = "coef10", groupSN = TRUE)[, c(2:8, 14:18)],
  col.names = c("Sensitivity", "Specificity", rep(c("Non-Significant (%)", "Error Free (%)", "Sign Error (%)", "Overestimated (%)", "Underestimated (%)"), 2)),
  align = "cccccccccccc",
  caption = "Coefficient Inferences in Models with 10 Predictors",
  digits = 1,
  booktabs = TRUE
  ) %>%
  add_header_above(
    c("Condition" = 2, "Unadjusted Model" = 5, "Adjusted Model" = 5)
    ) %>%
  pack_rows(
  index = c("N = 100" = 3, "N = 250" = 3, "N = 500" = 3, "N = 1000" = 3)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```
:::

# Overview of Applied Data Analyses

This final section is dedicated to summarizing the applied case example. The real-world data were taken from the National Alzheimer's Coordinating Center (NACC), which is a longitudinal, multisite data collection study funded by the National Institute of Aging. The neuropathology, cognitive testing, and clinical diagnoses from the NACC data were used with the clinical diagnoses being treated as the error-in-variables observations and the pathology classifications as the gold-standard reference. Two diagnostic contrasts were examined. The first was of the non-impairment versus dementia due to Alzheimer's disease, and the second was the non-impaired versus mild cognitive impairment condition.

In addition to the clinical diagnosis and pathology results, demographic data for the samples' age, educational attainment, race, sex, ethnicity, and APOE status were also selected. Race and ethnicity were dichotomized due to limited variability of participant responses. APOE status was also recoded to reflect the total number of e4 alleles, meaning it ranged from 0 to 2. The demographic data was then used to generate regression-based normative scores for participants' raw scores on digit span forward and backward, symbol-digit coding, trail making, animal and vegetable fluency, confrontation naming, and episodic memory tests. These regression equations are based on previously published norms by [Weintraub et al. (2009)](https://files.alz.washington.edu/documentation/weintraub-2009.pdf "UDS 2.0 Norms").

In addition to the cognitive testing data, dispersion data were also computed from the normed scores. Dispersion was extracted because its utility in detecting cases that go on to develop abnormal cognition compared to those who do not has been inconsistent across studies. One potential reason that inconsistent results in the value of dispersion data could be that the studies and sample selection methods vary in their accuracy to true disease state, meaning that some studies may be unduly impacted by poor accuracy of the diagnoses. Dispersion of scores was computed as the standard deviation of the standardized test scores. A total dispersion score was calculated over all of the tests in addition to domain specific dispersion scores. These domains corresponded to an executive functioning/attention/processing speed (EFAS) consisting of digit spans, coding, and trails; language consisting of animal and vegetable fluency and confrontation naming; and finally memory consisting of immediate and delayed episodic recall. While dispersion is an important characteristic of general test performance, it only conveys information about the variability of scores. To address this, a Mahalanobis distance was also calculated for the overall test battery, which summarizes the multivariate abnormality of the scores.

## Clinical Alzheimer's Disease Diagnoses

For the non-impaired versus dementia due to Alzheimer's disease comparisons, all cases were filtered first to those with no Alzheimer's disease pathology on autopsy compared to those with significant pathology via the `NPADNC` variable. The remaining cases were then further filtered by those whose baseline clinical diagnoses were with either no cognitive impairment with the `NORMCOG` variable or as having dementia due to Alzheimer's disease through the `NACCALZP` variable. These latter variables were used as the error-in-variable observations while the pathology diagnoses were used as the unobserved true diagnoses. To demonstrate that there is discrepancy between the clinical and later pathology diagnoses, the following table shows the correspondence of the classifications:

```{r}
df_AD %>%
  select(Impaired, Pathology) %>%
  mutate(Pathology = factor(Pathology, levels = c(0, 3), labels = c("No Pathology", "Heavy Pathology"))) %>%
  table() %>%
  kable(
    align = "ccc"
  ) %>%
  add_header_above(
    c("Clinical Diagnosis" = 1, "Alzheimer's Pathology" = 2)
    ) %>%
  kable_paper(
    fixed_thead = TRUE,
    full_width = FALSE,
    html_font = 'lato'
    )
```

Based on the table, the expected accuracy of the clinical diagnosis for Alzheimer's pathology at death is $\frac{106 + 659}{106 + 122 + 49 + 659} \approx 0.82$ with a sensitivity of 0.84 and specificity of 0.68. We can compare these results to the accuracy estimates of the unadjusted and adjusted models:

```{r}
matrix(
  c(
    sum(diag(table(round(log_mod_AD_std$summary(variables = "pop", mean)$mean),
                   df_AD$Impaired))) / nrow(df_AD),
    sum(diag(table(round(dsm_mod_AD_std$summary(variables = "pop", mean)$mean),
                   df_AD$Impaired))) / nrow(df_AD),
    sum(diag(table(round(log_mod_AD_TIV$summary(variables = "pop", mean)$mean),
                   df_AD$Impaired))) / nrow(df_AD),
    sum(diag(table(round(dsm_mod_AD_TIV$summary(variables = "pop", mean)$mean),
                   df_AD$Impaired))) / nrow(df_AD),
    sum(diag(table(round(log_mod_AD_DIV$summary(variables = "pop", mean)$mean),
                   df_AD$Impaired))) / nrow(df_AD),
    sum(diag(table(round(dsm_mod_AD_DIV$summary(variables = "pop", mean)$mean),
                   df_AD$Impaired))) / nrow(df_AD),

    sum(diag(table(round(log_mod_AD_std$summary(variables = "pop", mean)$mean),
                   df_AD$Standard))) / nrow(df_AD),
    sum(diag(table(round(dsm_mod_AD_std$summary(variables = "pop", mean)$mean),
                   df_AD$Standard))) / nrow(df_AD),
    sum(diag(table(round(log_mod_AD_TIV$summary(variables = "pop", mean)$mean),
                   df_AD$Standard))) / nrow(df_AD),
    sum(diag(table(round(dsm_mod_AD_TIV$summary(variables = "pop", mean)$mean),
                   df_AD$Standard))) / nrow(df_AD),
    sum(diag(table(round(log_mod_AD_DIV$summary(variables = "pop", mean)$mean),
                   df_AD$Standard))) / nrow(df_AD),
    sum(diag(table(round(dsm_mod_AD_DIV$summary(variables = "pop", mean)$mean),
                   df_AD$Standard))) / nrow(df_AD)
  ),
  byrow = FALSE,
  ncol = 2, 
  nrow = 6,
  dimnames = list(rep(c("Unadjusted", "Adjusted"), 3),
                  c("Clinical", "Pathology"))
) %>%
  kable(
    align = "cc",
    digits = 2
  ) %>%
  add_header_above(
    c("Model" = 1, "Diagnostic Reference" = 2)
    ) %>%
  pack_rows(
    index = c("Norm-Referenced Tests" = 2, "Total Dispersion" = 2, "Domain-Specific Dispersion" = 2)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    full_width = FALSE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

Both the adjusted and unadjusted models fail to exceed the accuracy for the pathology diagnoses implied by the general accuracy of the clinical diagnoses. The finding suggests that the accuracy for detecting Alzheimer's pathology at death cannot be improved beyond clinical diagnoses with the cognitive testing variables used. This is not a particularly surprising result as differentiating between no cognitive impairment and frank dementia is not generally a challenging diagnostic decision, suggesting that the tested statistical models cannot extract additional information from these variables than what was identified by clinicians.

We can also compare the expected sensitivity and specificity from the clinical diagnoses to the ones estimated by the adjusted model. For these analyses, we select the adjusted model with the best accuracy for the pathology diagnoses, which was the domain-specific dispersion model (though marginally -- 0.818 compared to 0.815 or 0.816).

```{r}
dsm_mod_AD_DIV$summary(variables = c("sens", "spec", "BR2"))[, c(2, 4, 6:7)] %>%
  add_column(Variable = c("Sensitivity", "Specificity", "Bayesian R2")) %>%
  relocate(Variable) %>%
  rename(
    "Mean" = mean, 
    "Standard Error" = sd, 
    "95% CI: LB" = q5, 
    "95% CI: UB" = q95
  ) %>%
  kable(
    align = "ccccc",
    digits = 2
  ) %>%
  pack_rows(
    index = c("Utility Statistics" = 2, "Model Performance" = 1)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    full_width = FALSE,
    html_font = 'lato'
  )
```

The model suggests that a much greater overall level of sensitivity and specificity is obtained from the adjusted model than would be implied by the accuracy of the clinical diagnoses. One potential take-away from this model is that clinical diagnostic accuracy for Alzheimer's disease pathology cannot improve too much from the neuropsychological test data available in NACC. This would not be an entirely surprising result as the clinical decision making for an individual without cognitive impairment versus one with frank dementia is not particularly challenging, meaning that if any clinical diagnosis was already close to as good as it could be then it would be this contrast. Further, the presence of Alzheimer's disease pathology is not the same thing as having an Alzheimer's disease dementia syndrome, and while the decision to focus on the extremes of pathology presence, there is still some chance that there are individuals with significant pathology but no other symptoms that would warrant a diagnosis of dementia. Further, there remains the possibility that additional comorbidities impact the results as no exclusion or filtering was done to limit neuropathology to only Alzheimer's pathology, meaning that -- while individuals may have significant Alzheimer's pathology -- they may also have more prominent pathology related to another disease process that dominates the clinical picture. Thus, while the model results suggest that the diagnostic accuracy of Alzheimer's disease detection against individuals with normal cognition is not improved, there are still meaningful results suggested by the model that warrant follow-up.

## Clinical Mild Cognitive Impairment Diagnoses

The detection of mild cognitive impairment (MCI) is a growing area of interest as there are some individuals with MCI who return to normal cognition, others who remain stable over time, while others go on to progress to a dementia. The identification of individuals in this latter group is increasingly important as there is growing evidence that lifestyle changes that can affect cognitive trajectory have greatest effects when implemented earlier in the disease course. To address this area of interest, this applied analysis focused on the identification of individuals who go on to develop dementia and have significant Alzheimer's disease pathology. To do this, all cases in the NACC data were first filtered for either no autopsy pathology or who went on to be diagnosed clinically as having dementia and also had significant autopsy pathology. Like with the dementia analyses, the pathology diagnoses were treated as the gold-standard, and the observed-with-error classifications were whether the individual was diagnosed as having normal cognition or any subtype of MCI (via the `NACCUDSD` variable) at baseline. The same predictors were used in the MCI analyses as in the dementia ones. The table below shows the accuracy of the models:

```{r}
matrix(
  c(
    sum(diag(table(round(log_mod_MCI_std$summary(variables = "pop", mean)$mean),
               df_MCI$Impaired))) / nrow(df_MCI),
    sum(diag(table(round(dsm_mod_MCI_std$summary(variables = "pop", mean)$mean),
               df_MCI$Impaired))) / nrow(df_MCI),
    sum(diag(table(round(log_mod_MCI_TIV$summary(variables = "pop", mean)$mean),
               df_MCI$Impaired))) / nrow(df_MCI),
    sum(diag(table(round(dsm_mod_MCI_TIV$summary(variables = "pop", mean)$mean),
               df_MCI$Impaired))) / nrow(df_MCI),
    sum(diag(table(round(log_mod_MCI_DIV$summary(variables = "pop", mean)$mean),
               df_MCI$Impaired))) / nrow(df_MCI),
    sum(diag(table(round(dsm_mod_MCI_DIV$summary(variables = "pop", mean)$mean),
               df_MCI$Impaired))) / nrow(df_MCI),
    
    sum(diag(table(round(log_mod_MCI_std$summary(variables = "pop", mean)$mean),
               df_MCI$Pathology))) / nrow(df_MCI),
    sum(diag(table(round(dsm_mod_MCI_std$summary(variables = "pop", mean)$mean),
               df_MCI$Pathology))) / nrow(df_MCI),
    sum(diag(table(round(log_mod_MCI_TIV$summary(variables = "pop", mean)$mean),
               df_MCI$Pathology))) / nrow(df_MCI),
    sum(diag(table(round(dsm_mod_MCI_TIV$summary(variables = "pop", mean)$mean),
               df_MCI$Pathology))) / nrow(df_MCI),
    sum(diag(table(round(log_mod_MCI_DIV$summary(variables = "pop", mean)$mean),
               df_MCI$Pathology))) / nrow(df_MCI),
    sum(diag(table(round(dsm_mod_MCI_DIV$summary(variables = "pop", mean)$mean),
               df_MCI$Pathology))) / nrow(df_MCI)
  ),
  byrow = FALSE,
  ncol = 2, 
  nrow = 6,
  dimnames = list(rep(c("Unadjusted", "Adjusted"), 3),
                  c("Clinical", "Pathology"))
) %>%
  kable(
    align = "cc",
    digits = 2
  ) %>%
  add_header_above(
    c("Model" = 1, "Diagnostic Reference" = 2)
    ) %>%
  pack_rows(
    index = c("Norm-Referenced Tests" = 2, "Total Dispersion" = 2, "Domain-Specific Dispersion" = 2)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    full_width = FALSE,
    lightable_options = c("hover"),
    html_font = 'lato'
    )
```

In this case, the accuracy of the models is much less than for dementia, which makes sense as MCI is often viewed as a transitional state with no clear boundary between normal and dementia. In this case, the norm-referenced test model produces the best overall accuracy for the pathological diagnostic standard, so it will be used for the analyses going forward.

```{r}
dsm_mod_MCI_std$summary(variables = c("sens", "spec", "BR2"))[, c(2, 4, 6:7)] %>%
  add_column(Variable = c("Sensitivity", "Specificity", "Bayesian R2")) %>%
  relocate(Variable) %>%
  rename(
    "Mean" = mean, 
    "Standard Error" = sd, 
    "95% CI: LB" = q5, 
    "95% CI: UB" = q95
  ) %>%
  kable(
    align = "ccccc",
    digits = 2
  ) %>%
  pack_rows(
    index = c("Utility Statistics" = 2, "Model Performance" = 1)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    full_width = FALSE,
    html_font = 'lato'
  )
```

Overall, the model suggests that the clinical diagnoses of MCI are fairly accurate and that relatively little variance in the classification of future observations remains to be modeled outside of the included demographics and normatively adjusted neuropsychological test scores. Ultimately, many of the same implications as were noted with the dementia modeling results could be raised here as well. Notably, in both cases so far, the adjusted model has been at least as accurate as the unadjusted model with simulation results suggesting that under better conditions (i.e., those wherein the initial diagnoses are not so accurate) the model can exceed by a wide margin the accuracy of other methods. Additionally, the simulation results suggest that parameter estimates and inference are improved with the adjusted model, meaning that there is not a clear situation wherein an unadjusted logistic regression model may be preferred. Still, more rigorous simulation research and applied data analyses should be conducted before a blanket statement that the adjusted model should be the default model to run -- perhaps the current recommendation may be to run both the unadjusted and adjusted model to inspect whether results deviate between the models and then pursuing explanations for the differences that may justify which result is to be preferred.

Since one of these interests is in the area of inference, it is expedient to examine how parameter estimates may differ between the two models in the context of an applied data study. Since the MCI diagnostic contrast resulted in the most difference between overall classification rates, this model is used for the illustration. Below are the tables summarizing the parameter results from both models:

```{r}
#| layout-ncol: 2
#| tbl-cap:
#|   - "Parameter Estimates from the Adjusted Logistic Regression"
#|   - "Parameter Estimates from the Unadjusted Logistic Regression"

dsm_mod_MCI_std$summary(variables = c("b_0_c", "t_x"))[, c(2, 4, 6:7)] %>%
  add_column(Coefficient = c("Intercept", "Age", "Education", "Race", "Sex", "Ethnicity", "APOE", "Digits Forward", "Digits Backward", "Coding", "Trails A", "Trails B", "Animal Fluency", "Vegetable Fluency", "Boston Naming", "Logical Memory I", "Logical Memory II")) %>%
  relocate(Coefficient) %>%
  rename(
    "Mean" = mean, 
    "Standard Error" = sd, 
    "95% CI: LB" = q5, 
    "95% CI: UB" = q95
  ) %>%
  kable(
    align = "ccccc",
    digits = 2,
    caption = "Parameter Estimates from Adjusted Logistic Regression Model"
  ) %>%
  pack_rows(
    index = c(" " = 1, "Demographics" = 6, "Test Performance" = 10)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    full_width = FALSE,
    lightable_options = c("hover"),
    html_font = 'lato'
  )

log_mod_MCI_std$summary(variables = c("b_0_c", "t_x"))[, c(2, 4, 6:7)] %>%
  add_column(Coefficient = c("Intercept", "Age", "Education", "Race", "Sex", "Ethnicity", "APOE", "Digits Forward", "Digits Backward", "Coding", "Trails A", "Trails B", "Animal Fluency", "Vegetable Fluency", "Boston Naming", "Logical Memory I", "Logical Memory II")) %>%
  relocate(Coefficient) %>%
  rename(
    "Mean" = mean, 
    "Standard Error" = sd, 
    "95% CI: LB" = q5, 
    "95% CI: UB" = q95
  ) %>%
  kable(
    align = "ccccc",
    digits = 2
  ) %>%
  pack_rows(
    index = c(" " = 1, "Demographics" = 6, "Test Performance" = 10)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    full_width = FALSE,
    lightable_options = c("hover"),
    html_font = 'lato'
  )
```

There are generally small differences between the two model estimates, but if one returns to the definition of significance as a 95% credible interval that excludes 0, then the two models differ in their identification of significant predictors. In particular, the unadjusted model estimates that the number of APOE e4 alleles significantly affects the risk of being identified as MCI and progressing to dementia while the adjusted model does not give a credible effect to this variable. The differences between the two models can be illustrated by examining the posterior distributions for the 4-5 variables they identified as "significant." Interval plots for these effects are shown below:

```{r}
#| layout-ncol: 2
#| fig-cap:
#|   - "Interval Plots from the Adjusted Logistic Regression"
#|   - "Interval Plots from the Unadjusted Logistic Regression"

dsm_mod_MCI_std$draws(variables = c("t_x[1]", "t_x[3]", "t_x[6]", "t_x[14]", "t_x[16]")) %>%
  mcmc_intervals(
   prob = 0.50,
   prob_outer = 0.95,
   point_est = "mean"
  ) +
  scale_y_discrete(
    labels = c("t_x[1]"  = "Age",
               "t_x[3]"  = "Race",
               "t_x[6]"  = "APOE",
               "t_x[14]" = "BNT",
               "t_x[16]" = "LMII"),
    limits = c("t_x[16]", "t_x[14]", "t_x[6]", "t_x[3]", "t_x[1]")
  )

log_mod_MCI_std$draws(variables = c("t_x[1]", "t_x[3]", "t_x[6]", "t_x[14]", "t_x[16]")) %>%
  mcmc_intervals(
   prob = 0.50,
   prob_outer = 0.95,
   point_est = "mean"
  ) +
  scale_y_discrete(
    labels = c("t_x[1]"  = "Age",
               "t_x[3]"  = "Race",
               "t_x[6]"  = "APOE",
               "t_x[14]" = "BNT",
               "t_x[16]" = "LMII"),
    limits = c("t_x[16]", "t_x[14]", "t_x[6]", "t_x[3]", "t_x[1]")
  )
```

These interval plots show the 50% credible interval (bold & darker line segment) and 95% credible interval (thin & lighter line segement) with the mean (circle) of each standardized effect size. While the signs agree between the two models, the effect estimates are underestimated for unadjusted model while the effect of race is seemingly exaggerated by the same model. As previously mentioned, however, the 95% credible interval may not be the best way of thinking about Bayesian significance tests. To address this point and further illustrate some of the subtle ways in which estimates may be biased in an unadjusted model, we will consider the probability of direction and the practical significance of estimates. Briefly, the probability of direction corresponds to the probability that a parameter positive or negative. The metric is sometimes referred to as the maximum probability of effect, and it is strongly correlated with the traditional *p*-value with a probability of direction of 0.975 generally corresponding to a two-tailed *p*-value of 0.05. The practical significance metric is the probability that a given effect is above some threshold that corresponds to what is considered a neglible effect. For this application, we consider a standardized log-odds value of 0.1814 to be the boundary for negligible effects based on the logistic conversion of a Cohen's *d* effect size of 0.1000. Tables for the effect sizes are shown below:

```{r}
#| layout-ncol: 2
#| tbl-cap: 
#|   - "Probabilities of Direction"
#|   - "Probabilities of Significance"

cbind(p_direction(dsm_mod_MCI_std$draws("t_x")), p_direction(log_mod_MCI_std$draws("t_x"))[, 2]) %>%
  mutate(Parameter = c("Age", "Education", "Race", "Sex", "Ethnicity", "APOE", "Digits Forward", "Digits Backward", "Coding", "Trails A", "Trails B", "Animal Fluency", "Vegetable Fluency", "Boston Naming", "Logial Memory I", "Logical Memory II")) %>%
  rename("PD: Adjusted" = 2,
         "PD: Unadjusted" = 3) %>%
  kable(
    digits = 2,
    align = "ccc"
  ) %>%
  pack_rows(
    index = c("Demographics" = 6, "Test Performance" = 10)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    full_width = FALSE,
    lightable_options = c("hover"),
    html_font = 'lato'
  )

cbind(p_significance(dsm_mod_MCI_std$draws("t_x"), threshold = 0.1814), p_significance(log_mod_MCI_std$draws("t_x"), threshold = 0.1814)[, 2]) %>%
  mutate(Parameter = c("Age", "Education", "Race", "Sex", "Ethnicity", "APOE", "Digits Forward", "Digits Backward", "Coding", "Trails A", "Trails B", "Animal Fluency", "Vegetable Fluency", "Boston Naming", "Logial Memory I", "Logical Memory II")) %>%
  rename("PS: Adjusted" = 2,
         "PS: Unadjusted" = 3) %>%
  kable(
    digits = 2,
    align = "ccc"
  ) %>%
  pack_rows(
    index = c("Demographics" = 6, "Test Performance" = 10)
  ) %>%
  kable_paper(
    fixed_thead = TRUE,
    full_width = FALSE,
    lightable_options = c("hover"),
    html_font = 'lato'
  )

```

These results are similar to those observed with the interval plots, but it is helpful to see how statistical inference can be affected by the modeling decisions. In the case of practical significance, for example, the adjusted model gives Logical Memory I a 96% probability of being practically significant based on the threshold of 0.1814, but the unadjusted model gives the same test a probability of just 0.77 of a practically significant effect.

Finally, in the interest of demonstrating how effects can be understood without dependence on thresholds or decision-rules, we provide the marginal effects plots for age, Boston Naming, and Logical Memory Delay as these were most robustly identified as plausible effects but differ in their overall effect. 

```{r}
#| fig-width: 7
#| fig-height: 5
#| column: body-inset

dsm_mod_MCI_std$draws(c("b_x"), format = "matrix") %>%
  .[, 1:16] %*% t(as.matrix(nd_MCIa)) %>%
  sweep(., 1, dsm_mod_MCI_std$draws(c("b_0"), format = "matrix"), FUN = "+") %>%
  summarise_draws() %>%
  select(c("mean", "q5", "q95")) %>%
  data.frame() %>%
  mutate_all(function(x) exp(x) / (1 + exp(x))) %>%
  bind_cols(nd_MCIa) %>%
  ggplot(aes(x = Age)) +
  geom_ribbon(aes(ymin = q5, ymax = q95),
              alpha = 1/2) +
  geom_line(
    aes(y = mean)
  ) +
  stat_slab(
    data = df_MCI %>% mutate(Impaired.n = as.numeric(Impaired) - 1),
    aes(
      y = Impaired.n,
      side = ifelse(Impaired.n == 1, "bottom", "top"),
      fill = Impaired, color = Impaired
    ),
    slab_type = "histogram",
    scale = 0.30,
    breaks = 25,
    size = 0.75
  ) +
  scale_fill_manual(
    "Diagnosis",
    values = c(alpha("#009E73", .7),
               alpha("#D55E00", .7))
  ) +
  scale_color_manual(
    "Diagnosis",
    values = c("#009E73", "#D55E00"),
    guide = "none"
  ) +
  scale_x_continuous(
    "Age (years)",
    breaks = seq(35, 94, 10),
    expand = c(0, 0),
    limits = c(35, 94)
  ) +
  scale_y_continuous(
    "Probability of Impairment",
    expand = c(0, 0),
    limits = 0:1
  ) +
  labs(
    title = "Marginal Effect of Age"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    fill = guide_legend(title = "Clinical Diagnosis", title.position = "top", title.hjust = 0.5,
                        title.theme = element_text(family = "serif", size = 14),
                        override.aes = list(color = c("#009E73", "#D55E00")))
  )

```
```{r}
#| fig-width: 7
#| fig-height: 5
#| column: body-inset

dsm_mod_MCI_std$draws(c("b_x"), format = "matrix") %>%
  .[, 1:16] %*% t(as.matrix(nd_MCIb)) %>%
  sweep(., 1, dsm_mod_MCI_std$draws(c("b_0"), format = "matrix"), FUN = "+") %>%
  summarise_draws() %>%
  select(c("mean", "q5", "q95")) %>%
  data.frame() %>%
  mutate_all(function(x) exp(x) / (1 + exp(x))) %>%
  bind_cols(nd_MCIb) %>%
  ggplot(aes(x = BNT.z)) +
  geom_ribbon(aes(ymin = q5, ymax = q95),
              alpha = 1/2) +
  geom_line(
    aes(y = mean)
  ) +
  stat_slab(
    data = df_MCI %>% mutate(Impaired.n = as.numeric(Impaired) - 1),
    aes(
      y = Impaired.n,
      side = ifelse(Impaired.n == 1, "bottom", "top"),
      fill = Impaired, color = Impaired
    ),
    slab_type = "histogram",
    scale = 0.30,
    breaks = 50,
    size = 0.75
  ) +
  scale_fill_manual(
    "Diagnosis",
    values = c(alpha("#009E73", .7),
               alpha("#D55E00", .7))
  ) +
  scale_color_manual(
    "Diagnosis",
    values = c("#009E73", "#D55E00"),
    guide = "none"
  ) +
  scale_x_continuous(
    "Boston Naming Test 30 Odd Item Standardized Score",
    breaks = seq(-5.5, 1.3, 1.5),
    expand = c(0, 0),
    limits = c(-5.5, 1.3)
  ) +
  scale_y_continuous(
    "Probability of Impairment",
    expand = c(0, 0),
    limits = 0:1
  ) +
  labs(
    title = "Marginal Effect of the Boston Naming Test"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    fill = guide_legend(title = "Clinical Diagnosis", title.position = "top", title.hjust = 0.5,
                        title.theme = element_text(family = "serif", size = 14),
                        override.aes = list(color = c("#009E73", "#D55E00")))
  )
```
```{r}
#| fig-width: 7
#| fig-height: 5
#| column: body-inset

dsm_mod_MCI_std$draws(c("b_x"), format = "matrix") %>%
  .[, 1:16] %*% t(as.matrix(nd_MCIm)) %>%
  sweep(., 1, dsm_mod_MCI_std$draws(c("b_0"), format = "matrix"), FUN = "+") %>%
  summarise_draws() %>%
  select(c("mean", "q5", "q95")) %>%
  data.frame() %>%
  mutate_all(function(x) exp(x) / (1 + exp(x))) %>%
  bind_cols(nd_MCIm) %>%
  ggplot(aes(x = LMII.z)) +
  geom_ribbon(aes(ymin = q5, ymax = q95),
              alpha = 1/2) +
  geom_line(
    aes(y = mean)
  ) +
  stat_slab(
    data = df_MCI %>% mutate(Impaired.n = as.numeric(Impaired) - 1),
    aes(
      y = Impaired.n,
      side = ifelse(Impaired.n == 1, "bottom", "top"),
      fill = Impaired, color = Impaired
    ),
    slab_type = "histogram",
    scale = 0.30,
    breaks = 50,
    size = 0.75
  ) +
  scale_fill_manual(
    "Diagnosis",
    values = c(alpha("#009E73", .7),
               alpha("#D55E00", .7))
  ) +
  scale_color_manual(
    "Diagnosis",
    values = c("#009E73", "#D55E00"),
    guide = "none"
  ) +
  scale_x_continuous(
    "Logical Memory II Standardized Score",
    breaks = seq(-5.0, 2.25, 1.25),
    expand = c(0, 0),
    limits = c(-5.0, 2.25)
  ) +
  scale_y_continuous(
    "Probability of Impairment",
    expand = c(0, 0),
    limits = 0:1
  ) +
  labs(
    title = "Marginal Effect of the Logical Memory II"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    fill = guide_legend(title = "Clinical Diagnosis", title.position = "top", title.hjust = 0.5,
                        title.theme = element_text(family = "serif", size = 14),
                        override.aes = list(color = c("#009E73", "#D55E00")))
  )
```

These plots provide the fitted log curve and its corresponding 95% credible interval for the effect of interest while holding all other variables in the model at their mean values. Plotted at the top and bottom of the plots are also the empirical histograms of the observed variables in the sample to help visualize the true group separation in the variables as an index for the effect size.