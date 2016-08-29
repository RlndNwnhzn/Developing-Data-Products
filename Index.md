---
title       : Testing Proportions App
subtitle    : Assignment for Coursera Developing Data Products Course
author      : Roland Nieuwenhuizen
job         : 
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : [mathjax]    # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Motivation

- When marketing research results are distributed, the audience often needs some guidance on the interpretation of the numbers.
- Questions that often arise are how accurate certain numbers are and how to interprete the difference between numbers.
- This app gives some guidance and self help for readers and users of research on those issues in the case of proportions.

--- .class #id 

## The App

- Type the sample sizes in the text boxes. The sample sizes have to be integer, greater than 40. If the sample size is numeric but not integer, it is rounded to the nearest integer. If the sample size is not numeric or smaller than 40, the app gives an error.
- Set the two proportions. Because of the method used, the proportions can't be too small or too large. If one of the proportions is set out off range, the app automatically adjusts to the nearest valid value.

- Now in the graph the app gives the confidence interval of the two proportions. With 95% confidence the 'true' value of the proportion is within the interval.
- The app also gives the p-value: the probability of the observed proportions under the hypothesis that in reality they are equal. So the smaller the p-value, the less likely they are really equal.

--- .class #id 

## Method and Prerequisites

In the calculations a normal approximation is used. This means that there are some prerequisites:

1. Theoretically the population sizes have to be infinite. In practise the approximation is valid when the population sizes are much (say at least ten times) larger than the sample sizes. 

2. The normal approximation is only valid when the values of the proportions are not too extreme. Literature gives a lot of different rules to guarantee that the approximation is valid. In this app the following condition is used on sample size $(n)$ and proportion $(p)$:

    $$np(1-p) \ge 10$$

3. The sample sizes have to be at least 40.

4. If the above rules are not obeyed, the proportions are adjusted to the nearest valid value.

--- .class #id 

## The Formula

On this slide the formula that is used to calculate the p-value is reproduced. It uses the same values for the sample sizes $(n)$ and proportions $(p)$ as the initial values of the app.


```r
survey1_p <- 0.4; survey1_n <- 100; survey2_p <- 0.5; survey2_n <- 100
pooled_p <- ((survey1_p*survey1_n)+(survey2_p*survey2_n)) / (survey1_n+survey2_n)
survey1_var <- (survey1_p*(1-survey1_p))/survey1_n
survey2_var <- (survey2_p*(1-survey2_p))/survey2_n
pooled_var <- (pooled_p*(1-pooled_p))*((1/survey1_n)+(1/survey2_n))

z_value <- abs(survey1_p-survey2_p)/sqrt(pooled_var)
    
p_value <- 2*(1-pnorm(z_value))
sprintf("%1.1f%%", 100 * p_value)
```

```
## [1] "15.5%"
```
