# Human Feedback!

Table 2 is redundant---you already describe all three numbers in the text.

Why loess in Figure 1 but not 2?

For the "relative importance" section, you could have included ANOVA F ratios.



# Feedback: bike sharing regression exercise

This is an outstanding write-up. It reads like a real journal article, with well formatted tables, figures with notes, and a reference for the data. Some highlights:

* You noticed that the lowest day, 22 rentals, was the day Hurricane Sandy reached Washington. That is great data detective work
* You spotted from the scatterplot that the relationship with temperature is curved, and said the linear slope should be read as an average. Then the month analysis confirms it
* Your omitted variable bias calculation is exactly right. $136.4 \times -0.42 = -57.4$ is precisely the gap between the wind slope on its own and the wind slope holding temperature constant
* You compared the two predictors on common scales, using both $R^2$ and standardised coefficients, rather than comparing raw slopes in different units
* You state clearly that you treated the maximum wind speed of 67 as km/h. The model answer treats it as mph, so its wind slopes are smaller (88 and 52 fewer bikes per mph), but the story is the same

## Things to work on

**Report the test of whether the slopes differ between months.** Your `.Rmd` compares the models with and without the temperature $\times$ month interaction using `anova()`, but the result is not in your write-up. The separate monthly slopes do not tell the reader whether the slopes really differ. Add the test: $F(11, 707)=7.38$, $p<.0001$.

**Keep to the language of association.** Most of your write-up says "was associated with", which is right. But the final paragraph says "warmer weather increases demand in winter, spring and autumn, but in midsummer additional heat reduces it". That is a causal claim, and these are observational data.

**Check the folder.** Is `regression.R` part of your analysis? The brief asks for the `.Rmd` and the knitted `.html`. If `regression.R` is an old draft, delete it, so it is clear which file reproduces the write-up.
