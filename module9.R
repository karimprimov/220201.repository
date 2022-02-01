library(sjmisc)
library(tidyverse)
library(base)
library(ggplot2)
f <- "https://raw.githubusercontent.com/difiore/ada-2022-datasets/main/KamilarAndCooperData.csv"
d <- read_csv(f, col_names = TRUE) #creates a tibble
d
library(skimr)
library(kableExtra)
s <- skim(d)
s %>%
  filter(skim_variable == "Scientific_Name" | skim_type == "numeric") %>%
  rename(variable=skim_variable, missing=n_missing, mean=numeric.mean,
         sd=numeric.sd, min=numeric.p0, p25=numeric.p25, median=numeric.p50,
         p75=numeric.p75, max=numeric.p100, hist=numeric.hist) %>%
  select(variable, missing, mean, sd, min, median, max, hist) %>%
  # drop p25 and p75 for purposes of display
  kable() %>%
  kable_styling(font_size = 10)
detach(package:skimr)
detach(package:kableExtra)
boxplot(log(d$Body_mass_female_mean)~d$Family)
#alternatively with ggplot
p <- ggplot(data=d, aes(x=Family, y=log(Body_mass_female_mean)))
p <- p + geom_boxplot(na.rm=TRUE)
p <- p + theme(axis.text.x=element_text(angle=90))
p <- p + ylab("log(Female Body Mass)")
p
#plotting relationship between female body size and female brain size 
library(skimr)
library(kableExtra)
s %>%
  filter(skim_variable == "Scientific_Name" | skim_type == "numeric") %>%
  rename(variable=skim_variable, missing=n_missing, mean=numeric.mean,
         sd=numeric.sd, min=numeric.p0, p25=numeric.p25, median=numeric.p50,
         p75=numeric.p75, max=numeric.p100, hist=numeric.hist) %>%
  select(variable, missing, mean, sd, min, median, max, hist) %>%
  # drop p25 and p75 for purposes of display
  kable() %>%
  kable_styling(font_size = 10)
detach(package:skimr)
detach(package:kableExtra)
plot(d$Brain_Size_Species_Mean~d$Body_mass_female_mean)#normal scatterplot
plot(log(d$Brain_Size_Species_Mean)~log(d$Body_mass_female_mean))#log-transformed scatterplot

#colored log-transformed brain mass/body mass scatterplot
p <- ggplot(data=d, aes(x=log(Body_mass_female_mean),
                        y=log(Brain_Size_Female_Mean),
                        color = factor(Family)
)) # first, we build a plot object and color points by Family
# then we modify the axis labels
p <- p + xlab("log(Female Body Mass)") + ylab("log(Female Brain Size)")
# then we make a scatterplot
p <- p + geom_point(na.rm=TRUE)
# then we modify the legend
p <- p + theme(legend.position="bottom", legend.title=element_blank())
# and, finally, we plot the object
p

library(ggExtra)
ggMarginal(p, type = "densigram") # try with other types, too!


library(car)

scatterplot(
  data = d,
  log(Brain_Size_Female_Mean) ~ log(Body_mass_female_mean),
  xlab = "log(Female Body Mass",
  ylab = "log(Female Brain Size",
  boxplots = "xy",
  regLine = 
    list(
      # method = lm,
      lty = 1,
      lwd = 2,
      col = "red"
    )
)























































































