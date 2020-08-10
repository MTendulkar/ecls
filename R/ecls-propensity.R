
# This is to be run after ecls-clean.R, which pulls in the raw data, 
# cleans it, converts variables to dummies, and standardizes math scores. 

setwd('~/Documents/GitHub/ecls/data-processed/')


library(MatchIt)
library(dplyr)
library(ggplot2)


# Read the data in
ecls <- read.csv("ecls.csv")

# Pipe into a summary stats table for inspection
ecls %>%
  group_by(catholic) %>%
  summarise(n_students = n(),
            mean_math = mean(c5r2mtsc_std),
            std_error = sd(c5r2mtsc_std) / sqrt(n_students))

# Standardize the math score, confirm that mean of standardized scores is unchanged
ecls %>%
  mutate(test = (c5r2mtsc - mean(c5r2mtsc)) / sd(c5r2mtsc)) %>% 
  group_by(catholic) %>%
  summarise(std_math = mean(test))

# Naive: are math scores dissimilar between Catholic/non-Catholic school students? 
with(ecls, t.test(c5r2mtsc_std ~ catholic))

# Ok but what else is different between groups? 
ecls_cov <- c('race_white', 'p5hmage', 'w3income', 'p5numpla', 'w3momed_hsb')
ecls %>%
  group_by(catholic) %>%
  select(one_of(ecls_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

# Run t-test on covariates identified above
lapply(ecls_cov, function(v) {
  t.test(ecls[, v] ~ ecls[, 'catholic'])
})

# Alternative way of testing a single variable
# Flexing the 'with' muscle
with(ecls, t.test(race_white ~ catholic))

# Transform the income variable
ecls <- ecls %>% mutate(w3income_1k = w3income / 1000)

# Create a generalized linear model
# Model probability of being in Catholic school according to covariates
m_ps <- glm(catholic ~ race_white + w3income_1k + p5hmage + p5numpla + w3momed_hsb,
            family = binomial(), data = ecls)
summary(m_ps)

# Create new DF that predicts propensity of student to be in Catholic school, 
# assigned to pr_score. Actual label is in $catholic. 
prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     catholic = m_ps$model$catholic)
head(prs_df)


# Create two labels
labs <- paste("Actual school type attended:", c("Catholic", "Public"))


# Plot prop score distribution according to actual label
# Note that actual-Catholic students have a flat-ish distribution, 
# concentrated between 0 and 0.4 Public school kids have a high 
# peak at low propensity. This is a start, but not a great classifier. 
prs_df %>%
  mutate(catholic = ifelse(catholic == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~catholic) +
  xlab("Probability of going to Catholic school") +
  theme_bw()


# MatchIt does not allow missing values
# Omit missing values and pipe into a clean DF
ecls_nomiss <- ecls %>%  
  select(c5r2mtsc_std, catholic, one_of(ecls_cov)) %>%
  na.omit()

# Performs nearest neighbor matching on pre-treatment covariates and binary 
# treatment indicator 
mod_match <- matchit(catholic ~ race_white + w3income + p5hmage + p5numpla + w3momed_hsb,
                     method = "nearest", data = ecls_nomiss)

# Examine results. Not sure how to read summary() yet. Visualization is intuitive. 
summary(mod_match)
plot(mod_match)

# match.data() takes matchit object and returns the matched units with their distance, 
# weights, and subclass
dta_m <- match.data(mod_match)
dim(dta_m)

# Plot each student's value against their propensity score, for each variable. 
# Both raw data and fit will be shown. We are looking for the two treatment 
# classes to be well-matched (i.e. curves should look equivalent in all variables)
fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  if (variable == 'w3income') dta$variable <- dta$variable / 10^3
  dta$catholic <- as.factor(dta$catholic)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = catholic)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

# Grid plot
library(gridExtra)
grid.arrange(
  fn_bal(dta_m, "w3income"),
  fn_bal(dta_m, "p5numpla") + theme(legend.position = "none"),
  fn_bal(dta_m, "p5hmage"),
  fn_bal(dta_m, "w3momed_hsb") + theme(legend.position = "none"),
  fn_bal(dta_m, "race_white"),
  nrow = 3, widths = c(1, 0.8)
)

# Individual plots for e.g. presentation purposes
fn_bal(dta_m, "w3income")
fn_bal(dta_m, "p5numpla") + theme(legend.position = "none")
fn_bal(dta_m, "p5hmage")
fn_bal(dta_m, "w3momed_hsb") + theme(legend.position = "none")
fn_bal(dta_m, "race_white")

# We've matched to our satisfaction.
# Now, pipe matched data into mean calculation for inspection. 
# All covariate means should be equal (or close) between groups. 
dta_m %>%
  group_by(catholic) %>%
  select(one_of(ecls_cov)) %>%
  summarise_all(funs(mean))

# Apply t-test to matched data set. When we have two students who 
# have an identical propensity to attend Catholic school, based on 
# their covariates, then do we see an actual difference in test scores
# if one DOES attend Catholic school? 
lapply(ecls_cov, function(v) {
  t.test(dta_m[, v] ~ dta_m$catholic)
})


# Turns out, no! 