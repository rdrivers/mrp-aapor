options(width=85)
library("tidyverse")
load("data/cleaned.RData")



##########################
# 2. Post-stratification #
##########################


## Recode pew data
pew <- pew %>%
  filter(
    complete.cases(age, raceeth, gender, educ, vote16)) %>%
  mutate(
    demvote = ifelse(vote16 == "clinton", 1, 0),
    age4 = factor(case_when(age < 30 ~ "18-29",
      age < 45 ~ "30-44", age < 65 ~ "45-64",
      TRUE ~ "65+")),
    race3 = fct_collapse(raceeth,
      white = c("white", "other")),
    educ4 = fct_collapse(educ,
      "hs" = c("grades 1-8", "hs dropout", "hs grad"),
      "some col" = c("some col", "assoc")))

# Save a full version for MNL
pew_nv <- pew 
pew <- pew %>% filter(vote16 != 'nonvoter')


## ...then do the same for CPS
cps <- cps %>%
  filter(
    complete.cases(age_top_codes,
      raceeth, gender, educ, turnout)) %>%
  mutate(
    age4 = factor(case_when(
      age_top_codes == "<80" & age < 30 ~ "18-29",
      age_top_codes == "<80" & age < 45 ~ "30-44",
      age_top_codes == "<80" & age < 65 ~ "45-64",
      TRUE ~ "65+")),
    race3 = fct_collapse(raceeth,
      white = c("white", "other")),
    educ4 = fct_collapse(educ,
      "hs" = c("grades 1-8", "hs dropout", "hs grad"),
      "some col" = c("some col", "assoc")))

# Save a full version for MNL
cps_nv <- cps
cps <- cps %>% filter(turnout == "yes")

## Check that the datasets are consistent -- mistakes will be made!
compare_distributions <- function(var, data1, data2, wgt1, wgt2, digits = 1) {
  stopifnot(all(levels(data1[[var]]) == levels(data2[[var]])))
  formula1 <- as.formula(paste(wgt1, "~", var))
  formula2 <- as.formula(paste(wgt2, "~", var))
  tbl <- rbind(round(100 * prop.table(xtabs(formula1, data1)), digits),
      round(100 * prop.table(xtabs(formula2, data2)), digits))
  row.names(tbl) <- c(substitute(data1), substitute(data2))
  tbl
}
compare_distributions("race3", pew, cps, "", "weight")


## Compare variables in `pew` and `cps`
compare_distributions("educ4", pew, cps, "", "weight")
compare_distributions("age4", pew, cps, "", "weight")
compare_distributions("gender", pew, cps, "", "weight")


## Step 2: create post-strata 
library(survey)
pop.counts <- xtabs(weight ~ age4 + gender + race3 + educ4, data = cps)
sample.counts <- xtabs(~ age4 + gender + race3 + educ4, data = pew)
pew <- mutate(pew,
  weight0 = sum(pop.counts) / sum(sample.counts))
sample.weights <- xtabs(weight0 ~ age4 + gender + race3 + 
    educ4, data = pew)
nr <- nonresponse(sample.weights, sample.counts, pop.counts)


## Check for empty cells and/or large weights
sparseCells(nr, nrweight = 4)


## Look for categories adjacent to empty cells
neighbours(14, nr) # use nr$index to get cell index


## Step 3: collapse cells
nr$index[,,"black","hs"]
nr <- joinCells(nr, 10, 11, 14, 15) # update the nr object
nr$index[,,"black","hs"]


## Eliminate remaining empty cells
nr <- joinCells(nr, 18, 19, 21, 22) # hisp 30-64 hs
nr <- joinCells(nr, 44, 48, 68, 72, 92, 96) # hisp 65+ >hs
nr <- joinCells(nr, 57, 61, 81, 85) # black 18-29 col+
sparseCells(nr, nrweight = 4) # no more bad cells


## Step 4: compute weights and add to dataframe
get_weights <- function(data, nr) {
  wgt_arr <- weights(nr)
  var.names <- names(dimnames(wgt_arr))
  indexes <- data %>%
    select(var.names) %>%
    mutate_all(as.integer) %>%
    as.matrix()
  wgt_arr[indexes]
}
pew$ps.weight <- get_weights(pew, nr)


## Check that the post-stratification worked
compare_distributions("race3", pew, cps, "ps.weight", "weight")
compare_distributions("educ4", pew, cps, "ps.weight", "weight")
compare_distributions("age4", pew, cps, "ps.weight", "weight")


## Step 5: compute estimates using the new weight
round(100 * prop.table(xtabs(ps.weight ~ vote16, pew)), 1)
design <- svydesign(ids = ~ 1, weights = ~ ps.weight, data = pew)
round(100 * prop.table(svytable(~ vote16, design)), 1)
cv <- function(x) sd(x) / mean(x) # coefficient of variation
cv(pew$ps.weight)^2 # weighting loss


## State estimates
tbl <- 100 * prop.table(xtabs(ps.weight ~ state + vote16, data = pew), 1)
round(tbl, 1)[1:10,]


## Plotting the estimates
estimates <- votes16 %>%
  transmute(state, name,
    actual = 100 * clinton / turnout) %>%
  mutate(post.stratified = tbl[,"clinton"])

library(ggplot2)
p1 <- ggplot(estimates, aes(actual, post.stratified)) +
  geom_abline(slope = 1, intercept = 0, col = "grey") +
  geom_point() +
  lims(x = c(0, 100), y = c(0, 100)) +
  labs(x = "Percent of votes", y = "Post-stratifed estimate") +
  theme_minimal()
p2 <- ggplot(estimates, aes(post.stratified - actual)) +
  geom_histogram(binwidth = 10, center = 0, fill = "gray") +
  labs(x = "Error in estimate") +
  theme_minimal()
library(gridExtra)
grid.arrange(p1, p2, nrow = 1)


## Mapping the estimates
library(maps)
library(mapproj)
us_map <- map_data("state")
estimates %>%
  mutate(state_name = tolower(name),
    clinton_pct = cut(post.stratified, breaks = c(-Inf, 40, 45, 50, 55, 60, 100),
      labels = c("<40", "40-45", "45-50", "50-55", "55-60", ">60"))) %>%
  ggplot(aes(map_id = state_name)) +
    geom_map(aes(fill = clinton_pct), map = us_map) +
    expand_limits(x = us_map$long, y = us_map$lat) +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    scale_fill_brewer(name = "Clinton %", type = "div", palette = "RdBu") +
    theme(axis.line = element_blank()) +
    theme_void()



############################
# 3. Multilevel regression #
############################


## Fixed effects (no pooling)
no.pooling <- lm(demvote ~ state - 1, data = pew)
no.pooling <- data_frame(
    state = gsub("state", "", names(coef(no.pooling))),
    no.pooling = 100 * coef(no.pooling)) %>%
  mutate(state = factor(state, levels = levels(pew$state)))
head(no.pooling)


## Grand mean (complete pooling)
round(100 * mean(pew$demvote), 1) # 1. mean of dichotomous indicator
round(100 * prop.table(xtabs(~ demvote, data = pew)), 1) # 2. cross-tabulation
complete.pooling <- lm(demvote ~ 1, data = pew) # 3. intercept in regression
complete.pooling <- 100 * coef(complete.pooling)
round(complete.pooling, 1)


## Predicting random effects
library(lme4)
partial.pooling <- lmer(demvote ~ 1 + (1 | state), data = pew)
partial.pooling


## Extracting predictions
fixef(partial.pooling) # grand mean
ranef(partial.pooling)$state %>% head(4) # state effects
coef(partial.pooling)$state %>% head(4) # state predictions


## Which is better?
partial.pooling <- coef(partial.pooling)$state %>%
  as_data_frame(rownames = "state") %>%
  transmute(state = factor(state, levels = levels(pew$state)),
    partial.pooling = 100 * `(Intercept)`)
estimates <- estimates %>%
  left_join(no.pooling, by = "state") %>%
  left_join(partial.pooling, by = "state") %>%
  mutate(n = as.integer(xtabs(~ state, data = pew)))
estimates
rmse <- function(est, act) sqrt(mean((est - act)^2, na.rm = TRUE))
RMSE <- estimates %>%
  summarize(complete.pooling = rmse(complete.pooling, actual),
    no.pooling = rmse(no.pooling, actual),
    partial.pooling = rmse(partial.pooling, actual)) %>%
  unlist()
RMSE


## 2016 U.S. election: estimates *vs.* actuals
p1 <- ggplot(estimates, aes(actual, no.pooling)) +
  geom_point() +
  labs(title = "No pooling", x = "Percent of voters",
    y = "Percent of voters") +
  lims(x = c(0,100), y = c(0,100)) +
  theme_minimal()
p2 <- ggplot(estimates, aes(actual, partial.pooling)) +
  geom_point() +
  labs(title = "Partial pooling", x = "Percent of voters",
    y = "Percent of voters") +
  lims(x = c(0,100), y = c(0,100)) +
  theme_minimal()
suppressMessages(library(gridExtra))
grid.arrange(p1, p2, nrow = 1)


## Shrinkage
na.omit(estimates) %>%
  select(-post.stratified) %>%
  rename("Actual" = actual, "No pooling" = no.pooling,
    "Partial pooling" = partial.pooling) %>%
  gather(Estimator, Percentage, -state, -name, -n) %>%
  mutate(Estimator = factor(Estimator, levels = c("Post-stratified",
    "No pooling", "Partial pooling", "Actual"))) %>%
  ggplot(aes(Estimator, Percentage, group = state, color = n)) +
    geom_line(size = 0.7) +
    scale_color_gradientn(colors = blues9[-1]) +
    lims(y = c(0, 100)) +
    theme_minimal()


## Complete pooling: common intercepts and slopes
pew <- mutate(pew, female = ifelse(gender == "female", 1, 0))
fit1 <- lm(demvote ~ 1 + female, data = pew)
arm::display(fit1)


## No pooling: separate intercepts and slopes
fit2 <- lm(demvote ~ 0 + state + state:female, data = pew)
coef.fit2 <- as.matrix(coef(fit2))
round(coef(fit2)[c(1:5, 90:98)], 2)
xtabs(~ gender + state, data = pew) # no men in WY
coef.fit2 <- coef(fit2)
coef.fit2 <- as.matrix(data.frame(intercept = coef.fit2[1:48],
  slope = coef.fit2[50:97], row.names =
  gsub("state", "", names(coef.fit2)[1:48])))


## Separate intercepts with common slope
fit3 <- lm(demvote ~ state + female - 1, data = pew)
coef.fit3 <- coef(fit3)
names(coef.fit3) <- gsub("state", "", names(coef.fit3))
round(coef.fit3, 2)


## Which should you believe?
par(mfrow=c(1,2))
plot.gender.by.state <- function(main, coefs, fixef) {
  n <- nrow(coefs)
  x <- rep(c(0,1), rep(n, 2))
  y <- 100 * c(coefs[[1]], coefs[[1]] + coefs[[2]])
  plot(x = x, y = y, pch = 19, cex = 0.5, xlim = c(0, 1),
    ylim = c(0, 100), main = main,xlab = "Gender",
    ylab = "Percent voting for Clinton", axes = FALSE)
  axis(1, at = c(0, 1), labels = c("Male", "Female"))
  axis(2, at = seq(0, 100, 20))
  for (i in seq_len(n)) lines(x = c(0, 1),
    y = c(y[i], y[i+n]), col = "grey", lwd = 0.5)
  abline(100 * fixef, col = "red3", lwd = 2)
}
plot.gender.by.state("No pooling", as_data_frame(coef.fit2)[-49,],
  coef(fit1))
coef.fit3.df <- data_frame(Intercept = coef.fit3[-length(coef.fit3)],
  Slope = coef.fit3[length(coef.fit3)])
plot.gender.by.state("Common slope", coef.fit3.df, coef(fit1))
par(mfrow=c(1,1))


## Random effects: varying intercepts, common slopes
fit4 <- lmer(demvote ~ 1 + female + (1 | state), data = pew)
fit4


## Random effects: varying intercepts and slopes (first try)
(fit5 <- lmer(demvote ~ 1 + female +  (1 + female | state),
  data = pew)) # fails to converge


## Fixing the convergence failure (by centering covariates)
pew <- mutate(pew, female.c = female - 0.5)
fit6 <- lmer(demvote ~ 1 + female.c + (1 + female.c | state),
  data = pew)
fixef(fit6) # not comparable to prior models
head(coef(fit6)$state)


## Unscale the estimates for comparability
fixef.fit6 <- fixef(fit6)
fixef.fit6[1] <- fixef.fit6[1] - 0.5 * fixef.fit6[2]
coef.fit6 <- coef(fit6)$state
coef.fit6[[1]] <- coef.fit6[[1]] - 0.5 * coef.fit6[[2]]
fixef.fit6
head(coef.fit6)


## Comparing the random effects estimates
par(mfrow=c(1,2))
plot.gender.by.state("Varying intercepts, common slope",
  coef(fit4)$state, fixef(fit4))
plot.gender.by.state("Varying intercepts and slopes",
  coef.fit6, fixef.fit6)
par(mfrow=c(1,1))



## Estimating models with covariates at both levels
obama12 <- votes12 %>%
  mutate(obama12 = obama / turnout) %>%
  select(state, obama12)
pew <- left_join(pew, obama12, by = "state")
fit7 <- lmer(demvote ~ 1 + female.c + obama12 + (1 + female.c | state), data = pew)
arm::display(fit7)


## First attempt at MRP
cps <- cps %>%
  mutate(female = ifelse(gender == "female", 1, 0),
    female.c = female - 0.5) %>%
  left_join(obama12, by = "state")
prob <- predict(fit7, newdata = cps, allow.new.levels = TRUE)
mrp1 <- cps %>%
  mutate(prob = prob) %>%
  group_by(state) %>%
  summarize(mrp1 = 100 * weighted.mean(prob, weight))
estimates <- left_join(estimates, mrp1, by = "state")
head(estimates)


## Plotting first MRP estimates
RMSE["mrp1"] <- with(estimates, rmse(mrp1, actual))
ggplot(estimates, aes(actual, mrp1)) +
  geom_abline(intercept = 0, slope = 1, col = "grey") +
  geom_point(size = 1.5) +
  lims(x = c(0, 100), y = c(0, 100)) +
  labs(x = "Percentage of vote for Clinton", y = "Estimate") +
  theme_minimal()


#########################
# 4. Bayesian inference #
#########################


## Example: What is the average airfare to AAPOR in Denver?
post.precis <- 1 / 100^2 + 3 / 150^2
post.mean <- (400 / 100^2 + 300 * 3 / 150^2) / post.precis
post.sd <- 1 / sqrt(post.precis)


## Bayesian updating: from prior to posterior
ggplot(data_frame(theta = c(0, 700) ), aes(theta)) +
  stat_function(fun = dnorm, color = "red",
    args = list(mean = 300, sd = 200 / sqrt(3))) +
  stat_function(fun = dnorm, color = "blue",
    args = list(mean = 400, sd = 100)) +
  stat_function(fun = dnorm, color = "purple",
    args = list(mean = post.mean, sd = post.sd)) +
  labs(x = quote(theta), y = "") +
  scale_x_continuous(breaks = seq(0, 700, 100)) +
  scale_y_continuous(breaks = NULL) +
  theme_minimal()


## A first Stan program
model_code <- "data {
  int n;
  real y[n];
  real theta_0;
  real<lower=0> omega_0;
  real sigma;
}
parameters {
  real theta;
  }
model {
  theta ~ normal(theta_0, omega_0);
  for (i in 1:n) {
    y[i] ~ normal(theta, sigma);
  }
}"


## Example (continued)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
y <- c(161, 250, 489)
data <- list(y = y, n = length(y), theta_0 = 350,
  omega_0 = 150, sigma = 100)
sims <- stan(model_code = model_code, data = data,
  chains = 4, iter = 500, seed = 1234)


## Stan output
print(sims)


## Stan code for estimating both mean and SD
data <- list(y = y, n = length(y), theta_0 = 350, omega_0 = 150)
model_code <- "data {
  int n;
  real y[n];
  real theta_0;
  real omega_0;
}
parameters {
  real theta;
  real<lower=0> sigma; // moved to parameter block
}
model {
  theta ~ normal(theta_0, omega_0);
  sigma ~ normal(150, 150);
  y ~ normal(theta, sigma); // assumed iid
}"
sims <- stan(model_code = model_code, data = data,
  iter = 500, seed = 1234)


## Output from two-parameter model
print(sims)


## Graphical display of parameters
plot(sims)


## Traceplot
traceplot(sims)


## First hierarchical model in Stan
data <- with(pew, list(y = demvote, group = as.integer(state),
  n = nrow(pew), J = nlevels(state)))
code <- "data {
  int n; // number of respondents
  int J; // number of groups (states)
  int<lower=0, upper=1> y[n]; // demvote
  int<lower=1, upper=J> group[n]; // state index
}
parameters {
  real mu_theta; // hyper parameters
  real<lower=0> sigma_theta; 
  vector[J] theta; // group parameters
}
model {
  sigma_theta ~ normal(0, 5);
  for (j in 1:J)
    theta[j] ~ normal(mu_theta, sigma_theta);
  for (i in 1:n)
    y[i] ~ bernoulli_logit(theta[ group[i] ]);
}"
sims <- stan(model_code = code, data = data)


## Stan output
print(sims)


## A plot of the state estimates
names(sims) <- c("mu_theta", "sigma_theta", levels(pew$state), "lp__")
plot(sims, par = "theta")


## Add a covariate to the model
data <- with(pew, list(y = demvote, group = as.integer(state),
  x = female, n = nrow(pew), J = nlevels(state)))
code <- "data {
  int n; // number of respondents
  int J; // number of groups (states)
  int<lower=0, upper=1> y[n]; // demvote
  int<lower=1, upper=J> group[n]; // state index
  vector[n] x; // added covariate (gender)
}
parameters {
  real mu_alpha; // mean intercept
  real<lower=0> sigma_alpha; // sd intercept
  real beta; // coefficient of gender
  vector[J] alpha; // group intercepts
}
model {
  sigma_alpha ~ normal(0, 5);
  for (j in 1:J)
    alpha[j] ~ normal(mu_alpha, sigma_alpha);
  for (i in 1:n)
    y[i] ~ bernoulli_logit(alpha[group[i]] + beta * x[i]);
}"
sims <- stan(model_code = code, data = data)


## Simplifying the computations
X <- model.matrix(~ 1 + age4 + gender + race3 + educ4 +
    region + qlogis(obama12), data = pew)
head(X, 4)


## Hierarchical model with multiple covariates
model_code <- "data {
  int n; // number of respondents
  int k; // number of covariates
  matrix[n, k] X; // covariate matrix
  int<lower=0, upper=1> y[n]; // outcome (demvote)
  int J; // number of groups (states)
  int<lower=1, upper=J> group[n]; // group index
}
parameters {
  vector[k] beta; // fixed effects
  real<lower=0> sigma_alpha; // sd intercept
  vector[J] alpha; // group intercepts
}
model {
  vector[n] Xb;
  beta ~ normal(0, 4);
  sigma_alpha ~ normal(0.2, 1); // prior for sd
  alpha ~ normal(0, 1); // standardized intercepts
  Xb = X * beta;
  for (i in 1:n)
    Xb[i] += sigma_alpha * alpha[ group[i] ];
  y ~ bernoulli_logit(Xb);
}"


## Estimating the model in R
X <- model.matrix(~ 1 + age4 + gender + race3 + educ4 +
  region + qlogis(obama12), data = pew)
data <- list(n = nrow(X), k = ncol(X), X = X, y = pew$demvote,
  J = nlevels(pew$state), group = as.integer(pew$state))
sims <- stan(model_code = model_code, data = data,
  seed = 1234)
names(sims)


## Rename the coefficients for easier reading
coef.names <- c(colnames(X), "sigma_alpha", levels(pew$state), "lp__")
names(sims) <- coef.names
names(sims)


## Summary of fixed effect estimates
print(sims, par = "beta")


## Imputation in Stan
X0 <- model.matrix(~ 1 + age4 + gender + race3 + educ4 +
    region + qlogis(obama12), data = cps)
data <- list(n = nrow(X), k = ncol(X), X = X, y = pew$demvote,
  J = nlevels(pew$state), group = as.integer(pew$state),
  N = nrow(X0), X0 = X0, group0 = as.integer(cps$state))


## The complete Stan program
model_code <- "data {
  int n; // number of respondents
  int k; // number of covariates
  matrix[n, k] X; // covariate matrix
  int<lower=0, upper=1> y[n]; // outcome (demvote)
  int J; // number of groups (states)
  int<lower=1, upper=J> group[n]; // group index
  int N; // population size
  matrix[N, k] X0; // population covariates
  int group0[N]; // group index in population
}
parameters {
  vector[k] beta; // fixed effects
  real<lower=0> sigma_alpha; // sd intercept
  vector[J] alpha; // group intercepts
}
model {
  vector[n] Xb;
  beta ~ normal(0, 4);
  sigma_alpha ~ normal(0.2, 1);
  alpha ~ normal(0, 1);
  Xb = X * beta;
  for (i in 1:n)
    Xb[i] += sigma_alpha * alpha[ group[i] ];
  y ~ bernoulli_logit(Xb);
}
generated quantities {
  int<lower=0, upper=1> yimp[N];
  {
    vector[N] Xb0;
    Xb0 = X0 * beta;
    for (i in 1:N)
      yimp[i] = bernoulli_logit_rng(Xb0[i] + sigma_alpha * alpha[ group0[i] ]);
  }
}"
sims <- stan(model_code = model_code, data = data,
  seed = 1234)


## Extracting the simulations
imputations <- extract(sims, pars = "yimp")$yimp[sample(
    nrow(sims), size = 500), ]
get_state_estimates <- function(imputations) {
  state_by_clinton <- function(imputed_values) 100 * prop.table(
    xtabs(weight ~ state + imputed_values, data = cps), 1)[,"1"]
  state_estimates <- apply(imputations, 1, state_by_clinton)
  apply(state_estimates, 1, mean)
}
estimates$mrp2 <- get_state_estimates(imputations)
RMSE["mrp2"] <- with(estimates, rmse(mrp2, actual))


## The easy way with `rstanarm`
library(rstanarm)
fit <- stan_glmer(demvote ~ 1 + age4 + gender + race3 + educ4 +
    region + qlogis(obama12) + (1 | state), data = pew, family = binomial)
imputations <- posterior_predict(fit, draws = 500,
  newdata = select(cps, age4, gender, race3, educ4, region, obama12, state))


## The complete program in `rstanarm`
library(rstanarm)
fit <- stan_glmer(demvote ~ 1 + age4 + gender + race3 + educ4 +
    region + qlogis(obama12) + (1 | state), data = pew, family = binomial)
cpstmp <- cps %>%
  select(age4, gender, race3, educ4, region, obama12, state)
imputations <- posterior_predict(fit, draws = 500,
  newdata = select(cps, age4, gender, race3, educ4, region, obama12, state))
estimates$mrp3 <- get_state_estimates(imputations)
RMSE["mrp3"] <- with(estimates, rmse(mrp3, actual))
RMSE


## Accuracy of state level estimates
p1 <- ggplot(estimates, aes(actual, mrp3)) +
  geom_abline(intercept = 0, slope = 1, col = "grey") +
  geom_point(size = 1.5) +
  lims(x = c(0, 100), y = c(0, 100)) +
  labs(x = "Percentage of vote for Clinton", y = "Estimate") +
  theme_minimal()
p2 <- ggplot(estimates, aes(mrp3 - actual)) +
  geom_histogram(binwidth = 4, center = 0, fill = "gray") +
  lims(x = c(-20, 20)) +
  labs(x = "Error in estimate") +
  theme_minimal()
grid.arrange(p1, p2, nrow = 1)


## What the map now looks like
estimates %>%
  mutate(state_name = tolower(name),
    clinton_pct = cut(mrp3, breaks = c(-Inf, 40, 45, 50, 55, 60, 100),
      labels = c("<40", "40-45", "45-50", "50-55", "55-60", ">60"))) %>%
  ggplot(aes(map_id = state_name)) +
    geom_map(aes(fill = clinton_pct), map = us_map) +
    expand_limits(x = us_map$long, y = us_map$lat) +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    scale_fill_brewer(name = "Clinton %", type = "div", palette = "RdBu") +
    theme(axis.line = element_blank()) +
    theme_void()



######################
# 5. Advanced Topics #
######################

## Allow race and gender effects to vary across states
fit <- stan_glmer(demvote ~ 1 + age4 + gender + race3 + educ4 +
  region + qlogis(obama12) + (1 + gender + race3 | state),
  data = pew, family = binomial)
cpstmp <- cps %>%
  select(age4, gender, race3, educ4, region, obama12, state)
imputations <- posterior_predict(fit, draws = 500,
  newdata = select(cps, age4, gender, race3, educ4, region, obama12, state))
estimates$mrp4 <- get_state_estimates(imputations)
RMSE["mrp4"] <- with(estimates, rmse(mrp4, actual))
RMSE

######################
# 6. MNL             #
######################

library(brms)

# Includes Non-Voters
pew_nv <- left_join(pew_nv, obama12, by = "state")
cps_nv <- left_join(cps_nv, obama12, by = "state")

fit <- brm(vote16 ~ 1 + age4 + gender + race3 + educ4 +
           region + qlogis(obama12) + (1 | state), data = pew_nv,
           family = categorical, chains = 4, cores = 4)
imputations <- posterior_predict(fit, nsamples = 500, allow_new_levels = TRUE,
               newdata = select(cps_nv, age4, gender, race3, educ4, region, obama12, state))

state_table <- function(imp){
  100 * prop.table(xtabs(weight ~ state + imp, data = cps_nv), 1)
}

tbls <- array(apply(imputations[1:10,], 1, state_table),
              dim=c(51, 4, 10), 
              dimnames=list(state=levels(cps_nv$state),
                            vote16=levels(pew_nv$vote16),
                            samples = 1:10))
estimate <- apply(tbls, c('state', 'vote16'), mean)
sd <- apply(tbls, c('state', 'vote16'), sd)

