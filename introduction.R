source("equations.R")
#### Introduction: Bayes' theorem ####

# posterior/confidence after a single study (theta=.5)
eq3(alpha=.05, beta=.05, theta=.5)

# confidence after N studies
bayes(alpha=.05, beta=.05, theta=.5, N=1, positive=1)
bayes(alpha=.05, beta=.05, theta=.5, N=2, positive=1)
bayes(alpha=.05, beta=.05, theta=.5, N=3, positive=1)

# non-conflicting evidence after 3 studies 
.95^3

# confidence after N studies (theta=.001)
bayes(alpha=.05, beta=.05, theta=.001, N=1, positive=1)
bayes(alpha=.05, beta=.05, theta=.001, N=2, positive=1)
bayes(alpha=.05, beta=.05, theta=.001, N=3, positive=1)
bayes(alpha=.05, beta=.05, theta=.001, N=4, positive=1)
bayes(alpha=.05, beta=.05, theta=.001, N=5, positive=1)

# non-conflicting evidence after 5 studies 
.95^5

#### Statistical power ####

# posterior/confidence after a single study with poor power (theta=.5)
eq3(alpha=.05, beta=.80, theta=.5)

# confidence after N studies with poor power
bayes(alpha=.05, beta=.80, theta=.5, N=1, positive=1)
bayes(alpha=.05, beta=.80, theta=.5, N=2, positive=1)
bayes(alpha=.05, beta=.80, theta=.5, N=3, positive=1)
bayes(alpha=.05, beta=.80, theta=.5, N=4, positive=1)
bayes(alpha=.05, beta=.80, theta=.5, N=5, positive=1)

# non-conflicting evidence after 3 & 5 studies 
.20^3
.20^5

# confidence after N studies (theta=.001)
bayes(alpha=.05, beta=.80, theta=.001, N=5, positive=1)
bayes(alpha=.05, beta=.80, theta=.001, N=10, positive=1)

# non-conflicting evidence after 10 studies 
.20^10

#### Table 1 ####
eq3(alpha=.05, beta=1-.90, theta=.1)

#### Bias ####

# expected positive findings
.95*.95 + .05*.05
.90*.90 + .10*.05
.85*.85 + .15*.05

# binomial test of 35 reproduced findings from 97 trials 
# assuming a minimum of 84% reproducibility (zero bias)
binom.test(35, 97, .84)

# threshold of >50% significant findings
.70*.70+.30*.05
eq3(alpha=.05, beta=1-.70, theta=.70)
.70*eq3(alpha=.05, beta=1-.70, theta=.70)

#### Figure 1 ####

source("figure1.R")


