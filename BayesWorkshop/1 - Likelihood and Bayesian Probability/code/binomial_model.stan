data {
    int<lower=0> N; // Number of flips
    int k;          // Number of heads
}

parameters {
    real<lower=0, upper = 1> p; // Binomial probability
}

model {
    k ~ binomial(N, p);
    p ~ beta(1,1);
}