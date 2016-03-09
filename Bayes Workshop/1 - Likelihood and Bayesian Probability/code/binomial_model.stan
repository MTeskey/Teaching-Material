data {
    int<lower=0> N; // Number of flips
    int h;          // Number of heads
}

parameters {
    real<lower=0, upper = 1> p; // Binomial probability
}

model {
    h ~ binomial(N, p);
    p ~ beta(1,1);
}