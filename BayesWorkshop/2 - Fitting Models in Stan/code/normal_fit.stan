data {
    int N;     // Sample size
    real x[N]; // Data
}

parameters {
    real mu;
}

model {
    x ~ normal(mu,1);
    mu ~ normal(0,1);
}