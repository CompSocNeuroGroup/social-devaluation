// Mean
data { 
int<lower=0> n;
real x[n];
//vector[2] sigmaerror;
}

parameters {
real mu;
real sigma;
} 


model {

mu ~ normal(0, 10);
sigma ~ cauchy(0, 2);

// Data
x ~ normal(mu, sigma);
}
