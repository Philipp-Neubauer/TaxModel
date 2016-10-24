model{ 
 #data and prediction likelihood 
 for(i in 1:NOBS){ 
 # likelihood for loo and waic for observations only 
 log_lik[i] <- logdensity.norm(resp[i],mu[i],epsilon[i]) 
 # data model 
 resp[i] ~ dnorm(mu[i],epsilon[i]) 
 mu[i] <-betas[1:ncovs]%*%covs[i,1:ncovs]  + species_mu[taxix[i,1]]+genus_mu[taxix[i,2]]+family_mu[taxix[i,3]]+order_mu[taxix[i,4]]  } 
 # save predictions 
  # fixed and regression effects 
 for(b in 1:2) { 
betas[b] ~ dnorm(regr_mu[b],regr_tau[b]) 
} 
 # random effects 
  # taxonomic effects 
 for(r in 1:820) { 
species_mu[r] <- genus.xi[genus[r]]*species.eta[r] 
species.eta[r] ~ dnorm(0,genus.prec) 
species.xi[r] ~ dnorm(0,species_scale) 
} 
species_scale ~ dgamma(2,hyper_scale) 
genus.prec ~ dgamma(0.5,0.5) 
sd.species <- sd(species_mu) 

for(r in 1:678) { 
genus_mu[r] <- family.xi[family[r]]*genus.eta[r] 
genus.eta[r] ~ dnorm(0,family.prec) 
genus.xi[r] ~ dnorm(0,genus_scale) 
} 
genus_scale ~ dgamma(2,hyper_scale) 
family.prec ~ dgamma(0.5,0.5) 
sd.genus <- sd(genus_mu) 

for(r in 1:297) { 
family_mu[r] <- order.xi[order[r]]*family.eta[r] 
family.eta[r] ~ dnorm(0,order.prec) 
family.xi[r] ~ dnorm(0,family_scale) 
} 
family_scale ~ dgamma(2,hyper_scale) 
order.prec ~ dgamma(0.5,0.5) 
sd.family <- sd(family_mu) 

for(r in 1:51) { 
order_mu[r] <- grand.xi*order.eta[r] 
order.eta[r] ~ dnorm(0,grand.prec) 
order.xi[r] ~ dnorm(0,order_scale) 
} 
order_scale ~ dgamma(2,hyper_scale) 
grand.prec ~ dgamma(0.5,0.5) 
sd.order <- sd(order_mu) 
grand.xi ~ dnorm(0,2/hyper_scale) # phylo model 
  # scale model 
 hyper_scale ~ dunif(0.00001,100000) 
 # estimate epsilon if needed 
  }