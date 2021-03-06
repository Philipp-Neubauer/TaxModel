model{

  for(p in 1:NPRED){

    pred_s[p] ~ dnorm(mu_pred_s[p],tau)
    mu_pred_s[p] <- grandmu + beta*cov[p] + speciesmu[species_pred[p]] + genusmu[genus_pred[p]] + familymu[family_pred[p]] + ordermu[order_pred[p]]
    
    pred_g[p] ~ dnorm(mu_pred_g[p],tau)
    mu_pred_g[p] <- grandmu + beta*cov[p] + species_predict[p] + genusmu[genus_pred[p]] + familymu[family_pred[p]] + ordermu[order_pred[p]]
    
    pred_f[p] ~ dnorm(mu_pred_f[p],tau)
    mu_pred_f[p] <- grandmu + beta*cov[p] + species_predict[p] + genus_predict[p] + familymu[family_pred[p]] + ordermu[order_pred[p]]
    
    pred_o[p] ~ dnorm(mu_pred_o[p],tau)
    mu_pred_o[p] <- grandmu + beta*cov[p]+ species_predict[p] + genus_predict[p] + family_predict[p] + ordermu[order_pred[p]]
    
    species_predict[p] <- genus.xi*species.eta_pred[p]
    species.eta_pred[p] ~ dnorm(0,genus.prec)
    
    genus_predict[p] <- family.xi*genus.eta_pred[p]
    genus.eta_pred[p] ~ dnorm(0,family.prec)
 
    family_predict[p] <- order.xi*family.eta_pred[p]
    family.eta_pred[p] ~ dnorm(0,order.prec)
    
  }
  
  for(i in 1:NOBS){
    lDD[i] ~ dnorm(mu[i],tau)
    mu[i] <- grandmu + beta*cov[i] + speciesmu[species[i]] + genusmu[genus[i]] + familymu[family[i]] + ordermu[order[i]]
    
  }
  
  beta ~ dnorm(0,1e-9)
  
  for(i in 1:NSPECIES){
    
    speciesmu[i] <- genus.xi*species.eta[i]
    species.eta[i] ~ dnorm(0,genus.prec)
    
  }
  
  # genus fx from order distributions
  for(g in 1:NGENUS){
    # genus mean drawn from family dist
    genusmu[g] <- family.xi*genus.eta[g]
    # genus tau drawn from hierarchical distr over all families
    genus.eta[g] ~ dnorm(0,family.prec)
  }
  
  # family fx from order distribution
  for(f in 1:NFAMILIES){
    # family mean drawn from order dist
     familymu[f] <- order.xi*family.eta[f]
      family.eta[f] ~ dnorm(0,order.prec)
  }
  
  # order fx from class distribution
  for(o in 1:NORDERS){
    # order mean drawn from order dist
    ordermu[o] <- grand.xi*order.eta[o]
    order.eta[o] ~ dnorm(0,grand.prec)
    
  }
  
  # priors tax hierachy
  genus.xi ~ dnorm(0,hc_scale)
  genus.prec ~ dgamma(0.5,0.5)
  sigma.species <- abs(genus.xi)/sqrt(genus.prec)
  
  family.xi ~ dnorm(0,hc_scale)
  family.prec ~ dgamma(0.5,0.5)
  sigma.genus <- abs(family.xi)/sqrt(family.prec)

  order.xi ~ dnorm(0,hc_scale)
  order.prec ~ dgamma(0.5,0.5)
  sigma.family <- abs(order.xi)/sqrt(order.prec)
  
  grand.xi ~ dnorm(0,hc_scale)
  grand.prec ~ dgamma(0.5,0.5)
  sigma.order <- abs(grand.xi)/sqrt(grand.prec) 
  
  grandmu ~ dnorm(2.45,0.2)
  hc_scale ~ dgamma(1e-9,1e-9)
  
  # finite population sds
  sd.genus <- sd(genusmu)
  sd.species <- sd(speciesmu)
  sd.family  <- sd(familymu)
  sd.order   <- sd(ordermu)
  sd.pop <- 1/sqrt(tau)
  # sd.continent_eff <- sd(continent_eff)
  
  tau ~ dgamma(0.01,0.01)
  
}