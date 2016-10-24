model{

  for(p in 1:NPRED){

    pred_s[p] ~ dnorm(mu_pred_s[p],tau)
    mu_pred_s[p] <- grandmu + beta*cov[p] + speciesmu[species_pred[p]] + genusmu[genus_pred[p]] + familymu[family_pred[p]] + ordermu[order_pred[p]]

    pred_g[p] ~ dnorm(mu_pred_g[p],tau)
    mu_pred_g[p] <- grandmu + beta*cov[p] + species_predict[p] + genusmu[genus_pred[p]] + familymu[family_pred[p]] + ordermu[order_pred[p]]

    pred_f[p] ~ dnorm(mu_pred_f[p],tau)
    mu_pred_f[p] <- grandmu + beta*cov[p] + species_predict_u[p] + genus_predict[p] + familymu[family_pred[p]] + ordermu[order_pred[p]]

    pred_o[p] ~ dnorm(mu_pred_o[p],tau)
    mu_pred_o[p] <- grandmu + beta*cov[p] + species_predict_u[p] + genus_predict_u[p] + family_predict[p] + ordermu[order_pred[p]]

    species_predict[p] <- genus.xi[genus_pred[p]]*species.eta_pred[p]
    species_predict_u[p] <- genus.xi_pred*species.eta_pred[p]
    species.eta_pred[p] ~ dnorm(0,genus.prec)

    genus_predict[p] <- family.xi[family_pred[p]]*genus.eta_pred[p]
    genus_predict_u[p] <- family.xi_pred*genus.eta_pred[p]
    genus.eta_pred[p] ~ dnorm(0,family.prec)

    family_predict[p] <- order.xi[order_pred[p]]*family.eta_pred[p]
    family.eta_pred[p] ~ dnorm(0,order.prec)

  }

  for(i in 1:NOBS){
    lDD[i] ~ dnorm(mu[i],tau)
    mu[i] <- grandmu + beta*cov[i]+ speciesmu[species[i]] + genusmu[genus[i]] + familymu[family[i]] + ordermu[order[i]]

  }

  beta ~ dnorm(0,1e-9)

  for(s in 1:NSPECIES){

    speciesmu[s] <- genus.xi[genus[s]]*species.eta[s]
    species.eta[s] ~ dnorm(0,genus.prec)

  }

  # genus fx from order distributions
  for(g in 1:NGENUS){
    # genus mean drawn from family dist
    genusmu[g] <- family.xi[family[g]]*genus.eta[g]
    # genus tau drawn from hierarchical distr over all families
    genus.eta[g] ~ dnorm(0,family.prec)
    genus.xi[g] ~ dnorm(0,genus.scale)
    sigma.species[g] <- abs(genus.xi[g])/sqrt(genus.prec)
  }

  # family fx from order distribution
  for(f in 1:NFAMILIES){
    # family mean drawn from order dist
     familymu[f] <- order.xi[order[f]]*family.eta[f]
      family.eta[f] ~ dnorm(0,order.prec)
    family.xi[f] ~ dnorm(0,family.scale)
    sigma.genus[f] <- abs(family.xi[f])/sqrt(family.prec)
  }

  # order fx from class distribution
  for(o in 1:NORDERS){
    # order mean drawn from order dist
    ordermu[o] <- grand.xi*order.eta[o]
    order.eta[o] ~ dnorm(0,grand.prec)
    order.xi[o] ~ dnorm(0,order.scale)
    sigma.family[o] <- abs(order.xi[o])/sqrt(order.prec)
  }

  # priors tax hierachy
  species.prec ~ dgamma(0.5,0.5)

  family.scale  ~ dgamma(2,hc_scale)
  family.prec ~ dgamma(0.5,0.5)
  family.xi_pred ~ dnorm(0,family.scale)

  genus.scale  ~ dgamma(2,hc_scale)
  genus.prec ~ dgamma(0.5,0.5)
  genus.xi_pred ~ dnorm(0,genus.scale)

  order.scale   ~ dgamma(2,hc_scale)
  order.prec ~ dgamma(0.5,0.5)

  grand.xi ~ dnorm(0,2/hc_scale)
  grand.prec ~ dgamma(0.5,0.5)
  sigma.order <- abs(grand.xi)/sqrt(grand.prec)

  grandmu ~ dnorm(2.45,0.2)
  hc_scale ~ dunif(0.00001,100000)

  # finite population sds
  sd.sigma.species <- sd(sigma.species)
  sd.sigma.genus <- sd(sigma.genus)
  sd.sigma.family <- sd(sigma.family)

  sd.genus <- sd(genusmu)
  sd.species <- sd(speciesmu)
  sd.family  <- sd(familymu)
  sd.order   <- sd(ordermu)
  sd.pop <- 1/sqrt(tau)
  # sd.continent_eff <- sd(continent_eff)

  tau ~ dgamma(1,1)

}
