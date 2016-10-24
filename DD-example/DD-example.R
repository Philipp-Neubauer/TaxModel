require(dplyr)
require(INLA)

DD <- read.csv("../DD-example/DD.csv",stringsAsFactors = F)

DD <- DD %>% filter(!is.na(EADDC) & !is.na(BDT.C))
DD<- DD[-c(522,1003,1555),]
# need numbers
n <- function(x) length(unique(x))
nidx <- function(x) as.numeric(factor(x,levels=unique(x)))

summary(inla(log10(EADDC)~BDT.C + f(Order,model='iid')+f(as.numeric(Species),model='iid'),data=DD,control.compute=list(dic=TRUE)))

SPECIES = nidx(DD$Species)

## matching taxonomy for taxonomy indices
# match unique species to genus

GE <- nidx(DD$Genus)

# match unique species to family
FAM <- nidx(DD$Family)

# match unique family to order
ORD <- nidx(DD$Order)


lpred <- 50
pred = sample.int(n = length(DD$BDT.C),lpred)
DDdata <- list(
               lDD = (DD$BDT.C),
               pred =pred ,
               NOBS = nrow(DD),
               cov=rep(1, nrow(DD)),
               beta=1,
               NPRED = lpred,
               NSPECIES = n(DD$Species),
               NGENUS = n(DD$Genus),
               NFAMILIES = n(DD$Family),
               NORDERS = n(DD$Order),
               species = SPECIES,
               genus = GE,
               family = FAM,
               order = ORD,
               species_pred = SPECIES[pred],
               genus_pred = GE[pred],
               family_pred = FAM[pred],
               order_pred = ORD[pred]
)

require(R2jags)
source('jags.R')

#DD_rfx_h
#traceplot(DD_rfx_h)
DDdata$pscale <- 1e-9
DD_rfx_s <- jags.parallel(model.file = 'DD-model_rfx_s.R',
                          n.iter = 35000,
                          n.burnin = 5000,
                          DIC = T,
                          n.thin = 10,
                          data=DDdata,
                          n.chains = 3,
                          parameters.to.save = c('sd.species',
                                                 'sd.genus',
                                                 'sd.family',
                                                 'sd.order',
                                                 'genus.scale',
                                                 'family.scale',
                                                 'order.scale',
                                                 'grandmu',
                                                 'grand.xi',
                                                 'pred_g',
                                                 'pred_f',
                                                 'mu',
                                                 'pred_o',
                                                 'pred_s',
                                                 'sigma.species',
                                                 'sigma.genus',
                                                 'sigma.family',
                                                 'sigma.order',
                                                 'sd.pop'))

DDdata$pscale <- 10
DD_rfx_si <- jags.parallel(model.file = 'DD-model_rfx_s.R',
                           n.iter = 35000,
                           n.burnin = 5000,
                           DIC = T,
                           n.thin = 10,
                           data=DDdata,
                           n.chains = 3,
                           parameters.to.save = c('sd.species',
                                                  'sd.genus',
                                                  'sd.family',
                                                  'sd.order',
                                                  'genus.scale',
                                                  'family.scale',
                                                  'order.scale',
                                                  'grandmu',
                                                  'grand.xi',
                                                  'pred_g',
                                                  'pred_f',
                                                  'pred_o',
                                                  'pred_s',
                                                  'mu',
                                                  'sigma.species',
                                                  'sigma.genus',
                                                  'sigma.family',
                                                  'sigma.order',
                                                  'sd.pop'))


DD_rfx_u <- jags.parallel(model.file = 'DD-model_rfx_u.R',
                          n.iter = 35000,
                          n.burnin = 5000,
                          DIC = T,
                          n.thin = 10,
                          data=DDdata,
                          n.chains = 3,
                          parameters.to.save = c('sd.species',
                                                 'sd.genus',
                                                 'sd.family',
                                                 'sd.order',
                                                 'genus.scale',
                                                 'family.scale',
                                                 'order.scale',
                                                 'grandmu',
                                                 'grand.xi',
                                                 'mu',
                                                 'pred_g',
                                                 'pred_f',
                                                 'pred_o',
                                                 'pred_s',
                                                 'sigma.species',
                                                 'sigma.genus',
                                                 'sigma.family',
                                                 'sigma.order',
                                                 'sd.pop',
                                                 'hc_scale'))


DD_rfx_g <- jags.parallel(model.file = 'DD-model_rfx_g.R',
                          n.iter = 35000,
                          n.burnin = 5000,
                          DIC = T,
                          n.thin = 10,
                          data=DDdata,
                          n.chains = 3,
                          parameters.to.save = c('sd.species',
                                                 'sd.genus',
                                                 'sd.family',
                                                 'sd.order',
                                                 'genus.scale',
                                                 'family.scale',
                                                 'order.scale',
                                                 'grandmu',
                                                 'grand.xi',
                                                 'pred_g',
                                                 'pred_f',
                                                 'pred_o',
                                                 'pred_s',
                                                 'mu',
                                                 'sigma.species',
                                                 'sigma.genus',
                                                 'sigma.family',
                                                 'sigma.order',
                                                 'sd.pop',
                                                 'hc_scale'))



#DD_rfx
#traceplot(DD_rfx)


DD_rfx_h <- jags.parallel(model.file = 'DD-model_rfx_h.R',
                          n.iter = 35000,
                          n.burnin = 5000,
                          DIC = T,
                          n.thin = 10,
                          data=DDdata,
                          n.chains = 3,
                          parameters.to.save = c('sd.species',
                                                 'sd.genus',
                                                 'sd.family',
                                                 'sd.order',
                                                 'mu',
                                                 'genus.scale',
                                                 'family.scale',
                                                 'order.scale',
                                                 'grandmu',
                                                 'grand.xi',
                                                 'pred_g',
                                                 'pred_f',
                                                 'pred_o',
                                                 'pred_s',
                                                 'sd.sigma.species',
                                                 'sd.sigma.genus',
                                                 'sd.sigma.family',
                                                 'sd.pop',
                                                 'hc_scale'
                          ))

as.mcmc.rjags <- function (x, subs=NULL,offs=100, ...)
{
  x <- x$BUGSoutput
  mclist <- vector("list", x$n.chains)
  mclis <- vector("list", x$n.chains)
  strt <- x$n.burnin + offs
  end <- x$n.iter
  if (is.null(subs)) {
    ord <- order(dimnames(x$sims.array)[[3]])
  } else {
    ord <- which(!grepl(subs,dimnames(x$sims.array)[[3]]))
  }
  for (i in 1:x$n.chains) {
    tmp1 <- x$sims.array[, i, ord]
    mclis[[i]] <- mcmc(tmp1, start = strt, end = end, thin = x$n.thin)
  }
  as.mcmc.list(mclis)
}

plot(as.mcmc(DD_rfx_g,subs='pred'))

save(DD,pred,DD_rfx_s,DD_rfx_si,DD_rfx_g,DD_rfx_u,DD_rfx_h,file='DD_model_runs.Rdata')
