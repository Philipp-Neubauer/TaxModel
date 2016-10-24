require(dplyr)

sizes <- read.table("MOMv3.3.txt",sep='\t')
colnames(sizes) <- c("continent",'status','order','family','genus','species','lmass','cmass','ref')
sizes <- sizes %>%
  filter(lmass>-100) %>%
  unique() %>%
  group_by(order,family,genus,species) %>%
  summarise(lmass = mean(mean(lmass)))

# need numbers
n <- function(x) length(unique(x))
nidx <- function(x) as.numeric(factor(x,levels=unique(x)))

SPECIES = nidx(paste(sizes$genus,sizes$species,sep=' '))

## matching taxonomy for taxonomy indices
# match unique species to genus

GE <- nidx(sizes$genus)

# match unique species to family
FAM <- nidx(sizes$family)

# match unique family to order
ORD <- nidx(sizes$order)

lpred <- 50
pred = sample.int(n = length(sizes$lmass),lpred)
jagsdata <- list(pred =pred ,
                 NPRED = lpred,
                 lmass = sizes$lmass,
                 CONTINENTS = n(sizes$continent),
                 NSPECIES = n(paste(sizes$genus,sizes$species,sep=' ')),
                 NGENUS = n(sizes$genus),
                 NFAMILIES = n(sizes$family),
                 NORDERS = n(sizes$order),
                 species = SPECIES,
                 genus = GE[match(unique(SPECIES),SPECIES)],
                 family = FAM[match(unique(GE),GE)],
                 order = ORD[match(unique(FAM),FAM)],
                 genus_pred = GE[pred],
                 family_pred = FAM[pred],
                 order_pred = ORD[pred],
                 tau=1/(sizes$lmass*0.05)

)

require(R2jags)


# hierarchical tree model
JM <- jags.parallel(model.file = 'size-model.R',
                    n.iter = 12000,
                    n.burnin = 2000,
                    DIC = T,
                    n.thin = 5,
                    data=jagsdata,
                    n.chains = 3,
                    parameters.to.save = c('sd.species',
                                           'sd.genus',
                                           'sd.family',
                                           'sd.order',
                                           'mu',
                                           'pred_g',
                                           'pred_f',
                                           'pred_o',
                                           'pred_s',
                                           'species.scale',
                                           'genus.scale',
                                           'family.scale',
                                           'order.scale',
                                           'grandmu',
                                           'hc_scale'))



#JM
# traceplot(JM)


jagsdata_rfx_h <- list(lmass = sizes$lmass,
                       pred =pred ,
                       NPRED = lpred,
                       NSPECIES = n(paste(sizes$genus,sizes$species,sep=' ')),
                       NGENUS = n(sizes$genus),
                       NFAMILIES = n(sizes$family),
                       NORDERS = n(sizes$order),
                       species = SPECIES,
                       genus = GE,
                       family = FAM,
                       order = ORD,
                       genus_pred = GE[pred],
                       family_pred = FAM[pred],
                       order_pred = ORD[pred],
                       tau=1/(sizes$lmass*0.05)

)

# hierarchical non-tree model - uniform prior on scale
JM_rfx_u <- jags.parallel(model.file = 'size-model_rfx_u.R',
                        n.iter = 12000,
                        n.burnin = 2000,
                        DIC = T,
                        n.thin = 5,
                        data=jagsdata_rfx_h,
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
                                               'hc_scale'))

# hierarchical non-tree model - gamma prior on scale
JM_rfx_g <- jags.parallel(model.file = 'size-model_rfx_g.R',
                          n.iter = 12000,
                          n.burnin = 2000,
                          DIC = T,
                          n.thin = 5,
                          data=jagsdata_rfx_h,
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
                                                 'hc_scale'))

# hierarchical non-tree model; cut off hierarchy
JM_rfx_s <- jags.parallel(model.file = 'size-model_rfx_s.R',
                        n.iter = 12000,
                        n.burnin = 2000,
                        DIC = T,
                        n.thin = 5,
                        data=jagsdata_rfx_h,
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
                                               'sigma.order'))


# JM_rfx
# traceplot(JM_rfx)

# hierarchical tree model in linear form - uniform prior on scale
JM_rfx_h <- jags.parallel(model.file = 'size-model_rfx_h.R',
                          n.iter = 12000,
                          n.burnin = 2000,
                          DIC = T,
                          n.thin = 5,
                          data=jagsdata_rfx_h,
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
                                                 'sd.sigma.species',
                                                 'sd.sigma.genus',
                                                 'sd.sigma.family',
                                                 'hc_scale'))



# JM_rfx_h
# traceplot(JM_rfx_h)

as.mcmc.rjags <- function (x, subs=NULL, ...)
{
  x <- x$BUGSoutput
  mclist <- vector("list", x$n.chains)
  mclis <- vector("list", x$n.chains)
  strt <- x$n.burnin + 1
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

save.image("size_model_runs.Rdata")
