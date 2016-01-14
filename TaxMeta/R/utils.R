#' Retrieve taxonomic information using taxize, and append to dataset. Note that the dataset needs a species column in [genus species] format, or separate genus and species columns.
#' @param dataset a dataframe for meta-analysis, containing species names
#' @param taxonomy the taxonomy to retrieve
#' @export
get_tax <- function(dataset, taxonomy){
  try(if(all(grepl(' ',dataset[,'species']))){
    cs <- taxize::classification(dataset[,'species'], db = 'ncbi')
  } else {
    cs <- taxize::classification(apply(dataset[,c('genus','species')],1,paste,collapse=' '), db = 'ncbi')
  }, silent=T)

  if(is(cs)=="try-error") stop("Could not retrieve taxonomic information, aborting. Either the dataframe did not contain genus and species columns, or you do not have an internet connection for R to use.")

  tax <- lapply(cs,function(tax) {
    taxonomy <- taxonomy[taxonomy != 'genus' & taxonomy!='species']
    res <- tax[match(taxonomy,tax[,'rank']),'name']
    names(res) <- taxonomy
    if(!any(is.na(res))) {
      return(res) } else {
        missing <- which(!taxonomy %in% tax[,'rank'])
        imp <- unique(tax[which(tax[,'rank'] %in% c(paste0('sub',taxonomy[missing]),paste0('super',taxonomy[missing])))+c(1,-1),'name'])
        res[missing] <- imp
        return(res)
      }
  })

  tax <- do.call('rbind',tax)
  cbind(dataset,tax)
}


# parse random fx
.parse_rfx <- function(random_fx,rfxix) {

  paste0("for(r in 1:",max(rfxix),") { \n",
         random_fx,"_mu[r] <- ",random_fx,".xi*",random_fx,".eta[r] \n",
         random_fx,".eta[r] ~ dnorm(0,",random_fx,".prec) \n } \n",
         random_fx,".xi ~ dnorm(0,scale) \n",
         random_fx,".prec ~ dgamma(0.5,0.5) \n",
         "sd.",random_fx," <- sd(",random_fx,"_mu)")
}

# parse taxonomic fx
.parse_tax <- function(tax_fx,tax_next,taxix,type) {

  # two types (full and std), last taxonomic ix drawn from over-all dist, first index only doable when !is.null(study_epsilon)

    if(type != "full"){

      paste0("for(r in 1:",max(taxix),") { \n",
             tax_fx,"_mu[r] <- ",tax_next,".xi*",tax_fx,".eta[r] \n",
             tax_fx,".eta[r] ~ dnorm(0,",tax_next,".prec) \n } \n",
             tax_next,".xi ~ dnorm(0,scale) \n",
             tax_next,".prec ~ dgamma(0.5,0.5) \n",
             "sigma.",tax_fx," <- abs(",tax_next,".xi)/sqrt(",tax_next,".prec) \n",
             "sd.",tax_fx," <- sd(",tax_fx,"_mu)")
    } else {

      paste0("for(r in 1:",max(taxix),") { \n",
             tax_fx,"_mu[r] <- ",tax_next,".xi",ifelse(tax_next != "grand",paste0('[',tax_next,'[r]]'),''),"*",tax_fx,".eta[r] \n",
             tax_fx,".eta[r] ~ dnorm(0,",tax_next,".prec) \n",
             tax_fx,".xi[r] ~ dnorm(0,",tax_fx,"_scale) \n} \n",
             tax_fx,"_scale ~ dgamma(2,hyper_scale) \n",
             #tax_fx,".xi_pred ~ dnorm(0,",tax_fx,"_scale) \n",
             tax_next,".prec ~ dgamma(0.5,0.5) \n",
             "sd.",tax_fx," <- sd(",tax_fx,"_mu)",
             ifelse(tax_next == "grand",'grand.xi ~ dnorm(0,2/hyper_scale)',''))

    }
  }

# parse distribution
.parse_dist <- function(dist,eps) {
  if (is.null(eps)){
    switch(dist,
           norm = "mu[i],epsilon",
           lnorm = "mu[i],epsilon",
           gamma = "epsilon,epsilon/exp(mu[i])")
  } else {
    switch(dist,
           norm = "mu[i],epsilon[i]",
           lnorm = "mu[i],epsilon[i]",
           gamma = "epsilon[i],epsilon[i]/exp(mu[i])")

  }
}


.parse.fixed_fx <- function(ncov) {

  paste0("for(b in 1:",ncov,") { \n",
            "betas[b] ~ dnorm(0,1e-6) \n",
         "} \n")


}


# parse distribution
.parse_scale_model <- function(type) {
  switch(type,
         fixed = '',
         gamma = "scale ~ dgamma(1e-9,1e-9) \n",
         uniform = "scale ~ dunif(0.00001,100000) \n",
         full = "hyper_scale ~ dunif(0.00001,100000) \n
                 scale ~ dgamma(2,hyper_scale) \n")
}

.parse.params <- function(type, preds, loo_waic, taxonomy, random_fx=NULL) {

  #cat(random_fx)
  c('betas',
  if(type!='full' & type!='fixed') 'scale',
  if(type=='full') c(paste0(taxonomy,'_scale'),"hyper_scale"),
  paste0('sd.',c(random_fx,taxonomy)),
  if(loo_waic==T) 'log_lik',
  if(any(preds)) 'pred'
   )


}

.summarise.res <- function(res, taxonomy) {

  if(all(!is(res$result)=="summary.mcmc")) {

    res <- do.call('rbind',res$result)
    res <- res[,grepl('sd',colnames(res))]

    res <- apply(res,1,function(z) z/sum(z))

    msd <- reshape2::melt(res)

    msd <- msd %>%
      mutate(Factor = do.call('rbind',strsplit(as.character(Var1),'sd.'))[,2]) %>%
      group_by(Factor) %>%
      summarise(means = median(value),
                q1 = quantile(value,0.025),
                q11 = quantile(value,0.25),
                q33 = quantile(value,0.75),
                q3 = quantile(value,0.975))

  } else {

    ix <- unlist(lapply(taxonomy,function(x) which(grepl(x,rownames(res$result[[2]])))))
    msd <- res$result[[2]][ix,]
    colnames(msd) <- c('q1','q11','means',"q33",'q3')

    msd <- data.frame(msd) %>%
      mutate(Factor = do.call('rbind',strsplit(rownames(msd),'sd.'))[,2])
  }

  .simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
  }

  msd$Factor <- factor(sapply(as.character(msd$Factor),.simpleCap),
                       levels = sapply(taxonomy,.simpleCap))

  msd

}