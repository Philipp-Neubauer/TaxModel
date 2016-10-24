#' print results a taxonomic analysis
#' @name print
#' @param res a result of class TaxMeta
#'
#' @author Philipp Neubauer
#' @references Neubauer.P. Minto, C, and Jensen, O.P. (in prep)
#' @seealso \code{\link{TaxMeta}}
#' @export
NULL

#' @method print TaxMeta
#' @export
print.TaxMeta <- function(res){

  if(all(!is(res$result)=="summary.mcmc")) {

    result <- summary(as.mcmc(lapply(res$result,function(x) as.mcmc(x[,!grepl('log_lik',colnames(x)) & !grepl('_mu',colnames(x))]))))

    diagn <- try(gelman.diag(as.mcmc(lapply(res$result,function(x) as.mcmc(x[,!grepl('pred',colnames(x)) & !grepl('_mu',colnames(x))])))))

  } else {
    result <- res$result
    diagn <- res$convergence
  }

  print(result)

  print(diagn)

  print(res$dic)

  cat('\n','\n','waic',res$waic$waic,'waic SE', res$waic$se_waic,'\n')
  cat('\n','\n','loo',res$loo$loo,'loo SE', res$loo$se_loo,'\n')

}
