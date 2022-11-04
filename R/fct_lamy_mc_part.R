#' lamy_mc_part 
#'
#' @description   
#' 
#' Code provided in the supplement to Lamy et al. 2019, which has the following
#' description (lightly edited):
#' 
#' "Partitions ecological variability across two spatial scales (local and regional)
#' and two organizational levels (population and community).
#' Six synchrony metrics serve as scaling factors to measure how variability scales
#' from one specific hierarchical level to the next. These synchrony metrics are 
#' the square-root transformation of the synchrony metric developed by 
#' Loreau and de Mazancourt (2008)."
#'
#' 
#' @param Y Community table. An observation by species matrix with row blocks corresponding to local patches
#'  containing the biomass of species (i) in local patch (k).
#' @param s Number of patches.
#' @param t Number of years.
#' 
#' @author Thomas Lamy
#'
#' @return Synchrony and stability metrics as spatial and organizational levels of the ecosystem.:
#'  
#'  \item{CV_SL}{Species-level variability at the local scale}
#'  \item{CV_CL}{Community-level variability at the local scale}
#'  \item{CV_SR}{Species-level variability at the regional scale; the weighted average of metapopulation variability across species}
#'  \item{CV_CR}{Community-level variability at the regional scale; the variability of the whole metacommunity}
#'  
#' Species-level table (i):
#'
#'  \item{phi_i_LR}{Spatial synchrony of species i}
#'  \item{w_i}{Weight of species i (SD contribution to metacommunity SD)}
#'
#' Patch-level table (k):
#' 
#'  \item{phi_SC_k}{Species synchrony within patch k}
#'  \item{w_k}{Weight of patch k (biomass contribution to metacommunity)}
#'
#' @export
#' 
#' @references
#' 
#' Wang S, Lamy T, Hallett LM and Loreau M (2019) Stability and synchrony across ecological hierarchies in heterogeneous metacommunities: linking theory to data.
#' Ecography. doi: 10.1111/ecog.04290.
#' 
#' Wang and Loreau (2014) Ecosystem stability in space: alpha, beta and gamma variability.
#' Ecology Letters. 17: 891-901.
#' 
#' Loreau M and de Mazancourt C (2008) Species synchrony and its drivers: neutral and nonneutral community dynamics in fluctuating environments.
#' American Naturalist. 172: E48-66.
#' 
#' Data S1 to Lamy T, Wang S, Renard D, Lafferty KD, Reed DC and Miller RJ (2019)
#' Species insurance trumps spatial insurance in stabilizing biomass of a marine macroalgal metacommunity. Ecology.

fct_lamy_mc_part <- function(Y, s, t)
{
  # Total community biomass (sum within rows)
  tot.bio <- apply(Y, 1, sum)
  
  
  # Check that data is balanced
  if(length(tot.bio) != s*t) stop("STOP: sites are not surveyed every year")
  
  
  # Matrix of total community biomass within each patch
  Y.CL <- matrix(tot.bio, nrow=t, ncol=s, byrow=FALSE)
  
  
  # List of local patches containing population biomass of each species
  # for each of 20 sites
  SiteL <- list()
  for(k in 1:s) {
    SiteL[[k]] <- Y[c(((k-1)*t+1):(k*t)),]
  }
  
  
  # Matrix of metapopulation biomass
  Y.SR <- Reduce("+", SiteL) 
  
  
  # Temporal mean biomass of the whole metacommunity
  mu_TT <- mean(apply(Y.SR, 1, sum))
  
  
  # Temporal sd of species i in patch k
  sd_ik <- matrix(unlist(lapply(SiteL, function(x){apply(x, 2, sd)})), 
                  nrow=s, ncol=dim(Y)[2], byrow=TRUE)
  
  
  # Temporal mean biomass of species i in patch k
  mu_ik <- matrix(unlist(lapply(SiteL, function(x){apply(x, 2, mean)})), 
                  nrow=s, ncol=dim(Y)[2], byrow=TRUE)
  
  
  # Temporal cv of species i in patch k
  cv_ik <- sd_ik/mu_ik
  
  
  # Temporal mean total community biomass in patch k
  mu_Tk <- matrix(unlist(lapply(SiteL, function(x){mean(apply(x, 1, sum))})), 
                  nrow=1, ncol=s, byrow=TRUE)
  
  
  # Temporal sd of total community biomass in patch k
  sd_Tk <- matrix(unlist(lapply(SiteL, function(x){sd(apply(x, 1, sum))})), 
                  nrow=1, ncol=s, byrow=TRUE)
  
  
  # Temporal cv of total community biomass in patch k
  cv_Tk <- (sd_Tk/mu_Tk) 
  
  
  # temporal sd of species i at the regional scale
  sd_iT <- apply(Y.SR, 2, sd)
  
  
  # temporal mean biomass of species i at the regional scale
  mu_iT <- apply(Y.SR, 2, mean)
  
  # temporal cv of species i at the regional scale
  cv_iT <- sd_iT/mu_iT
  
  # Species-level variability within patch k
  # defined as the weighted average of species variability across species
  cv_SL_k <- rowSums(sd_ik)/mu_Tk
  
  # Average species-level variability at the local scale
  # defined as the weighted average of species-level variability across patches
  CV_SL_part <- data.frame(CV_SL_part = as.numeric(cv_SL_k),
                           CV_SL_weight = as.numeric(mu_Tk/mu_TT))
  CV_SL <- sum(mu_Tk/mu_TT * cv_SL_k)
  
  # Average community-level variability at the local scale
  # defined as the weighted average of community variability across patches
  CV_CL_part <- data.frame(CV_CL_part = as.numeric(cv_Tk),
                           CV_CL_weight = as.numeric(mu_Tk/mu_TT))
  CV_CL <- sum(mu_Tk/mu_TT * cv_Tk)
  
  # Species-level variability at the regional scale
  # defined as the weighted average of metapopulation variability across species
  CV_SR_part <- data.frame(CV_SR_part = cv_iT,
                           SD_SR_part = sd_iT,
                           weight = mu_iT/mu_TT)
  CV_SR <- sum(mu_iT/mu_TT * cv_iT)
  SD_SR <- sum(mu_iT/mu_TT * sd_iT)
  
  # Community-level variability at the regional scale, or the variability of the whole metacommunity
  # variance-covariance matrix of the temporal biomass across sites
  W <- cov(Y.CL)
  
  # Sum of the temporal covariances of the communities in each patch
  Covsum <- sum(W)
  
  # Temporal variability at the metacommunity scale is the coefficient of temporal variation of metacommunity biomass
  CV_CR <- sqrt(Covsum)/mu_TT
  SD_CR <- sqrt(Covsum)
  
  # Species synchrony within patch k
  phi_SC_k <- as.numeric(sd_Tk/rowSums(sd_ik))
  
  # Average local-scale species synchrony
  # defined as the weighted average of species synchrony across patches
  w_k <- rowSums(sd_ik)/sum(sd_ik)
  
  phi_SC_L <- sum(w_k * phi_SC_k)
  
  # Spatial synchrony of species i
  phi_i_LR <- sd_iT/colSums(sd_ik)
  
  # Average species-level spatial synchrony
  # defined as the weighted average of spatial synchrony across species
  w_i <- colSums(sd_ik)/sum(sd_ik) 
  phi_S_LR <- sum(w_i * phi_i_LR, na.rm=TRUE)
  
  # Community-level spatial synchrony
  phi_C_LR <- sqrt(Covsum)/((sum(sd_Tk)))
  
  # Regional-scale species synchrony
  phi_SC_R <- sqrt(Covsum)/sum(sd_iT)
  
  # Overall table
  part <- data.frame(CV_SL = CV_SL, 
                     CV_CL = CV_CL, 
                     CV_SR = CV_SR,
                     SD_SR = SD_SR,
                     SD_CR = sqrt(Covsum),
                     CV_CR = CV_CR,
                     SD_CR = SD_CR,
                     mu_TT = mu_TT,
                     phi_SC_L=phi_SC_L, phi_S_LR=phi_S_LR, phi_C_LR=phi_C_LR, phi_SC_R=phi_SC_R)
  
  # Species-level table (i)
  spe <- data.frame(phi_i_LR=phi_i_LR, w_i=w_i)
  
  # Patch-level table (k)
  patch <- data.frame(phi_SC_k=phi_SC_k, w_k=w_k)
  
  res <- list(part=part, spe=spe, patch=patch, CV_SR_part = CV_SR_part, 
              CV_CL_part = CV_CL_part,
              CV_SL_part = CV_SL_part)
  res
}
