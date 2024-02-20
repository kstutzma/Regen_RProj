#################################
# FUNCTION: species_richness
# packages: vegan
# purpose: to get species richness estimates
# input: df
# output: df
# -------------------------------
species_richness <- function(x=NULL) {
  
  specpool(x, smallsample = T)
}
