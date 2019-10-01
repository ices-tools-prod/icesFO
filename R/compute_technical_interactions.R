# import pipe
`%>%` <- tidyr::`%>%`

#' Compute a matrix of technical interactions.
#'
#' Based on landings and catches by species and gear, compute
#' a matrix of technical interaction values and main gear contributions.
#'
#' @param x a dataframe in the same format as the STECF data.
#' @param threshByCatch minimum tonnage for a species to be considered
#'        a by catch (default 5)
#' @param thresholdMainGear a threshold for including gears contributing 
#'        to co-catches, to retain only the gear with the greatest contribution
#'        set this to 1 (default is 0.8)
#' @param thresholdCoCatches a proportion giving the threshold for when 
#'        we decide that there is co-catches (default 0.5)
#' @param catchCoverage the propotion of total catch to cover in the tables
#'        i.e. 0.95 gives a table convering 95\% of the total catch (default 0.95)
#'
#' @return A list with four elements, technical interactions for landings (recapLand)
#'         and catch (recapCatch) and tables of main gears involved in the interactions
#'         based on landings (MainGearsLandings) and catches (MainGearsCatches)
#'
#' @note
#' 
#' Technical interaction appears between stocks when they are caught by the same 
#' gear during a fishing operation. Ideally the technical interaction should then 
#' be studied at the scale of the fishing operation to prevent artificially creating 
#' technical interaction between stocks that might only be caught at day/night or in 
#' different areas/timing of the year. However, often, the finest available information 
#' is per stock/gear/area/season. Knowing these limitations, the methodology used here 
#' consists in computing the sum of landings per strata of one species given that a 
#' second species is also present in the landings of this strata and then dividing 
#' this number by the total landings of the first species:
#'
#'  \deqn{T_{i,j}=\frac{\sum_s L_{s,j}*P_{s,j}}{\sum_s L_{s,i}}*100}{%
#'       T(i,j)=(sum_s L(s,j)*P(s,j)) / (sum_s L(s,i)) *100}
#'
#' Where T is the value of the technical interaction, i and j are the two species for 
#' which the technical interaction is assessed. P is an indicator variable and equals 
#' 0 if the total landings of a species for a for a given strata is less than 5% the 
#' total landings for that strata and 1 otherwise. L is the landings for a given 
#' species and strata.
#' 
#' Strata corresponds to the provided disaggregation of the landings.
#'
#' @seealso
#'
#' \code{\link{plot_technical_interactions}} for plotting technical interactions.
#' 
#' \code{\link{icesFO-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' technical_interacton <- 
#'   compute_technical_interactions(STECF_landings, catchCoverage = 0.99)
#' 
#' plot_technical_interactions(technical_interacton$recapLand)
#' }
#'
#' @import data.table
#' 
#' @export

compute_technical_interactions <- 
  function(x, 
           threshByCatch = 5, thresholdMainGear = 0.8, thresholdCoCatches = 0.5,
           catchCoverage = 0.95) 
{

  #### define treshold for not only accidental bycatch

  # initial formatting
  sp <- 
    x %>% 
      dplyr::select(annex, regulated.area, country, 
             regulated.gear, specon, species, 
             sum_landings, sum_discards) %>%
      dplyr::rename(
        reg_area = regulated.area,
        reg_gear = regulated.gear,
        land = sum_landings,
        disc = sum_discards
        ) %>%
      dplyr::mutate(catch = land + disc) %>%
      data.table::data.table()

  ### preparation

  sp[, id:=paste(annex,reg_area,reg_gear,specon, sep='')]

  sp <- sp[, list(land = sum(land, na.rm = TRUE), 
                  catch = sum(catch, na.rm = TRUE)), 
             by = list(id, species, reg_gear)]

  sp[, propLanStock := land / sum(land) * 100, by = list(species)]
  sp[, propLanGear := land / sum(land) * 100, by = list(id)]

  sp[, propCatchStock := catch / sum(catch, na.rm = TRUE) * 100, by = list(species)]
  sp[, propCatchGear := catch / sum(catch, na.rm = TRUE) * 100, by=list(id)]

  ## Option 1: threshold based on the landings of one species compared to the total landings of that species
  #sp[propLanStock>threshByCatch, landSpp:=1]
  #sp[propLanStock<=threshByCatch, landSpp:=0]

  ## Option 2: threshold based on the landings of one species compared to the total landings of that métier
  sp[propLanGear > threshByCatch, landSpp := 1]
  sp[propLanGear <= threshByCatch, landSpp := 0]

  sp[propCatchGear > threshByCatch, landSpp := 1]
  sp[propCatchGear <= threshByCatch, landSpp := 0]

  sp[is.na(sp)] <- 0  

  # select species for table
  species_catches <- sp[,sum(catch, na.rm = TRUE), by = list(species)]
  species_catches$V1 <- species_catches$V1 / sum(species_catches$V1)
  species_catches <- species_catches[rev(order(species_catches$V1)),]
  species_catches$V1 <- cumsum(species_catches$V1)
  species_list <- species_catches$species[species_catches$V1 < catchCoverage]
  sp <- sp[which(species %in% species_list), ]


  # useful summaries
  species <- unique(sp$species)
  n_species <- length(species)

  # create empty DFs to hold results
  recap <- recapCatch <-
    matrix(NA, n_species, n_species, dimnames = list(species, species))
  recapGearsCatch <- recapGears <-  
    matrix("", n_species, n_species, dimnames = list(species, species))
  

  #### compute the proportion of landings of one spp that are caught together with other species based on the landings

  for (i in species) {
    for (j in species) {
      ## get the "targetting"
      thres <- sp[species == j, c('landSpp','id'), with = FALSE]
      prop <- sp[species == i, ]
      prop <- merge(prop, thres, by = 'id', all.x = TRUE)

      landCombined <- prop[, sum(land * landSpp.y, na.rm = TRUE) / sum(land) * 100]
      recap[i, j] <- landCombined

      # get list of main gears
      prop[, coocLand := land * landSpp.y]
      prop <- prop[rev(order(coocLand)),]
      prop <- prop[, list(sumGear = sum(coocLand, na.rm = TRUE)), by = reg_gear]
      prop[, Cumsum := cumsum(sumGear)]
      prop[, propLand := Cumsum / max(Cumsum)]
      if (landCombined > thresholdCoCatches) {
        mainGears <- prop[propLand < thresholdMainGear, reg_gear]
        if (length(mainGears) == 0) mainGears <- prop$reg_gear[1]
        recapGears[i, j] <- paste(mainGears, collapse='_')
      } 
    }
  }

  #### compute the proportion of landings of one spp that are caught together with other species based on the catches

  for (i in species) {
    for (j in species) {
      ## get the "targetting"
      thres <- sp[species == j, c('landSpp','id'), with = FALSE]
      prop <- sp[species == i, ]
      prop <- merge(prop, thres, by = 'id', all.x = TRUE)

      catchCombined <- prop[, sum(catch * landSpp.y, na.rm = TRUE) / sum(catch) * 100]
      recapCatch[i, j] <- catchCombined

      # get list of main gears
      prop[, coocCatch := catch * landSpp.y]
      prop <- prop[rev(order(coocCatch)),]
      prop <- prop[, list(sumGear = sum(coocCatch, na.rm = TRUE)), by = reg_gear]
      prop[, Cumsum := cumsum(sumGear)]
      prop[, propCatch := Cumsum / max(Cumsum)]
      if (catchCombined > thresholdCoCatches) {
        mainGears <- prop[propCatch < thresholdMainGear, reg_gear]
        if (length(mainGears) == 0) mainGears <- prop$reg_gear[1]
        recapGearsCatch[i, j] <- paste(mainGears, collapse='_')
      } 
    }
  }

  list(recapLand = recap, 
       MainGearsLandings = recapGears, 
       recapCatch = recapCatch, 
       MainGearsCatches = recapGearsCatch)
}
