#filter_setPA function
filter_setPA <- function(Daily, paStart, paLong){

  monthsToUse <- seq(from = paStart,
                     by = 1,
                     length.out = paLong)
  monthsToUse[monthsToUse > 12] <- monthsToUse[monthsToUse > 12] - 12

  Daily_filtered <- Daily %>%
    filter(Month %in% monthsToUse) %>%
    mutate(YearIndex = trunc(DecYear))

  crossesYear <- paLong + (paStart - 1) > 12

  if(crossesYear){
    Daily_filtered$YearIndex[Daily_filtered$Month >= paStart] <-
      Daily_filtered$YearIndex[Daily_filtered$Month >= paStart] + 1
  }

  get_years <- Daily_filtered %>%
    group_by(YearIndex) %>%
    mutate(Year = trunc(mean(DecYear))) %>%
    ungroup()


  return(get_years)

}

# setupSeasons function
setupSeasons <- function(eList){

  Daily <- eList$Daily

  paLong <- eList$INFO$paLong
  paStart <- eList$INFO$paStart

  Daily_season <- filter_setPA(Daily,
                               paStart = paStart,
                               paLong = paLong)
  Daily_annual <- filter_setPA(Daily,
                               paStart = paStart,
                               paLong = 12)

  #Cleanup units:
  divideBy <- 1000000
  divideBy <- 1

  # Convert flux to kg/year
  unit_scale <- fluxConst[[9]]@unitFactor / divideBy

  SeasonResults <- Daily_season %>%
    group_by(Year) %>%
    summarize(DecYear = mean(DecYear),
              Counts = sum(!is.na(ConcDay)),
              Flux = mean(FluxDay) * unit_scale,
              FNFlux = mean(FNFlux) * unit_scale)

  AnnualResults <- Daily_annual %>%
    group_by(Year) %>%
    summarize(AnnualCounts = sum(!is.na(ConcDay)),
              AnnualFlux = mean(FluxDay) * unit_scale,
              AnnualFNFlux = mean(FNFlux) * unit_scale) %>%
    filter(AnnualCounts >= 365)


  seasonPctResults <- SeasonResults %>%
    select(DecYear, Year, Flux, FNFlux, Counts) %>%
    left_join(select(AnnualResults,
                     Year, AnnualCounts,
                     AnnualFlux, AnnualFNFlux), by="Year") %>%
    filter(!is.na(AnnualFlux)) %>%
    mutate(pctFlux = 100*Flux*Counts/(AnnualFlux*AnnualCounts),
           pctFNFlux = 100*FNFlux*Counts/(AnnualFNFlux*AnnualCounts)) %>%
    rename(SeasonFlux = Flux,
           SeasonFNFlux = FNFlux)

  return(seasonPctResults)
}
