# Created by Matthew A. Birk
# Dependencies: birk, marelac
# Converts % air saturation to other O2 units
# Last updated: Feb 2015

#' Convert Units of Oxygen
#'
#' Given the percent of oxygen compared to air-saturated water (at equilibrium with air) (i.e. percent air saturation), a list of commonly used units of oxygen partial pressures and concentrations are returned.
#'
#' Conversions are based on relationships and values from the package \code{\link[marelac]{marelac}}.
#'
#' @param perc_a.s. percent of air saturation. Default is 100\%.
#' @param salinity salinity of water sample (ppt). Default is 35 ppt.
#' @param temp temperature of water sample (°C). Default is 25 °C.
#' @param air_pres pressure of air overlying water sample (bar). Default is 1.013253 bar.
#'
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#'
#' @examples
#' o2_unit_conv(perc_a.s. = 50)
#' o2_unit_conv(perc_a.s. = 50, salinity = 0, temp = 10, air_pres = 1.2)['umol_per_l']
#'
#' @encoding UTF-8
#' @export
#' @import marelac
#' @import birk

o2_unit_conv=function(perc_a.s.=100,salinity=35,temp=25,air_pres=1.013253){
  x=list(
  percent_a.s.=perc_a.s.,
  percent_o2=marelac::atmComp('O2')*perc_a.s.,
  hPa=birk::conv_unit((air_pres-marelac::vapor(S=salinity,t=temp))*marelac::atmComp('O2')*perc_a.s./100,'atm','hPa'),
  torr=birk::conv_unit((air_pres-marelac::vapor(S=salinity,t=temp))*marelac::atmComp('O2')*perc_a.s./100,'atm','torr'),
  mg_per_l=marelac::gas_satconc(S=salinity,t=temp,P=air_pres,species='O2')*1e-6*marelac::molweight('O2')*1e3*perc_a.s./100,
  umol_per_l=marelac::gas_satconc(S=salinity,t=temp,P=air_pres,species='O2')*perc_a.s./100
)
attr(x[['percent_o2']],'names')=NULL
attr(x[['hPa']],'names')=NULL
attr(x[['torr']],'names')=NULL
attr(x[['mg_per_l']],'names')=NULL
attr(x[['umol_per_l']],'names')=NULL
return(x)
}