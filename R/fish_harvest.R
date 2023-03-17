library(tidyverse)

#' Fish Harvest
#'
#' This function runs a basic fisheries model using a logistic growth equation. It uses the initial
#' stock level, annual harvest, the number of years, and some parameters to compute the final
#' stock level, stock status, and sustainability of fishing.
#' @param stock_initial (kg)
#' @param annual_harvest (kg)
#' @param growth_rate (kg) default is .5
#' @param carrying_capacity = stock_initial
#' @author Kort Alexander and Evie Vermeer
#' @return final stock (kg)
#' @return sustainability of harvest (sustainable or unsustainable)
#' @return status of stock (healthy, overfished, critically low or fully depleted)
#'
#'

fish_harvest <- function(stock_initial, annual_harvest, growth_rate = 0.5, carrying_capacity = stock_initial, num_years) {
  # error checking
  if(annual_harvest > stock_initial){
    stop("Annual harvest is greater than initial stock level")
  }

  # initialize current stock level as the initial stock level
  current_stock = stock_initial

  # run for the number of years input
  for(i in 1:num_years) {

    # stop if stock level reaches 0
    if(current_stock <= 0) {
      break
    }

    # subtract harvest from current stock, calculate recruitment using logistic growth model, and add recruitment to current stock each year
    current_stock = current_stock - annual_harvest
    recruitment = growth_rate * current_stock * ((carrying_capacity - current_stock) / carrying_capacity)
    current_stock = current_stock + recruitment
  }

  # set final stock to current stock
  stock_final <- case_when(current_stock > 0 ~ current_stock,
                           current_stock <= 0 ~ 0)

  # define maximum sustainable yield using carrying capacity and growth rate
  msy = carrying_capacity * growth_rate / 4

  # assess fishing sustainability and stock status
  fishing_sustainability <- case_when(annual_harvest < msy ~ "sustainable",
                                      annual_harvest >= msy ~ "unsustainable")
  stock_status <- case_when(stock_final > 0.5 * carrying_capacity ~ "healthy",
                            stock_final <= 0.5 * carrying_capacity & stock_final > 0.1 * carrying_capacity ~ "overfished",
                            stock_final <= 0.1 * carrying_capacity & stock_final > 0 ~ "critically low",
                            stock_final <= 0 ~ "fully depleted")

  # return outputs
  return(list(stock_final = stock_final, fishing_sustainability = fishing_sustainability, stock_status = stock_status))
}
