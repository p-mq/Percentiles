# Class allowing recursive calculation of stratified percentiles
# (c) Peter Marquardt, 2020

#' R6 Class representing a compund of data and methods used to calculate stratified percentiles
#'
#' A calculator has:
#'  - raw_data representing the data.frame passed in for calculation
#'  - result_data an environment containing the result data.frame $data, shared with
#'  - sub_results representing subordinate steps in recursive calculation process
#' @importFrom R6 R6Class
Stratified_percentile_calculator_generator <- R6::R6Class(
  'Stratified_percentile_calculator',
  private = list(
    ..raw_data = NULL,  # data.frame containing current subset of original data
    ..result_data = NULL,  # environment containing a data.frame containing results of current hierarchy
    ..current_stratification_characteristic = NULL,  # list indicating current item and levels of stratification
    ..remaining_stratification_characteristics = NULL,  # named list indicating characteristics to be stratified by and their values
    ..value_column = NULL,  # character value indicating the column with values
    ..output_column = NULL,  # character value indicating the column to store resulting values
    ..use.na = FALSE,  # logical indicating whether or not to consider NA values as a stratification level
    ..sub_results = list()  # named list with Stratified_percentile_calculator_generator objects for recursive stacking
  ),

  public = list(
    #' @description
    #' Create a new Stratified_percentile_calculator object.
    #' @param raw_data data.frame to perform calculation/stratification on.
    #' @param result_data environment containing $data, a data.frame with the current state of results.
    #' @param current_stratification_characteristic named list with column name and levels of characteristic to stratify by.
    #' @param remaining_stratification_characteristics named list with column names and levels of characteristics to stratify by.
    #' @param value_column character column with values to calculate percentiles on
    #' @param output_column character column to write calculated percentile values to
    #' @param use.na logical indicating whether or not NA/non-listed stratification values should be included as a separate group
    #' @return A new `Stratified_percentile_calculator` object.
    initialize = function(raw_data=NULL, result_data=new.env(), current_stratification_characteristic=NULL, remaining_stratification_characteristics=NULL, value_column=NULL, output_column=NULL, use.na=FALSE){
      private$..raw_data <- raw_data
      private$..result_data <- result_data
      private$..current_stratification_characteristic <- current_stratification_characteristic
      private$..remaining_stratification_characteristics <- remaining_stratification_characteristics
      private$..value_column <- value_column
      private$..output_column <- output_column
      private$..use.na <- use.na
    },

    #' @description
    #' recursively calculate stratified percentiles on data.frame
    #' Updates following private fields:
    #' - ..result_data$data
    #' - ::sub_results
    #' - ..current_stratification_characteristic
    #' - ..remaining_stratification_characteristics
    #' @return void, but updates ..result_data field
    #'
    #' @import assertive.types
    #' @import dplyr
    #' @import assertthat
    divide_and_calculate = function(){
      if (is.null(private$..current_stratification_characteristic)){
        # calculate results for current data and append to current results
        if(!nrow(private$..raw_data) == 0){  # ensuring the combination of stratified characteristics still contains data
          if(nrow(private$..raw_data) < 10){message('Warning: Calculated percentiles on a subset with less than 10 cases')}
          temp_result_data <- private$..raw_data
          temp_result_data[private$..output_column] <- calculate_percentiles(private$..raw_data, private$..value_column)
          private$..result_data$data <- dplyr::bind_rows(private$..result_data$data, temp_result_data)
        }
      }

      else {
        # create sub_results
        for(split_level in private$..current_stratification_characteristic[[1]]){  # iterates over levels of current split characteristic
          if(is.na(split_level)){
            assertthat::assert_that(private$..use.na)
            # This monster condition ensures to include rows if they are either (1) NA or (2) a level not included in the specified stratification
            raw_data_split <- private$..raw_data[is.na(private$..raw_data[[names(private$..current_stratification_characteristic)]]) |
                                                   !private$..raw_data[[names(private$..current_stratification_characteristic)]] %in% private$..current_stratification_characteristic[[1]]
                                                   , ]
          }
          else{
            raw_data_split <- private$..raw_data[private$..raw_data[[names(private$..current_stratification_characteristic)]] == split_level, ]
            raw_data_split <- raw_data_split[!is.na(raw_data_split[[names(private$..current_stratification_characteristic)]]), ]  # remove NAs
          }


          if(length(private$..remaining_stratification_characteristics) == 0){
            new_current_strat <- NULL
          } else{new_current_strat <- private$..remaining_stratification_characteristics[1]}
          if(length(private$..remaining_stratification_characteristics) == 1){
            new_remaining_strat <- NULL
          } else{new_remaining_strat <- private$..remaining_stratification_characteristics[2:length(private$..remaining_stratification_characteristics)]}

          value_col <- private$..value_column
          sub_res <- Stratified_percentile_calculator_generator$new(raw_data=raw_data_split, result_data=private$..result_data, current_stratification_characteristic=new_current_strat, remaining_stratification_characteristics=new_remaining_strat, value_column=value_col, output_column = private$..output_column)
          private$..sub_results <- append(private$..sub_results, sub_res)
        }
        # call function on subitems
        for (sub_res in private$..sub_results) {
          sub_res$divide_and_calculate()
        }

      }
    }

  ),


  active = list(
    #' @field
    #' raw_data
    #' Return the data.frame originally handed to the object
    raw_data = function(value){
      if (missing(value)){
        return(private$..raw_data)
      }
      else{
        assertive.types::assert_is_data.frame(value)
        private$..raw_data <- value
      }
    },
    #' @field
    #' result_data
    #' Return the environment containing a data.frame (§data) containing results of current hierarchy
    result_data = function(value){
      if (missing(value)){
        return(private$..result_data)
      }
      else{
        assertive.types::assert_is_data.frame(value)
        private$..result_data <- value
      }
    },
    #' @field
    #' sub_results
    #' Return the named list with Stratified_percentile_calculator_generator objects for recursive stacking
    sub_results = function(){
      return(private$..sub_results)
    }
  )

)
