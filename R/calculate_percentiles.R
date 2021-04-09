# Calculating (stratified) percentiles of data
# (c) Peter Marquardt, 2020


#' Calculate percentiles
#'
#' Calculate percentiles for values in a data.frame
#'
#' @param data A data.frame
#' @param value_col character name of column containing values
#'
#' @return A vector of numerics with percentile values of length of nrow(data)
#'
#' @example
#' data <- data.frame('values' = 100:1, 'group' = rep(c('A', 'B', 'C', 'D'), 25))
#' calculate_percentiles(data, 'values')
#'

#' @export
#' @import assertive.types
#'
#' @author Peter Marquardt
calculate_percentiles <- function(data, value_col){
  #1. Copy relevant data to reusable dataframe
  perc <- data.frame("rnum" = 1:nrow(data))
  perc$values <- data[[value_col]]
  #2. Order according to values
  perc <- perc[order(perc$values),]
  #3. Assign ranks
  perc$rank <- NA

  for (i in 1:length(perc$values)){
    if (!is.na(perc$values[i])) {
      perc$rank[i] <- i
    }
  }
  #4. counting non-NAs in column
  not_nas = 0
  for (i in 1:length(perc$values)){
    if (!is.na(perc$values[i])) {
      not_nas <- not_nas + 1
    }
  }
  #5. Assigning percentiles
  for (i in 1:length(perc$rank)){
    perc$percentile[i] <- ifelse(!is.na(perc$rank[i]), perc$rank[i]*100/not_nas, NA)
  }

  #6. Export percentiles
  data['output_col'] <- as.double(NA)
  for (i in 1:length(perc$percentile)){
    ind = perc$rnum[i]
    data[ind,'output_col'] <- perc$percentile[i]
  }
  return(data$output_col)
}




#' Calculate stratified percentiles
#'
#' Calculate percentiles for values in a data.frame while stratifying for other characteritics in same df
#'
#' @param data A data frame
#' @param value_col character name of column containing values
#' @param stratify_by list or vector. Use a named list to specify column name as key and a value of type vector indicating accepted levels of the property stratified by to be included. If an unnamed list or vector is passed, all levels of indicated columns will be used
#' @param use.na A logical indicating whether NA values should be used. If TRUE, NA values and non-included value levels will be grouped like a separate value level
#'
#' @return A vector of numerics with percentile values of length of nrow(data)
#'
#' @examples
#' data <- data.frame('values' = 100:1, 'group' = rep(c('A', 'B', NA, 'D'), 25))
#' calculate_stratified_percentiles(data, 'values', list(group = c('A', 'B', 'D')))
#' calculate_stratified_percentiles(data, 'values', c('group'), use.na = TRUE)
#' calculate_stratified_percentiles(data, 'values', list(group = c('A', 'C')), use.na=TRUE)
#' # The following example will result in NA values caused by NAs in 'group'.
#' # Therefore, it will return the percentile vector, but issue a warning.
#' \donttest{calculate_stratified_percentiles(data, 'values', 'group')}
#'
#'
#' @export
#'
#' @import assertive.types
#' @import dplyr
#'
#' @author J. Peter Marquardt
calculate_stratified_percentiles <- function(data, value_col, stratify_by, use.na=FALSE){

  data$RowNumberTemporaryIndex <- c(1:nrow(data))  # assigning row number to sort dataframe later on

  # ensuring we work with a named list from here on out
  if( (!is.list(stratify_by) & is.vector(stratify_by)) | is.null(names(stratify_by)) ){  # the passed argument was a vector or unnamed list, so we'll produce a named list with all levels
    strat_list <- list()
    for(item in stratify_by){
      for (level in unique(data[[item]])) {
        if(is.na(level)){
          if(use.na){strat_list[[item]] <- append(strat_list[[item]], level)}
          else{warning('Some stratification characteristics contain NA values, leading to rows being dropped.')}
        }
        else
          {strat_list[[item]] <- append(strat_list[[item]], level)}
      }
    }
  }
  else{
    assertive.types::assert_is_list(stratify_by)
    strat_list <- stratify_by
  }

  if(use.na){  #ensuring we always include NA if we intend to use it
    for (strat_item in names(strat_list)) {
      if (! NA %in% strat_list[[strat_item]]) {strat_list[[strat_item]] <- append(strat_list[[strat_item]], NA)}
    }
  }



  if(length(strat_list) == 0){stop('No valid items for stratification found. Check input or use calculate_percentiles() instead.')}
  if(length(strat_list) == 1){
    perc_data <- Stratified_percentile_calculator_generator$new(raw_data = data,
                                                                current_stratification_characteristic = strat_list,
                                                                remaining_stratification_characteristics = NULL,
                                                                value_column = value_col,
                                                                output_column = 'output_col',
                                                                use.na = use.na)
  }
  else {
    perc_data <- Stratified_percentile_calculator_generator$new(raw_data = data,
                                                                current_stratification_characteristic = strat_list[1],
                                                                remaining_stratification_characteristics = strat_list[2:length(strat_list)],
                                                                value_column = value_col,
                                                                output_column = 'output_col',
                                                                use.na = use.na)
  }

  perc_data$divide_and_calculate()
  result_data <- perc_data$result_data$data[, c('RowNumberTemporaryIndex', 'output_col')]
  result_df <- dplyr::left_join(data, result_data, by='RowNumberTemporaryIndex', copy=TRUE)
  return(result_df[['output_col']])
}
