#' @name producers
#' @title synthetic data on producers
#' @description 
#'   A synthetic dataset listing several sources of turnover
#'   and other income for producers. The producers are classified
#'   in size classes and SBI (a refinement of NACE). Load with \code{data(producers)}.
#'
#'
#' \itemize{
#'  \item  sbi: Classification of economic activity (refinement of NACE2008)
#'  \item  size: Size class in 0 (smallest) to 9.
#'  \item  industrial: Turnover from industrial activities.
#'  \item  trade: Turnover from trade
#'  \item  other: Turnover from other activities
#'  \item  other_income: Income not from turnover (e.g. from financial transactions)
#'  \item  total: Rowwise sum of indistrial, trade, and other turnover and other income.
#'}
#'
#' @family datasets
#'
#' @docType data
#' @format A \code{.rda} file, one producer per row.
NULL



