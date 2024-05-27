


#' Common Cold Data
#'
#' Data presented in Heasman & Reid (1961), originally gathered and analyzed by Brimblecombe et al (1958).
#' The data set describes 664 outbreaks of the common cold in 72 families over two years.
#' All families consists of two parents and three children. The data is available in aggregated form
#' as presented in the paper, with counts of the number of outbreaks that belong to a given classification.
#'
#'
#' @usage heasman_reid_1961_chains
#' @usage heasman_reid_1961_crowding
#' @usage heasman_reid_1961_intro_case_status
#'
#' @section Chain Data:
#'
#' Each outbreak was classified to a specific chain suitable for analysis by the
#' Chain Binomial model by Heasman & Reid (1961), Table V.
#'
#' `heasman_reid_1961_chains`: A data frame with 24 rows and 2 columns:
#'
#' \describe{
#'   \item{chain}{the number of infected in each generation, separated by '-', ie the Chain.}
#'   \item{n}{Number of outbreaks}
#' }
#'
#'
#' @section Crowding:
#'
#' Each outbreak classified according to the degree of domestic overcrowding. Heasman & Reid (1961), Table IV.
#' Overcrowded homes have either one or two rooms, crowded homes have three rooms, while uncrowded homes have more than three rooms.
#'
#' `heasman_reid_1961_crowding`: A data frame with 5 rows and 4 columns:
#'
#' \describe{
#'   \item{further_cases}{The number of cases in the outbreak, in addition to the primary case.}
#'   \item{overcrowding}{Number of outbreaks that belong to the overcrowded household category.}
#'   \item{crowded}{Number of outbreaks that belong to the crowded household category.}
#'   \item{uncrowded}{Number of outbreaks that belong to the uncrowded household category.}
#' }
#'
#' @section Index case status:
#'
#' Each outbreak classified according to who the the introducing case was. Heasman & Reid (1961), Table II.
#'
#' `heasman_reid_1961_intro_case_status`: A data frame with 5 rows and 5 columns:
#'
#' \describe{
#'   \item{further_cases}{The number of cases in the outbreak, in addition to the primary case.}
#'   \item{father}{Number of outbreaks with father as the index case.}
#'   \item{mother}{Number of outbreaks with mother as the index case.}
#'   \item{school_child}{Number of outbreaks with a school child as the index case.}
#'   \item{pre_school_child}{Number of outbreaks with a pre-school child as the index case.}
#' }
#'
#'
#'
#' @section References:
#' \itemize{
#'  \item{Heasman & Reid (1961) Theory And Observation In Family Epidemics Of The Common Cold. Brit. J. prev. soc. Med.}
#'  \item{Brimblecombe et al (1958) Family Studies Of Respiratory Infections. British Medical Journal.}
#' }
#'
#'@name heasman_reid_1961
"heasman_reid_1961_chains"


#'@rdname heasman_reid_1961
"heasman_reid_1961_intro_case_status"


#'@rdname heasman_reid_1961
"heasman_reid_1961_crowding"

