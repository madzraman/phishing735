#' Return splitted data
#'
#' This function returns train/test x or y from the original data
#'
#' @param name a string from train_x, train_y, test_x, test_y
#'
#' @return A dataframe for train/test x; a factor of level 0 or 1 for train/test y
#'
#' @examples
#'
#' get_data('train_x')
#'
#' @export
get_data <- function(name) {
    out <- read.csv(paste0('..//derived_data/',name,'.csv'))
    if (ncol(out) == 1) {
        out <- factor(out[,1])
    }
    return(out)
}
