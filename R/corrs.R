#' Phishing Data Correlation plot
#'
#' This function returns a correlation plot based on all variables in the original phishing data
#'
#' @return A ggplot object of the correlation plot
#'
#' @examples
#'
#' phishing_corrs_plot()
#'
#' @export

phishing_corrs_plot <- function(){
    library(tidyverse)
    library(GGally)
    library(ggcorrplot)
    df <- read_csv("source_data/web-page-phishing.csv")
    summary(df$url_length)
   # ggplot(data=df, mapping=aes(x=url_length)) +
   #      geom_histogram() + 
   #      theme_minimal()
    #df %>% arrange(., by_group=desc(url_length)) %>% View(.)
    # at least one clear outlier in terms of url_length
    c <- ggcorrplot(round(cor(df), 1), method="square", type="upper", 
               hc.order = TRUE, lab = TRUE) + 
        theme_minimal() + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.text.x = element_text(angle = 45, vjust=1, hjust=0.1)) +
        scale_x_discrete(position = "top")
    ggsave(c, )
    return(c)
}
