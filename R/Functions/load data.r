load_dsc_data <- function(y=2015){
  read.csv(file=paste0("./../Data/dsc data ", y, ".csv"), head=TRUE, na.string=c("", " ", "  "), stringsAsFactors=F)
}

load_dsc_data_long <- function(y=2015) {
	library(tidyr)
	d <- load_dsc_data(y=y)
	dl <- d %>% gather(measure, npr, fall.math:spring.print) %>%
		separate(measure, into = c("round", "subject"), sep = "\\.")
	return(dl)
}