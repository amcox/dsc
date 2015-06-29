library(dplyr)
library(stringr)
library(tidyr)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

d <- load_dsc_data_long(y=2015)

d$npr[d$npr == 0] <- 1
d$nce <- make_nce_from_percentile(d$npr)

d <- d %>% select(id, round, subject, nce)

save_df_as_csv(d, 'dsc data for tif')