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

df <- load_dsc_data()
dm <- df %>%
  gather(measure, npg, -c(id, first.name, last.name, teacher, school), na.rm = TRUE) %>%
  separate(measure, c('round', 'subject'))
dm$npg[dm$npg == 0] <- 1
dm$nce <- make_nce_from_percentile(dm$npg)

names(dm)[names(dm) == 'id'] <- 'student_number'
dm <- dm[, c('student_number', 'round', 'subject', 'nce')]

save_df_as_csv(dm, 'dsc data for tif')