library(reshape2)
library(dplyr)
library(ggplot2)
library(scales)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

make_pk_dsc_eval_setting_plot <- function(d, year.string) {
	ggplot(d, aes(x=percentile.ecdf, y=perc.50.better))+
		geom_text(aes(label=round(perc.50.better*100)), size=1.75)+
		geom_vline(x=c(0.1, 0.55, 0.9), linetype=3)+
		scale_y_continuous(limits=c(0,1), label=percent)+
		scale_x_continuous(limits=c(0,1), breaks=seq(0, 1, 0.1), label=percent)+
		labs(x="Percent of Teachers at or Below Performance Level",
			y="Percent of Students in Class\nEnding at 50th NPR or Higher",
			title=paste0("PK DSC Data, ", year.string)
		)+
		theme_bw()+
		theme(axis.title.x = element_text(size=8),
			axis.title.y = element_text(size=8),
			axis.text.x = element_text(size=7),
			axis.text.y = element_text(size=7),
			plot.title = element_text(size=9)
		)+
		facet_wrap(~subject, ncol=1)
}

# 13-14 DSC Data Eval Setting Plot
d <- load_dsc_data_long(y=2014)
d <- subset(d, !is.na(npr) & round == 'spring')

dg <- d %>% group_by(teacher, subject) %>%
	summarize(perc.50.better = sum(npr >= 50)/n(), n = n()) %>%
	group_by(subject) %>%
	mutate(percentile.ecdf=ecdf(perc.50.better)(perc.50.better)) %>%
	arrange(perc.50.better)

p <- make_pk_dsc_eval_setting_plot(dg, '2013-14')
save_plot_as_pdf_adjustable(p, 'PK DSC 13-14', w=7.5, h=9)

dg$year <- rep('2014', nrow(dg))
teacher.table <- dg

# 14-15 STEP Data Eval Setting Plot
d <- load_dsc_data_long(y=2015)
d <- subset(d, !is.na(npr) & round == 'spring')

dg <- d %>% group_by(teacher, subject) %>%
	summarize(perc.50.better = sum(npr >= 50)/n(), n = n()) %>%
	group_by(subject) %>%
	mutate(percentile.ecdf=ecdf(perc.50.better)(perc.50.better)) %>%
	arrange(perc.50.better)

p <- make_pk_dsc_eval_setting_plot(dg, '2014-15')
save_plot_as_pdf_adjustable(p, 'PK DSC 14-15', w=7.5, h=9)

dg$year <- rep('2015', nrow(dg))
teacher.table <- rbind(teacher.table, dg)

save_df_as_csv(teacher.table, 'PK DSC Eval Setting Teacher Percentiles')