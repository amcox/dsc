library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)

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

# DSC NCE averages by school
d.avg <- dm %>% group_by(school, round, subject) %>% summarize(nce.mean=mean(nce))
p <- ggplot(d.avg, aes(x=round, y=nce.mean, color=school))+
  geom_line(aes(group=school), size=1)+
  scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, 10))+
  labs(x='Round',
    y='Average NCE',
    title='PK DSC Averages by School'
  )+
  theme_bw()+
  facet_wrap(~subject) 
save_plot_as_pdf(p, 'DSC Averages by School')

# DSC NCE averages by teacher
d.avg <- dm %>% group_by(school, teacher, round, subject) %>% summarize(nce.mean=mean(nce))
p <- ggplot(d.avg, aes(x=round, y=nce.mean, color=teacher))+
  geom_line(aes(group=teacher), size=1)+
  scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, 10))+
  labs(x='Round',
    y='Average NCE',
    title='PK DSC Averages by Teacher'
  )+
  theme_bw()+
  facet_grid(subject~school) 
save_plot_as_pdf(p, 'DSC Averages by Teacher')

# Percent of students that met goal by school
d.met <- dm %>% group_by(school, round, subject) %>% summarize(perc.above.50=mean(npg >= 50))
p <- ggplot(d.met, aes(x=round, y=perc.above.50, color=school))+
  geom_line(aes(group=school), size=1)+
  scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, .1), labels=percent)+
  labs(x='Round',
    y='Percent of Students At or Above 50th Percentile',
    title='PK DSC Percent at Goal by School'
  )+
  theme_bw()+
  facet_wrap(~subject) 
save_plot_as_pdf(p, 'DSC Percent at Goal by School')

# Percent of students that met goal by teacher
d.met <- dm %>% group_by(school, teacher, round, subject) %>% summarize(perc.above.50=mean(npg >= 50))
p <- ggplot(d.met, aes(x=round, y=perc.above.50, color=teacher))+
  geom_line(aes(group=teacher), size=1)+
  scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, .1), labels=percent)+
  labs(x='Round',
    y='Percent of Students At or Above 50th Percentile',
    title='PK DSC Percent at Goal by Teacher'
  )+
  theme_bw()+
  facet_grid(subject~school) 
save_plot_as_pdf(p, 'DSC Percent at Goal by Teacher')