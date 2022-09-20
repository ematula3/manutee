# core figure for manuscript

library(readr)
library(ggplot2)


fall21_cores <- read_csv("data/fall21_cores.csv")
fall21_cores$site <- factor(fall21_cores$site)
fall21_cores$seed_type <- factor(fall21_cores$seed_type)
fall21_cores$depth <- factor(fall21_cores$depth, levels = c("29-30","28-29","27-28","26-27","25-26","24-25","23-24","22-23","21-22","20-21","19-20","18-19","17-18","16-17","15-16","14-15","13-14","12-13","11-12","10-11","9-10","8-9","7-8","6-7","5-6","4-5","3-4","2-3","1-2","0-1"))

#("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18","18-19","19-20","20-21","21-22","22-23","23-24","24-25","25-26","26-27","27-28","28-29","29-30")

ggplot(fall21_cores) +
  aes(x = depth, fill = seed_type, weight = density) +
  geom_bar(color = "black") +
  coord_flip() +
  scale_fill_hue(direction = 1) +
  theme_bw() +
  facet_wrap(vars(site))+
  labs(x = "Depth (cm)",
  y = "Seed Density (m^2)")+
  scale_fill_discrete(name = "Seed Type", labels = c("Nonviable", "Viable"))


#summer 2022 cores
## not using
cores_5_22 <- read.csv("data/cores_5_22.csv")
View(cores_5_22)
cores_5_22$site <- factor(cores_5_22$site, levels = c("WR5", "WR8","WR18"))
cores_5_22$core <- factor(cores_5_22$core, levels = c("1","2","3","4","5"))

ggplot(cores_5_22) +
  aes(x = core, weight = density) +
  geom_bar(fill = "#112446") +
  theme_bw() +
  facet_wrap(vars(site))+
  labs(y = "Seed Density (m^2)")














