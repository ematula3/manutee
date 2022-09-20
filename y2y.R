 # Year to Year in Wild Rice Lake


library(ggplot2)
library(readr)

#plot 1: Growth Rate
growth_rate <- read_csv("data/growth_rate.csv", 
                        col_types = cols(sample_date = col_date(format = "%m/%d/%Y"), 
                                         year = col_factor(levels = c("2021", 
                                                                      "2022"))))
View(growth_rate)

ggplot(data=growth_rate, aes(x=sample_date))+
  geom_smooth(aes(y=depth_cm), size=2, color="slateblue", se=FALSE)+
  geom_point(aes(y=avg_height_cm, color = year), size=4, shape=18)+
  geom_smooth(aes(y=avg_height_cm, color = year), se=FALSE, size=1.5, linetype = "twodash")+
  scale_color_manual(values = c("2021"="#edae49", "2022"="#00798c"))+
  ylim(0,250)+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 20))+
  xlab("Date")+
  ylab("cm")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data=y2y2, aes(x=cap_date))+
  geom_point(aes(y=total_stems, color = year), size=4, shape=18)+
  geom_smooth(aes(y=total_stems, color = year), se=FALSE, size=1.5, linetype = "twodash")+
  scale_color_manual(values = c("2021"="#edae49", "2022"="#00798c"))+
  ylim(0,150)+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 20))+
  xlab("Date")+
  ylab("Count")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#plot 2: Rice Occurances

# rice_occ <- read_csv("data/rice_occ.csv", 
#                      col_types = cols(sample_date = col_date(format = "%m/%d/%Y"), 
#                                       year = col_factor(levels = c("2021", 
#                                                                    "2022"))))
# View(rice_occ)
# 
# ggplot(data = rice_occ, aes(x=sample_date))+
#   geom_point(aes(y=count, color=year), size=4)+
#   geom_line(aes(y=count, color=year), size=1.5)+
#   scale_color_manual(values=c("2021"="#edae49","2022"="#00798c"))+
#   ylim(10,21)+
#   theme(axis.title.x = element_text(size = 20),
#       axis.text.x = element_text(size = 18),
#       axis.text.y = element_text(size = 18),
#       axis.title.y = element_text(size = 20))+
#   xlab("Date")+
#   ylab("cm")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))
#   

#plot 3: Pre Harvest and Post Germination Densities   

## Depth Hist
attach(growth_rate)
ggplot(data = growth_rate, aes(x=depth_cm))+
  geom_histogram(binwidth = 1)
library(Rmisc)
summarySE(growth_rate, measurevar = "depth_cm")

## Grow up and do mutations in R
y2y2 <- read_csv("data/y2y2.csv", col_types = cols(sample_date = col_date(format = "%m/%d/%Y"), 
                                                   cap_date = col_date(format = "%m/%d/%Y"), 
                                                   year = col_factor(levels = c("2021", 
                                                                                "2022"))))
y2y2$quad_id <- factor(y2y2$quad_id)
View(y2y2)

library(dplyr)

### Shallow and Deep points
attach(y2y2)
trial1 = case_when(depth_cm<=91 ~ 'shallow', TRUE ~ 'deep') # 3ft deep cutofff
trial2 = case_when(depth_cm<=60 ~ 'shallow', TRUE ~ 'deep') # half of the 0-4 ft range

y2y2$trial1 <- trial1
y2y2$trial2 <- trial2

View(y2y2)

y2y2$trial1 <- factor(y2y2$trial1)
y2y2$trial2 <- factor(y2y2$trial2)

### Pre and Post emergence
library(lubridate)
attach(y2y2)
emergance <- case_when(cap_date<= ymd('2022-06-24') ~ 'pre', TRUE ~ 'post')
y2y2$emergence <- emergance
y2y2$emergence <- factor(y2y2$emergence)


## Plot
### Violin <- needs woork
# ggplot(y2y2) +
#   aes(x = quad_id, y = total_stems, fill = year) +
#   geom_violin(adjust = 1L, scale = "area") +
#   scale_fill_hue(direction = 1) +
#   theme_bw() +
#   theme(legend.position = "bottom")+
#   facet_grid(vars(emergence))

### Box
# ggplot(y2y2) +
#   aes(x = quad_id, y = total_stems, fill = year) +
#   geom_boxplot() +
#   scale_fill_hue(direction = 1) +
#   theme_bw() +
#   facet_grid(vars(emergence))

# Plot 5
ggplot(data=y2y2, aes(x=cap_date))+
  geom_smooth(aes(y=depth_cm), size=2, color="slateblue", se=FALSE)+
  geom_point(aes(y=avg_height_cm, color = year), size=4, shape=18)+
  geom_smooth(aes(y=avg_height_cm, color = year), se=FALSE, size=1.5, linetype = "twodash")+
  scale_color_manual(values = c("2021"="#edae49", "2022"="#00798c"))+
  facet_wrap(vars(trial1))+
  ylim(0,250)+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 20))+
  xlab("Date")+
  ylab("cm")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
## faceted trial 1 and 2

# Plot 5.5
ggplot(data=y2y2, aes(x=cap_date))+
  geom_point(aes(y=total_stems, color = year), size=4, shape=18)+
  geom_smooth(aes(y=total_stems, color = year), se=FALSE, size=1.5, linetype = "twodash")+
  scale_color_manual(values = c("2021"="#edae49", "2022"="#00798c"))+
  facet_wrap(vars(trial1))+
  ylim(0,150)+
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 20))+
  xlab("Date")+
  ylab("Count")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Plot 6
## violin
# ggplot(y2y2) +
#   aes(x = quad_id, y = avg_height_cm, fill = year) +
#   geom_violin(adjust = 1L, scale = "area") +
#   scale_fill_hue(direction = 1) +
#   theme_bw() +
#   facet_grid(vars(trial1))

##box
# ggplot(y2y2) +
#   aes(x = quad_id, y = avg_height_cm, fill = year) +
#   geom_boxplot(adjust = 1L, scale = "area") +
#   scale_fill_hue(direction = 1) +
#   theme_bw() +
#   facet_grid(vars(trial1))


