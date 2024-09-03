# Tyler Steven Coleman
# Anthony Johnson
# Jasmine Nasser
# Maggie McGreal
# Chris Murray
#
# 
#
##### ----------------------------------------------------------------------------------------------
#####  BASS STRESS ---------------------------------------------------------------------------------
##### ----------------------------------------------------------------------------------------------
#' goal here is to ...
#' 



# clear the environment
rm(list=ls())

##### ----------------------------------------------------------------------------------------------
##### PACKAGES NEEDED ------------------------------------------------------------------------------
##### ----------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)      # figures
library(Rmisc)        # random cool functions
library(fitdistrplus) # test distributions
library(lubridate)    # date
library(ggpubr)       # ggplot arrange
library(stringr)      # character replacement


##### ----------------------------------------------------------------------------------------------
##### YSI ------------------------------------------------------------------------------------------
##### ----------------------------------------------------------------------------------------------
# just making a figure of YSI stuff

##### ----------------------------------------------------------------------------------------------
##### DATA -----------------------------------------------------------------------------------------
##### ----------------------------------------------------------------------------------------------
# read in data
dat_ysi <- read.csv("data/bass_ysi.csv",
                    header = TRUE)
summary(dat_ysi)
head(dat_ysi)

# play with data
dat_ysi$week <- as.factor(dat_ysi$week)

dat_ysi$datedate <- mdy(dat_ysi$date)
class(dat_ysi$datedate)
dat_ysi$month_day <- class(format(dat_ysi$datedate, format="%m-%d"))

summary(dat_ysi)

# add Day number to dataset
dat_ysi <- dat_ysi %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(dayday = datedate - first(datedate) + 2)
dat_ysi <- dat_ysi %>%
  dplyr::mutate(dayday = str_remove(dayday, " days"))
dat_ysi <- dat_ysi %>% 
  dplyr::mutate(dayday = str_replace(dayday, "^", "Day ")) %>% 
  as.data.frame()

# order dayday column
day_order <- c("Day 2", "Day 3", "Day 10", 
               "Day 11", "Day 14", "Day 15", 
               "Day 17")
dat_ysi$dayday <- factor(dat_ysi$dayday, levels = day_order)

head(dat_ysi)


ggplot() +
  geom_jitter(mapping = aes(x = as.factor(dayday),
                            y = mmhg,
                            color = week),
              data = dat_ysi) +
  scale_y_continuous(limits = c(755, 770)) +
  theme_bw()

# convert temperature from F to C
dat_ysi$temp_c <- (dat_ysi$temp_f - 32) * (5 / 9)




# scale_for_xaxis <- c("Week 1",
#                      "Week 2",
#                      "Week 3")
custom_colors <- c("cd_high" = "red",
                   "cd_low" = "hotpink",
                   "control" = "gray50",
                   "pfas_low" = "deepskyblue",
                   "pfas_high" = "navy")
custom_labels <- c("Cadmium High",
                   "Cadmium Low",
                   "Control",
                   "PFPeA High",
                   "PFPeA Low")
custom_breaks <- c("cd_high", 
                   "cd_low",
                   "control",
                   "pfas_high",
                   "pfas_low")

# create the plot with custom color mapping TEMP
temp_plot <- 
  ggplot(dat_ysi, 
         aes(x = factor(dayday), 
             y = temp_c, 
             color = treatment)) +
    geom_point(position = position_jitter(width = 0.2, height = 0), 
               size = 3,
               alpha = 0.60) +
    labs(x = "", 
         y = "Temperature (°C)") +
    scale_y_continuous(limits = c(15, 25),
                       breaks = seq(15, 25, 2),
                       labels = seq(15, 25, 2)) +
    scale_color_manual(values = custom_colors,
                       name = "Group",
                       breaks = custom_breaks,
                       labels = custom_labels) +  # Apply custom color palette
    theme_classic() +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 13,
                                     face = "bold"),
          axis.title.y = element_text(size = 13,
                                      face = "bold"))
temp_plot

# create the scatter plot with custom color mapping DO mgL
do_plot <- 
  ggplot(dat_ysi, 
         aes(x = factor(dayday), 
             y = do_mg.l, 
             color = treatment)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), 
             size = 3,
             alpha = 0.60) +
  labs(x = "", 
       y = "Dissolved Oxygen (mg/L)") +
  scale_y_continuous(limits = c(5, 10)) +
  scale_color_manual(values = custom_colors,
                     name = "Group",
                     breaks = custom_breaks,
                     labels = custom_labels) +  
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11,
                                   face = "bold",
                                   angle = 45,
                                   vjust = 0.6),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 13,
                                   face = "bold"),
        axis.title.y = element_text(size = 13,
                                    face = "bold"))
do_plot

# create the scatter plot with custom color mapping PH
ph_plot <- 
  ggplot(dat_ysi, 
         aes(x = factor(dayday), 
             y = pH, 
             color = treatment)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), 
             size = 3,
             alpha = 0.60) +
  labs(x = "", 
       y = "pH") +
  scale_y_continuous(limits = c(7, 9.5),
                     labels = seq(7, 9, 1),
                     breaks = seq(7, 9, 1)) +
  scale_color_manual(values = custom_colors,
                     name = "Group",
                     breaks = custom_breaks,
                     labels = custom_labels) +  
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 13,
                                   face = "bold"),
        axis.title.y = element_text(size = 13,
                                    face = "bold"))
ph_plot

# create the scatter plot with custom color mapping salt
salt_plot <- 
  ggplot(dat_ysi, 
         aes(x = factor(dayday), 
             y = sal.ppt, 
             color = treatment)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), 
             size = 3,
             alpha = 0.60) +
  labs(x = "", 
       y = "Salinity (ppt)") +
  # scale_y_continuous(limits = c(7, 10)) +
  scale_color_manual(values = custom_colors,
                     name = "Group",
                     breaks = custom_breaks,
                     labels = custom_labels) +  
  theme_classic() +
  theme(legend.position = c(0.35, 0.6),
        legend.text = element_text(size = 10,
                                   face = "bold"),
        legend.title = element_text(size = 10,
                                    face = "bold"),
        axis.text.x = element_text(size = 11,
                                   face = "bold",
                                   angle = 45,
                                   vjust = 0.6),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 13,
                                   face = "bold"),
        axis.title.y = element_text(size = 13,
                                    face = "bold"))
salt_plot

# make figure #
tmp_plot <- 
  ggarrange(temp_plot,
            ph_plot,
            do_plot,
            salt_plot,
            ncol = 2, nrow = 2,
            heights = c(0.775, 1, 0.775, 1))
tmp_plot

ysi_figure <- tmp_plot

### save figure ###
# ggsave("writing/figures/ysi_figure.png",
#        plot = ysi_figure,
#        dpi = 1000,
#        height = 8,
#        width = 12,
#        units = "in")
# ggsave("writing/figures/ysi_figure.pdf",
#        plot = ysi_figure,
#        dpi = 1000,
#        height = 8,
#        width = 12,
#        units = "in")





##### ----------------------------------------------------------------------------------------------
##### STRESS ---------------------------------------------------------------------------------------
##### ----------------------------------------------------------------------------------------------

##### ----------------------------------------------------------------------------------------------
##### DATA -----------------------------------------------------------------------------------------
##### ----------------------------------------------------------------------------------------------
# read in data
dat_og <- read.csv("data/bass_nlr.csv",
                   header = TRUE)
summary(dat_og)
head(dat_og)
dat <- dat_og
dat$date <- mdy(dat_og$date_arrive)
dat$id <- as.factor(dat_og$id)
dat$wr <- as.numeric(dat_og$condition_num)
dat$nlr <- as.numeric(dat_og$nlr)
dat$treatment <- (dat_og$treatment)

dat <- dat[ ,c("date", "id", "wr", "treatment", "nlr")]
summary(dat)

# remove treatment group na's
dat <- dat %>% 
  dplyr::filter(treatment != "na")
summary(dat)

dat$treatment <- as.factor(dat$treatment)
summary(dat)

dat$salt_time <- ifelse(dat$date < "2024-02-20", "pre", "post")





##### ----------------------------------------------------------------------------------------------
##### ANALYSES -------------------------------------------------------------------------------------
##### ----------------------------------------------------------------------------------------------
summary(dat)

dat$blood <- ifelse(dat$date == "2024-02-08", "Blood 1", 
                    ifelse(dat$date == "2024-02-19", "Blood 2",
                           ifelse(dat$date == "2024-02-20", "Blood 3",
                                  "Blood 4")))
dat$blood <- as.factor(dat$blood)

dat$blood_num <- ifelse(dat$date == "2024-02-08", 1, 
                      ifelse(dat$date == "2024-02-19", 2,
                             ifelse(dat$date == "2024-02-20", 3, 4)))

length(which(is.na(dat$nlr) == TRUE))
dat <- dat %>% 
  filter(nlr != "NA")

hist(dat$nlr,
     breaks = 50)
unique(dat$nlr)



# check for outliers (outside 99% of data)
one_perc_quant <- quantile(dat$nlr, 0.01, na.rm = TRUE)
ninenine_perc_quant <- quantile(dat$nlr, 0.99, na.rm = TRUE)

# check distribution of data
descdist(dat$nlr,
         discrete = FALSE,
         boot = 500) # gamma
descdist(log(dat$nlr),
         discrete = FALSE,
         boot = 500) # gamma
plotdist(dat$nlr,
         histo = TRUE, 
         demp = TRUE)

summarySE(data = dat, measurevar = "nlr", groupvars = c("treatment", "blood"))
# order dayday column
treatment_order <- c("control", "cd_low", "cd_high", "pfas_low", "pfas_high")
dat$treatment <- factor(dat$treatment, levels = treatment_order)
treat_labels <- c(`control` = "Control", 
                  `cd_low` = "Cd Low",
                  `cd_high` = "Cd High",
                  `pfas_low` = "PFPeA Low",
                  `pfas_high` = "PFPeA High")
treat_fig <- 
  ggplot() +
    geom_jitter(mapping = aes(x = blood,
                             y = nlr,
                             color = treatment), 
                width = 0.0,
                size = 4,
                alpha = 0.50,
                data = dat) +
    geom_smooth(mapping = aes(x = blood,
                              y = nlr,
                              color = treatment),
                method = "lm",
                lwd = 2,
                se = FALSE,
                data = dat) +
    facet_wrap(~treatment, 
               labeller = labeller(treatment = treat_labels), 
               ncol = 5) +
    scale_color_manual(values = custom_colors) +
    labs(y = "Neutrophil-to-Lymphocyte Ratio (NLR)") +
    geom_vline(xintercept = 1.5, linetype = "solid", color = "black", linewidth = 0.5) +
    geom_vline(xintercept = 2.5, linetype = "dashed", color = "black", linewidth = 0.5) +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 11,
                                     face = "bold",
                                     angle = 45,
                                     vjust = 0.6),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 13,
                                     face = "bold"),
          axis.title.y = element_text(size = 13,
                                      face = "bold"),
          strip.text = element_text(size = 11,
                                    face = "bold"),
          strip.background = element_rect(fill = "grey90"))
treat_fig

### save figure ###
# ggsave("writing/figures/treat_figure.png",
#        plot = treat_fig,
#        dpi = 1000,
#        height = 4,
#        width = 12,
#        units = "in")
# ggsave("writing/figures/treat_figure.pdf",
#        plot = treat_fig,
#        dpi = 1000,
#        height = 4,
#        width = 12,
#        units = "in")


# figure with blood samples on x axis and slopes between each plotted
unique(dat$treatment)
# spline_control <- as.data.frame(spline(dat$blood_num[dat$treatment == "control"],
#                                        dat$nlr[dat$treatment == "control"]))
control_dat <- dat[dat$treatment == "control", ]
control_dat_12 <- control_dat[control_dat$blood_num == 1 | control_dat$blood_num == 2, ]
control_dat_23 <- control_dat[control_dat$blood_num == 2 | control_dat$blood_num == 3, ]
control_dat_34 <- control_dat[control_dat$blood_num == 3 | control_dat$blood_num == 4, ]
pfas_high_dat <- dat[dat$treatment == "pfas_high", ]
pfas_high_dat_12 <- pfas_high_dat[pfas_high_dat$blood_num == 1 | pfas_high_dat$blood_num == 2, ]
pfas_high_dat_23 <- pfas_high_dat[pfas_high_dat$blood_num == 2 | pfas_high_dat$blood_num == 3, ]
pfas_high_dat_34 <- pfas_high_dat[pfas_high_dat$blood_num == 3 | pfas_high_dat$blood_num == 4, ]
pfas_low_dat <- dat[dat$treatment == "pfas_low", ]
pfas_low_dat_12 <- pfas_low_dat[pfas_low_dat$blood_num == 1 | pfas_low_dat$blood_num == 2, ]
pfas_low_dat_23 <- pfas_low_dat[pfas_low_dat$blood_num == 2 | pfas_low_dat$blood_num == 3, ]
pfas_low_dat_34 <- pfas_low_dat[pfas_low_dat$blood_num == 3 | pfas_low_dat$blood_num == 4, ]
cd_high_dat <- dat[dat$treatment == "cd_high", ]
cd_high_dat_12 <- cd_high_dat[cd_high_dat$blood_num == 1 | cd_high_dat$blood_num == 2, ]
cd_high_dat_23 <- cd_high_dat[cd_high_dat$blood_num == 2 | cd_high_dat$blood_num == 3, ]
cd_high_dat_34 <- cd_high_dat[cd_high_dat$blood_num == 3 | cd_high_dat$blood_num == 4, ]
cd_low_dat <- dat[dat$treatment == "cd_low", ]
cd_low_dat_12 <- cd_low_dat[cd_low_dat$blood_num == 1 | cd_low_dat$blood_num == 2, ]
cd_low_dat_23 <- cd_low_dat[cd_low_dat$blood_num == 2 | cd_low_dat$blood_num == 3, ]
cd_low_dat_34 <- cd_low_dat[cd_low_dat$blood_num == 3 | cd_low_dat$blood_num == 4, ]

ind.slopes.fig <- 
  ggplot() +
  geom_jitter(mapping = aes(x = blood_num,
                           y = nlr,
                           color = treatment),
             size = 3,
             alpha = 0.30,
             width = 0.15,
             data = dat) +
  geom_line(mapping = aes(x = blood_num,
                            y = nlr,
                            color = treatment),
            stat = "smooth",
            method = "lm",
            lwd = 2,
            se = FALSE,
            alpha = 0.75,
            position = position_dodge(0.3),
            data = control_dat_12) +
  geom_line(mapping = aes(x = blood_num,
                          y = nlr,
                          color = treatment),
            stat = "smooth",
            method = "lm",
            lwd = 2,
            se = FALSE,
            alpha = 0.75,
            position = position_dodge(0.3),
            data = control_dat_23) +
  geom_line(mapping = aes(x = blood_num,
                          y = nlr,
                          color = treatment),
            stat = "smooth",
            method = "lm",
            lwd = 2,
            se = FALSE,
            alpha = 0.75,
            position = position_dodge(0.3),
            data = control_dat_34) +
  geom_line(mapping = aes(x = blood_num,
                            y = nlr,
                            color = treatment),
            stat = "smooth",
            method = "lm",
            lwd = 2,
            se = FALSE,
            alpha = 0.75,
            position = position_dodge(0.3),
            data = pfas_high_dat_12) +
  geom_line(mapping = aes(x = blood_num,
                          y = nlr,
                          color = treatment),
            stat = "smooth",
            method = "lm",
            lwd = 2,
            se = FALSE,
            alpha = 0.75,
            position = position_dodge(0.3),
            data = pfas_high_dat_23) +
  geom_line(mapping = aes(x = blood_num,
                          y = nlr,
                          color = treatment),
            stat = "smooth",
            method = "lm",
            lwd = 2,
            se = FALSE,
            alpha = 0.75,
            position = position_dodge(0.3),
            data = pfas_high_dat_34) +
  geom_line(mapping = aes(x = blood_num,
                            y = nlr,
                            color = treatment),
            stat = "smooth",
            method = "lm",
            lwd = 2,
            se = FALSE,
            alpha = 0.75,
            position = position_dodge(0.3),
            data = pfas_low_dat_12) +
  geom_line(mapping = aes(x = blood_num,
                          y = nlr,
                          color = treatment),
            stat = "smooth",
            method = "lm",
            lwd = 2,
            se = FALSE,
            alpha = 0.75,
            position = position_dodge(0.3),
            data = pfas_low_dat_23) +
  geom_line(mapping = aes(x = blood_num,
                          y = nlr,
                          color = treatment),
            stat = "smooth",
            method = "lm",
            lwd = 2,
            se = FALSE,
            alpha = 0.75,
            position = position_dodge(0.3),
            data = pfas_low_dat_34) +
  geom_line(mapping = aes(x = blood_num,
                            y = nlr,
                            color = treatment),
            stat = "smooth",
            method = "lm",
            lwd = 2,
            se = FALSE,
            alpha = 0.75,
            position = position_dodge(0.3),
            data = cd_high_dat_12) +
  geom_line(mapping = aes(x = blood_num,
                          y = nlr,
                          color = treatment),
            stat = "smooth",
            method = "lm",
            lwd = 2,
            se = FALSE,
            alpha = 0.75,
            position = position_dodge(0.3),
            data = cd_high_dat_23) +
  geom_line(mapping = aes(x = blood_num,
                          y = nlr,
                          color = treatment),
            stat = "smooth",
            method = "lm",
            lwd = 2,
            se = FALSE,
            alpha = 0.75,
            position = position_dodge(0.3),
            data = cd_high_dat_34) +
  geom_line(mapping = aes(x = blood_num,
                            y = nlr,
                            color = treatment),
            stat = "smooth",
            method = "lm",
            lwd = 2,
            se = FALSE,
            alpha = 0.75,
            position = position_dodge(0.3),
            data = cd_low_dat_12) +
  geom_line(mapping = aes(x = blood_num,
                          y = nlr,
                          color = treatment),
            stat = "smooth",
            method = "lm",
            lwd = 2,
            se = FALSE,
            alpha = 0.75,
            position = position_dodge(0.3),
            data = cd_low_dat_23) +
  geom_line(mapping = aes(x = blood_num,
                          y = nlr,
                          color = treatment),
            stat = "smooth",
            method = "lm",
            lwd = 2,
            se = FALSE,
            alpha = 0.75,
            position = position_dodge(0.3),
            data = cd_low_dat_34) +
  scale_color_manual(values = custom_colors, labels = treat_labels) +
  labs(y = "Neutrophil-to-Lymphocyte Ratio (NLR)",
       x = "Blood Sample") +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "black", linewidth = 0.5) +
  annotate(geom = "text", x = 1.6, y = 4, angle = 60, label = "Treatment", color = "black") +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "black", linewidth = 0.5) +
  annotate(geom = "text", x = 2.55, y = 4, angle = 60, label = "Salt", color = "black") +
  theme_bw() +
  theme(legend.position = c(0.10, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 12),
        legend.background = element_rect(fill = "transparent"), 
        axis.text.x = element_text(size = 11,
                                   face = "bold",
                                   angle = 45,
                                   vjust = 0.6),
        axis.title.x = element_text(size = 13,
                                    face = "bold"),
        axis.text.y = element_text(size = 13,
                                   face = "bold"),
        axis.title.y = element_text(size = 13,
                                    face = "bold"),
        strip.text = element_text(size = 11,
                                  face = "bold"),
        strip.background = element_rect(fill = "grey90"))
ind.slopes.fig
### save figure ###
# ggsave("writing/figures/ind.slopes_treat_figure.png",
#        plot = ind.slopes.fig,
#        dpi = 1000,
#        height = 8,
#        width = 12,
#        units = "in")
# ggsave("writing/figures/ind.slopes_treat_figure.pdf",
#        plot = ind.slopes.fig,
#        dpi = 1000,
#        height = 8,
#        width = 12,
#        units = "in")




### group * date ###
# do groups respond differently temporally 
# blood 1 ref 
fit_control_b1 <- glm(nlr ~ relevel(treatment, ref = "control") * blood,
                                 data = dat,
                                 family = Gamma(link = "log"))
fit_cd_high_b1 <- glm(nlr ~ relevel(treatment, ref = "cd_high") * blood,
                                 data = dat,
                                 family = Gamma(link = "log"))
fit_cd_low_b1 <- glm(nlr ~ relevel(treatment, ref = "cd_low") * blood,
                                 data = dat,
                                 family = Gamma(link = "log"))
fit_pfas_high_b1 <- glm(nlr ~ relevel(treatment, ref = "pfas_high") * blood,
                                 data = dat,
                                 family = Gamma(link = "log"))
fit_pfas_low_b1 <- glm(nlr ~ relevel(treatment, ref = "pfas_low") * blood,
                                 data = dat,
                                 family = Gamma(link = "log"))
# blood 2 ref
fit_control_b2 <- glm(nlr ~ relevel(treatment, ref = "control") * relevel(blood, ref = "Blood 2"),
                                 data = dat,
                                 family = Gamma(link = "log"))
fit_cd_high_b2 <- glm(nlr ~ relevel(treatment, ref = "cd_high") * relevel(blood, ref = "Blood 2"),
                                 data = dat,
                                 family = Gamma(link = "log"))
fit_cd_low_b2 <- glm(nlr ~ relevel(treatment, ref = "cd_low") * relevel(blood, ref = "Blood 2"),
                                 data = dat,
                                 family = Gamma(link = "log"))
fit_pfas_high_b2 <- glm(nlr ~ relevel(treatment, ref = "pfas_high") * relevel(blood, ref = "Blood 2"),
                                 data = dat,
                                 family = Gamma(link = "log"))
fit_pfas_low_b2 <- glm(nlr ~ relevel(treatment, ref = "pfas_low") * relevel(blood, ref = "Blood 2"),
                                 data = dat,
                                 family = Gamma(link = "log"))
# blood 3 ref
fit_control_b3 <- glm(nlr ~ relevel(treatment, ref = "control") * relevel(blood, ref = "Blood 3"),
                                 data = dat,
                                 family = Gamma(link = "log"))
fit_cd_high_b3 <- glm(nlr ~ relevel(treatment, ref = "cd_high") * relevel(blood, ref = "Blood 3"),
                                 data = dat,
                                 family = Gamma(link = "log"))
fit_cd_low_b3 <- glm(nlr ~ relevel(treatment, ref = "cd_low") * relevel(blood, ref = "Blood 3"),
                                 data = dat,
                                 family = Gamma(link = "log"))
fit_pfas_high_b3 <- glm(nlr ~ relevel(treatment, ref = "pfas_high") * relevel(blood, ref = "Blood 3"),
                                 data = dat,
                                 family = Gamma(link = "log"))
fit_pfas_low_b3 <- glm(nlr ~ relevel(treatment, ref = "pfas_low") * relevel(blood, ref = "Blood 3"),
                                 data = dat,
                                 family = Gamma(link = "log"))
summary(fit_control_b1)
anova(fit_control_b1)

summary(fit_cd_high_b1)
summary(fit_cd_low_b1)
summary(fit_pfas_high_b1)
summary(fit_pfas_low_b1)
summary(fit_control_b2)
summary(fit_cd_high_b2)
summary(fit_cd_low_b2)
summary(fit_pfas_high_b2)
summary(fit_pfas_low_b2)
summary(fit_control_b3)
summary(fit_cd_high_b3)
summary(fit_cd_low_b3)
summary(fit_pfas_high_b3)
summary(fit_pfas_low_b3)

test_fig <- ggplot(data = dat) +
  geom_jitter(mapping = aes(x = date,
                            y = log(nlr),
                            color = treatment),
              data = dat,
              alpha = 0.5,
              size = 3) +
  geom_smooth(mapping = aes(x = date,
                            y = log(nlr),
                            color = treatment),
              data = dat,
              method = "lm",
              lwd = 2,
              se = FALSE) +
  scale_color_manual(values = custom_colors) +
  theme_bw()
test_fig

# output <- TukeyHSD(aov(fit_temporal))
# which(output$`treatment:blood`[,4] < 0.1)

#' For a more thorough analysis, you might want to compare the fit of the model
#' with the interaction term against a model without the interaction term using
#' a likelihood ratio test. This can be done if both models are nested.
# fit a reduced model without the interaction
fit_temporal <- glm(nlr ~ treatment * blood,
                    data = dat,
                    family = Gamma(link = "log"))
model_reduced <- glm(nlr ~ treatment + blood, 
                     data = dat, 
                     family = Gamma(link = "log"))
# compare models
anova(model_reduced, fit_temporal, test = "Chisq")
#' This test will provide a Chi-squared statistic and a p-value:
#' A significant p-value suggests that the model with the interaction
#' term (more complex model) provides a significantly better fit to the 
#' data than the model without the interaction term.

summary(model_reduced)

# custom_colors <- c("cd_high" = "red",
#                    "cd_low" = "pink",
#                    "control" = "gray50",
#                    "pfas_low" = "deepskyblue",
#                    "pfas_high" = "navy")
# custom_labels <- c("Cadmium High",
#                    "Cadmium Low",
#                    "Control",
#                    "PFPeA High",
#                    "PFPeA Low")
# custom_breaks <- c("cd_high", 
#                    "cd_low",
#                    "control",
#                    "pfas_high",
#                    "pfas_low")



# do groups respond differently to salt 