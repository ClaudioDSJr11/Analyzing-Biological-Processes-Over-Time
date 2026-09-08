# Packages
library(readxl) # Reading excel files
library(tidyverse) # Wrangling
library(msm) # Delta method
library(lme4) # Linear mixed models
library(emmeans) # emmeans for mult comp
library(glmmTMB) # beta regression / glmm

# Read the data
df <- read_excel("Example_df1.xlsx")

# Control trt factor order
df <- df %>%
  mutate(
    trt = as.factor(trt),
    trt = fct_relevel(trt, c("UN", "F1", "F2")))

# Fit the model
m3 <- glmmTMB(y ~ trt*day + (1|block), data = df, family = beta_family(link = "logit"))

# Check model parameters
summary(m3)

# Obtain predictions + plot
pred_df3 <- data.frame(
  trt = as.factor(c(rep("UN", 60), rep("F1", 60), rep("F2", 60))),
  day = c(rep(seq(1, 60, by = 1), 3)))

pred_df3$trt <- fct_relevel(pred_df3$trt, c("UN", "F1", "F2"))

pred_df3$pred_l <- predict(m3, newdata = pred_df3, re.form = NA, type = "link") # link scale
pred_df3$pred_p <- predict(m3, newdata = pred_df3, re.form = NA, type = "response") # response scale

ggplot(pred_df3, aes(day, pred_p, colour = trt, group = trt)) +
  geom_line() +
  #  geom_point(aes(day, pred_l, colour = trt)) +
  scale_color_manual(values = c("turquoise4", "#8B0000", "green4")) +
  theme_bw() +
  labs(y = "Severity (Prop.)", x = "Days", colour = "Treatment")

# Slope

## We can compare the slope on the link scale:
slopes_link <- emtrends(m3, ~ trt, var = "day")
pairs(slopes_link)

slopes3 <- data.frame(slopes_link)

## We can also find the max slopes at the response scale, and compare then. As a shortcut, here the maximum occurs at mu = 0.5 (this max slope is also a derived quantity).
der_df3 <- data.frame(
  trt = c("UN", "F1", "F2"),
  y_inf = c(0.5, 0.5, 0.5),
  x_inf = c(-fixef(m3)$cond[1]/slopes3$day.trend[1], -(fixef(m3)$cond[1]+fixef(m3)$cond[2])/slopes3$day.trend[2], -(fixef(m3)$cond[1]+fixef(m3)$cond[3])/slopes3$day.trend[3]),
  max_s = c(slopes3$day.trend[1]/4, slopes3$day.trend[2]/4, slopes3$day.trend[3]/4),
  se = c(slopes3$SE[1]/4, slopes3$SE[2]/4, slopes3$SE[3]/4),
  CI.lb = c(slopes3$asymp.LCL[1]/4, slopes3$asymp.LCL[2]/4, slopes3$asymp.LCL[3]/4),
  CI.ub = c(slopes3$asymp.UCL[1]/4, slopes3$asymp.UCL[2]/4, slopes3$asymp.UCL[3]/4)
)

### Max slope contrast
cont_s3 <- data.frame(
  contrast = c("UN-F1", "UN-F2", "F1-F2"),
  z = c(
    
    (der_df3$max_s[1] - der_df3$max_s[2])/(deltamethod(~ x4/4 - (x4+x5)/4, fixef(m3)$cond, vcov(m3)$cond)),
    
    (der_df3$max_s[1] - der_df3$max_s[3])/(deltamethod(~ x4/4 - (x4+x6)/4, fixef(m3)$cond, vcov(m3)$cond)),
    
    (der_df3$max_s[2] - der_df3$max_s[3])/(deltamethod(~ (x4+x5)/4 - (x4+x6)/4, fixef(m3)$cond, vcov(m3)$cond))
    
  )
)

cont_s3$p.value <- 2*pnorm(abs(cont_s3$z), lower.tail = FALSE)

# Predicted disease severity at day 60
pred60_3 <- data.frame(emmeans(m3, ~ trt | day, at = list(day = 60), type = "response"))

ggplot(pred60_3, aes(trt, response, colour = trt)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1) +
  scale_color_manual(values = c("turquoise4", "#8B0000", "green4")) +
  labs(y = "Severity (Prop.) at day 60", x = "Treatments", colour = "Treatment") +
  theme_bw()