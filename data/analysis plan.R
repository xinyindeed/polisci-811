library("here")
library("tidyverse")
library("broom")
library("psych")
install.packages("texreg")
library("texreg")
install.packages("interactions")
library("interactions")
install.packages("margins")
library("margins")

data <- read_csv(here("final_data.csv"))
# delete empty data
d1 <- data %>% filter(!is.na(Q7.2_8))
d1 <- d1 %>% slice(-1)

#recode variables 
#susceptibility to peer influence (Q5.1_1 - Q5.1_7)

d1 <- d1 %>%
  mutate_at(c("Q5.1_1", "Q5.1_2", "Q5.1_3", "Q5.1_4", "Q5.1_5", "Q5.1_6", "Q5.1_7"),
        recode, 
               "Definitely yes" = 5, 
               "Probably yes" = 4,
               "Might or might not" = 3,
               "Probably not" = 2,
               "Definitely not" = 1,
               .default = NaN
               )
#Calculate Cronbach Alpha
grep("Q5.1_1", colnames(d1))
alpha1 <- alpha(d1[, grep("Q5.1_1", colnames(d1)): grep("Q5.1_7", colnames(d1))])
alpha1
#Calculate mean for susceptiblity to peer influence
vars2 <- c("Q5.1_1", "Q5.1_2", "Q5.1_3", "Q5.1_4", "Q5.1_5", "Q5.1_6", "Q5.1_7")
d1 <- d1%>% mutate(
  suscep_mean = rowMeans(select(., vars2)))
ave2 <- colMeans(d1[grep("suscep_mean", colnames(d1))], na.rm = TRUE)
d1 <- d1 %>% 
  mutate(suscep_meanimpute = ifelse(is.na(suscep_mean), ave2, suscep_mean))

#as.numeric(NA)
#Norm perception (perceived descriptive: Q14.1#1_1, Q14.1#1_2) 
d1 <- d1 %>% 
  mutate_at(c("Q14.1#1_1", "Q14.1#1_2", "Q14.1#1_3"),
              recode,
                "Almost everyday" = 6,
                "About once a week" = 5,
                "A few times a month" = 4,
                "A few times a year" = 3,
                "Once a year" = 2,
                "Never" = 1,
                .default = NaN
              )
vars1 <- c("Q14.1#1_1", "Q14.1#1_2")

d1 <- d1%>% mutate(
      desnorm_mean = rowMeans(select(., vars1)))
      
ave1 <- colMeans(d1[grep("desnorm_mean", colnames(d1))], na.rm = TRUE)
d1 <- d1 %>% 
  mutate(desnorm_meanimpute = ifelse(is.na(desnorm_mean), ave1, desnorm_mean))
#actual marijuana use
d1 %>% group_by("Q14.1#1_3") %>% tally()
summarize(
  d1,
  mean_ideology = mean(`Q14.1#1_3`, na.rm = TRUE),
  sd_ideology = sd(`Q14.1#1_3`, na.rm = TRUE)
)
  
#norm perception (perceived injunctive: Q14.1#2_1, Q14.1#2_2, Q14.1#2_3, Q14.2, Q14.3, Q14.4, Q14.5)

d1 <- d1 %>% 
  mutate_at(c("Q14.1#2_1","Q14.1#2_2", "Q14.1#2_2", "Q14.1#2_3", "Q14.2",
              "Q14.3","Q14.4","Q14.5"),
              recode,
                "Disapprove" = 1,
                "Don't care" = 2,
                "Approve" = 3,
                .default = NaN,
            )

##sdfdfd

#intention of behavior (Q15.1_1	Q15.1_2	Q15.1_3	Q15.1_4	Q15.1_5)
d1 <- d1 %>% 
  mutate_at(c("Q15.1_1", "Q15.1_2", "Q15.1_3", "Q15.1_4",	"Q15.1_5"),
              recode,
                "Strongly Disagree" = 1,
                "Disagree" = 2,
                "Somewhat disagree" = 3,
                "Neither agree nor disagree" = 4,
                "Somewhat agree" = 5,
                "Agree" = 6,
                "Strongly agree" = 7,
                .default = NaN,
            
              )

#change reverse code
d1$Q15.1_3 <- 8 - d1$Q15.1_3
#calcualte average score

vars3 <- c("Q15.1_1", "Q15.1_2", "Q15.1_3", "Q15.1_4","Q15.1_5")
d1 <- d1%>% mutate(
  intent_mean = rowMeans(select(., vars3), na.rm = TRUE))

ave3 <- colMeans(d1[grep("intent_mean", colnames(d1))], na.rm = TRUE)
d1 <- d1 %>% 
  mutate(intent_meanimpute = ifelse(is.na(intent_mean), ave3, desnorm_mean))



#Dummy variable (pro/con/control)
#This is the pro-pro conditioin with Dummy code

d1 <- d1 %>% replace_na(list(`stimuli_pro-pro_DO` = 0))

d1 <- d1 %>% 
  mutate(
    pro_dummy =
      case_when(
        `stimuli_pro-pro_DO` != 0 ~ 1,
        `stimuli_pro-pro_DO` == 0 ~ 0
      )
  )
d1 %>% group_by(pro_dummy) %>% tally()


#This is the con-con condition with Dummy code
d1 <- d1 %>% replace_na(list(`stimuli_con-con_DO` = 0))

d1 <- d1 %>% 
  mutate(
    con_dummy =
      case_when(
        `stimuli_con-con_DO` != 0 ~ 1,
        `stimuli_con-con_DO` == 0 ~ 0
      )
  )

d1 %>% group_by(con_dummy) %>% tally()


#pro-con condition
d1 <- d1 %>% replace_na(list(`conditionstimuli_pro-con(1)_DO` = 0))

d1 <- d1 %>% 
  mutate(
    mix_dummy =
      case_when(
        `conditionstimuli_pro-con(1)_DO` != 0 ~ 1,
        `conditionstimuli_pro-con(1)_DO` == 0 ~ 0
      )
  )

d1 %>% group_by(mix_dummy) %>% tally()

    
#control condition
d1 <- d1 %>% replace_na(list(`Controlcondition_DO` = 0))

d1 <- d1 %>% 
  mutate(
    control_dummy =
      case_when(
        `Controlcondition_DO` != 0 ~ 0,
        `Controlcondition_DO` == 0 ~ 0
      )
  )
d1 %>% group_by(control_dummy) %>% tally()
##Covariates
d1 <- d1 %>% mutate(
  age = Q18.1_1
)

d1 <- d1 %>% mutate(
    gender = case_when(
      Q18.2 ==  "Female" ~ 0,
      Q18.2 == "Male" ~ 1
    )
)

d1 <- d1 %>% mutate(
  race = case_when(
    Q18.3 == "White" ~ 0,
    Q18.3 != "White" ~ 1
  )
)

d1 <- d1 %>% mutate(
  political = case_when(
    Q18.4 == "Strong Democrat" | Q18.4 == "Democrat" | Q18.4 == "Lean Democrat" ~ 0,
    Q18.4 == "Independent" ~ 1,
    Q18.4 == "Lean Republican" | Q18.4 == "Republican" | Q18.4 == "Strong Republican" ~ 2
  )
)

d1$age <- as.numeric(d1$Q18.1_1)

summarize(
  d1,
  mean = mean(Q18.1_1, na.rm = TRUE),
  sd= sd(Q18.1_1, na.rm = TRUE)
)



d1 <- d1 %>% mutate(
  SES = Q18.5
)

d2 <- select(d1, c("desnorm_meanimpute", "suscep_meanimpute", "intent_meanimpute", 
                   "pro_dummy", "con_dummy", "mix_dummy"))

cor(d2)

d1 %>%
  group_by("desnorm_meanimpute") %>% 
  summarise(
    mean = mean(desnorm_meanimpute),
    SD = sd(desnorm_meanimpute)
  )

d1 %>%
  group_by("intent_meanimpute") %>% 
  summarise(
    mean = mean(intent_meanimpute),
    SD = sd(intent_meanimpute)
  )

###actual regression
##lm1a DV: perceived norm (continuous) IV: stimuli (same/mix/control dummy)
##lm1b: approval and norm The finding shows that a misperception exists between perceived marijuana approval of 
#typical students and the extent to which students personally approve of marijuana.

  
## desnorm_meanimpute, pro_dummy, con_dummy, mix_dummy, control_dummy

lm1 <- lm(desnorm_meanimpute ~ pro_dummy + con_dummy + mix_dummy + control_dummy + age + gender + political, data = d1)
tidy(lm1)
glance(lm1)

ano1 <- aov(desnorm_meanimpute ~ pro_dummy + con_dummy + mix_dummy + control_dummy, data = d1)
tidy(ano1)
glance(ano1)
summary(ano1)
turkey_hsd(ano1, which = "dose")

##Assumption check highlight there's a couple outliers, overall okay 
augment(lm1) %>% 
ggplot(aes(x = .fitted, y = .resid)) + 
  geom_point(alpha = 0.2) +
  stat_smooth(method="loess", col = "#00BCD8")+
  geom_hline(yintercept=0, col="#00BCD8", linetype="dashed") +
  theme_bw()
## Check normal distribution
augment(lm1) %>% 
  ggplot(aes(sample=.fitted)) +
  stat_qq() +
  geom_qq_line(line.p = c(0.25, 0.75), col = "#00BCD8") +
  theme_bw()
## Plot graph

lm1_summary <- d1 %>%
  group_by(pro_dummy,con_dummy,mix_dummy,control_dummy) %>% 
  summarise(
  mean= mean(desnorm_meanimpute),
  SD = sd(desnorm_meanimpute)
  )

lm1_summary %>% 
  ggplot(aes(x = row.names(lm1_summary), y = mean)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean - SD, ymax = mean + SD), width = 0.2) +
  scale_x_discrete(labels = c("Control", "Mix", "Benefit", "Consequence"))


##lm2: Does susceptibility to social influence/peer pressure moderate the perception of norm?
lm2 <- lm(desnorm_meanimpute ~ pro_dummy + con_dummy + mix_dummy + suscep_mean + 
            pro_dummy * suscep_mean + con_dummy * suscep_mean + mix_dummy * suscep_mean +
            age + gender + political, data = d1)
tidy(lm2)
glance(lm2)

ano2 <- aov(intent_meanimpute ~ pro_dummy + con_dummy + mix_dummy + control_dummy, data = d1)
tidy(ano2)
glance(ano2)

#assumption check
augment(lm2) %>% 
  ggplot(aes(x = .fitted, y = .resid)) + 
  geom_point(alpha = 0.2) +
  stat_smooth(method="loess", col = "#00BCD8")+
  geom_hline(yintercept=0, col="#00BCD8", linetype="dashed") +
  theme_bw()
#normality check
augment(lm3) %>% 
  ggplot(aes(sample=.fitted)) +
  stat_qq() +
  geom_qq_line(line.p = c(0.25, 0.75), col = "#00BCD8") +
  theme_bw()

summarize(
  d1,
  mean = mean(Q18.1_1),
  sd_ideology = sd(nominate_dim1)
)

#lm3: suscpetibility to social 
#check linearity
ggplot (d1, aes (intent_meanimpute,desnorm_meanimpute, color = suscep_meanimpute)) + geom_point()

d1 <- d1 %>% 
  mutate(iv = scale(desnorm_meanimpute,center = TRUE, scale = FALSE))

d1<- d1 %>% 
  mutate(mod = scale(suscep_meanimpute, center = TRUE, scale = FALSE))

ggplot(d1, aes(suscep_meanimpute)) +
  geom_histogram()

lm3 <- lm(intent_meanimpute ~ desnorm_meanimpute * suscep_meanimpute, data = d1)
tidy(lm3)
glance(lm3)
plot(lm3)

augment(lm3) %>% 
  ggplot(aes(x = .fitted, y = .resid)) + 
  geom_point(alpha = 0.2) +
  stat_smooth(method="loess", col = "#00BCD8")+
  geom_hline(yintercept=0, col="#00BCD8", linetype="dashed") +
  theme_bw()

augment(lm3) %>% 
  ggplot(aes(sample=.fitted)) +
  stat_qq() + 
  geom_qq_line(line.p = c(0.25, 0.75), col = "#00BCD8") +
  theme_bw()

#plot
d1 <- d1 %>%
  group_by("suscep_meanimpute") %>% 
  mutate(
    suscep.m = mean(suscep_meanimpute),
    suscep.SD = sd(suscep_meanimpute)
  )
x.seq <- seq(from = round(min(d1$desnorm_meanimpute)), 
             to = round(max(d1$desnorm_meanimpute)), by = 0.05)



low.m <- mean(d1$suscep_meanimpute) - sd(d1$suscep_meanimpute)
high.m <- mean(d1$suscep_meanimpute) + sd(d1$suscep_meanimpute)
low.df <- data_frame(desnorm_meanimpute = x.seq, suscep_meanimpute = low.m)
high.df <- data_frame(desnorm_meanimpute = x.seq, suscep_meanimpute = high.m)

predict.low <- cplot(lm3, x = "desnorm_meanimpute", what = c("prediction"),
                     data = low.df, draw = FALSE)

predict.low <- predict.low %>% 
  mutate(m = -1)

predict.high <- cplot(lm3, x = "desnorm_meanimpute", what = c("prediction"),
                    data = high.df, draw = FALSE)

predict.high <- predict.high %>% 
  mutate(m = 1)

predict.model <- bind_rows(predict.low, predict.high) %>%
  mutate(m = factor(m, 
                    levels = c(-1, 1),
                    labels = c("Low Suscep", "High Suscep")))


predict.plot <- ggplot(data = predict.model, aes(x = xvals, group = m)) + 
  geom_line(aes(y = yvals, color = m)) + 
  geom_ribbon(alpha = .2, aes(ymin = lower, ymax = upper)) + 
  labs(
       subtitle = "Simple Slopes at +/- 1 Standard Deviation of Mean-Centered M",
       y = "Predicted Values of Intent to Smoke",
       x = "Mean Centered Value of Descriptive Norm", 
       colour = "") +
  theme_bw() +
  theme(legend.position = c(.1, .85)) +
  options(repr.plot.width=800, repr.plot.height=500)

predict.plot

aspect_ratio <- 2
height <- 4
ggsave("lm31.jpg", height = height, width = height * aspect_ratio)


d3 = augment(lm3)
d3 %>% 
  ggplot(aes(x = desnorm_meanimpute, y = intent_meanimpute , 
                 color = suscep_meanimpute, group = suscep_meanimpute)) +
  geom_smooth(method = "lm", se = FALSE)
  
  
  
d1 %>%
  ggplot(aes(desnorm_meanimpute)) +
  geom_histogram()

