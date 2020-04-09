library("here")
library("tidyverse")
library("broom")


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
               .default = as.numeric(NA)
               )

#Norm perception (perceived descriptive: Q14.1#1_1, Q14.1#1_2) 
d1 <- d1 %>% 
  mutate_at(c("Q14.1#1_1", "Q14.1#1_2"),
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
  
#norm perception (perceived injunctive: Q14.1#2_1, Q14.1#2_2, Q14.1#2_3, Q14.2, Q14.3, Q14.4, Q14.5)

d1 <- d1 %>% 
  mutate_at(c("Q14.1#2_1","Q14.1#2_2", "Q14.1#2_2", "Q14.1#2_3", "Q14.2",	"Q14.3","Q14.4","Q14.5"),
              recode,
                "Disapprove" = 1,
                "Don't care" = 2,
                "Approve" = 3,
                .default = NaN,
            )


## NAs introduced by coercion
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
                .default = NaN
              )

#change reverse code
d1 <- recode(df$)

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

###actual regression
#lm1 DV: perceived norm (continuous) IV: stimuli (same/mix/control dummy)
##lm2: approval and norm The finding shows that a misperception exists between perceived marijuana approval of 
#typical students and the extent to which students personally approve of marijuana.

## desnorm_meanimpute, pro_dummy, con_dummy, mix_dummy, control_dummy
lm1 <- lm(desnorm_meanimpute ~ pro_dummy + con_dummy + mix_dummy + control_dummy, data = d1)
