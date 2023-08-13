##########################################################
#   --- Fisher's hypothesis test ----
##########################################################

library(tidyverse)
library(xtable)

# We test the Sharp null against
# i)   a two-sided alternative:        H1: Yi(1)!=Yi(0)
# ii)  a one-sided alternative Rigth:  H1: Yi(1)>Yi(0)
# iii) a one-sided alternative Left:   H1: Yi(1)<Yi(0)


#### Fisher's hypothesis test (1): depth as treatment within sessions ####
load(file='output/data/treat_depth.rda')

N_A=length(depthA$imaging_depth)
N_B=length(depthB$imaging_depth)
K<- 10000           # P-values estimated using 10000 draws

# Absolute value of the difference in average outcomes by treatment status:
Tobs_Int_sA <- mean(depthA$average_intensity[depthA$imaging_depth=="175"]) -
    mean(depthA$average_intensity[depthA$imaging_depth=="375"])
Tobs_Int_sA
Tobs_Int_sA_abs  <- abs(Tobs_Int_sA)
Tobs_Int_sA_abs

Tobs_Count_sA <- mean(depthA$average_count[depthA$imaging_depth=="175"]) -
    mean(depthA$average_count[depthA$imaging_depth=="375"])
Tobs_Count_sA
Tobs_Count_sA_abs  <- abs(Tobs_Count_sA)
Tobs_Count_sA_abs

Tobs_Int_sB <- mean(depthB$average_intensity[depthB$imaging_depth=="175"]) -
    mean(depthB$average_intensity[depthB$imaging_depth=="375"])
Tobs_Int_sB
Tobs_Int_sB_abs  <- abs(Tobs_Int_sB)
Tobs_Int_sB_abs

Tobs_Count_sB <- mean(depthB$average_count[depthB$imaging_depth=="175"]) -
    mean(depthB$average_count[depthB$imaging_depth=="375"])
Tobs_Count_sB
Tobs_Count_sB_abs  <- abs(Tobs_Count_sB)
Tobs_Count_sB_abs

# save information
p.twosided <- p.onesided.R <- p.onesided.L  <- matrix(0, ncol=2,nrow=2)
colnames(p.twosided) <- colnames(p.onesided.R) <- colnames(p.onesided.L) <- c("Session A","Session B")
row.names(p.twosided) <- row.names(p.onesided.R) <- row.names(p.onesided.L) <- c("Intensity","Count")
dif_I_A_ave <- dif_I_A_abs <- NULL
dif_C_A_ave <- dif_C_A_abs <- NULL
dif_I_B_ave <- dif_I_B_abs <- NULL
dif_C_B_ave <- dif_C_B_abs <- NULL

set.seed(1)
for(k in 1:K){
    w_A <- sample(depthA$imaging_depth, N_A, replace=FALSE)
    w_B <- sample(depthB$imaging_depth, N_B, replace=FALSE)

    dif_I_A <- mean(depthA$average_intensity[w_A=="175"]) - mean(depthA$average_intensity[w_A=="375"])
    dif_C_A <- mean(depthA$average_count[w_A=="175"]) - mean(depthA$average_count[w_A=="375"])
    dif_I_B <- mean(depthB$average_intensity[w_B=="175"]) - mean(depthB$average_intensity[w_B=="375"])
    dif_C_B <- mean(depthB$average_count[w_B=="175"]) - mean(depthB$average_count[w_B=="375"])

    # section A - Intensity
    dif_I_A_ave <- c(dif_I_A_ave, dif_I_A)
    dif_I_A_abs <- c(dif_I_A_abs, abs(dif_I_A))

    # section A - Count
    dif_C_A_ave <- c(dif_C_A_ave, dif_C_A)
    dif_C_A_abs <- c(dif_C_A_abs, abs(dif_C_A))

    # section B - Intensity
    dif_I_B_ave <- c(dif_I_B_ave, dif_I_B)
    dif_I_B_abs <- c(dif_I_B_abs, abs(dif_I_B))

    # section B - Count
    dif_C_B_ave <- c(dif_C_B_ave, dif_C_B)
    dif_C_B_abs <- c(dif_C_B_abs, abs(dif_C_B))

    # p.values for two sided test
    p.twosided[1,1] <- p.twosided[1,1]  + 1*(abs(dif_I_A)>=Tobs_Int_sA_abs)
    p.twosided[2,1] <- p.twosided[2,1]  + 1*(abs(dif_C_A)>=Tobs_Count_sA_abs)
    p.twosided[1,2] <- p.twosided[1,2]  + 1*(abs(dif_I_B)>=Tobs_Int_sB_abs)
    p.twosided[2,2] <- p.twosided[2,2]  + 1*(abs(dif_C_B)>=Tobs_Count_sB_abs)

    # p.values for one sided Rigth test
    p.onesided.R[1,1] <- p.onesided.R[1,1] + 1*(dif_I_A>=Tobs_Int_sA)
    p.onesided.R[2,1] <- p.onesided.R[2,1] + 1*(dif_C_A>=Tobs_Count_sA)
    p.onesided.R[1,2] <- p.onesided.R[1,2] + 1*(dif_I_B>=Tobs_Int_sB)
    p.onesided.R[2,2] <- p.onesided.R[2,2] + 1*(dif_C_B>=Tobs_Count_sB)

    # p.values for one sided Left test
    p.onesided.L[1,1] <- p.onesided.L[1,1] + 1*(dif_I_A<=Tobs_Int_sA)
    p.onesided.L[2,1] <- p.onesided.L[2,1] + 1*(dif_C_A<=Tobs_Count_sA)
    p.onesided.L[1,2] <- p.onesided.L[1,2] + 1*(dif_I_B<=Tobs_Int_sB)
    p.onesided.L[2,2] <- p.onesided.L[2,2] + 1*(dif_C_B<=Tobs_Count_sB)

}

p.twosided<-p.twosided/K
p.onesided.R<-p.onesided.R/K
p.onesided.L<-p.onesided.L/K

xtable(cbind(p.twosided,p.onesided.R,p.onesided.L), digits = 4)

#### Fisher's hypothesis test (2): session as treatment within depth ####
load(file='output/data/treat_session.rda')

N_1=length(depth175$session_type)
N_3=length(depth375$session_type)
K<- 10000           # P-values estimated using 10000 draws

# Absolute value of the difference in average outcomes by treatment status:
Tobs_Int_d1 <- mean(depth175$average_intensity[depth175$session_type=="Session A"]) -
    mean(depth175$average_intensity[depth175$session_type=="Session B"])
Tobs_Int_d1
Tobs_Int_d1_abs  <- abs(Tobs_Int_d1)
Tobs_Int_d1_abs

Tobs_Count_d1 <- mean(depth175$average_count[depth175$session_type=="Session A"]) -
    mean(depth175$average_count[depth175$session_type=="Session B"])
Tobs_Count_d1
Tobs_Count_d1_abs  <- abs(Tobs_Count_d1)
Tobs_Count_d1_abs

Tobs_Int_d3 <- mean(depth375$average_intensity[depth375$session_type=="Session A"]) -
    mean(depth375$average_intensity[depth375$session_type=="Session B"])
Tobs_Int_d3
Tobs_Int_d3_abs  <- abs(Tobs_Int_d3)
Tobs_Int_d3_abs

Tobs_Count_d3 <- mean(depth375$average_count[depth375$session_type=="Session A"]) -
    mean(depth375$average_count[depth375$session_type=="Session B"])
Tobs_Count_d3
Tobs_Count_d3_abs  <- abs(Tobs_Count_d3)
Tobs_Count_d3_abs

# save information
p.twosided <- p.onesided.R <- p.onesided.L   <- matrix(0, ncol=2,nrow=2)
colnames(p.twosided) <- colnames(p.onesided.R) <- colnames(p.onesided.L) <- c("175","375")
row.names(p.twosided) <- row.names(p.onesided.R) <- row.names(p.onesided.L) <- c("Intensity","Count")
dif_I_1_ave <- dif_I_1_abs <- NULL
dif_C_1_ave <- dif_C_1_abs <- NULL
dif_I_3_ave <- dif_I_3_abs <- NULL
dif_C_3_ave <- dif_C_3_abs <- NULL

set.seed(1)
for(k in 1:K){
    w_1 <- sample(depth175$session_type, N_1, replace=FALSE)
    w_3 <- sample(depth375$session_type, N_3, replace=FALSE)

    dif_I_1 <- mean(depth175$average_intensity[w_1=="Session A"]) - mean(depth175$average_intensity[w_1=="Session B"])
    dif_C_1 <- mean(depth175$average_count[w_1=="Session A"]) - mean(depth175$average_count[w_1=="Session B"])
    dif_I_3 <- mean(depth375$average_intensity[w_3=="Session A"]) - mean(depth375$average_intensity[w_3=="Session B"])
    dif_C_3 <- mean(depth375$average_count[w_3=="Session A"]) - mean(depth375$average_count[w_3=="Session B"])

    # 175 - Intensity
    dif_I_1_ave <- c(dif_I_1_ave, dif_I_1)
    dif_I_1_abs <- c(dif_I_1_abs, abs(dif_I_1))

    # 175 - Count
    dif_C_1_ave <- c(dif_C_1_ave, dif_C_1)
    dif_C_1_abs <- c(dif_C_1_abs, abs(dif_C_1))

    # 375 - Intensity
    dif_I_3_ave <- c(dif_I_3_ave, dif_I_3)
    dif_I_3_abs <- c(dif_I_3_abs, abs(dif_I_3))

    # 375 - Count
    dif_C_3_ave <- c(dif_C_3_ave, dif_C_3)
    dif_C_3_abs <- c(dif_C_3_abs, abs(dif_C_3))

    # p.values for two sided test
    p.twosided[1,1] <- p.twosided[1,1]  + 1*(abs(dif_I_1)>=Tobs_Int_d1_abs)
    p.twosided[2,1] <- p.twosided[2,1]  + 1*(abs(dif_C_1)>=Tobs_Count_d1_abs)
    p.twosided[1,2] <- p.twosided[1,2]  + 1*(abs(dif_I_3)>=Tobs_Int_d3_abs)
    p.twosided[2,2] <- p.twosided[2,2]  + 1*(abs(dif_C_3)>=Tobs_Count_d3_abs)

    # p.values for one sided Rigth test
    p.onesided.R[1,1] <- p.onesided.R[1,1] + 1*(dif_I_1>=Tobs_Int_d1)
    p.onesided.R[2,1] <- p.onesided.R[2,1] + 1*(dif_C_1>=Tobs_Count_d1)
    p.onesided.R[1,2] <- p.onesided.R[1,2] + 1*(dif_I_3>=Tobs_Int_d3)
    p.onesided.R[2,2] <- p.onesided.R[2,2] + 1*(dif_C_3>=Tobs_Count_d3)

    # p.values for one sided Left test
    p.onesided.L[1,1] <- p.onesided.L[1,1] + 1*(dif_I_1<=Tobs_Int_d1)
    p.onesided.L[2,1] <- p.onesided.L[2,1] + 1*(dif_C_1<=Tobs_Count_d1)
    p.onesided.L[1,2] <- p.onesided.L[1,2] + 1*(dif_I_3<=Tobs_Int_d3)
    p.onesided.L[2,2] <- p.onesided.L[2,2] + 1*(dif_C_3<=Tobs_Count_d3)

}

p.twosided<-p.twosided/K
p.onesided.R<-p.onesided.R/K
p.onesided.L<-p.onesided.L/K

xtable(cbind(p.twosided,p.onesided.R,p.onesided.L), digits = 4)

#### Fisher's hypothesis test (3): stimulus as treatments within session~depth ####
load(file='output/data/treat_stimulus.rda')

K<- 3000

p.values_function <- function(treat, depth, intensity, count){

    N_1=length(treat[depth=="175"])
    N_3=length(treat[depth=="375"])

    # observed test statistics
    Tobs_Int_d1 <- mean(intensity[treat==1 & depth=="175"]) - mean(intensity[treat==0 & depth=="175"])
    Tobs_Int_d1_abs  <- abs(Tobs_Int_d1)

    Tobs_Count_d1 <- mean(count[treat==1 & depth=="175"]) - mean(count[treat==0 & depth=="175"])
    Tobs_Count_d1_abs  <- abs(Tobs_Count_d1)

    Tobs_Int_d3 <- mean(intensity[treat==1 & depth=="375"]) - mean(intensity[treat==0 & depth=="375"])
    Tobs_Int_d3_abs  <- abs(Tobs_Int_d3)

    Tobs_Count_d3 <- mean(count[treat==1 & depth=="375"]) - mean(count[treat==0 & depth=="375"])
    Tobs_Count_d3_abs  <- abs(Tobs_Count_d3)

    # save information
    p.twosided <- p.onesided.R <- p.onesided.L   <- matrix(0, ncol=2,nrow=2)
    colnames(p.twosided) <- colnames(p.onesided.R) <- colnames(p.onesided.L) <- c("175","375")
    row.names(p.twosided) <- row.names(p.onesided.R) <- row.names(p.onesided.L) <- c("Intensity","Count")
    dif_I_1_ave <- dif_I_1_abs <- NULL
    dif_C_1_ave <- dif_C_1_abs <- NULL
    dif_I_3_ave <- dif_I_3_abs <- NULL
    dif_C_3_ave <- dif_C_3_abs <- NULL

    intensity_175=intensity[depth=="175"]
    intensity_375=intensity[depth=="375"]
    count_175=count[depth=="175"]
    count_375=count[depth=="375"]

    set.seed(1)
    for(k in 1:K){
        w_1 <- sample(treat[depth=="175"], N_1, replace=FALSE)
        w_3 <- sample(treat[depth=="375"], N_3, replace=FALSE)

        dif_I_1 <- mean(intensity_175[w_1==1]) - mean(intensity_175[w_1==0])
        dif_C_1 <- mean(count_175[w_1==1]) - mean(count_175[w_1==0])
        dif_I_3 <- mean(intensity_375[w_3==1]) - mean(intensity_375[w_3==0])
        dif_C_3 <- mean(count_375[w_3==1]) - mean(count_375[w_3==0])

        # 175 - Intensity
        dif_I_1_ave <- c(dif_I_1_ave, dif_I_1)
        dif_I_1_abs <- c(dif_I_1_abs, abs(dif_I_1))

        # 175 - Count
        dif_C_1_ave <- c(dif_C_1_ave, dif_C_1)
        dif_C_1_abs <- c(dif_C_1_abs, abs(dif_C_1))

        # 375 - Intensity
        dif_I_3_ave <- c(dif_I_3_ave, dif_I_3)
        dif_I_3_abs <- c(dif_I_3_abs, abs(dif_I_3))

        # 375 - Count
        dif_C_3_ave <- c(dif_C_3_ave, dif_C_3)
        dif_C_3_abs <- c(dif_C_3_abs, abs(dif_C_3))

        # p.values for two sided test
        p.twosided[1,1] <- p.twosided[1,1]  + 1*(abs(dif_I_1)>=Tobs_Int_d1_abs)
        p.twosided[2,1] <- p.twosided[2,1]  + 1*(abs(dif_C_1)>=Tobs_Count_d1_abs)
        p.twosided[1,2] <- p.twosided[1,2]  + 1*(abs(dif_I_3)>=Tobs_Int_d3_abs)
        p.twosided[2,2] <- p.twosided[2,2]  + 1*(abs(dif_C_3)>=Tobs_Count_d3_abs)

        # p.values for one sided Rigth test
        p.onesided.R[1,1] <- p.onesided.R[1,1] + 1*(dif_I_1>=Tobs_Int_d1)
        p.onesided.R[2,1] <- p.onesided.R[2,1] + 1*(dif_C_1>=Tobs_Count_d1)
        p.onesided.R[1,2] <- p.onesided.R[1,2] + 1*(dif_I_3>=Tobs_Int_d3)
        p.onesided.R[2,2] <- p.onesided.R[2,2] + 1*(dif_C_3>=Tobs_Count_d3)

        # p.values for one sided Left test
        p.onesided.L[1,1] <- p.onesided.L[1,1] + 1*(dif_I_1<=Tobs_Int_d1)
        p.onesided.L[2,1] <- p.onesided.L[2,1] + 1*(dif_C_1<=Tobs_Count_d1)
        p.onesided.L[1,2] <- p.onesided.L[1,2] + 1*(dif_I_3<=Tobs_Int_d3)
        p.onesided.L[2,2] <- p.onesided.L[2,2] + 1*(dif_C_3<=Tobs_Count_d3)

    }

    p.twosided<-p.twosided/K
    p.onesided.R<-p.onesided.R/K
    p.onesided.L<-p.onesided.L/K

    return(cbind(p.twosided,p.onesided.R,p.onesided.L))
}

pair1_all <- p.values_function(treat= 1*(pair1$stimulus=="drifting_gratings"),
                               depth= as.numeric(pair1$imaging_depth),
                               intensity= pair1$average_intensity,
                               count=pair1$average_count)
pair2_all <- p.values_function(treat= 1*(pair2$stimulus=="natural_scenes"),
                               depth= as.numeric(pair2$imaging_depth),
                               intensity= pair2$average_intensity,
                               count=pair2$average_count)
pair3_all <- p.values_function(treat= 1*(pair3$stimulus=="natural_movie_three"),
                               depth= as.numeric(pair3$imaging_depth),
                               intensity= pair3$average_intensity,
                               count=pair3$average_count)
pair4_all <- p.values_function(treat= 1*(pair4$stimulus=="natural_movie_three"),
                               depth= as.numeric(pair4$imaging_depth),
                               intensity= pair4$average_intensity,
                               count=pair4$average_count)

xtable(rbind(pair1_all,pair4_all,pair2_all,pair3_all), digits = 4)

#### Fisher's hypothesis test (4): stimulus as treatments within session~depth~position ####
load(file='output/data/treat_position.rda')

K <- 3000

# drifting_gratings against static_grating
pair1_temp<-pair1[pair1$block==2, ]
pair1_2_2 <- p.values_function(treat= 1*(pair1_temp$stimulus=="drifting_gratings"),
                               depth= as.numeric(pair1_temp$imaging_depth),
                               intensity= pair1_temp$average_intensity,
                               count=pair1_temp$average_count)
pair1_temp<-rbind(pair1[pair1$block==2 & pair1$stimulus=="drifting_gratings", ],
                  pair1[pair1$block==9, ])
pair1_2_9 <- p.values_function(treat= 1*(pair1_temp$stimulus=="drifting_gratings"),
                               depth= as.numeric(pair1_temp$imaging_depth),
                               intensity= pair1_temp$average_intensity,
                               count=pair1_temp$average_count)
pair1_temp<-rbind(pair1[pair1$block==2 & pair1$stimulus=="drifting_gratings", ],
                  pair1[pair1$block==15, ])
pair1_2_15 <- p.values_function(treat= 1*(pair1_temp$stimulus=="drifting_gratings"),
                                depth= as.numeric(pair1_temp$imaging_depth),
                                intensity= pair1_temp$average_intensity,
                                count=pair1_temp$average_count)
pair1_temp<-rbind(pair1[pair1$block==2 & pair1$stimulus=="static_gratings", ],
                  pair1[pair1$block==8, ])
pair1_8_2 <- p.values_function(treat= 1*(pair1_temp$stimulus=="drifting_gratings"),
                               depth= as.numeric(pair1_temp$imaging_depth),
                               intensity= pair1_temp$average_intensity,
                               count=pair1_temp$average_count)
pair1_temp<-pair1[pair1$block==8 | pair1$block==9, ]
pair1_8_9 <- p.values_function(treat= 1*(pair1_temp$stimulus=="drifting_gratings"),
                               depth= as.numeric(pair1_temp$imaging_depth),
                               intensity= pair1_temp$average_intensity,
                               count=pair1_temp$average_count)
pair1_temp<-pair1[pair1$block==8 | pair1$block==15, ]
pair1_8_15 <- p.values_function(treat= 1*(pair1_temp$stimulus=="drifting_gratings"),
                                depth= as.numeric(pair1_temp$imaging_depth),
                                intensity= pair1_temp$average_intensity,
                                count=pair1_temp$average_count)

pair1_temp<-rbind(pair1[pair1$block==2 & pair1$stimulus=="static_gratings", ],
                  pair1[pair1$block==13, ])
pair1_13_2 <- p.values_function(treat= 1*(pair1_temp$stimulus=="drifting_gratings"),
                                depth= as.numeric(pair1_temp$imaging_depth),
                                intensity= pair1_temp$average_intensity,
                                count=pair1_temp$average_count)
pair1_temp<-pair1[pair1$block==13 | pair1$block==9, ]
pair1_13_9 <- p.values_function(treat= 1*(pair1_temp$stimulus=="drifting_gratings"),
                                depth= as.numeric(pair1_temp$imaging_depth),
                                intensity= pair1_temp$average_intensity,
                                count=pair1_temp$average_count)
pair1_temp<-pair1[pair1$block==13 | pair1$block==15, ]
pair1_13_15 <- p.values_function(treat= 1*(pair1_temp$stimulus=="drifting_gratings"),
                                 depth= as.numeric(pair1_temp$imaging_depth),
                                 intensity= pair1_temp$average_intensity,
                                 count=pair1_temp$average_count)

xtable(rbind(pair1_2_2,pair1_2_9,pair1_2_15,
             pair1_8_2,pair1_8_9,pair1_8_15,
             pair1_13_2,pair1_13_9,pair1_13_15), digits = 4)


# natural_scenes against drifting_gratings
table(pair2$stimulus, pair2$block)

pair2_temp<-pair2[pair2$block==4 | pair2$block==2, ]
pair2_4_2 <- p.values_function(treat= 1*(pair2_temp$stimulus=="natural_scenes"),
                               depth= as.numeric(pair2_temp$imaging_depth),
                               intensity= pair2_temp$average_intensity,
                               count=pair2_temp$average_count)
pair2_temp<-pair2[pair2$block==4 | pair2$block==8, ]
pair2_4_8 <- p.values_function(treat= 1*(pair2_temp$stimulus=="natural_scenes"),
                               depth= as.numeric(pair2_temp$imaging_depth),
                               intensity= pair2_temp$average_intensity,
                               count=pair2_temp$average_count)
pair2_temp<-rbind(pair2[pair2$block==13 & pair1$stimulus=="drifting_gratings", ],
                  pair2[pair2$block==4, ])
pair2_4_13 <- p.values_function(treat= 1*(pair2_temp$stimulus=="natural_scenes"),
                                depth= as.numeric(pair2_temp$imaging_depth),
                                intensity= pair2_temp$average_intensity,
                                count=pair2_temp$average_count)
pair2_temp<-pair2[pair2$block==7 | pair2$block==2, ]
pair2_7_2 <- p.values_function(treat= 1*(pair2_temp$stimulus=="natural_scenes"),
                               depth= as.numeric(pair2_temp$imaging_depth),
                               intensity= pair2_temp$average_intensity,
                               count=pair2_temp$average_count)
pair2_temp<-pair2[pair2$block==7 | pair2$block==8, ]
pair2_7_8 <- p.values_function(treat= 1*(pair2_temp$stimulus=="natural_scenes"),
                               depth= as.numeric(pair2_temp$imaging_depth),
                               intensity= pair2_temp$average_intensity,
                               count=pair2_temp$average_count)
pair2_temp<-rbind(pair2[pair2$block==13 & pair2$stimulus=="drifting_gratings", ],
                  pair2[pair2$block==7, ])
pair2_7_13 <- p.values_function(treat= 1*(pair2_temp$stimulus=="natural_scenes"),
                                depth= as.numeric(pair2_temp$imaging_depth),
                                intensity= pair2_temp$average_intensity,
                                count=pair2_temp$average_count)

pair2_temp<-rbind(pair2[pair2$block==13 & pair2$stimulus=="natural_scenes", ],
                  pair2[pair2$block==2, ])
pair2_13_2 <- p.values_function(treat= 1*(pair2_temp$stimulus=="natural_scenes"),
                                depth= as.numeric(pair2_temp$imaging_depth),
                                intensity= pair2_temp$average_intensity,
                                count=pair2_temp$average_count)
pair2_temp<-rbind(pair2[pair2$block==13 & pair2$stimulus=="natural_scenes", ],
                  pair2[pair2$block==8, ])
pair2_13_8 <- p.values_function(treat= 1*(pair2_temp$stimulus=="natural_scenes"),
                                depth= as.numeric(pair2_temp$imaging_depth),
                                intensity= pair2_temp$average_intensity,
                                count=pair2_temp$average_count)
pair2_temp<-pair2[pair2$block==13, ]
pair2_13_13 <- p.values_function(treat= 1*(pair2_temp$stimulus=="natural_scenes"),
                                 depth= as.numeric(pair2_temp$imaging_depth),
                                 intensity= pair2_temp$average_intensity,
                                 count=pair2_temp$average_count)

xtable(rbind(pair2_4_2,pair2_4_8,pair2_4_13,
             pair2_7_2,pair2_7_8,pair2_7_13,
             pair2_13_2,pair2_13_8,pair2_13_13), digits = 4)
