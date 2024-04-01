library(readxl)
library(bookdown)
library(webshot)
library(ggplot2)
library(gridExtra)
library(ggforce)
library(gtable)
library(tibble)
library(dplyr)
library(tidyverse)
library(tsdl)
library(xts)
library(scales)
library(extraDistr)
library(fpp3)
library(aTSA)
library(urca)
library(lmtest)
library(olsrr)
library(forecast)
library(DescTools)
library(tseries)
library(fBasics)
library(nortest)
library("numbers")
library(lubridate)
library(qqplotr)
library(EnvStats)
library(astsa)
library(FitAR)
library(fitdistrplus)
library(goftest)
library(glogis)
library(car)
library(pracma)
library(NlcOptim)
library(BiocManager)
library(stats4)
library(GO.db)
library(dynamicTreeCut)
library(fastcluster)
library(WGCNA)

################### DATA PREPARATION ###################

dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

MNM38 <- t(read_excel("MNM38.xlsx"))
MNM38_vec = vec(MNM38)

# aggiungo la colonne anno e mese nel data set 
MNM38_ts <- ts(MNM38_vec, start=c(1977, 12), frequency=12)
MNM38_df <- data.frame(Year = trunc(time(MNM38_ts)), Month=month.abb[cycle(MNM38_ts)], MNM38_ts) 
head(MNM38_df)
tail(MNM38_df)
# aggiungo la colonna t che va da 1 a 150
MNM38_df <- add_column(MNM38_df, t=c(1:nrow(MNM38_df)), .before="Year")
head(MNM38_df)
tail(MNM38_df)
# rinomino la colonna Series.1 in Values
MNM38_df <- MNM38_df %>% rename("Series.1" = "Values")
head(MNM38_df)
tail(MNM38_df)

# utilizzo l'85% dei dati come training set
Data_df <- MNM38_df
length <- nrow(Data_df)
TS_length <- floor(length*0.85)
TsS_length <- length - TS_length

# Scatter plot
First_Date <- paste(Data_df$Month[1],Data_df$Year[1])
Last_Date <- paste(Data_df$Month[length],Data_df$Year[length])
title_content <- bquote(atop("Essentials of Time Series Analysis \u0040 Master in Data Science 2022-2023", 
                             paste("University of Roma \"Tor Vergata\""),
                             paste("Scatter Plot of MNM38 Training and Test Set from ", .(First_Date), " to ", .(Last_Date))))
subtitle_content <- bquote(paste("path length ", .(length), " sample points.Data by courtesy of https://forecasters.org/resources/time-series-data/m-competition/"))
caption_content <- "Author: Fabio Palmigiani"
x_name <- bquote("")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$Month[x_breaks],Data_df$Year[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Values")
y_breaks_num <- 10
y_max <- max(na.omit(Data_df$Values))
y_min <- min(na.omit(Data_df$Values))
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
col_k <- bquote("Training set")
col_b <- bquote("Test set")
col_g <- bquote("Retta di regressione")
col_r <- bquote(" Curva LOESS")
leg_labs   <- c(col_k, col_b, col_g, col_r)
leg_cols   <- c("col_k"="black", "col_b"="blue", "col_r"="red", "col_g"="green")
leg_breaks <- c("col_k", "col_b", "col_g", "col_r")
MNM38_sp <- ggplot(Data_df) +
  geom_smooth(data=subset(Data_df, Data_df$t <= t[TS_length]), alpha=1, size = 0.8, linetype="solid", 
              aes(x=t, y=Values, color="col_g"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=FALSE) +
  geom_smooth(data=subset(Data_df, Data_df$t <= t[TS_length]), alpha=1, size = 0.8, linetype="dashed", 
              aes(x=t, y=Values, color="col_r"), method = "loess", formula = y ~ x, se=FALSE) +
  geom_point(data=subset(Data_df, Data_df$t <= t[TS_length]), alpha=1, size=1.5, shape=19, 
             aes(x=t, y=Values, color="col_k")) +
  geom_point(data=subset(Data_df, Data_df$t > t[TS_length]), alpha=1, size=1.5, shape=19, 
             aes(x=t, y=Values, color="col_b")) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,19,NA,NA), 
                                                           linetype=c("blank", "blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust= 0.5),
        axis.text.x = element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(MNM38_sp)

# Line plot
title_content <- bquote(atop("Essentials of Time Series Analysis \u0040 Master in Data Science 2022-2023", 
                             paste("University of Roma \"Tor Vergata\""),
                             paste("Line Plot of MNM38 training and Test Set from ", .(First_Date), " to ", .(Last_Date))))
MNM38_lp <- ggplot(Data_df) +
  geom_smooth(data=subset(Data_df, Data_df$t <= t[TS_length]), alpha=1, size = 0.8, linetype="solid", 
              aes(x=t, y=Values, color="col_g"), method = "lm" , formula = y ~ x, se=FALSE, 
              fullrange=FALSE) +
  geom_smooth(data=subset(Data_df, Data_df$t <= t[TS_length]), alpha=1, size = 0.8, linetype="dashed", 
              aes(x=t, y=Values, color="col_r"), method = "loess", formula = y ~ x, se=FALSE) +
  geom_line(data=subset(Data_df, Data_df$t <= t[TS_length]), alpha=1, size=0.8, linetype="solid", 
            aes(x=t, y=Values, color="col_k", group=1)) +
  geom_line(data=subset(Data_df, Data_df$t >= t[TS_length]), alpha=1, size=0.8, linetype="solid", 
            aes(x=t, y=Values, color="col_b", group=1)) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "solid", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(MNM38_lp)


###### PROVIAMO A DESCRIVERE IL MODELLO TRAMITE UNA FUNZIONE LINEARE #####
MNM38_lm <- lm(Values[1:TS_length]~t[1:TS_length], data=MNM38_df)
class(MNM38_lm)
MNM38_fit <- MNM38_lm[["fitted.values"]]
head(MNM38_fit)
tail(MNM38_fit)

# prendo i residui
MNM38_res <- MNM38_lm[["residuals"]]
head(MNM38_res)
tail(MNM38_res)

# gradi di libertà dei residui 
MNM38_degfr <- MNM38_lm[["df.residual"]]
show(MNM38_degfr)

#inserisco i dati derivanti dal modello lineare nel data frame
MNM38_lm_fitted <- c(as.vector(MNM38_fit), rep(NA, (length-TS_length)))
MNM38_lm_residuals <- c(as.vector(MNM38_res), rep(NA, (length-TS_length)))
MNM38_df <- add_column(MNM38_df, MNM38_lm_fit=MNM38_lm_fitted, MNM38_lm_res=MNM38_lm_residuals, .after="Values")
head(MNM38_df)
tail(MNM38_df)

# creo una variabile che contiene solo i dati di test
MNM38_train_df <- MNM38_df[1:TS_length,]
head(MNM38_train_df)
tail(MNM38_train_df)

# scatter plot dei residui
Data_df  <- MNM38_train_df
length <- nrow(Data_df)
First_Date <- paste(Data_df$Month[1],Data_df$Year[1])
Last_Date <- paste(Data_df$Month[length],Data_df$Year[length])
title_content <- bquote(atop("Essentials of Time Series Analysis \u0040 Master in Data Science 2022-2023", 
                             paste("University of Roma \"Tor Vergata\""),
                             paste("Scatter Plot of Residuals vs Indices of the Linear Model for the Training Set of MNM38 Trainig Set from ", .(First_Date), " to ", .(Last_Date))))
subtitle_content <- bquote(paste("path length ", .(length), " sample points. Data by courtesy of https://forecasters.org/resources/time-series-data/m-competition/"))
caption_content <- "Author: Fabio Palmigiani"
x_name <- bquote("")
y_name <- bquote("Values")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$Month[x_breaks],Data_df$Year[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$MNM38_lm_res)-min(Data_df$MNM38_lm_res))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$MNM38_lm_res)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$MNM38_lm_res)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
col_1 <- bquote("Residui vs Indici")
col_2 <- bquote("Curva LOESS")
col_3 <- bquote("Retta di regressione")
leg_labs <- c(col_1, col_2, col_3)
leg_cols <- c("col_1"="blue", "col_2"="red", "col_3"="green")
leg_breaks <- c("col_1", "col_2", "col_3")
MNM38_Res_sp <- ggplot(Data_df) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=MNM38_lm_res, color="col_3"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=MNM38_lm_res, color="col_2"),
              method = "loess", formula = y ~ x, se=FALSE) +
  geom_point(alpha=1, size=1.0, shape=19, aes(x=t, y=MNM38_lm_res, color="col_1")) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(MNM38_Res_sp)

# Line plot dei residui
Data_df  <- MNM38_train_df
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2022-2023", paste("Line Plot of Residuals vs Indices of the Linear Model for the Training Set of MNM38 Training Set from ", .(First_Date), " to ", .(Last_Date))))
MNM38_Res_lp <- ggplot(Data_df) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=MNM38_lm_res, color="col_3"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=MNM38_lm_res, color="col_2"),
              method = "loess", formula = y ~ x, se=FALSE) +
  geom_line(alpha=1, size=0.8, linetype="solid", aes(x=t, y=MNM38_lm_res, color="col_1")) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(MNM38_Res_lp)

# ADF Test
MNM38_lm_res_ADF_none <- ur.df(MNM38_res, type="none", lags=0, selectlags="Fixed") 
summary(MNM38_lm_res_ADF_none)

# KPSS Test
MNM38_lm_res_KPSS_mu <- ur.kpss(MNM38_res, type="mu", lags="nil", use.lag=NULL)    
summary(MNM38_lm_res_KPSS_mu)


# grafico a dispersione delle radici quadrate dei residui
plot(MNM38_lm,3)

# grafico a dispersione delle radici quadrate dei residui rispetto ai valori fittati.
length <- nrow(MNM38_df)
MNM38_lm_sqrt_abs_residuals <- c(as.vector(sqrt(abs(MNM38_res))), rep(NA, (length-TS_length)))
MNM38_df <- add_column(MNM38_df, MNM38_lm_sqrt_abs_res=MNM38_lm_sqrt_abs_residuals, .after="MNM38_lm_res")
head(MNM38_df)
# aggiorno il dataset di training
MNM38_train_df <- MNM38_df[1:TS_length,]
head(MNM38_train_df)
tail(MNM38_train_df)

# Scatter plot
Data_df  <- MNM38_train_df
length <- nrow(Data_df)
First_Date <- paste(Data_df$Month[1],Data_df$Year[1])
Last_Date <- paste(Data_df$Month[length],Data_df$Year[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2022-2023", 
                             paste("Scatter Plot of Square Root of Absolute Residuals of the Linear Model for MNM38 Training Set from ", .(First_Date), " to ", .(Last_Date))))
subtitle_content <- bquote(paste("path length ", .(length), " sample points."))
caption_content <- "Author: Fabio Palmigiani"
x_name <- bquote("Fitted values")
x_breaks_num <- 10
x_breaks_low <- min(Data_df$MNM38_lm_fit)
x_breaks_up <-  max(Data_df$MNM38_lm_fit)
x_binwidth <- round((x_breaks_up-x_breaks_low)/x_breaks_num, digits=1)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- format(x_breaks, scientific=FALSE)
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Radice quadrata dei valori assoluti dei residui")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$MNM38_lm_sqrt_abs_res)-min(Data_df$MNM38_lm_sqrt_abs_res))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$MNM38_lm_sqrt_abs_res)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$MNM38_lm_sqrt_abs_res)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 1.0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
col_1 <- bquote("Radice quadrata dei valori assoluti dei residui vs valori fittati")
col_2 <- bquote("Curva LOESS")
col_3 <- bquote("Retta di regressione")
leg_labs <- c(col_1, col_2, col_3)
leg_cols <- c("col_1"="blue", "col_2"="red", "col_3"="green")
leg_breaks <- c("col_1", "col_2", "col_3")
MNM38_Sqrt_Abs_Res_sp <- ggplot(Data_df) +
  geom_hline(yintercept = mean(Data_df$MNM38_lm_sqrt_abs_res), size=0.3, colour="black") +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=MNM38_lm_fit, y=MNM38_lm_sqrt_abs_res, color="col_3"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=MNM38_lm_fit, y=MNM38_lm_sqrt_abs_res, color="col_2"),
              method = "loess", formula = y ~ x, se=FALSE) +
  geom_point(alpha=1, size=1.0, shape=19, aes(x=MNM38_lm_fit, y=MNM38_lm_sqrt_abs_res, color="col_1")) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(MNM38_Sqrt_Abs_Res_sp)

# Line plot 
Data_df <- MNM38_train_df
length <- nrow(Data_df)
First_Date <- paste(Data_df$Month[1],Data_df$Year[1])
Last_Date <- paste(Data_df$Month[length],Data_df$Year[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2022-2023", 
                             paste("Line Plot of Square Root of Absolute Residuals of the Linear Model for MNM38 Training Set from ", .(First_Date), " to ", .(Last_Date))))
x_name <- bquote("")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$Month[x_breaks],Data_df$Year[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$MNM38_lm_sqrt_abs_res)-min(Data_df$MNM38_lm_sqrt_abs_res))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$MNM38_lm_sqrt_abs_res)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$MNM38_lm_sqrt_abs_res)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 1.0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
leg_labs   <- c(col_1, col_2, col_3)
leg_cols   <- c("col_1"="blue", "col_2"="red", "col_3"="green")
leg_breaks <- c("col_1", "col_2", "col_3")
MNM38_Sqrt_Abs_Res_lp <- ggplot(Data_df) +
  geom_hline(yintercept = mean(Data_df$MNM38_lm_sqrt_abs_res), size=0.3, colour="black") +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=MNM38_lm_sqrt_abs_res, color="col_3"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=MNM38_lm_sqrt_abs_res, color="col_2"),
              method = "loess", formula = y ~ x, se=FALSE) +
  geom_line(alpha=1, size=0.8, linetype="solid", aes(x=t, y=MNM38_lm_sqrt_abs_res, color="col_1")) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(MNM38_Sqrt_Abs_Res_lp)

# Breusch-Pagan test per omoschedasticità
Data_df  <- data.frame(x=MNM38_train_df$t, y=MNM38_train_df$Values, y_fit=MNM38_train_df$MNM38_lm_fit, y_res=MNM38_train_df$MNM38_lm_res) 
Kurt_Gauss <- DescTools::Kurt(x=MNM38_vec, weights = NULL, na.rm = TRUE, method = 2, conf.level = 0.95, ci.type = "classic")
show(Kurt_Gauss)

lmtest::bptest(formula = y~x, varformula = NULL, studentize = FALSE, data=Data_df)
lmtest::bptest(formula = y~x, varformula = NULL, studentize = TRUE, data=Data_df)

# W Test per omoschedasticità
var.formula <- ~ x+I(x^2)
lmtest::bptest(formula = y ~ x, varformula = var.formula, studentize = TRUE, data=Data_df)


# autocorrelogramma
Data_df <-  MNM38_train_df
y <-  Data_df$MNM38_lm_res
T <- length(y)
maxlag <- ceiling(min(2*12, T/5)) 
Aut_Fun_y_acf <- acf(y, lag.max=maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(T)
ci_95 <- qnorm((1+0.95)/2)/sqrt(T)
ci_99 <- qnorm((1+0.99)/2)/sqrt(T)
Aut_Fun_y_df <- data.frame(lag=Aut_Fun_y_acf$lag, acf=Aut_Fun_y_acf$acf)
First_Date <- paste(Data_df$Month[1],Data_df$Year[1])
Last_Date <- paste(Data_df$Month[length],Data_df$Year[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2022-2023", paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for MNM38 Training Set from ", .(First_Date), " to ", .(Last_Date))))
x_name <- bquote("Lags")
x_breaks_num <- maxlag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_acf$lag
x_labs <- format(x_breaks, scientific=FALSE)
y_name <- bquote("acf value")
Aut_Corr_Res_bp <- ggplot(Aut_Fun_y_df, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size=1, col="black") +
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend=TRUE, lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend=TRUE, lwd=0.8, lty=2) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend=TRUE, lwd=0.8, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lwd=0.8, lty=4) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_colour_manual(name="Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_90="green", CI_95="blue", CI_99="red"),
                      guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5), 
        plot.subtitle=element_text(hjust= 0.5),
        plot.caption=element_text(hjust=1.0),
        legend.key.width=unit(0.8,"cm"), legend.position="bottom")
plot(Aut_Corr_Res_bp)

# Ljung-Box (LB) test per autocorrelazione in maniera computazionale
Data_df <-  MNM38_train_df
y <- Data_df$MNM38_lm_res
T <- length(y)
maxlag <- ceiling(min(2*12, T/5)) 
y_LB <- LjungBoxTest(y, k=1, lag.max=maxlag, StartLag=1, SquaredQ=FALSE)
show(y_LB)


# rimuovo le colonne che descrivono i dati attraverso il modello lineare dal dataset
MNM38_train_df <-  subset(MNM38_train_df, select=-c(MNM38_lm_fit, MNM38_lm_res, MNM38_lm_sqrt_abs_res))
head(MNM38_train_df)
tail(MNM38_train_df)


############################## DECOMPOSIZIONE ##############################
y <- MNM38_train_df$Values
MNM38_train_df_ts = ts(y, frequency=12, start=c(1977,12))
class(MNM38_train_df_ts)
str(MNM38_train_df_ts)
window(MNM38_train_df_ts, start=c(1977,12))

MNM38_train_mat <- t(matrix(as.vector(MNM38_train_df_ts), nrow=12))                            
show(MNM38_train_mat)


MNM38_ann_av_vec <- rowMeans(MNM38_train_mat, na.rm=TRUE)                  
show(MNM38_ann_av_vec)


MNM38_trend_vec <- rep(MNM38_ann_av_vec, each=12)           
show(MNM38_trend_vec[1:36])


MNM38_trend_ts <- ts(MNM38_trend_vec, start=c(1977,12), frequency=12)
str(MNM38_trend_ts)
window(MNM38_trend_ts, end=c(1988,06))  


# Plot della componente trend
plot(MNM38_trend_ts, type="l", col="blue", xlab="Date", ylab="Values", main="MNM38 - Compnente media annua")

plot(MNM38_train_df_ts, type="l", col="blue", xlab="Date", ylab="Values")

# Rimozione del trend.
MNM38_det_ts <- MNM38_train_df_ts-MNM38_trend_ts      

# Plot della serie detrendizzata
plot(MNM38_det_ts, col="blue", xlab="Date", ylab="Values", main="MNM38 - Detrend della componente media annua")


######## tolgo la staginalità #######
MNM38_det_mat <- t(matrix(as.vector(MNM38_det_ts), nrow=12))
show(MNM38_det_mat[1:6,1:12]) 


MNM38_monthly_av_vec <- colMeans(MNM38_det_mat, na.rm=TRUE)
show(MNM38_monthly_av_vec)


MNM38_seas_vec <- rep(MNM38_monthly_av_vec, times=nrow(MNM38_train_mat))
show(MNM38_seas_vec[1:36]) 


MNM38_seas_ts <- ts(MNM38_seas_vec, start=c(1977,12), frequency=12)
head(MNM38_seas_ts)
tail(MNM38_seas_ts)

# mostro la stagionalità della serie
plot(MNM38_seas_ts, col="blue", xlab="Date", ylab="Value", main="MNM38 - Componente stagionalità")

# costruisco la time series destagionalizzata
MNM38_deseas_ts <- MNM38_ts-MNM38_seas_ts

plot(MNM38_deseas_ts, xlab="Time", ylab="", main="MNM38 - Serie destagionalizzata")

# Costruisco la serie detrendizzata e destagionalizzata, ovvero la componente residua della time series
MNM38_res_ts <- MNM38_ts - MNM38_trend_ts - MNM38_seas_ts
plot(MNM38_res_ts, xlab="Time", ylab="", main="MNM38 - Componente residua della serie destagionalizzata e detrendizzata")


# memorizzo il risultato della decomposizione in un dataframe
MNM38_dec_df <- data.frame(t=MNM38_train_df$t, Year=MNM38_train_df$Year, Month=MNM38_train_df$Month, y=MNM38_train_df$Values, y_trend=as.vector(MNM38_trend_ts)[1:127], y_seas=as.vector(MNM38_seas_ts)[1:127], 
                               y_res=as.vector(MNM38_res_ts)[1:127], y_det=as.vector(MNM38_det_ts)[1:127], y_deseas=as.vector(MNM38_deseas_ts)[1:127])

head(MNM38_dec_df)
tail(MNM38_dec_df)

##### ANALISI DEI RESIDUI ######

# Line plot
Data_df <- MNM38_dec_df
length <- nrow(Data_df)
First_Date <- paste(Data_df$Month[1],Data_df$Year[1])
Last_Date <- paste(Data_df$Month[length],Data_df$Year[length])
title_content <- bquote(atop("Essentials of Time Series Analysis \u0040 Master in Data Science 2022-2023", 
                             paste("Line Plot del rumore di MNM38 da ", .(First_Date), " a ", .(Last_Date))))
subtitle_content <- bquote(paste("path length ", .(length), " sample points."))
caption_content <- "Author: Fabio Palmigiani"
x_name <- bquote("")
y_name <- bquote("Residui")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$Month[x_breaks],Data_df$Year[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$y_res)-min(Data_df$y_res))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$y_res)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$y_res)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
K <-  1
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
col_1 <- bquote("Noise Comp.")
col_2 <- bquote("Curva LOESS")
col_3 <- bquote("Retta di regressione")
leg_labs <- c(col_1, col_2, col_3)
leg_cols <- c("col_1"="blue", "col_2"="red", "col_3"="green")
leg_breaks <- c("col_1", "col_2", "col_3")
MNM38_res_lp <- ggplot(Data_df) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=y_res, color="col_3"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=y_res, color="col_2"),
              method = "loess", formula = y ~ x, se=FALSE) +
  geom_line(alpha=1, size=0.8, linetype="solid", aes(x=t, y=y_res, color="col_1", group=1)) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width = unit(1.0,"cm"), legend.position="bottom")
plot(MNM38_res_lp)


# ADF Test
Res_DF_none <- ur.df(MNM38_dec_df$y_res, type="none", lags=0, selectlags="Fixed")
summary(Res_DF_none)

# KPSS Test
Res_KPSS_mu <- ur.kpss(MNM38_dec_df$y_res, type="mu", lags="nil", use.lag=NULL) 
summary(Res_KPSS_mu) 


# BP e W test
Data_df <- data.frame(x=MNM38_dec_df$t,y=MNM38_dec_df$y_res)
lmtest::bptest(formula = y~x, varformula = NULL, studentize = TRUE, data=Data_df)

var.formula <- ~ x+I(x^2)
lmtest::bptest(formula = y ~ x, varformula = var.formula, studentize = TRUE, data=Data_df)


# Autocorrelogramma
Data_df <- MNM38_dec_df
y <- Data_df$y_res
length <- length(y)
T <- length
maxlag <- ceiling(min(10, T/4))  
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
First_Date <- paste(Data_df$Month[1],Data_df$Year[1])
Last_Date <- paste(Data_df$Month[length],Data_df$Year[length])
title_content <- bquote(atop("Essentials of Time Series Analysis \u0040 Master in Data Science 2022-2023", 
                             paste("Autocorrelogramma dei residui di MNM38 da ", .(First_Date), " a ", .(Last_Date))))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")

# Autocottelogramma parziale
Data_df <- MNM38_dec_df
y <- Data_df$y_res
length <- length(y)
T <- length
maxlag <- ceiling(min(10, T/4))  
P_Aut_Fun_y <- pacf(y, lag.max = maxlag, plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_P_Aut_Fun_y <- data.frame(lag=P_Aut_Fun_y$lag, pacf=P_Aut_Fun_y$acf)
First_Date <- paste(Data_df$Month[1],Data_df$Year[1])
Last_Date <- paste(Data_df$Month[length],Data_df$Year[length])
title_content <- bquote(atop("Essentials of Time Series Analysis \u0040 Master in Data Science 2022-2023", 
                             paste("Autocorrelogramma parziale dei residui di MNM38 da ", .(First_Date), " a ", .(Last_Date))))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
ggplot(Plot_P_Aut_Fun_y, aes(x=lag, y=pacf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=pacf), size = 1, col="black") +
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="pacf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")


#Ljung-Box (LB) test
T <- length(MNM38_dec_df$y_res)
maxlag <- ceiling(min(10, T/4))
y_LB <- LjungBoxTest(MNM38_dec_df$y_res, k=1, lag.max=maxlag, StartLag=1, SquaredQ=FALSE)
show(y_LB)

plot(y_LB[,3], main="Ljung-Box Q Test", ylab="P-values", xlab="Lag")

################################ ARIMA ################################
y <- as.vector(MNM38_dec_df$y_res)
AUTOARIMA_y_AIC <- auto.arima(y, start.p=0, max.p=6, start.q=0, max.q=6, max.order=12, ic="aic", allowmean=TRUE, trace =TRUE, stepwise=FALSE, nmodels =94, approximation=FALSE)
show(AUTOARIMA_y_AIC)
#### Miglior modello ARIMA(3,0,1) ########

# Autocorrelogramma
AUTOARMA_y_AIC_res_ts <- AUTOARIMA_y_AIC[["residuals"]]
AUTOARMA_y_AIC_res_vec <- as.vector(AUTOARMA_y_AIC_res_ts)
sum_pq <- length(AUTOARIMA_y_AIC[["coef"]])
y <- AUTOARMA_y_AIC_res_vec
length <- length(y)
T <- length
maxlag <- ceiling(min(10, T/4))   
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
First_Date <- paste(Data_df$Month[1],Data_df$Year[1])
Last_Date <- paste(Data_df$Month[length],Data_df$Year[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2022-2023", paste("Plot of the Autocorrelogram of the Residual Component of the MNM38 from ", .(First_Date), " to ", .(Last_Date))))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")

# Autocottelogramma parziale
AUTOARMA_y_AIC_res_ts <- AUTOARIMA_y_AIC[["residuals"]]
AUTOARMA_y_AIC_res_vec <- as.vector(AUTOARMA_y_AIC_res_ts)
sum_pq <- length(AUTOARIMA_y_AIC[["coef"]])
y <- AUTOARMA_y_AIC_res_vec
length <- length(y)
T <- length
maxlag <- ceiling(min(10, T/4))    
P_Aut_Fun_y <- pacf(y, lag.max = maxlag, plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_P_Aut_Fun_y <- data.frame(lag=P_Aut_Fun_y$lag, pacf=P_Aut_Fun_y$acf)
First_Date <- paste(Data_df$Month[1],Data_df$Year[1])
Last_Date <- paste(Data_df$Month[length],Data_df$Year[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2022-2023", paste("Plot of the Partial Autocorrelogram of the Residual Component of the MNM38 from ", .(First_Date), " to ", .(Last_Date))))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
ggplot(Plot_P_Aut_Fun_y, aes(x=lag, y=pacf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=pacf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="pacf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")

#Ljung-Box (LB) test
y_LB <- LjungBoxTest(AUTOARMA_y_AIC_res_vec, k=1, lag.max=maxlag, StartLag=1, SquaredQ=FALSE)
show(y_LB)

plot(y_LB[,3], main="Ljung-Box Q Test", ylab="P-values", xlab="Lag")

# ADF Test
Res_DF_none <- ur.df(AUTOARMA_y_AIC_res_vec, type="none", lags=0, selectlags="Fixed")
summary(Res_DF_none)

# KPSS Test
Res_KPSS_mu <- ur.kpss(AUTOARMA_y_AIC_res_vec, type="mu", lags="nil", use.lag=NULL) 
summary(Res_KPSS_mu) 

###############################################################################################

# Statistiche dei residui

# test di Shapiro-Wilk
y <- AUTOARIMA_y_AIC[["residuals"]]
y_SW <- shapiro.test(y)
show(y_SW)

# test di D’Agostino-Pearson
y_DP <- dagoTest(y)
show(y_DP)

# test di Anderson-Darling
y_AD <- ad.test(y)
show(y_AD)

# test di Jarque Bera
y_JB <- jarque.bera.test(y)
show(y_JB)


y <- as.vector(AUTOARIMA_y_AIC[["residuals"]])
summary(y)

sd(y)

MNM38_dec_df_vec<-as.vector(MNM38_dec_df$y_res)
Skew_Gauss <- DescTools::Skew(x=MNM38_dec_df_vec, weights = NULL, na.rm =TRUE, method =2, conf.level =0.95, ci.type ="classic")
show(Skew_Gauss)

Kurt_Gauss <- DescTools::Kurt(x=MNM38_dec_df_vec, weights = NULL, na.rm =TRUE, method =2, conf.level =0.95, ci.type ="classic")
show(Kurt_Gauss)

# Grafico
z <- as.vector(AUTOARIMA_y_AIC[["residuals"]])
z_st <- (1/sd(z))*as.vector(z-mean(z)) # standardizzo i residui di ARIMA(3,0,1)
z_st_qemp <- qemp(ppoints(z_st), z_st)
z_st_demp <- demp(z_st_qemp, z_st)
z_st_pemp <- pemp(z_st_qemp, z_st)
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
hist(z_st, col="green", border="black", xlim=c(x[1]-2.0, x[length(x)]+2.0), 
                     ylim=c(0, y_d[length(y_d)]+ 0.75), 
                     freq=FALSE, main="Istogramma e funzione di densità empirica dei residui standardizzati del modello ARIMA(3,0,1)",
                     xlab="Residui Standardizzati", ylab="Densità",cex.main=0.9)
lines(density(z_st), lwd=2, col="darkgreen")
ecdfPlot(z_st, discrete=TRUE, prob.method="emp.probs", type="s", plot.it=TRUE, add=FALSE, ecdf.col="darkgreen", 
         ecdf.lwd=2, ecdf.lty=1, curve.fill=FALSE, main="Funzione di ripartizione empirica dei residui standardizzati del modello ARIMA(3,0,1)", 
         xlab="Residui Standardizzati", ylab="Funzione di Ripartizione",
         xlim=c(x[1]-1.0, x[length(x)]+1.0),
         cex.main=0.9)

dt_ls <- function(x, m, s, df)	1/s*dt((x-m)/s, df)
pt_ls <- function(q, m, s, df)  pt((q-m)/s, df)
qt_ls <- function(p, m, s, df)  qt(p, df)*s+m
rt_ls <- function(n, m, s, df)  rt(n,df)*s+m

# Fitto la distribuzione 
fitdistr_t_ls <- fitdistr(z_st, dt_ls, start=list(m=0, s=sqrt(1/3), df=3), lower=c(0,0,2))
fitdistr_t_ls_logLik <- fitdistr_t_ls$loglik
fitdistr_t_ls_AIC <- AIC(fitdistr_t_ls, k=2)
fitdistr_t_ls_BIC <- AIC(fitdistr_t_ls, k=log(fitdistr_t_ls$n))
show(fitdistr_t_ls)
show(fitdistr_t_ls_logLik)
show(fitdistr_t_ls_AIC)
show(fitdistr_t_ls_BIC)

m <- as.numeric(fitdistr_t_ls$estimate[1])
s <- as.numeric(fitdistr_t_ls$estimate[2])
df <- as.numeric(fitdistr_t_ls$estimate[3])

# Plot della densità tra la distribuzione dei residui standardizzati del modello ARIMA(3,0,1) e la distribuzione di Student Generalizzata
hist(z_st, col="green", border="black", xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), 
     freq=FALSE, main="Confronto densità tra la distribuzione dei residui standardizzati del modello ARIMA(3,0,1) e la distribuzione di Student Generalizzata", 
     xlab="Residui Standardizzati", ylab="Densità")
lines(density(z_st), lwd=2, col="darkgreen")
lines(x, dt_ls(x, m=m, s=s, df=df), lwd=2, col="blue")
legend("topleft", legend=c("Densità Empirica", "Densità Teorica"), col=c("darkgreen", "blue"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.40, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty="n")

# test di goodness of fit
x <- z_st
KS_x_st_t_ls <- ks.test(x, y="pt_ls", m=m, s=s, df=df, alternative="two.sided")
show(KS_x_st_t_ls)

CVM_x_st_t_ls <- cvm.test(z_st, null="pt_ls", m=m, s=s, df=df, estimated=FALSE)
show(CVM_x_st_t_ls)

AD_x_st_t_ls <- ad.test(z_st,"pt_ls", m=m, s=s, df=df, estimated=FALSE)
show(AD_x_st_t_ls)

# Q-Q plot dei residui standardizzati del modello ARIMA(3,0,1)
x <- z_st_qemp
car::qqPlot(x, distribution ="t_ls", m=m, s=s, df=df,
            line="robust", 
            envelope=list(level=0.95, alpha=0.15, border=TRUE), col=carPalette()[2], col.lines=carPalette()[8],
            pch=16, cex=0.5, las=1,
            main=bquote(atop("Q-Q plot dei residui standardizzati del modello ARIMA(3,0,1)",
                             paste("Contro distribuzione di Student Gen. con m = ", .(m),", s = ", .(round(s, 3)),", e df = ", .(round(df,3)),"."))),
            xlab=bquote(paste("Quantili teorici distribuzione di Student Generalizzata")), 
            ylab="Quantili empirici distribuzione dei residui")

probs <- c(0.25,0.75)
quart_x <- as.vector(quantile(x, probs))
quart_t <- qt_ls(probs, m=m, s=s, df=df)
slope <- diff(quart_x)/diff(quart_t)
intercept <- quart_x[1]-slope*quart_t[1]
abline(a=intercept, b=slope, col="cyan", lwd=1)

abline(a=0, b=1, col="black", lwd=1)
legend("topleft", 
       legend=c("Retta di regressione", "Retta interquantile", "y=x"),
       col=c("red", "black", "green"), lty=1, lwd=0.1, cex=0.80, x.intersp=0.50, 
       y.intersp=0.40, text.width=2, seg.len=1, inset=-0.01, bty="n")

m
s
df


###################################### PREDIZIONE ######################################
Test_end <- nrow(MNM38_df)
Test_start <- TS_length+1

MNM38_test_df <- MNM38_df[Test_start:Test_end,]

# PREDIZIONE TREND
t <- 1:TS_length
y <- MNM38_dec_df$y_trend

trend_lm <- lm(y_trend~t, data=MNM38_dec_df)
trend_pred = predict(trend_lm, MNM38_test_df)

# PREDIZIONE STAGIONALITA'
y_seas <- ts(MNM38_dec_df$y_seas,start=c(1977, 12), frequency=12 )
y_seas_naive_pred <- c(y_seas[(length(y_seas)-11):length(y_seas)],y_seas[(length(y_seas)-11):(length(y_seas)-2)])
y_seas_naive_for <- c(y_seas, y_seas_naive_pred)

#######################################################################################################

y_rem <- as.vector(AUTOARIMA_y_AIC[["x"]])
train_trend = MNM38_dec_df$y_trend 
train_seas = MNM38_dec_df$y_seas
train_res = MNM38_dec_df$res
training_data = train_trend+train_seas+train_res

AUTOARIMA_y_AIC[["coef"]]

phi_1 = AUTOARIMA_y_AIC[["coef"]][["ar1"]]
phi_2 = AUTOARIMA_y_AIC[["coef"]][["ar2"]]
phi_3 = AUTOARIMA_y_AIC[["coef"]][["ar3"]]
sigma2 = AUTOARIMA_y_AIC[["sigma2"]]
sigma_w <- sqrt(sigma2)

n <- 100000
y_pred_stud_mat <- matrix(NA, nrow=TsS_length, ncol=n)
for(k in 1 :n){ 
  set.seed(k) 
  w <- rt_ls(TsS_length, m=m, s=s, df=df)
  y_pred_stud_mat[1,k] <- phi_1*y_rem[TS_length] + phi_2*y_rem[(TS_length-1)] + phi_3*y_rem[(TS_length-2)] + sigma_w*w[1] 
  y_pred_stud_mat[2,k] <- phi_1*y_pred_stud_mat[1,k] + phi_2*y_rem[TS_length] + phi_3*y_rem[(TS_length-1)] + sigma_w*w[2] 
  y_pred_stud_mat[3,k] <- phi_1*y_pred_stud_mat[2,k] + phi_2*y_pred_stud_mat[1] + phi_3*y_rem[TS_length] + sigma_w*w[3] 
  for(t in 4:TsS_length){ 
    y_pred_stud_mat[t,k] <- phi_1*y_pred_stud_mat[(t-1),k] + phi_2*y_pred_stud_mat[(t-2),k] + phi_3*y_pred_stud_mat[(t-3),k] + sigma_w*w[t] 
  } 
}


y_pred_stud_mat_mean <- rowMeans(y_pred_stud_mat)
y_pred_stud_mat_mean
y_pred_norm_mat_090_low_int <- rowQuantileC(y_pred_stud_mat, 0.10)
y_pred_norm_mat_090_low_int
y_pred_norm_mat_090_upp_int <- rowQuantileC(y_pred_stud_mat, 0.90)
y_pred_norm_mat_090_upp_int
y_pred_norm_mat_095_low_int <- rowQuantileC(y_pred_stud_mat, 0.05)
y_pred_norm_mat_095_low_int
y_pred_norm_mat_095_upp_int <- rowQuantileC(y_pred_stud_mat, 0.95)
y_pred_norm_mat_095_upp_int

MNM38_train_df_vec <-as.vector(MNM38_train_df$Values)

prediction <- trend_pred + y_seas_naive_pred + y_pred_stud_mat_mean
prediction
MNM38_point_pred <- prediction
MNM38_point_pred_int <- c(MNM38_train_df$Values, MNM38_point_pred)
MNM38_point_pred_int
MNM38_boot_090_low_naive_pred_int <- (MNM38_point_pred + y_pred_norm_mat_090_low_int)
MNM38_boot_090_upp_naive_pred_int <- (MNM38_point_pred + y_pred_norm_mat_090_upp_int)
MNM38_boot_095_low_naive_pred_int <- (MNM38_point_pred + y_pred_norm_mat_095_low_int)
MNM38_boot_095_upp_naive_pred_int <- (MNM38_point_pred + y_pred_norm_mat_095_upp_int)

MNM38_boot_090_low_naive_for_int <- c(rep(NA,TS_length),MNM38_boot_090_low_naive_pred_int)
MNM38_boot_090_upp_naive_for_int <- c(rep(NA,TS_length),MNM38_boot_090_upp_naive_pred_int)
MNM38_boot_095_low_naive_for_int <- c(rep(NA,TS_length),MNM38_boot_095_low_naive_pred_int)
MNM38_boot_095_upp_naive_for_int <- c(rep(NA,TS_length),MNM38_boot_095_upp_naive_pred_int)


MNM38_pred_full_df <- data.frame(Year = trunc(time(MNM38_ts)), Month=month.abb[cycle(MNM38_ts)]) 
MNM38_pred_full_df <- add_column(MNM38_pred_full_df, t=c(1:nrow(MNM38_pred_full_df)), .before="Year")

MNM38_pred_full_df <- add_column(MNM38_pred_full_df, 
                                 MNM38_Value=MNM38_df$Values, 
                                 MNM38_point_for=MNM38_point_pred_int,
                                 MNM38_090_low_for_int=MNM38_boot_090_low_naive_for_int, 
                                 MNM38_090_upp_for_int=MNM38_boot_090_upp_naive_for_int,
                                 MNM38_095_low_for_int=MNM38_boot_095_low_naive_for_int, 
                                 MNM38_095_upp_for_int=MNM38_boot_095_upp_naive_for_int)



##################################### GRAFICO PREDIZIONE FINALE #####################################

Data_df <- MNM38_pred_full_df
length <- nrow(Data_df)
T<- TS_length
First_Date <- paste(Data_df$Month[1],Data_df$Year[1])
Last_Date <- paste(Data_df$Month[length],Data_df$Year[length])
title_content <- bquote(atop("Università di Roma \"Tor Vergata\"- Master in Data Science 2022-2023", 
                             paste("Training Set e previsione del Test Set della serie MNM38")))
subtitle_content <- bquote(paste("Osservazioni Training Set: ", .(TS_length),". Osservazioni Test Set: ", .(TsS_length),"."))
caption_content <-"Autore: Fabio Palmigiani"
x_name <- bquote("")
x_breaks_num <-33
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$Month[x_breaks],Data_df$Year[x_breaks])
J <-0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("MNM38")
y_breaks_num <-10
y_max <- max(na.omit(Data_df$MNM38_Value),na.omit(Data_df$MNM38_095_upp_for_int))
y_min <- min(na.omit(Data_df$MNM38_Value),na.omit(Data_df$MNM38_095_low_for_int))
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor(y_min/y_binwidth)*y_binwidth
y_breaks_up <- ceiling(y_max/y_binwidth)*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <-0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
line_black <- bquote("Training Set")
line_magenta <- bquote("Test Set")
line_brown <- bquote("Pred.")
line_green <- bquote("90% Int.Pred.")
line_red <- bquote("95% Int.Pred.")
leg_line_labs <- c(line_black, line_brown, line_magenta, line_green, line_red)
leg_line_breaks <- c("line_black","line_brown","line_magenta","line_green","line_red")
leg_line_cols <- c("line_black"="black","line_brown"="brown","line_magenta"="magenta","line_green"="green","line_red"="red")
fill_g <- bquote("Banda 90%")
fill_r <- bquote("Banda 95%")
leg_fill_labs <- c( fill_g, fill_r)
leg_fill_breaks <- c("fill_g","fill_r")
leg_fill_cols <- c("fill_g"="lightgreen","fill_r"="orangered")
leg_col_labs <- leg_line_labs
leg_col_breaks <- leg_line_breaks
leg_col_cols <- leg_line_cols
y_pred_lp <- ggplot(Data_df, aes(x=t)) + 
  geom_line(data=subset(Data_df, Data_df$t <= t[T+1]), aes(y=MNM38_Value, color="line_black"), linetype="solid", alpha=1, size=1, group=1) + 
  geom_line(data=subset(Data_df, Data_df$t >= t[T+1]), aes(y=MNM38_Value, color="line_magenta"), linetype="solid", alpha=1, size=1, group=1) + 
  geom_line(data=subset(Data_df, Data_df$t >= t[T+1]), aes(y=MNM38_point_for, colour="line_brown"), linetype="solid", alpha=1, size=1) + 
  geom_line(data=subset(Data_df, Data_df$t >= t[T+1]), aes(y=MNM38_095_low_for_int, colour="line_red"), linetype="solid", alpha=1, size=1) + 
  geom_line(data=subset(Data_df, Data_df$t >= t[T+1]), aes(y=MNM38_095_upp_for_int, colour="line_red"), linetype="solid", alpha=1, size=1) + 
  geom_line(data=subset(Data_df, Data_df$t >= t[T+1]), aes(y=MNM38_090_low_for_int, colour="line_green"), linetype="solid", alpha=1, size=1) +
  geom_line(data=subset(Data_df, Data_df$t >= t[T+1]), aes(y=MNM38_090_upp_for_int, colour="line_green"), linetype="solid", alpha=1, size=1) + 
  geom_ribbon(data=subset(Data_df, Data_df$t >= t[T+1]),
              alpha=0.3, colour="orangered", aes(ymin=MNM38_095_low_for_int, ymax=MNM38_090_low_for_int, fill="fill_r")) + 
  geom_ribbon(data=subset(Data_df, Data_df$t >= t[T+1]), 
              alpha=0.3, colour="orangered", aes(ymin=MNM38_090_upp_for_int, ymax=MNM38_095_upp_for_int, fill="fill_r")) + 
  geom_ribbon(data=subset(Data_df, Data_df$t >= t[T+1]), 
              alpha=0.3, colour="lightgreen", aes(ymin=MNM38_090_low_for_int, ymax=MNM38_090_upp_for_int, fill="fill_g")) + 
  scale_x_continuous(name=x_name, breaks=x_breaks, labels=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis= sec_axis(~., breaks=y_breaks, labels=y_labs)) + 
  ggtitle(title_content) + labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") + scale_colour_manual(name="", labels=leg_line_labs, values=leg_line_cols, breaks=leg_line_breaks) + 
  scale_fill_manual(name="", labels=leg_fill_labs, values=leg_fill_cols, breaks=leg_fill_breaks) + 
  guides(colour=guide_legend(order=1), fill=guide_legend(order=2)) + theme(plot.title=element_text(hjust =0.5, size =10), 
  plot.subtitle=element_text(hjust =0.5, size =8), 
  plot.caption = element_text(hjust =1.0, size =8), 
  axis.text.x = element_text(angle=-45, vjust=1, hjust=-0.3), 
  legend.key.width = unit(0.4,"cm"), legend.position="bottom")
plot(y_pred_lp)

