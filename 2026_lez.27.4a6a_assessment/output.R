## lez.27.4a6a assessment - outputs for reporting
## April 2026

library(icesTAF)
library(icesAdvice)
library(xtable)
library(tidyverse)

source("functions/funcs_no_retape_Kokkalis.r")

mkdir("output")

#### Strip out Model Parameters
xtab<-function(x,caption='Table X.', file=stdout(), width='"100%"', cornername='', dec=rep(1,ncol(x))){
  nc<-ncol(x)
  lin<-paste('<table width=',width,'>', sep='')
  lin<-c(lin,sub('$','</td></tr>',sub('\\. |\\.$','.</b> ',
                                      sub('^', paste('<tr><td colspan=',nc+1,'><b>',sep=''), caption))))
  hr<-paste('<tr><td colspan=',nc+1,'><hr noshade></td></tr>', sep='')
  lin<-c(lin,hr)
  cnames<-colnames(x)
  cnames<-paste(sub('$','</b></td>',sub('^','<td align=right><b>',cnames)), collapse='\t')
  lin<-c(lin,paste('<tr>',paste('<td align=left><b>',cornername,'</b></td>',sep=''),cnames,'</tr>'))
  lin<-c(lin,hr)
  rnames<-sub('$','</b></td>',sub('^','<tr> <td align=left><b>',rownames(x)))
  #x<-sapply(1:ncol(x),function(i)sub('NA','  ',format(round(x[,i],dec[i]))))
  x<-sapply(1:ncol(x),function(i)sub('NA','  ',formatC(round(x[,i],dec[i]),digits=dec[i], format='f')))
  for(i in 1:nrow(x)){
    thisline<-paste(rnames[i],paste(sub('$','</td>',sub('^','<td align=right>',x[i,])), collapse='\t'),'</tr>', sep='')
    lin<-c(lin,thisline)
  }
  lin<-c(lin,hr)
  lin<-c(lin,'</table><br>\n')
  writeLines(lin,con=file)
}


##############################
#### Management scenarios ####
##############################

int_yr <- 2026

# Set management scenario's agreed at benchmark
TAC <- mean(tail(inp$obsC, 1)) 
# TAC2 <- mean(tail(inp$obsC, 3)) # catch last 3 years

res <- manage(res, scenarios=c(4,2,3,8), maninterval = c(2027, 2028), intermediatePeriodCatch = TAC)

sumspict.manage(res)

#### F status quo fix

Fsq_value <- get.par("logFFmsy", res$man[[1]], exp = T)[as.character(2026-0.0625), 2] ### change year as needed
Fint_value <- get.par("logFFmsy", res$man[[1]], exp = T)[as.character(2027-0.0625), 2] ### change year as needed
res <- add.man.scenario2(res, "F=Fsq_ffac", ffac = Fsq_value/Fint_value, maninterval = c(2027, 2028), 
                         intermediatePeriodCatch = TAC)

sumspict.manage(res) # management summary 

sumspict.predictions(res, ndigits = 2) # predictions

plotspict.catch(res)

png("output/catch options.png", width = 720)
plotspict.catch(res)
dev.off()

# Saving on res object 
#save(res, file = "output/res.RData")

# plotspict.hcr(res)

plotspict.hcr(res, xlim = c(0,1))

#### Tables of estimates ####

# Parameter Estimates
tab1 <- sumspict.parest(res$man[[1]]);
xtab(tab1,caption="Parameter estimates",cornername="Parameter",
     file= "output/Parameter_estimates.html",dec=rep(4,ncol(tab1)))

# Reference Points
tab2 <- sumspict.srefpoints(res$man[[1]]);
xtab(tab2,caption="Stochastic reference points",cornername="Reference points",
     file= "output/Reference_points.html",dec=rep(4,ncol(tab2)))

# Estimated States
tab3 <- sumspict.states(res$man[[1]]);
xtab(tab3,caption="Estimated states",cornername="",
     file= "output/Estimated_states.html",dec=rep(4,ncol(tab3)))


#### Produce csv for use in standard graphs and summary of assessment ####

bbmsy <- get.par("logBBmsy",res$man[[4]],exp=TRUE)[,1:3]
# bbmsy <- bbmsy[grep(".9375",rownames(bbmsy), fixed=TRUE),] # select bbmsy at end of year 
bbmsy<- bbmsy[grep(".",rownames(bbmsy), fixed=TRUE, invert=TRUE),] # select bbmsy at start of year 


df_bbmsy <- data.frame(bbmsy)
df_bbmsy <- cbind(substr(rownames(df_bbmsy), 1, 4), df_bbmsy)
colnames(df_bbmsy) <- c("Year", "BBmsy_lower", "BBmsy_est", "BBmsy_upper")
df_bbmsy$Year <- as.numeric(df_bbmsy$Year)

ffmsy <- get.par("logFFmsy",res$man[[4]],exp=TRUE)[,1:3]
ffmsy <- ffmsy[grep(".9375",rownames(ffmsy), fixed=TRUE),] 
df_ffmsy <- data.frame(ffmsy)
df_ffmsy <- cbind(substr(rownames(df_ffmsy), 1, 4), df_ffmsy)
colnames(df_ffmsy) <- c("Year", "FFmsy_lower", "FFmsy_est", "FFmsy_upper")
df_ffmsy$Year <- as.numeric(df_ffmsy$Year)

ld <- data.frame(Year = megC$Year,
                 Landings = round(megC$Assessment_Land),
                 Discards = round(megC$Assessment_Disc))

sg_df <- left_join(df_bbmsy[,1:4], ld)
sg_df <- left_join(sg_df, df_ffmsy[,1:4])

sg_df <- sg_df[-nrow(sg_df), ] # remove the last row
sg_df$FFmsy_lower[nrow(sg_df)] <- NA
sg_df$FFmsy_est[nrow(sg_df)] <- NA
sg_df$FFmsy_upper[nrow(sg_df)] <- NA

write.csv(sg_df, "output/summary_assessment_and_standard_graphs.csv")

sg_df$BBmsy_lower <- icesRound(sg_df$BBmsy_lower)
sg_df$BBmsy_est <- icesRound(sg_df$BBmsy_est)
sg_df$BBmsy_upper <- icesRound(sg_df$BBmsy_upper)

sg_df$FFmsy_lower <- icesRound(sg_df$FFmsy_lower)
sg_df$FFmsy_est <- icesRound(sg_df$FFmsy_est)
sg_df$FFmsy_upper <- icesRound(sg_df$FFmsy_upper)

write.csv(sg_df, "output/summary_assessment_and_standard_graphs_rounded.csv")

# Intermediate year table


ffmsy_int <- sg_df[(nrow(sg_df)-1),8]
bbmsy_int <- sg_df[(nrow(sg_df)),3]
Catch_int <- TAC

intyr_tab <- data.frame(Variable = c("F/FMSY", "B/BMSY", "Catch"),
                        Value = c(ffmsy_int, bbmsy_int, Catch_int))

intyr_export <- xtable(intyr_tab, digits = 4, caption = "Interim Year Assumptions")

print(intyr_export, type = "html",
      file = "output/Interim_Assumptions.html",
      include.rownames = FALSE)

# catch options not rounded

prev.advice <- 8050 ## needs to be manually updated

sumspict.manage(res, include.abs = T)

Basis = c("FMSY_35", "FMSY", "FConstant", "F0", "Fsq")

Opt_Catch <- c(get.par("logCpred", res$man$ices, exp = T)[nrow(megC)+2, 2],
               get.par("logCpred", res$man$Fmsy, exp = T)[nrow(megC)+2, 2],
               get.par("logCpred", res$man$currentF, exp = T)[nrow(megC)+2, 2],
               get.par("logCpred", res$man$noF, exp = T)[nrow(megC)+2, 2],
               get.par("logCpred", res$man$`F=Fsq_ffac`, exp = T)[nrow(megC)+2, 2])

Opt_ffmsy <- c(get.par("logFFmsy", res$man$ices, exp = T)[as.character(int_yr+1.9375), 2],
               get.par("logFFmsy", res$man$Fmsy, exp = T)[as.character(int_yr+1.9375), 2],
               get.par("logFFmsy", res$man$currentF, exp = T)[as.character(int_yr+1.9375), 2],
               get.par("logFFmsy", res$man$noF,  exp = T)[as.character(int_yr+1.9375), 2],
               get.par("logFFmsy", res$man$`F=Fsq_ffac`,  exp = T)[as.character(int_yr+1.9375), 2])

Opt_bbmsy <- c(get.par("logBBmsy", res$man$ices, exp = T)[as.character(int_yr+2), 2],
               get.par("logBBmsy", res$man$Fmsy, exp = T)[as.character(int_yr+2), 2],
               get.par("logBBmsy", res$man$currentF, exp = T)[as.character(int_yr+2), 2],
               get.par("logBBmsy", res$man$noF, exp = T)[as.character(int_yr+2), 2],
               get.par("logBBmsy", res$man$`F=Fsq_ffac`, exp = T)[as.character(int_yr+2), 2])


# tail(get.par("logFFmsy", res$man[[3]], exp = T)  )


catch_opt <- data.frame (Basis = Basis,
                         Catch = round(Opt_Catch),
                         FFMSY = round(Opt_ffmsy, digits = 3),
                         BBMSY = round(Opt_bbmsy, digits = 3),
                         B_Change = round(((Opt_bbmsy/bbmsy_int)-1)*100, digits = 3),
                         Adv_change = round(((Opt_Catch/prev.advice)-1)*100, digits = 3))

write.csv(catch_opt, "output/catch_options_for_advice_sheet.csv")


catch_opt_export<- xtable(catch_opt, caption = "Catch Scenarios")

print(catch_opt_export, type = "html",
      file = "output/Catch_Scenarios.html",      ,
      include.rownames = FALSE)

#### Final saves #### 

### Save management scenario's
write.csv(sumspict.manage(res), 
          "output/Outputsmge_Options.csv") 
write.csv(sumspict.predictions(res, ndigits = 2), 
          "output/OutputsPredictions.csv")



#### average discard rate ####

## for table 1 on advice sheet

megC$DiscRate <-megC$Assessment_Disc/megC$catch
disc_rate_interim <- mean(tail(megC$DiscRate,3))
Proj_disc <- disc_rate_interim * intyr_tab[3,2]

Proj_land <- intyr_tab[3,2] - Proj_disc 

Proj_disc
Proj_land

