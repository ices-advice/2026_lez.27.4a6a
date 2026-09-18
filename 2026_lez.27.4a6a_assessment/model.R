## Run analysis, write model results

## Before:
## After:

library(icesTAF)
library(spict)
library(FishLife)
library(corrplot)
library(ggplot2)
library(patchwork)

options(scipen = 1e6)

#source("functions/myretro_function.r")

mkdir("model")

megC <- read.taf("data/megC.csv")
megI1 <- read.taf("data/megI1.csv")
megI2 <- read.taf("data/megI2.csv")

### data structured as a list for SPiCT
inp <- list(obsC=megC$catch, timeC=megC$Year)

inp$obsI[[1]] <- megI1$est
inp$timeI[[1]] <- megI1$Year
inp$obsI[[2]] <- megI2$est
inp$timeI[[2]] <- megI2$Year

# adjust index for time of year
inp$timeI[[1]] <- inp$timeI[[1]] + (1/12)*3
inp$timeI[[2]] <- inp$timeI[[2]] + (1/12)*10

# include survey index uncertainty
inp$stdevfacI[[1]] <- megI1$se/mean(megI1$se, na.rm = T)
inp$stdevfacI[[2]]  <- megI2$se/mean(megI2$se, na.rm = T)


#### intrinsic growth rate (r) from Thorsen Fishlife for Megrim ####
par(mfrow=c(1, 2))

stk.fishlife<-Plot_taxa(Search_species(Genus="Lepidorhombus",
                                       Species="whiffiagonis")$match_taxonomy, 
                        mfrow=c(1,1))

lnr <- stk.fishlife[[1]]$Mean_pred["ln_r"]
sd_lnr <- sqrt(stk.fishlife[[1]]$Cov_pred["ln_r", "ln_r"])
curve(dlnorm(x, lnr, sd_lnr), from = 0, to = 1)

exp(lnr)
exp(sd_lnr)

#### Fix model parameters ####
inp$priors$logalpha <- c(1, 1, 0) # turn off alpha and beta priors
inp$priors$logbeta <- c(1, 1, 0)

inp$priors$logr <- c(lnr, sd_lnr, 1) # (r prior) 
inp$ini$logn <- log(2) # shape parameter 
inp$phases$logn <- -1 # fixed to Schaefer 

## Check that the 1 prior (r active)
get.no.active.priors(inp)

#### Check and plot input data ####
check.inp(inp)

png(file="model/input_data.png")
plotspict.data(inp)
dev.off()

#### Fit Spict to data ####
res <- fit.spict(inp) 
res
plot(res)

png("model/Assessment_Res.png", width = 12, height = 8, units = "in", res = 300)
plot(res)
dev.off()

png("model/Assessment_Res_brief.png")
plot2(res)
dev.off()


### Save fit summary
fitSummary <- capture.output(res)
write.csv(fitSummary ,"model/lez4a6a_fitsummary.csv", quote = TRUE,
          eol = "\n", na = "NA", row.names = TRUE, fileEncoding = "")

writeLines(paste("<pre>", paste(fitSummary, collapse = "\n"), "</pre>"), "model/lez4a6a_fitsummary.html")

#### Check Model Acceptance (NO VIOLATIONS) ####

res$opt$convergence # should equal 0
all(is.finite(res$sd)) # should be TRUE

res <- calc.osa.resid(res) # Calculate residuals
plotspict.diagnostic(res) # No Violations 


png("model/Assessment_residuals.png", width = 12, height = 8, units = "in", res = 300)
plotspict.diagnostic(res)
dev.off()


retro <- retro(res, nretroyear=5) # Retrospective analysis
plotspict.retro(retro) # trajectories of those two quantities should be inside the confidence intervals of the base run.

png("model/Assessment_retros.png", width = 12, height = 8, units = "in", res = 300)
plotspict.retro(retro)
dev.off()

res <- hindcast(res, npeels = 5)
png("model/Assessment_hindcast.png", width = 12, height = 8, units = "in", res = 300)
plotspict.hindcast(res, legend.pos = NULL)
dev.off()

calc.bmsyk(res) # should be between 0.1 and 0.9
calc.om(res)  # should not span more than 1 order of magnitude

res <- check.ini(res) # sensitivity check
res$check.ini$resmat #the estimates should be the same for all initial values

png("model/Production_curve.png")
plotspict.production(res)
dev.off()

png("model/Biomass.png")
plotspict.biomass(res)
dev.off()

png("model/F mort.png")
plotspict.f(res)
dev.off()

#### Check parameters correlation ####
colp <- colorRampPalette(rev(c("#67001F", "#B2182B", "#D6604D", 
                               "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
                               "#4393C3", "#2166AC", "#053061"))) ## intiutively think cold is negative and blue

## full correlation (fixed and random effects)
precision <- sdreport(res$obj, getJointPrecision = TRUE)

all_cov <- solve(precision$jointPrecision)

pars <- names(res$opt$par)
pars <- pars[pars != "logn"]
idx <- which(colnames(all_cov) %in% pars)

## first logB
idxB0 <- which(colnames(all_cov) == "logB")[1]
idall <- c(idx, idxB0)

corr_B0 <- cov2cor(all_cov[idall, idall])
colnames(corr_B0)[colnames(corr_B0) == "logB"] <- "logB0"
rownames(corr_B0)[rownames(corr_B0) == "logB"] <- "logB0"

corrplot(corr_B0, method = "ellipse",         ## no one above +-0.7
         type = "upper", col = colp(200),
         addCoef.col = "black", diag = FALSE)

dev.print(pdf, "model/correlation_plot.pdf")


#### Laurie Kell Diagnostics  #####

pres <- calc.process.resid(res)

plotspict.diagnostic.process(pres)

pe <- data.frame(year = pres$process.resid$time,
                 pe = pres$process.resid$B)


B_est <- get.par("logB", res ,exp=TRUE)
B_est2 <-  B_est[grep(".",rownames(B_est), fixed=TRUE, invert=TRUE),]
B_est2 <- B_est2 [-nrow(B_est2 ), ]
pe$biomass <- B_est2[,2]

p_time = ggplot(pe, aes(x = year, y = pe)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_line(colour = "steelblue") +
  geom_point(colour = "steelblue") +
  labs(x = "Year", y = "Process residual",
       title = "Over time") +
  theme_bw()

p_fit = ggplot(pe, aes(x = biomass, y = pe)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_point(alpha = 0.6, colour = "steelblue") +
  labs(x = "Fitted biomass (or state)", y = "Process residual",
       title = "Residuals vs Biomass") +
  theme_bw()

acf_obj = acf(pe$pe, plot = FALSE)
acf_df = with(acf_obj, data.frame(lag = lag, acf = acf))[-1, ]
crit = qnorm(0.975) / sqrt(nrow(pe))
p_acf = ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_hline(yintercept = c(-crit, crit), linetype = "dashed",
             colour = "grey60") +
  geom_col(fill = "steelblue") +
  labs(x = "Lag", y = "ACF",
       title = "Autocorrelation") +
  theme_bw()

p_hist = ggplot(pe, aes(x = pe)) +
  geom_histogram(aes(y = ..density..), bins = 20,
                 fill = "steelblue", colour = "white", alpha = 0.7) +
  stat_function(fun = dnorm,
                args = list(mean = mean(pe$pe), sd = sd(pe$pe)),
                colour = "red", linewidth = 0.8) +
  labs(x = "Residual", y = "Density",
       title = "Distribution") +
  theme_bw()

(p_time | p_fit) /  (p_acf | p_hist)
dev.print(pdf, "model/process residual diagnostics_LK.pdf")

