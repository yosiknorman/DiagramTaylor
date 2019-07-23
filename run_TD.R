#!/usr/bin/Rscript
setwd("~/Desktop/Friend/DiagramJahit/")
library(openair)
library(lattice)

input = read.csv(file = "input/in2.csv", sep = ",")
input = as_tibble(input)
# input2 = cbind(input, month = mod1$month, model1 = mod1$mod, model2 = mod2$mod, model3 = mod3$mod)
# write.csv(file = "input/in2.csv", input2, row.names = F)

mod_name = c("model 1", "model 2", "model 3")


input$date = as.POSIXct(input$date, tz = "UTC")
# dat <- selectByDate(input, year = 2003)
dat <- data.frame(date = input$date, obs = input$nox, mod = input$nox)
mod1 <- data.frame(date = input$date, obs = input$nox, mod = input$model1)
mod2 <- data.frame(date = input$date, obs = input$nox, mod = input$model2)
mod3 <- data.frame(date = input$date, obs = input$nox, mod = input$model3)

## now make mod worse by adding bias and noise according to the month
## do this for 3 different models
dat <- transform(dat, month = as.numeric(format(date, "%m")))
# mod1 <- transform(dat, mod = mod + 10 * month + 10 * month * rnorm(nrow(dat)),
                  # model = "model 1")
mod1 = cbind(dat$date,)

## lag the results for mod1 to make the correlation coefficient worse
## without affecting the sd
mod1 <- transform(mod1, mod = c(mod[5:length(mod)], mod[(length(mod) - 3) :
                                                          length(mod)]))

## model 2
mod2 <- transform(dat, mod = mod + 7 * month + 7 * month * rnorm(nrow(dat)),
                  model = "model 2")
## model 3
mod3 <- transform(dat, mod = mod + 3 * month + 3 * month * rnorm(nrow(dat)),
                  model = "model 3")

mod.dat <- rbind(mod1, mod2, mod3)

## basic Taylor plot
source("_TAI_lor_Diag.R")
pdf(file = "output/allseason.pdf")
TaylorDiagram(mod.dat, obs = "obs", mod = "mod", group = "model")
dev.off()
## Taylor plot by season
pdf(file = "output/musiman.pdf")
TaylorDiagram(mod.dat, obs = "obs", mod = "mod", group = "model", type = "season")
dev.off()


#lag(1:10,-1)
#lag(ldeaths, 12)
