options(digits = 15)
library(tidyverse)
library(patchwork)
#library(scales)
labelSize <- 2.5
smallLabelSize <- 2
titleSize <- 7
subtitleSize <- 7
legendTextSize <- 6
axisTextSize <- 6
axisTitleSize <- 7
facetTitleSize <- 7
thisFolder <- "D:/OneDrive - CGIAR/Documents 1/Personal stuff/finMoves/bnkStats/"
fileVec <- list.files(thisFolder)
#========================================================================
list_df <- list()
startBalVec <- c()
for(j in 1:length(fileVec)){
  thisFile <- fileVec[j]
  #thisFile <- "extracto_202302.txt"
thisFilePath <- paste0(thisFolder, thisFile)
thisYr <- gsub("[^0-9]+", "", thisFile) %>% substring(1, 4) %>% as.integer()
df <- read.delim(thisFilePath)
#-------------------------------------------------------------------------
startBalance <- df$H.01[grep("Saldo Anterior", df$H.01)]
#startBalance <- gsub("\\..*$", "", startBalance)
startBalance <- as.numeric(gsub("[^0-9\\.]", "\\1", startBalance))
startBalVec[j] <- startBalance
#-------------------------------------------------------------------------
indStart <- grep("Fecha", df$H.01) + 1
indEnd <- grep("Este producto cuenta con", df$H.01) - 1
indEnd[length(indEnd) + 1] <- nrow(df) - 1
keepRows <- c(indStart[1]:indEnd[1], indStart[2]:indEnd[2])
txtVec <- df[keepRows, ]
fechaVec <- c()
valVec <- c()
sgnVec <- c()
descripVec <- c()
for(i in 1:length(txtVec)){
  xx <- str_split(txtVec[i], "\\$")
  fechaVec[i] <- paste0(gsub(" ", "", xx[[1]][1]), thisYr)
  val <- gsub("(-|\\+).*", "\\1", xx[[1]][2])
  sgn <- substring(val, nchar(val), nchar(val))
  sgn <- ifelse(sgn == "-", -1, 1)
  val <- gsub("(-|\\+).*", "", val)
  #val <- gsub("\\..*$", "", val)
  valVec[i] <- sgn * as.numeric(gsub("[ ,]", "", val))
  descripVec[i] <- trimws(gsub("[0-9,\\.\\+-]", "", xx[[1]][2]))
}
#-----------------------------------------------------------------------
df <- data.frame(Date = fechaVec,
                 Change = valVec,
                 Descrip = descripVec)
ind <- which(is.na(df$Change))
if(length(ind) != 0){
  df$Descrip[ind - 1] <- paste(df$Descrip[ind - 1], df$Fecha[ind])
  df <- df[-ind, ]
}
df$Date <- as.Date(df$Date, "%d%m%Y")
#-----------------------------------------------------------------------
bills <- "GASES|CELULAR|EMCALI|Safetypay"
groceries <- "SUPERTIENDAS CANAV|EXITO|CRUZ VERDE|PORVENIR|JUMBO|FARMATODO"
rent <- "Dctotransferencia otra entidad                                BTA PROCESOS ESP"
admon <- "Compra A Toda Hora  SA                                         Compras y Pagos PSE"
df$Type <- NA
df$Type[grep("Dctotransferencia|Descuento Por Transf", df$Descrip)] <- "Transfer"
df$Type[grep("Transferencia ITAU", df$Descrip)] <- "Salary"
df$Type[grep("Compra", df$Descrip)] <- "Compra"
df$Type[grep(groceries, df$Descrip)] <- "Groceries"
df$Type[grep("Retiro", df$Descrip)] <- "Retiro"
df$Type[grep(bills, df$Descrip)] <- "Bill"
df$Type[grep(rent, df$Descrip)] <- "Rent"
df$Type[grep(admon, df$Descrip)] <- "Admon"
df$Type[which(is.na(df$Type))] <- "Other"
#-----------------------------------------------------------------------
balVec <- c()
xx <- startBalance
for(i in 1:nrow(df)){
  xx <- xx + df$Change[i]
  balVec[i] <- xx
  #print(xx)
}
df$Balance <- balVec
list_df[[j]] <- df
}
df <- as.data.frame(do.call(rbind, list_df))
#df <- df[order(df$Date), ]
#========================================================================
df$yr <- year(df$Date)
df$wk <- week(df$Date)
df$wk[which(df$wk == 53)] <- 52
dfWk <- df %>%
  group_by(yr, wk) %>%
  summarise(Change = sum(Change)) %>% as.data.frame()
#---
balVec <- c()
xx <- startBalVec[1]
for(i in 1:nrow(dfWk)){
  xx <- xx + dfWk$Change[i]
  balVec[i] <- xx
  #print(xx)
}
difLnVec <- diff(log(c(startBalVec[1], balVec)))
difArVec <- dfWk$Change / c(startBalVec[1], balVec)[-(length(balVec) + 1)]
dfWk$Balance <- balVec
dfWk$`Log Change` <- difLnVec
dfWk$`Pct. Change` <- 100 * difArVec
# hist(difLnVec)
# hist(difArVec)
#---
dfWk$wk[which(dfWk$wk < 10)] <- paste0("0", dfWk$wk[which(dfWk$wk < 10)])
dfWk$Date <- paste0(dfWk$yr, dfWk$wk, "4")
# as.Date("2021525", "%Y%W%u")
# as.Date("2022526", "%Y%W%u")
# as.Date("2022537", "%Y%W%u")
dfWk$Date <- as.Date(dfWk$Date, "%Y%W%u")
dfWk$Balance <- dfWk$Balance * 10^-6
colnames(dfWk)[which(colnames(dfWk) == "Balance")] <- "Balance (million COP)"
#-----------------------------------------------------------------------
gg <- ggplot(dfWk, aes(x = Date, y = `Balance (million COP)`))
gg <- gg + geom_line()
gg <- gg + scale_x_date(breaks = scales::breaks_pretty(n = 8), labels = date_format("%b\n%Y"))
gg <- gg + theme_bw()
gg <- gg + theme(axis.title = element_text(size = axisTitleSize),
                 # axis.title.y = element_blank(),
                 axis.text = element_text(size = axisTextSize)#,
                 # strip.background = element_rect(fill = "white"),
                 # strip.text = element_text(size = facetTitleSize),
                 # plot.title = element_text(size = titleSize)
                 )
ggBal <- gg
#---
# %>% gather(var, val, `Balance (million COP)`:`Pct. Change`)
gg <- ggplot(dfWk, aes(x = Date, y = `Pct. Change`))
gg <- gg + geom_hline(yintercept = 0, color = "red")
gg <- gg + geom_line()
gg <- gg + scale_x_date(breaks = scales::breaks_pretty(n = 8), labels = date_format("%b\n%Y"))
gg <- gg + theme_bw()
gg <- gg + theme(axis.title = element_text(size = axisTitleSize),
                 # axis.title.y = element_blank(),
                 axis.text = element_text(size = axisTextSize)#,
                 # strip.background = element_rect(fill = "white"),
                 # strip.text = element_text(size = facetTitleSize),
                 # plot.title = element_text(size = titleSize)
                 )
ggChng <- gg
#---
ggBal + ggChng + plot_layout(ncol = 1, heights = c(1, 1 / 3))
#-----------------------------------------------------------------------
df$Type[grep("Rent|Admon", df$Type)] <- "Rent & Admon"
df$month <- month(df$Date)
df$month[which(df$month < 10)] <- paste0("0", df$month[which(df$month < 10)])
dfCat <- df %>% group_by(yr, month, Type) %>%
  summarise(Change = sum(Change)) %>% as.data.frame()
dfCat$Change <- dfCat$Change * 10^-6
colnames(dfCat)[which(colnames(dfCat) == "Change")] <- "Monthly Flow\n(million COP)"
dfCat$Date <- as.Date(paste(dfCat$yr, dfCat$month, "04", sep = "-"), "%Y-%m-%d")
n <- length(unique(dfCat$Type))
bag_of_colors <- randomcoloR::distinctColorPalette(k = 2 * n)
colorVec <- sample(bag_of_colors, n)
gg <- ggplot(dfCat, aes(x = Date, y = `Monthly Flow\n(million COP)`,
                        fill = Type))
gg <- gg + geom_area()
gg <- gg + geom_hline(yintercept = 0, color = "red")
gg <- gg + scale_x_date(breaks = scales::breaks_pretty(n = 8), labels = date_format("%b\n%Y"))
gg <- gg + scale_fill_manual(values = colorVec)
gg <- gg + theme_bw()
gg <- gg + theme(axis.title = element_text(size = axisTitleSize),
                 # axis.title.y = element_blank(),
                 axis.text = element_text(size = axisTextSize),
                 legend.key.size = unit(0.3, "cm"),
                 legend.position = "top",
                 legend.title = element_blank(),
                 legend.text = element_text(size = legendTextSize)
                 # strip.background = element_rect(fill = "white"),
                 # strip.text = element_text(size = facetTitleSize),
                 # plot.title = element_text(size = titleSize)
                 )
ggFlowArea <- gg
#---
dfCat <- dfCat %>% group_by(yr, month) %>%
  mutate(netFlow = sum(`Monthly Flow\n(million COP)`),
         totGast = sum(`Monthly Flow\n(million COP)`[which(`Monthly Flow\n(million COP)` < 0)]),
         totIngres = sum(`Monthly Flow\n(million COP)`[which(`Monthly Flow\n(million COP)` > 0)])) %>% as.data.frame()
dfCat$`Monthly Expenses\n(% of total)` <- 100 * dfCat$`Monthly Flow\n(million COP)` / dfCat$totGast
df_plot <- subset(dfCat, `Monthly Flow\n(million COP)` < 0)
gg <- ggplot(df_plot, aes(x = Date, y = `Monthly Expenses\n(% of total)`,
                        fill = Type))
gg <- gg + geom_area()
gg <- gg + scale_x_date(breaks = scales::breaks_pretty(n = 8), labels = date_format("%b\n%Y"))
gg <- gg + scale_fill_manual(values = colorVec)
gg <- gg + theme_bw()
gg <- gg + theme(axis.title = element_text(size = axisTitleSize),
                 # axis.title.y = element_blank(),
                 axis.text = element_text(size = axisTextSize),
                 legend.key.size = unit(0.3, "cm"),
                 legend.position = "top",
                 legend.title = element_blank(),
                 legend.text = element_text(size = legendTextSize)
                 # strip.background = element_rect(fill = "white"),
                 # strip.text = element_text(size = facetTitleSize),
                 # plot.title = element_text(size = titleSize)
)
#gg <- gg + guides(fill = guide_legend(nrow = 4, byrow = T))
ggGastPctArea <- gg
#---
df_plot <- dfCat[, c("Date", "month", "yr", "netFlow", "totGast", "totIngres")]
df_plot$yrmon <- paste(df_plot$yr, df_plot$month)
df_plot <- df_plot[-which(duplicated(df_plot$yrmon)), ]
df_plot <- df_plot %>% gather(var, `Million COP`, netFlow:totIngres)
colorVec <- sample(bag_of_colors, n)
gg <- ggplot(df_plot, aes(x = Date, y = `Million COP`, group = var, color = var))
gg <- gg + geom_hline(yintercept = 0, color = "red")
gg <- gg + geom_line(lwd = 1)
gg <- gg + scale_x_date(breaks = scales::breaks_pretty(n = 8), labels = date_format("%b\n%Y"))
gg <- gg + scale_color_manual(values = colorVec[1:3])
gg <- gg + theme_bw()
gg <- gg + theme(axis.title = element_text(size = axisTitleSize),
                 # axis.title.y = element_blank(),
                 axis.text = element_text(size = axisTextSize),
                 legend.position = "top",
                 legend.title = element_blank(),
                 legend.text = element_text(size = legendTextSize)
                 # strip.background = element_rect(fill = "white"),
                 # strip.text = element_text(size = facetTitleSize),
                 # plot.title = element_text(size = titleSize)
)
ggFlowLine <- gg
#---
ggGastPctArea + ggFlowLine + plot_layout(ncol = 2)
#-----------------------------------------------------------------------
