

bam.AR1=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude, by= monthly.miss.scaled ) + s(week) + factor(year) + s(collection, bs = "re"), data= df.menhaden, method = "REML", family = Gamma(link = "log") , correlation = corARMA(form = ~1| Julian, p = 1))

bam.AR2=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude, by= monthly.miss.scaled ) + s(week) + factor(year) + s(collection, bs = "re"), data= df.menhaden, method = "REML", family = Gamma(link = "log") , correlation = corARMA(form = ~1| Julian, p = 2))
acf(bam.AR2$lme)
acf(river.bam.vc.lag.0$lme)
