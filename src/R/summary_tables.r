##Summary plots

source(file="~/GitHub/HalibutSAA/begin.r")

#Frequency histogram of number of observations (ages 7-18) each year by sex for
#each regulatory area
summary.reg <- subset(summary, reg!="CLS")
summary.reg <- subset(summary.reg, reg!="4EE")
hist <- ggplot(summary.reg, aes(factor(year), col=sex))
hist <- hist + geom_histogram()
hist <- hist + facet_wrap(~reg, nrow=4)
hist <- hist + theme_bw() + scale_alpha() + labs(x="Year") + 
  theme(axis.text.x = element_text(size = rel(0.8)))+ theme(axis.title = element_text(size = rel(1.8)))
ggsave(hist, file="hist_reg.png",width=20, height=10)

#Age 7
age7 <- subset(summary.reg, s.age==7)
pp <- ggplot(age7,aes(factor(year), mean, col=sex))
pp <- pp + geom_point()
pp <- pp + facet_wrap(~reg, nrow=4)
pp <- pp + labs(x="Year", y="Average fork length (cm) (age-7)")
limits <- aes(ymax = mean + se, ymin= mean - se)
pp <- pp + geom_errorbar(limits, width=0.4)
pp <- pp + scale_colour_grey() + theme_bw()
pp <- pp + theme(axis.text.x = element_text(size = rel(0.8)))
pp <- pp + theme(axis.title = element_text(size = rel(1.8)))
ggsave(pp, file="age7.png",width=20, height=10)


#Age 10
age10 <- subset(summary.reg, s.age==10)
pp <- ggplot(age10,aes(factor(year), mean, col=sex))
pp <- pp + geom_point()
pp <- pp + facet_wrap(~reg, nrow=4)
pp <- pp + labs(x="Year", y="Average fork length (cm) (age-10)")
limits <- aes(ymax = mean + se, ymin= mean - se)
pp <- pp + geom_errorbar(limits, width=0.4)
pp <- pp + scale_colour_grey() + theme_bw()
pp <- pp + theme(axis.text.x = element_text(size = rel(0.8)))
pp <- pp + theme(axis.title = element_text(size = rel(1.8)))
ggsave(pp, file="age10.png",width=20, height=10)


#Age 12
age12 <- subset(summary.reg, s.age==12)
pp <- ggplot(age12,aes(factor(year), mean, col=sex))
pp <- pp + geom_point()
pp <- pp + facet_wrap(~reg, nrow=4)
pp <- pp + labs(x="Year", y="Average fork length (cm) (age-12)")
limits <- aes(ymax = mean + se, ymin= mean - se)
pp <- pp + geom_errorbar(limits, width=0.4)
pp <- pp + scale_colour_grey() + theme_bw()
pp <- pp + theme(axis.text.x = element_text(size = rel(0.8)))
pp <- pp + theme(axis.title = element_text(size = rel(1.8)))
ggsave(pp, file="age12.png",width=20, height=10)


#Age 15
age15 <- subset(summary.reg, s.age==15)
pp <- ggplot(age15,aes(factor(year), mean, col=sex))
pp <- pp + geom_point()
pp <- pp + facet_wrap(~reg, nrow=4)
pp <- pp + labs(x="Year", y="Average fork length (cm) (age-15)")
limits <- aes(ymax = mean + se, ymin= mean - se)
pp <- pp + geom_errorbar(limits, width=0.4)
pp <- pp + scale_colour_grey() + theme_bw()
pp <- pp + theme(axis.text.x = element_text(size = rel(0.8)))
pp <- pp + theme(axis.title = element_text(size = rel(1.8)))
ggsave(pp, file="age15.png",width=20, height=10)

#Age 18
age18 <- subset(summary.reg, s.age==18)
pp <- ggplot(age18,aes(factor(year), mean, col=sex))
pp <- pp + geom_point()
pp <- pp + facet_wrap(~reg, nrow=4)
pp <- pp + labs(x="Year", y="Average fork length (cm) (age-18)")
limits <- aes(ymax = mean + se, ymin= mean - se)
pp <- pp + geom_errorbar(limits, width=0.4)
pp <- pp + scale_colour_grey() + theme_bw()
pp <- pp + theme(axis.text.x = element_text(size = rel(0.8)))
pp <- pp + theme(axis.title = element_text(size = rel(1.8)))
ggsave(pp, file="age18.png",width=20, height=10)


#Exploratory plots
survey2$saa <- survey$length/survey$s.age

xyplot(survey$length~survey$s.age | survey$reg)
xyplot(survey$s.age~survey$length | survey$reg)
hist(survey2$saa)