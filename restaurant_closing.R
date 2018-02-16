library (RCurl)
library(plyr)
library(geepack)
library(ggplot2)

download <- getURL("https://data.cityofnewyork.us/api/views/xx67-kt59/rows.csv?accessType=DOWNLOAD")
rest.dat <- read.csv (text = download)

rest.dat <- rest.dat[order(rest.dat$CAMIS, as.Date(rest.dat$INSPECTION.DATE,"%m/%d/%Y")),]
rest.dat$CLOSED <- grepl("Establishment Closed by DOHMH", rest.dat$ACTION)

inspections <- ddply(rest.dat[!is.na(rest.dat$SCORE)&rest.dat$SCORE>0&as.Date(rest.dat$INSPECTION.DATE,"%m/%d/%Y")>=as.Date("2014-01-01")&as.Date(rest.dat$INSPECTION.DATE,"%m/%d/%Y")<as.Date("2018-01-01")&rest.dat$BORO!="Missing",], c("CAMIS","DBA","BORO","CUISINE.DESCRIPTION","INSPECTION.DATE","SCORE","GRADE"), summarise, CRITICAL=sum(CRITICAL.FLAG=="Critical"), NOT.CRITICAL=sum(CRITICAL.FLAG=="Not Critical"), CLOSED=max(CLOSED==TRUE))
inspections$BORO <- factor(inspections$BORO)
inspections$MANHATTAN <- inspections$BORO=="MANHATTAN"
inspections <- inspections[order(inspections$CAMIS, as.Date(inspections$INSPECTION.DATE, "%m/%d/%Y")),]

boro.gee <- geeglm(CLOSED ~ SCORE + BORO, id = CAMIS, data = inspections, family = binomial(link="logit"), corstr = "independence")

inspections$SCORE_BUCKET <- cut(inspections$SCORE, (0:15)*10, ordered_result = TRUE, labels = FALSE)
inspections.bucketed <- ddply(inspections[inspections$SCORE<50,], c("MANHATTAN","SCORE_BUCKET"), summarise, CLOSED=sum(CLOSED), NUM_INSPECTIONS=length(CAMIS), CLOSE_RATE=sum(CLOSED)/length(CAMIS))
inspections.bucketed$SCORE_UPPER <- inspections.bucketed$SCORE_BUCKET*10
inspections.bucketed$Neighborhood <- ifelse(inspections.bucketed$MANHATTAN,"Manhattan","Boroughs")
g <- ggplot(inspections.bucketed, aes(SCORE_UPPER, CLOSE_RATE, colour = Neighborhood)) + geom_line(size = 1) + labs(x="Score bucket", y="Percent of inspections resulting in closing") + scale_y_continuous(labels = scales::percent) + theme(legend.title=element_blank())
ggsave(filename = "close_rates.png", plot = g, height = 5, width = 5)
