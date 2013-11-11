#### Data munging in Terminal
# Remove ø = "sed -i 's/\xf8/o/g' koege-data.csv"
# Remove å = "sed -i 's/\xe5/aa/g' koege-data.csv"
# Remove æ = "sed -i 's/\xe6/ae/g' koege-data.csv"

setwd("/home/thomas/Dropbox/komhen/data")
data = read.csv("koege-data.csv", sep=";", header=T, encoding="latin1")

names(data) <- tolower(names(data))

data$x <- NULL
head(data)

##### Create department categories #####

data$afdeling[data$enhed == "259bestiller"] <- "Bestiller"
data$afdeling[data$enhed == "259hjælpemidler"] <- "Hjælpemidler"
data$afdeling[data$enhed == "259plads"] <- "Pladsanvisningen"
data$afdeling[data$enhed == "259kultur"] <- "Kulturafdelingen"
data$afdeling[data$enhed == "259økonomi"] <- "Økonomisk afdeling"
data$afdeling[data$enhed == "259natur"] <- "Natur og Miljø"
data$afdeling[data$enhed == "259bygogplan"] <- "Byg og Plan"
data$afdeling[data$enhed == "259skole"] <- "Skoleafdelingen"
data$afdeling[data$enhed == "259familie"] <- "Familierådgivningen"


data$afdeling[data$enhed %in% c("259veje","259byrum")] <- "Veje og Byrum"
data$afdeling[data$enhed %in% c("259besk","259sek","259sdp")] <- "Jobcenter"
data$afdeling[data$enhed %in% c("259information","259skranke","259telefon",
                                    "259borgerservice")] <- "Borgerservice"
data$afdeling[data$enhed %in% c("259fleks","259føpey","259kontant",
                                    "259syge")] <- "Ydelsesservice"
##### Create channel categories #####

allo_ch <- function(df) {

  df$kanalnavn[df$kanal %in% c("PPRUD","PPRIND")] <- "Papirbrev"
  df$kanalnavn[df$kanal %in% c("TLFUD","TLFIND")] <- "Telefon"
  df$kanalnavn[df$kanal %in% c("EMLUD","EMLIND")] <- "Email"
  df$kanalnavn[df$kanal %in% c("DOKUD","DOCIND")] <- "Digital Post"
  df$kanalnavn[df$kanal %in% c("SMSUD","SMSIND")] <- "SMS"
  df$kanalnavn[df$kanal %in% c("SYSECOUD","SYSECOIND")] <- "System Økonomi"
  df$kanalnavn[df$kanal %in% c("SYSMEDIND")] <- "System Medcom"
  df$kanalnavn[df$kanal %in% c("FPRNT")] <- "Fjernprint"
  df$kanalnavn[df$kanal %in% c("SYSIND")] <- "System Generel"
  df$kanalnavn[df$kanal %in% c("MGRUD")] <- "Output Manager"
  df$kanalnavn[df$kanal %in% c("SLVBTJN")] <- "Selvbetjening"
  df$kanalnavn[df$kanal %in% c("PRSNLG")] <- "Personlig"
  df$kanalnavn[df$kanal %in% c("WEB")] <- "Hjemmeside"

  return(df)
}

data = allo_ch(data)
##### Make all tasks xx.xx.xx #####
data$opgave = factor(ifelse(grepl("^\\d{2}\\.\\d{2}$",data$opgave, perl=T), 
                    paste(data$opgave,"00",sep="."), paste(data$opgave)))
##### Insert enquirytypes
data$hentype[data$type == "FEJL"] <- "Fejlhenvendelse"
data$hentype[data$type == "INFO"] <- "Information om eksisterende sag"
data$hentype[data$type == "OPRET"] <- "Oprettelse af ny sag"
data$hentype[data$type == "VEJL"] <- "Råd/Vejledning"
data$hentype[data$type == "STATUS"] <- "Ændring status, eksisterende sag"
data$hentype[data$type == "SLVBTJN"] <- "Hjælp til selvbetjening"
##### Merge KLE with assigment names
opgData <- read.csv("opgaver.csv", sep=";", header=T, encoding="latin1")

data = merge(opgData, data, by="opgave")
##### Make direction #####
ud <- c("DOKUD", "EMLUD", "MGRUD", "PPRUD", "SMSUD", "SYSECOUD","TLFUD","FPRNT")

data$retning <- factor(ifelse(data$kanal %in% ud, "ud", "ind"))
##### gplots #####
require(gplots) || install.packages(gplots)
library(gplots)

##### Clean the data #####

## Specify factor variables

var = c("kanal", "enhed", "afslutning", "er.borgeren.over.eller.under.65.år.",
        "opgave", "type", "blanket","datakilde", "afdeling", "opgavenavn", "hentype",
        "kanalnavn", "retning")

data[,var] = sapply(data[,var], as.factor)

## Convert dates, to real dates

dateVar = c("dato", "starttidspunkt", "sluttidspunkt")

data[,dateVar] <- sapply(data[,dateVar], as.POSIXlt, format="%d-%m-%Y %H:%M:%S")
data$dato <- as.Date(data$dato)

### Remove non-counting dates

## Dates that have no relevance
data <- subset(data, !(dato %in% as.Date(c("2013-10-26","2013-10-27","2013-10-28"))))

## The 29 of oct, that does not include 259skole
data <- subset(data, dato != "2013-10-29" | enhed == "259skole")

## Empty enhed on dates after 29. oct
data <- subset(data, !(dato > as.Date("2013-10-29") & enhed == ""))


### Convert "" to NULL

data$enhed[data$enhed == ""] <- NA

##### Draw graph of enquiries per department #####

## Table of department roles

tabAfd = table(data$afdeling) # Ændr denne for at få data på andre variable
tabAfdOrder = tabAfd[order(tabAfd,decreasing=T)]

## Barchart of Departments
library(gridBase)

pdf(file="afdelinger.pdf",width=6.5,height=5)


## Plot, but suppress the labels
par(mar=c(7,4,2,1))

x <- barplot(tabAfdOrder, col=rainbow(20), names.arg="", las=2, ylim = c(0, 4000))

## Use grid to add the labels    
vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(names(tabAfdOrder),
          x = unit(x, "native"), y=unit(-0.5, "lines"),
          just="right", rot=50)

popViewport(3)

text(x, tabAfdOrder, labels = tabAfdOrder, pos = 3, cex = 0.7)

dev.off()

######## Department graphs #######


##### START #####

### Roller
subfolder = "Roller/"
departs = as.vector(na.omit(unique(data$enhed[data$enhed != ""])))
org = data$enhed

### Afdelinger
 subfolder = "Afdelinger/"
 departs = as.vector(na.omit(unique(data$afdeling)))
 org = data$afdeling

 for (str in departs) {

##### Channel graph #####

org = org         ### Ændrer denne for at få data på enhedsniveau
str = str


tabCh = table(data$kanal[org == str]) 

### Add KanalType for indgående og udgående

ud <- c("DOKUD", "EMLUD", "MGRUD", "PPRUD", "SMSUD", "SYSECOUD","TLFUD","FPRNT")

dotType = as.data.frame.table(tabCh[order(tabCh,decreasing=F)])
names(dotType) = c("kanal","antal")

dotType$type <- factor(ifelse(dotType$kanal %in% ud, "Udgående", "Indgående"))
dotType$color <- ifelse(dotType$kanal %in% ud, "blue", "green")

# Create labels
dotType <- allo_ch(dotType)


## New dotChar for KanalType

pdf(file=paste("ch/",subfolder,"kanal-",str,".pdf", sep=""),width=6.5,height=4.5)

par(mar = c(3,3,0,2), oma = c(1,1,4,1))

dotchart(dotType$antal, labels=paste(dotType$kanalnavn,": ",dotType$antal),
         cex=0.7, groups=dotType$type, color=dotType$color, pch=19)

title(paste(str, ": Antal henvendelser fordelt på kanal",sep=""), outer=T)

dev.off()
##### Assignment graph #####
org = org         
str = str

pdf(file=paste("as/",subfolder,"assign-",str,".pdf", sep=""),width=6.5,height=4.5)

## Graph of assignments

par(mar = c(3, 13.5, 0, 1), oma = c(1,1,4,1))

dfOpg = as.data.frame.table(table(data$opgavenavn[org == str]))
names(dfOpg) = c("opgave", "antal")
dfOpgOrdered = dfOpg[order(dfOpg$antal, decreasing=F),]

x <- barplot(dfOpgOrdered$antal, names.arg="", las = 1, horiz=T, axes=T, 
             cex.names=0.7, mar = c(3, 13.5, 3, 1), 
             col=rev(rich.colors(nrow(dfOpgOrdered),"blues")))

title(paste(str, ": Antal af henvendelser fordelt på opgave",sep=""), outer=T)

axis(2,at=x,labels=paste(dfOpgOrdered$opgave, ": ", dfOpgOrdered$antal), las = 2, cex.axis=0.5)

dev.off()
##### Enquirytype graph w/o web #####
org = org         ### Ændrer denne for at få data på enhedsniveau
str = str

pdf(file=paste("he/",subfolder,"hentype-",str,".pdf", sep=""),width=6.5,height=4.5)

## Graph of assignments

par(mar = c(3, 13.5, 0, 1), oma = c(1,1,4,1))

dfOpg = as.data.frame.table(table(data$hentype[org == str & data$kanal != "WEB"]))
names(dfOpg) = c("hentype", "antal")
dfOpgOrdered = dfOpg[order(dfOpg$antal, decreasing=F),]

x <- barplot(dfOpgOrdered$antal, names.arg="", las = 1, horiz=T, axes=T, 
             cex.names=0.7, mar = c(3, 13.5, 3, 1), 
             col=rev(rich.colors(nrow(dfOpgOrdered),"blues")))

title(paste(str, ": Antal af henvendelser fordelt på henvendelsestype",sep=""), outer=T)

axis(2,at=x,labels=paste(dfOpgOrdered$hentype, ": ", dfOpgOrdered$antal), las = 2, cex.axis=0.5)

dev.off()

##### END #####

}

####### Department subgraphs #######

##### By Channel

##### START #####
subfolder = subfolder
departs = departs
org = org  ### Ændrer denne for at få data på enhedsniveau

getMaxChannel <- function(df) {
  m <- table(df$kanalnavn, df$retning)
  maxIndex <- seq(along=m)[m == max(m)]
  retning <- ifelse(maxIndex > length(rownames(m)), colnames(m)[2], colnames(m)[1])
  kanalIndex <- ifelse(maxIndex > length(rownames(m)), maxIndex-length(rownames(m)), maxIndex)
  kanal <- rownames(m)[kanalIndex]
  
  return(c(retning, kanal))
}
getMaxAssignment <-function(df) {
  
  m <- table(df$opgavenavn)
  maxIndex <- seq(along=m)[m == max(m)]
  assign <- rownames(m)[maxIndex]
  
  return(assign)
}
getMaxEnquiry <-function(df) {
  m <- table(df$hentype)
  maxIndex <- seq(along=m)[m == max(m)]
  enquiry <- rownames(m)[maxIndex]
  
  return(enquiry)
}

for (str in departs) {
##### By Channel: Assignment graph #####
  
org = org         ### Ændrer denne for at få data på enhedsniveau
str = str
  
max = getMaxChannel(subset(data, org == str))
tab = table(data$opgavenavn[data$retning == max[1] & data$kanalnavn == max[2] & org == str])

pdf(file=paste("asCh/",subfolder,"opgave-max-kanal-",str,".pdf", sep=""),width=6.5,height=4.5)

## Graph of assignments

par(mar = c(3, 13.5, 0, 1), oma = c(1,1,4,1))

dfOpg = as.data.frame.table(tab)
names(dfOpg) = c("opgave", "antal")
dfOpgOrdered = dfOpg[order(dfOpg$antal, decreasing=F),]

x <- barplot(dfOpgOrdered$antal, names.arg="", las = 1, horiz=T, axes=T, 
           cex.names=0.7, mar = c(3, 13.5, 3, 1), 
           col=rev(rich.colors(nrow(dfOpgOrdered),"blues")))

title(main = paste(str, ": Henvendelser fordelt på opgave",sep=" "),
    outer=T)

mtext(paste("Kanal:",max[2], max[1], sep=" "), outer=T)

axis(2,at=x,labels=paste(dfOpgOrdered$opgave, ": ", dfOpgOrdered$antal), las = 2, cex.axis=0.5)

dev.off()
##### By Channel: Enquirytype graph w/o web #####
org = org         ### Ændrer denne for at få data på enhedsniveau
str = str

max = getMaxChannel(subset(data, org == str))
tab = table(data$hentype[data$retning == max[1] & data$kanalnavn == max[2] & org == str])

pdf(file=paste("henCh/",subfolder,"hentype-max-kanal-",str,".pdf", sep=""),width=6.5,height=4.5)

## Graph of assignments

par(mar = c(3, 13.5, 0, 1), oma = c(1,1,4,1))

dfOpg = as.data.frame.table(tab)
names(dfOpg) = c("hentype", "antal")
dfOpgOrdered = dfOpg[order(dfOpg$antal, decreasing=F),]

x <- barplot(dfOpgOrdered$antal, names.arg="", las = 1, horiz=T, axes=T, 
           cex.names=0.7, mar = c(3, 13.5, 3, 1), 
           col=rev(rich.colors(nrow(dfOpgOrdered),"blues")))

title(paste(str, ": Henvendelser fordelt på henvendelsestype",sep=""), outer=T)

mtext(paste("Kanal:",max[2], max[1], sep=" "), outer=T)

axis(2,at=x,labels=paste(dfOpgOrdered$hentype, ": ", dfOpgOrdered$antal), las = 2, cex.axis=0.5)

dev.off()
##### By Assignment: Channel Graph ######

org = org         ### Ændrer denne for at få data på enhedsniveau
str = str

max <- getMaxAssignment(subset(data, org == str))
tab = table(data$kanal[data$opgavenavn == max & org == str]) 

### Add KanalType for indgående og udgående

ud <- c("DOKUD", "EMLUD", "MGRUD", "PPRUD", "SMSUD", "SYSECOUD","TLFUD","FPRNT")

dotType <- if (length(tab) == 1) as.data.frame.table(tab) else as.data.frame.table(tab[order(tab,decreasing=F)])

names(dotType) = c("kanal","antal")

dotType$type <- factor(ifelse(dotType$kanal %in% ud, "Udgående", "Indgående"))
dotType$color <- ifelse(dotType$kanal %in% ud, "blue", "green")

# Create labels
dotType <- allo_ch(dotType)


## New dotChar for KanalType

pdf(file=paste("chAs/",subfolder,"kanal-max-opgave-",str,".pdf", sep=""),width=6.5,height=4.5)

par(mar = c(3,3,1,2), oma = c(1,1,4,1))

dotchart(dotType$antal, labels=paste(dotType$kanalnavn,": ",dotType$antal),
         cex=0.7, groups=dotType$type, color=dotType$color, pch=19)

title(paste(str, ": Henvendelser fordelt på kanal",sep=""), outer=T)

mtext(paste("Opgave:",max, sep=" "), outer=T)

dev.off()
##### By Assignment: Equiry Graph ######

org = org         ### Ændrer denne for at få data på enhedsniveau
str = str

max = getMaxAssignment(subset(data, org == str))
tab = table(data$hentype[data$opgavenavn == max & org == str])

pdf(file=paste("henAs/",subfolder,"hentype-max-opgave-",str,".pdf", sep=""),width=6.5,height=4.5)

## Graph of assignments

par(mar = c(3, 13.5, 0, 1), oma = c(1,1,4,1))

dfOpg = as.data.frame.table(tab)
names(dfOpg) = c("hentype", "antal")
dfOpgOrdered = dfOpg[order(dfOpg$antal, decreasing=F),]

x <- barplot(dfOpgOrdered$antal, names.arg="", las = 1, horiz=T, axes=T, 
             cex.names=0.7, mar = c(3, 13.5, 3, 1), 
             col=rev(rich.colors(nrow(dfOpgOrdered),"blues")))

title(paste(str, ": Henvendelser fordelt på henvendelsestype",sep=""), outer=T)

mtext(paste("Opgave", max, sep=" "), outer=T)

axis(2,at=x,labels=paste(dfOpgOrdered$hentype, ": ", dfOpgOrdered$antal), las = 2, cex.axis=0.5)

dev.off()
##### By Enquirytype: Assignment Graph #####

org = org         ### Ændrer denne for at få data på enhedsniveau
str = str

max = getMaxEnquiry(subset(data, org == str))
tab = table(data$opgavenavn[data$hentype == max & org == str])

pdf(file=paste("asHen/",subfolder,"opgave-max-hentype-",str,".pdf", sep=""),width=6.5,height=4.5)

## Graph of assignments

par(mar = c(3, 13.5, 0, 1), oma = c(1,1,4,1))

dfOpg = as.data.frame.table(tab)
names(dfOpg) = c("opgave", "antal")
dfOpgOrdered = dfOpg[order(dfOpg$antal, decreasing=F),]

x <- barplot(dfOpgOrdered$antal, names.arg="", las = 1, horiz=T, axes=T, 
             cex.names=0.7, mar = c(3, 13.5, 3, 1), 
             col=rev(rich.colors(nrow(dfOpgOrdered),"blues")))

title(main = paste(str, ": Henvendelser fordelt på opgave",sep=" "),
      outer=T)

mtext(paste("Kanal:",max, sep=" "), outer=T)

axis(2,at=x,labels=paste(dfOpgOrdered$opgave, ": ", dfOpgOrdered$antal), las = 2, cex.axis=0.5)

dev.off()
##### By Enquirytype: Channel Graph #####
org = org         ### Ændrer denne for at få data på enhedsniveau
str = str

max <- getMaxEnquiry(subset(data, org == str))
tab = table(data$kanal[data$hentype == max & org == str]) 

### Add KanalType for indgående og udgående

ud <- c("DOKUD", "EMLUD", "MGRUD", "PPRUD", "SMSUD", "SYSECOUD","TLFUD","FPRNT")

dotType <- if (length(tab) == 1) as.data.frame.table(tab) else as.data.frame.table(tab[order(tab,decreasing=F)])

names(dotType) = c("kanal","antal")

dotType$type <- factor(ifelse(dotType$kanal %in% ud, "Udgående", "Indgående"))
dotType$color <- ifelse(dotType$kanal %in% ud, "blue", "green")

# Create labels
dotType <- allo_ch(dotType)


## New dotChar for KanalType

pdf(file=paste("chHen/",subfolder,"kanal-max-hentype-",str,".pdf", sep=""),width=6.5,height=4.5)

par(mar = c(3,3,1,2), oma = c(1,1,4,1))

dotchart(dotType$antal, labels=paste(dotType$kanalnavn,": ",dotType$antal),
         cex=0.7, groups=dotType$type, color=dotType$color, pch=19)

title(paste(str, ": Henvendelser fordelt på kanal",sep=""), outer=T)

mtext(paste("Henvendelsestype:",max, sep=" "), outer=T)

dev.off()

}
##### Tidsstatistik #####

timeData = subset(data, enhed %in% c("259skranke","259telefon"), 
                  select = c(enhed,afdeling,opgavenavn,kanal,hentype,starttidspunkt,sluttidspunkt))

timeData$diff = as.vector(difftime(timeData$sluttidspunkt, timeData$starttidspunkt, 
                          units="mins")


### Summaries

pdf("tid.pdf")

for (ch in c("Alle", "Skranke", "Telefon")) {

gruppeStr = ch

timeData = subset(data, enhed %in% c("259skranke","259telefon"), 
                  select = c(enhed,afdeling,opgavenavn,kanal,hentype,starttidspunkt,sluttidspunkt))

timeData$diff = as.vector(difftime(as.POSIXlt(timeData$sluttidspunkt, format="%d-%m-%Y %H:%M:%S"), 
                                   as.POSIXlt(timeData$starttidspunkt, format="%d-%m-%Y %H:%M:%S"), 
                                   units="mins"))

if (gruppeStr == "Skranke") {
  timeData = timeData[timeData$enhed == "259skranke",]
}
if (gruppeStr == "Telefon") {
  timeData = timeData[timeData$enhed == "259telefon",]
}

## Barplot: Mean time per enquirytype
gnsHen <- aggregate(list(gns=timeData$diff), list(hentype=timeData$hentype), mean)
gnsHenOrd = gnsHen[order(gnsHen$gns, decreasing=F),]

par(mar=c(4,8,1,1), oma = c(1,1,2,1))

bar <- barplot(gnsHenOrd$gns, las=1, names=gnsHenOrd$hentype, xlab="Tid i minutter (gns)", 
        col=rev(rich.colors(length(gnsHenOrd$gns),"blues")), horiz=T, cex.names=0.6)

mtext(paste("Antal tidsregistrerede henvendelser:", length(timeData$diff), sep=" "))

title(main=paste("Gennemsnitlig tidsregistrering fordelt på henvendelsestype", 
                 gruppeStr, sep=" - "), outer=T)

### Boxplot: Time per enquirytype 

par(mar=c(5,12,1,2), oma = c(1,1,2,1))

sort = sort(with(timeData, tapply(diff, hentype, median)))
count = as.vector(by(timeData, factor(timeData$hentype, levels=names(sort)), nrow))

boxplot(diff ~ factor(hentype, levels=names(sort)), timeData, col=rev(rich.colors(length(sort), "blues")), range=1.5, outline=F,
        xlab="Tid i minutter", las=1, horizontal=T, cex.axis = 0.8,
        names=paste(names(sort), count, sep=": "))

mtext(paste("Antal tidsregistrerede henvendelser:", length(timeData$diff), sep=" "))

title(main=paste("Tidsregistrering fordelt på henvendelsestype", gruppeStr, sep=" - "), outer=T)

## Barplot: Mean time per assigntype

gns <- aggregate(list(gns=timeData$diff), list(opgavenavn=timeData$opgavenavn), mean)
gnsOrd = gns[order(gns$gns, decreasing=F),]

par(mar=c(5,12,2,1), oma = c(1,1,2,1))

bar <- barplot(gnsOrd$gns, las=1, names.arg=gnsOrd$opgavenavn, xlab="Tid i minutter (gns)", 
               col=rev(rich.colors(length(gnsOrd$gns),"blues")), horiz=T, cex.names=0.6, xpd=T)

mtext(paste("Antal tidsregistrerede henvendelser:", length(timeData$diff), sep=" "))

title(main=paste("Gennemsnitlig tidsregistrering fordelt på opgaveniveau", 
                 gruppeStr, sep=" - "), outer=T)


### Boxplot: Time per assigntype 

par(mar=c(5,12,2,2), oma = c(1,1,2,1))

sort = sort(with(timeData, tapply(diff, opgavenavn, median)))
count = as.vector(by(timeData, factor(timeData$opgavenavn, levels=names(sort)), nrow))

box <- boxplot(diff ~ factor(opgavenavn, levels=names(sort)), timeData, col=rev(rich.colors(length(sort), "blues")), 
               range=1.5, outline=F, xlab="Tid i minutter", las=1, horizontal=T, cex.axis = 0.6,
               names=paste(names(sort), count, sep=": "))

mtext(paste("Antal tidsregistrerede henvendelser:", length(timeData$diff), sep=" "))

title(main=paste("Tidsregistrering fordelt på opgaveniveau", gruppeStr, sep=" - "), outer=T)

}

dev.off()

##### Summaries for hjemmesidedata ######

## Plads + Skole
result <- subset(data, 
               (enhed == "259plads" | enhed == "259skole") &
                type == "VEJL" & retning == "ind"
                 & (opgave == "17.13.10" | opgave == "17.20.01"), select=enhed)

table(factor(result$enhed))

## Flytning + Folkeregister - Borgerservice hjemmesidedata

result <- subset(data, 
                  enhed %in% c("259borgerservice","259telefon","259skranke") &
                  type == "VEJL" & retning == "ind"
                 & (opgave == "23.05.10" | opgave == "23.05.00"), select=opgave)

table(factor(result$opgave))


### Folkepension + Varmetillæg + Personlit tillæg og helbredstillæg - Borgerservice hjemmeside

result <- subset(data, 
                  enhed %in% c("259borgerservice","259telefon","259skranke") &
                  type == "VEJL" & retning == "ind" & 
                  (opgave == "32.03.04" | opgave == "32.03.10" | opgave == "32.03.12"),
                  select=opgave)

table(factor(result$opgave))

### Børnetilskud + Børne- og ungeydelse - Borgerservice hjemmeside

result <- subset(data, 
                 enhed %in% c("259borgerservice","259telefon","259skranke") &
                   type == "VEJL" & retning == "ind" & 
                   (opgave == "32.12.00" | opgave == "32.09.00"),
                 select=opgave)

table(factor(result$opgave))

### Merudgiftsydelse + Tabt arbejdsfortjeneste

result <- subset(data, 
                 enhed == "259familie" &
                   type == "VEJL" & retning == "ind" & 
                   (opgave == "32.18.04" | opgave == "32.18.12"),
                 select=opgave)

table(factor(result$opgave))
                          
                          
##### Save the datafile
                          
write.table(data, file = "processed-koege-data.csv", sep = ";", quote = F, row.names=F, fileEncoding = "ISO-8859-1")