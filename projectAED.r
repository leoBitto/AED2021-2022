CO2 = read.table("/home/leonardo/Documenti/UNIFI/2021-2022/primoSemestre_Aveiro/Analise estatistica de dados/csv/CO2_industries.csv", header=TRUE, dec=",", sep="\t")
millions = read.table("/home/leonardo/Documenti/UNIFI/2021-2022/primoSemestre_Aveiro/Analise estatistica de dados/csv/million_sectors.csv", header=TRUE, dec=",", sep="\t")
names = names(CO2)[2:]
#take the years from the data
years = CO2[,1]
# create the data containing all the CO2 emitted by sector, 
# we take out the first columns on both of the dataset
# to exclude the years
data = CO2[,-1] #* millions[,-1]
# add the years as rownames
rownames(data) = years

#we take out the total column
total = data[,1]
data = data[,-1]

# stacked barplot
# data is a data.frame so it need to be
# converted to a matrix
# then trasposed to have the years in
# the x-axis
data = t(as.matrix(data))

# create color palette:
#install.packages("pals")
library(pals)
coul <- polychrome()

dev.new(width=18, height=8)

# Save current graphical parameters
opar <- par(no.readonly = TRUE)

# Change the margins of the plot 
# (the fourth is the right margin)
par(mar = c(6, 5, 4, 49.5)
    #mfrow=c(1,2)
    )

# Transform this data in %
data_percentage <- apply(data, 2, function(x){x*100/sum(x,na.rm=T)})
# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul , border="white", xlab="years", main="stacked percentages of pollution")

#barplot(data,col=coul,border="white", space=0.04, xlab="years",)
legend( "topright",
        fill=coul,
        legend=rownames(data),
        inset=c(-1.2,0),
        box.lty = 0, # Removes the box line
        cex = 1.5, # Legend size
        xpd = TRUE, # Needed to put the legend outside the plot
        )
# Back to the default graphical parameters
on.exit(par(opar))

### most polluting industries by colMeans
## industries that pollute more than 500 CO2/million GVA
mostPolluting = colMeans(t(data))[colMeans(t(data))>500]


par(mfrow=c(3,2))

#example of graph of agriculture:
agr = t(data)[,1]
#par(mfrow=c(1,2))
#plot(years, agr,  ylab="CO2 for million of GVA")
#plot(years, agr,  ylab="CO2 for million of GVA", main="agriculture fishing forestry")#ylim=c(0, 10645))
#lines(years, agr)
boxplot(agr, main="agriculture fishing forestry")
#title("agriculture, forestry and fishing", line = -2, outer = TRUE)

#example of graph of extractive:
extr = t(data)[,2]
#par(mfrow=c(1,2))
#plot(years, extr, ylab="CO2 for million of GVA", main="extractive")#ylim=c(0, 10645))
#lines(years, extr)
boxplot(extr, main="extractive")
#title("extractive", line = -2, outer = TRUE)

#example of graph of manufacturing:
manu = t(data)[,3]
#par(mfrow=c(1,2))
#plot(years, manu, ylab="CO2 for million of GVA", main="manufacturing")# ylim=c(0, 10645))
#lines(years, manu)
boxplot(manu, main="manufacturing")
#title("manufacturing", line = -2, outer = TRUE)

#example of graph of electricity:
elec = t(data)[,4]
#par(mfrow=c(1,2))
#plot(years, elec, ylab="CO2 for million of GVA", main="electricity")# ylim=c(0, 10645))
#lines(years, elec)
boxplot(elec, main="electricity")
#title("electricity", line = -2, outer = TRUE)

#example of graph of water manag:
wat = t(data)[,5]
#par(mfrow=c(1,2))
#plot(years, wat, ylab="CO2 for million of GVA", main="water")# ylim=c(0, 10645))
#lines(years, wat)
boxplot(wat, main="water management")
#title("water management", line = -2, outer = TRUE)

#example of graph of trasport:
tra = t(data)[,8]
#par(mfrow=c(1,2))
#plot(years, tra, ylab="CO2 for million of GVA", main="trasport")# ylim=c(0, 10645))
#lines(years, tra)
boxplot(tra, main="transport")
#title("transport", line = -2, outer = TRUE)

#correlation among the most polluting industries
int_ind = c(1,2,3,4,5,8)
interested_industries = t(data)[,int_ind]
corr = round(cor(interested_industries), 2)
corr[lower.tri(corr)] = "-"

#resuming table of the interested industries
s = apply(interested_industries, 2, summary)
asym = (s[5,]-s[3,])-(s[3,]-s[2,])