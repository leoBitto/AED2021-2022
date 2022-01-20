CO2 = read.table("/home/leonardo/Documenti/UNIFI/2021-2022/primoSemestre_Aveiro/Analise estatistica de dados/csv/CO2_industries.csv", header=TRUE, dec=",", sep="\t")
millions = read.table("/home/leonardo/Documenti/UNIFI/2021-2022/primoSemestre_Aveiro/Analise estatistica de dados/csv/million_sectors.csv", header=TRUE, dec=",", sep="\t")
names = names(CO2)[2:]
#take the years from the data
years = CO2[,1]
# create the data containing all the CO2 emitted by sector, 
# we take out the first columns on both of the dataset
# to exclude the years
data = CO2[,-1] * millions[,-1]
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
par(mar = c(6, 5, 4, 40.5)
    #mfrow=c(1,2)
    )

# Transform this data in %
data_percentage <- apply(data, 2, function(x){x*100/sum(x,na.rm=T)})
# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul , border="white", xlab="years")

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
