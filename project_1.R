##number in this csv is string, so avoid converting sting to factor,
data_crime <-read.csv("1960-2015crime.csv",stringsAsFactors = F)
## convert scientific expression to regular and then turn to numeric from character
data_crime[,2:12]<-lapply(data_crime[,2:12],function(x) as.numeric(gsub(",", "", x)))
#show the crime trends last 55 yrs
g<-ggplot(data_crime,aes(x=Year.,y=Violent.))
g+geom_line()
##find the story between the crime and party
party1<-rep(c("Democratic","Republican"),c(8,8))
party2<-rep(c("Democratic","Republican"),c(4,12))
party3<-rep(c("Democratic","Republican","Democratic"),c(8,8,7))
party<-c("Republican",party1,party2,party3)
Crime_party<-mutate(data_crime,party=party)

## Safest Time since 1992
ggplot(Crime_party,aes(x=Year.,y=Total))+geom_bar(aes(fill=party),stat = "identity")+theme_bw()+labs(title="Safest Time since 1992",x="Year",y="Total Crime")+scale_fill_manual(values = c("blue","red"))+coord_cartesian(ylim = c(3000000,16000000))

#show who is fightinh against violent
ggplot(Crime_party,aes(x=Year.,y=Violent.))+
  geom_bar(aes(fill=party),stat = "identity")+
  theme_bw()+
  labs(title="Who fight against Violent?!",x="Year",y="Violent Crime")+
  scale_fill_manual(values = c("blue","red"))

ggplot(Crime_party,aes(x=Year.,y=Murder.))+geom_bar(aes(color=party),stat = "identity")
ggplot(Crime_party,aes(x=Year.,y=Forcible.Rape.))+geom_bar(aes(fill=party),stat = "identity")+coord_cartesian(xlim = c(2007,2015),ylim = c(70000,100000))

ggplot(Crime_party,aes(x=Year.,y=Murder.))+geom_bar(aes(fill=party),stat = "identity")+coord_cartesian(xlim = c(2007,2015),ylim = c(10000,20000))
ggplot(Crime_party,aes(x=Year.,y=(Murder.+Forcible.Rape.)))+geom_bar(aes(fill=party),stat = "identity")+coord_cartesian(ylim=c(30000,150000))
ggplot(Crime_party,aes(x=Year.,y=Murder.))
  + geom_bar(aes(fill=party),stat = "identity")
  + theme_bw()+
  + labs(title="Republican make US safer?",x="Year",y="Murder Crime")
  + scale_fill_manual(values = c("blue","red"))+coord_cartesian(ylim = c(7000,27000))

MRR<-Crime_party%>%mutate(Selection=ifelse(Year.%in%c(1960,1964,1968,1972,1976,1980,1984,1988,1992,1996,2000,2004,2008,2012),"E",ifelse(Year.%in%c(1963,1967,1971,1975,1979,1983,1987,1991,1995,1999,2003,2007,2011,2015),"PREE",ifelse(Year.%in%c(1961,1965,1969,1973,1977,1981,1985,1989,1993,1997,2001,2005,2009,2013),"PostE",NA))))
MR<-MRR%>%filter(Selection%in%c("E","PREE","PostE"))

##show that Both party worked hard for 10years to make american safer and safer 
##but After 2012 President Obama spent 3 year make US unsafe again, back to the 10 years' level. 
ggplot(Crime_party,aes(x=Year.,y=(Murder.+Forcible.Rape.)))+
  geom_bar(aes(fill=party),stat = "identity")+
  coord_cartesian(xlim=c(2005,2016),ylim=c(90000,115000))+
  theme_bw()+labs(title="US feeling unsafe after 2012",x="Year")+scale_fill_manual(values = c("blue","red"))
##show the police lose the control of violent crime
ggplot(shot_party,aes(x=Year.,y=officer.))+
  geom_point(aes(color = party))+
  coord_cartesian(xlim=c(2005,2016),ylim=c(320,500))+
  theme_bw()+labs(title="Police,support or punished?",x="Year",y="Death by Law Enforcement Officer")+
  scale_color_manual(values = c("blue","red"))

shot<-read.csv("FireArm.csv",stringsAsFactors = F)
shot[,2:3]<-lapply(shot[,2:3],function(x) as.numeric(gsub(",", "", x)))

shot<-shot%>%arrange(Year)
shot_party<-left_join(shot,Crime_party,by="Year")

##explore the crime composition
MRR_all<-MRR%>%mutate(Murder.Ratio=Murder./Total,Forcible.Rape.Ratio=Forcible.Rape./Total,Robbery.Ratio=Robbery./Total,Aggravated.assault.Ratio=Aggravated.assault./Total,Burglary.Ratio=Burglary./Total,Larceny..Theft.Ratio=Larceny..Theft./Total,Vehicle.Theft.Ratio=Vehicle.Theft./Total)
MRR_all[,15:21]<-lapply(MRR_all[,15:21],function(x) round(x,digits = 3))
MRR_Ratio<-select(MRR_all,-(2:12))
MRR_Ratio_stack<-melt(MRR_Ratio,id=c("Year.","party","Selection"))
## recent 3 years rape ratio doubled, unsafe to female now
ggplot(MRR_Ratio,aes(x=Year.,y=Forcible.Rape.Ratio))+
  geom_bar(aes(fill=party),stat = "identity")+
  theme_bw()+
  coord_cartesian(ylim = c(0.004,0.011))+
  scale_fill_manual(values = c("blue","red"))+
  labs(title="Violent Crime Against Women",x="Year")

##Robbery ratio reflect US oil crisis and economy crisis,also show since 1990, D reduce rabbery signific,but R
## R did worse than D
ggplot(MRR_Ratio,aes(x=Year.,y=Robbery.Ratio))+
  geom_bar(aes(fill=party),stat = "identity")+
  theme_tufte()+
  scale_fill_manual(values = c("blue","red"))+
  coord_cartesian(ylim = c(0.025,0.05))+
  labs(title="Republican=Robbery!?",x="Year")

##US CRime Composition
ggplot(MRR_Ratio_stack,aes(x=Year.,y=value))+
  geom_bar(aes(fill=variable),stat = "identity")+theme_bw()+
  labs(title="US Crime Segmentation Change",x="Year",y="Percentage",fill="Crime Segmentation")

ggplot(MRR_stack1,aes(x=Year.,y=value))+
geom_bar(aes(fill=variable),stat = "identity")+
coord_cartesian(xlim=c(1960,2015),ylim=c(0,0.25))+
labs(title="US Crime Segmentation",x="Year",y="Ratio",fill="Crime Segmentation")+
theme_bw()+scale_fill_manual(labels = c("Property.Crime", "Violent.Crime"), values = c("green", "red"))

