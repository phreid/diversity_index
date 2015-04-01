library(reshape)
library(ggplot2)
library(plyr)
library(scales)

setwd("V:/Personal EE Folders/PReid/Diversification")

# -----------------------------------------------------------
# Functions for loading data and computing total variances
# -----------------------------------------------------------
load.clean<-function(filepath) {

  df<-read.csv(filepath)
  df<-melt(df)
  names(df)<-c("Industry","City","Year","GDP")
  df<-df[df$Industry != 'All Industries',]
  df$Year<-as.numeric(substr(as.character(df$Year),2,5))
  df$GDP<-as.numeric(as.character(df$GDP))
  
  share <- function(x) x/sum(x)
  df<-ddply(df,c("City","Year"),here(transform),Share=share(GDP))
  
  herfindal<-function(x) sum(x**2)
  entropy<-function(x) -sum(x*log(x))
  df<-ddply(df,c("City","Year"),here(transform),herfindal=herfindal(Share),entropy=entropy(Share))
  df<-ddply(df,c("City","Industry"),here(transform), growth=c(NA,exp(diff(log(GDP)))-1))
  
  return(df)
}

total.var<-function(df,start,end,part){
  df<-df[df$Year >= start & df$Year <= end,]
  growth.df<-cast(df,City+Year~Industry,value='growth')
  cov.matrix<-cov(growth.df[,-c(1,2)],use="complete.obs")
  share.vec<-df[df$Year==end,'Share']
  if (part == 'var') {
    return(t(share.vec) %*% diag(diag(cov.matrix)) %*% share.vec)
  } else if (part == 'cov') {
    return(t(share.vec) %*% (cov.matrix - diag(diag(cov.matrix))) %*% share.vec)
  } else if (part == 'total') {
    return(t(share.vec) %*% cov.matrix %*% share.vec)
  } 
}

data<-load.clean("data/gdp.csv")

city.vars<-ddply(data,"City",function(df) c(total.var(df,1987,2014,'var'),
                                            total.var(df,1987,2014,'cov'),
                                            total.var(df,1987,2014,'total')))
names(city.vars)<-c("City","Var","Cov","Total")

city.vars.time<-ddply(data,"City",function(df) c(total.var(df,1987,1996,'total'),
                                                 total.var(df,1997,2006,'total'),
                                                 total.var(df,2007,2014,'total')))
names(city.vars.time)<-c("City","1987-1996","1997-2006","2007-2014")

# -----------------------------------------------------------
# Plotting functions
# -----------------------------------------------------------

# Set position for stacked bar chart labels
data<-ddply(data,c("City","Year"), transform, position = cumsum(Share) - 0.5*Share)

# Breaks and cities for plotting
breaks<-seq(1987,2014,9)
majcities<-c("Calgary","Ottawa and Gatineau","Toronto","Vancouver","Montreal","Edmonton")

ThemeMod <- theme_grey() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line=element_line(color='black'),axis.ticks=element_blank(),
        panel.background=element_blank(),legend.key=element_blank())


# Barchart of 2014 total variances
varbar<-function(df,city) {

  df$City<-factor(df$City,levels=df$City[order(df$Total)])
  df<-melt(df)
  df$highlight<-df$City == city
  varbar<-ggplot(df[df$variable == 'Total',],aes(x=City,y=value,fill=highlight)) + 
    geom_bar(stat='identity',show_guide=FALSE) +
    geom_text(aes(label=sprintf("%1.2f%%", 100*value)),hjust=-0.25) +
    coord_flip() +
    scale_x_discrete(name=element_blank()) +
    scale_y_continuous(name='Total Variance',expand=c(0,0),limits=c(0,0.002),labels=percent) +
    ggtitle('Total Variance Based on 2014 GDP by Industry\n(Higher Variance = Less Stability)') +
    ThemeMod
}

# Barchart of 2014 variances
stackedvarbar<-ggplot(city.vars[city.vars$variable != 'Total',],aes(x=City,y=value,fill=variable)) + 
  geom_bar(stat='identity') +
  #geom_text(aes(label=sprintf("%1.2f%%", 100*Var)),hjust=-0.25) +
  coord_flip() +
  scale_x_discrete(name=element_blank()) +
  scale_y_continuous(name='Total Variance',expand=c(0,0),limits=c(0,0.002),labels=percent) +
  ggtitle('Total Variance Based on 2014 GDP by Industry\n(Higher Variance = Less Stability)') +
  ThemeMod

# Line chart of variances over time
city.vars.time<-melt(city.vars.time)
varlines<-ggplot(city.vars.time[city.vars.time$City %in% majcities,],aes(x=variable,y=value,group=City,color=City)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(name=element_blank()) +
  scale_y_continuous(name='Total Variance',labels=percent) +
  ggtitle('Total Variance Based on GDP by Industry Over Time') +
  ThemeMod

# Faceted line charts of GDP growth by industry with max/min shaded
data<-ddply(data,c("Year","Industry"),transform,ymax=max(growth),ymin=min(growth))
industrylines<-ggplot(data) +
  geom_ribbon(aes(x=Year,ymax=ymax,ymin=ymin,fill='area'),alpha=0.5) +
  geom_line(data=data[data$City=='Edmonton',],aes(x=Year,y=growth,color='line')) +
  facet_wrap(~Industry) +
  scale_x_continuous(name=element_blank(),breaks=breaks,labels=breaks) +
  scale_y_continuous(name='GDP Growth',labels=percent) +
  scale_fill_manual(values=c('area'='grey'),labels="Range of City Growth Rates") +
  scale_color_manual(values=c('line'='#00a6ff'),labels="Edmonton Growth Rate") +
  ThemeMod

# Barchart of 2014 Herfindal indices
makeherfbar <- function(df,year,city) {
  df.sub<-df[df$Year==year & df$Industry=='Business Services',]
  df.sub$City<-factor(df.sub$City,levels=df.sub$City[order(df.sub$herfindal)])
  df.sub$highlight<-df.sub$City == city
  herfbar<-ggplot(df.sub,aes(x=City,y=herfindal)) + 
    geom_bar(aes(fill=highlight),stat='identity',show_guide=FALSE) +
    geom_text(aes(label=sprintf("%1.1f%%", 100*herfindal)),hjust=-0.25) +
    coord_flip() +
    scale_x_discrete(name=element_blank()) +
    scale_y_continuous(name='Economic Concentration',expand=c(0,0),limits=c(0,0.2),labels=percent) +
    ggtitle('Economic Concentration, 2014\n(Lower Concentration = More Diverse)') +
    ThemeMod
  return(herfbar)
}

# Line chart of Herfindal indices over time
herfline<-ggplot(data[data$City %in% majcities,],aes(x=Year,y=herfindal,colour=City)) +
  geom_line(size=1) +
  ggtitle('Index of Economic Concentration 1987-2014, Major Canadian Cities\n(Lower Index = More Diverse)') +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous(name=element_blank(),breaks=breaks,labels=breaks) +
  scale_y_continuous(name='Economic Concentration',labels=percent) +
  ThemeMod

# Stacked area chart of GDP data, Edmonton
gdparea<-ggplot(data[data$City=='Edmonton',],aes(Year,Share,fill=Industry)) + 
  geom_area(colour='white',show_guide=FALSE) +
  geom_text(data=data[data$City=='Edmonton' & data$Year==2014,],aes(label=Industry,y=position),color='white',size=3,hjust=1) +
  ggtitle('GDP Breakdown by Industry, Edmonton CMA') +
  scale_x_continuous(name=element_blank(),breaks=breaks,labels=breaks,expand=c(0,0)) +
  scale_y_continuous(name='% of GDP',labels=percent,expand=c(0,0)) +
  ThemeMod

# Stacked bar chart of GDP data, Edmonton 1994,2004,2014
gdpbars<-ggplot(data[data$City=='Edmonton' & data$Year %in% c(1994,2004,2014),],aes(factor(Year),Share)) + 
  geom_bar(aes(fill=Industry),colour='white',stat='identity',width=0.5) +
  geom_text(aes(label=sprintf("%1.f%%", 100*Share),y=position),color='white',size=3) +
  ggtitle('GDP Breakdown by Industry 1994 - 2014, Edmonton CMA') +
  guides(fill = guide_legend(reverse = TRUE),color=FALSE) +
  scale_y_continuous(name='% of GDP',labels=percent) +
  scale_x_discrete(name=element_blank()) +
  ThemeMod

# Stacked bar chart of GDP data, major cities
data.2014$City<-factor(data.2014$City,levels=data.2014$City[order(data.2014$herfindal)])
gdpcompare<-ggplot(data.2014[data.2014$City %in% majcities,],aes(x=City,y=Share)) + 
  geom_bar(aes(fill=Industry),colour='white',stat='identity',width=0.5) +
  geom_text(aes(label=sprintf("%1.f%%", 100*Share),y=position),color='white',size=3) +
  ggtitle('GDP Breakdown by Industry, Major Canadian Cities') +
  guides(fill = guide_legend(reverse = TRUE),color=FALSE) +
  scale_y_continuous(name='% of GDP',labels=percent) +
  scale_x_discrete(name=element_blank()) +
  ThemeMod

