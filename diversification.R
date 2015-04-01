library(reshape)
library(ggplot2)
library(plyr)
library(scales)

setwd("V:/Personal EE Folders/PReid/Diversification")

# -----------------------------------------------------------
# Functions for loading data and computing total variances
# -----------------------------------------------------------
load.data <- function(filepath) {

  df <- read.csv(filepath)
  df <- melt(df)
  names(df) <- c("Industry", "City", "Year", "Value")
  df <- df[df$Industry != "All Industries", ]
  df$Year <- as.numeric(substr(as.character(df$Year), 2, 5))
  df$GDP <- as.numeric(as.character(df$Value))
  
  share <- function(x) x / sum(x)
  df <- ddply(df, c("City", "Year"), here(transform), Share=share(Value))
  
  herfindal <- function(x) sum(x ** 2)
  entropy <- function(x) -sum(x * log(x))
  df <- ddply(df, c("City", "Year"), here(transform), herfindal=herfindal(Share), 
              entropy=entropy(Share))
  df <- ddply(df, c("City", "Industry"), here(transform), 
              growth=c(NA, exp(diff(log(Value))) - 1))
  
  return(df)
}

total.var <- function(df, start, end, part) {
  df <- df[df$Year >= start & df$Year <= end, ]
  growth.df <- cast(df, City + Year ~ Industry, value="growth")
  cov.matrix <- cov(growth.df[, -c(1, 2)], use="complete.obs")
  share.vec <- df[df$Year==end, "Share"]
  
  if (part == "var") {
    return(t(share.vec) %*% diag(diag(cov.matrix)) %*% share.vec)
  } else if (part == "cov") {
    return(t(share.vec) %*% (cov.matrix - diag(diag(cov.matrix))) %*% share.vec)
  } else if (part == "total") {
    return(t(share.vec) %*% cov.matrix %*% share.vec)
  } 
}

# -----------------------------------------------------------
# Plotting functions
# -----------------------------------------------------------

ThemeMod <- theme_grey() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(color="black"), axis.ticks = element_blank(),
        panel.background = element_blank(), legend.key = element_blank(),
        strip.background = element_blank())


make.var.bars <- function(df, city, title) {
  # Create bar chart of total variances
  
  df$City <- factor(df$City, levels=df$City[order(df$Total)])
  df <- melt(df)
  df$highlight <- df$City == city
  max.limit <- max(df$value) + 0.05 * max(df$value)
  
  plot <- ggplot(df[df$variable == "Total", ], aes(x=City, y=value, 
                                                   fill=highlight)) + 
    geom_bar(stat="identity", show_guide=FALSE) +
    geom_text(aes(label=sprintf("%1.2f%%", 100 * value)), hjust=-0.25) +
    coord_flip() +
    scale_x_discrete(name=element_blank()) +
    scale_y_continuous(name="Total Variance", expand=c(0, 0), 
                       limits=c(0, max.limit), labels=percent) +
    ggtitle(title) +
    ThemeMod
  
  return(plot)
}

make.var.lines <- function(df, regions, title) {
  # Create line chart of variances over time
  
  df <- melt(df)
  
  plot <- ggplot(df[df$City %in% regions, ], aes(x=variable, y=value, group=City, 
                                                 color=City)) +
    geom_line() +
    geom_point() +
    scale_x_discrete(name=element_blank()) +
    scale_y_continuous(name="Total Variance", labels=percent) +
    ggtitle(title) +
    ThemeMod
  
  return(plot)
}

make.growth.lines <- function(df, city, title) {
  # Create faceted line charts of growth by industry with max/min shaded
  
  df <- ddply(df, c("Year", "Industry"), transform, ymax=max(growth), 
              ymin=min(growth))
  breaks <- seq(1987, 2014, 9)
  
  plot <- ggplot(df) +
    geom_ribbon(aes(x=Year, ymax=ymax, ymin=ymin, fill="area"), alpha=0.5) +
    geom_line(data=df[df$City == city, ], aes(x=Year, y=growth, color="line")) +
    facet_wrap(~ Industry) +
    scale_x_continuous(name=element_blank(), breaks=breaks, labels=breaks) +
    scale_y_continuous(name="GDP Growth", labels=percent) +
    scale_fill_manual(values=c("area"="grey"), labels="Range of City Growth Rates") +
    scale_color_manual(values=c("line"="#00a6ff"), labels=paste(city, "Growth Rate")) +
    ggtitle(title) +
    ThemeMod +
    theme(legend.position = c(1, 0))
  
  return(plot)
}

make.herf.bar <- function(df, year, city, title) {
  # Create bar chart of herfindal index by city
  
  df <- df[df$Year == year & df$Industry == "Business Services", ]
  df$City <- factor(df$City, levels=df$City[order(df$herfindal)])
  df$highlight <- df$City == city
  max.limit <- max(df$herfindal) + 0.05
  
  plot <- ggplot(df, aes(x=City, y=herfindal)) + 
    geom_bar(aes(fill=highlight), stat="identity", show_guide=FALSE) +
    geom_text(aes(label=sprintf("%1.1f%%", 100 * herfindal)), hjust=-0.25) +
    coord_flip() +
    scale_x_discrete(name=element_blank()) +
    scale_y_continuous(name="Economic Concentration", expand=c(0,0), 
                       limits=c(0,max.limit), labels=percent) +
    ggtitle(title) +
    ThemeMod
  
  return(plot)
}

make.herf.lines <- function(df, regions, title) {
  # Create line chart of Herfindal indices over time
  
  breaks <- seq(1987, 2014, 9)
  
  plot <- ggplot(df[df$City %in% regions, ],aes(x=Year, y=herfindal, colour=City)) +
    geom_line(size=1) +
    ggtitle(title) +
    guides(fill=guide_legend(reverse=TRUE)) +
    scale_x_continuous(name=element_blank(), breaks=breaks, labels=breaks) +
    scale_y_continuous(name="Economic Concentration", labels=percent) +
    ThemeMod
  
  return(plot)
}

make.share.area <- function(df, city, title) {
  # Create stacked area chart of share by industry
  
  df <- ddply(df, c("City", "Year"), transform, 
              position=cumsum(Share) - 0.5 * Share)
  breaks <- seq(1987, 2014, 9)
  
  plot <- ggplot(df[df$City == city, ], aes(Year, Share, fill=Industry)) + 
    geom_area(colour="white", show_guide=FALSE) +
    geom_text(df=df[df$City == city & df$Year == 2014, ], 
              aes(label=Industry, y=position), color="white", size=3, hjust=1) +
    ggtitle(title) +
    scale_x_continuous(name=element_blank(), breaks=breaks, labels=breaks, 
                       expand=c(0,0)) +
    scale_y_continuous(name="% of GDP", labels=percent, expand=c(0,0)) +
    ThemeMod

  return(plot)
}

make.share.bars <- function(df, city, years, title) {
  # Create stacked bar chart of annual shares for given city and years
  
  df <- ddply(df, c("City", "Year"), transform, 
              position=cumsum(Share) - 0.5 * Share)
  
  plot <- ggplot(df[df$City == city & df$Year %in% years, ], 
                 aes(factor(Year), Share)) + 
    geom_bar(aes(fill=Industry), colour="white", stat="identity", width=0.5) +
    geom_text(aes(label=sprintf("%1.f%%", 100 * Share), y=position), 
              color="white", size=3) +
    ggtitle(title) +
    guides(fill=guide_legend(reverse=TRUE), color=FALSE) +
    scale_y_continuous(name="% of GDP",labels=percent) +
    scale_x_discrete(name=element_blank()) +
    ThemeMod
  
  return(plot)
}

make.share.compare <- function(df, regions, year, title) {
  # Create stacked bar chart of annual shares for given cities and year
  
  df <- ddply(df, c("City", "Year"), transform, 
              position=cumsum(Share) - 0.5 * Share)
  df <- df[df$Year == year, ]
  df$City <- factor(df$City, levels=df$City[order(df$herfindal)])
  
  plot <- ggplot(df[df$City %in% regions, ], aes(x=City, y=Share)) + 
    geom_bar(aes(fill=Industry), colour="white", stat="identity", width=0.5) +
    geom_text(aes(label=sprintf("%1.f%%", 100 * Share), y=position), 
              color="white", size=3) +
    ggtitle(title) +
    guides(fill=guide_legend(reverse=TRUE), color=FALSE) +
    scale_y_continuous(name="% of GDP", labels=percent) +
    scale_x_discrete(name=element_blank()) +
    ThemeMod
  
  return(plot)
}

# ---------------------------------
# GDP data
# ---------------------------------

data <- load.data("data/gdp.csv")

city.vars <- ddply(data, "City", function(df) 
  c(total.var(df, 1987, 2014, "var"), 
    total.var(df, 1987, 2014, "cov"), 
    total.var(df, 1987, 2014, "total")))
names(city.vars) <- c("City", "Var", "Cov", "Total")

city.vars.time <- ddply(data, "City", function(df) 
  c(total.var(df, 1987, 1996, "total"), 
    total.var(df, 1997, 2006, "total"), 
    total.var(df, 2007, 2014, "total")))
names(city.vars.time) <- c("City", "1987-1996", "1997-2006", "2007-2014")

majcities <- c("Calgary", "Ottawa and Gatineau", "Toronto", "Vancouver", 
               "Montreal", "Edmonton")

gdp.growth.lines <- make.growth.lines(data, "Edmonton", "GDP Growth by Industry")
gdp.herf.bar <- make.herf.bar(data, 2014, "Edmonton", "GDP Concentration, 2014")
gdp.herf.lines <- make.herf.lines(data, majcities, "GDP Concentration, 1987-2014")
gdp.share.area <- make.share.area(data, "Edmonton", "GDP by Industry, 1987-2014")
gdp.share.bars <- make.share.bars(data, "Edmonton", c(1994, 2004, 2014), 
                                  "GDP by Industry, Edmonton")
gdp.share.compare <- make.share.compare(data, majcities, 2014, "GDP by Industry")
gdp.var.bars <- make.var.bars(city.vars, "Edmonton", "GDP Growth Volatility, 1987-2014")
gdp.var.lines <- make.var.lines(city.vars.time, majcities, "GDP Growth Volatility")

plots <- list(gdp.growth.lines, gdp.herf.bar, gdp.herf.lines, gdp.share.area, 
              gdp.share.bars, gdp.share.compare, gdp.var.bars, gdp.var.lines)

ggsave("plots/gdp/gdp.growth.lines.pdf", gdp.growth.lines)
ggsave("plots/gdp/gdp.herf.bar.pdf", gdp.herf.bar)
ggsave("plots/gdp/gdp.herf.lines.pdf", gdp.herf.lines)
ggsave("plots/gdp/gdp.share.area", gdp.share.area)
ggsave("plots/gdp/gdp.share.bars", gdp.share.bars)
ggsave("plots/gdp/gdp.share.compare", gdp.share.compare)
ggsave("plots/gdp/gdp.var.bars", gdp.var.bars)
ggsave("plots/gdp/gdp.var.lines", gdp.var.lines)


# ---------------------------------
# Employment data
# ---------------------------------
