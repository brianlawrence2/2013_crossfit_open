library(parallel)
library(foreach)
library(doParallel)

getAthleteData<-function(links) {
  require(RCurl)
  require(XML)
  i<-1
  for (url in links$links) {
    #print(c(url,i))
    cg<-try(getURL(url),silent=T)

    cg.tree<-try(htmlParse(cg),silent=T)
    cg.tabs<-try(readHTMLTable(cg.tree),silent=T)
    
    name<-try(xpathApply(cg.tree,"//h2[@id='page-title']/text()"),silent=T)
    info<-try(xpathApply(cg.tree,"//dl/dd/text()"),silent=T)
    area<-try(xpathApply(cg.tree,"//dl/dd/a/text()"),silent=T)
    
    wo<-try(cg.tabs[[1]]$V2,silent=T)
    maxes<-try(cg.tabs[[2]]$V2,silent=T)
    
    if (length(area)==3) {
      region<-try(xmlValue(area[[1]]),silent=T)
      team<-try(xmlValue(area[[2]]),silent=T)
      affiliate<-try(xmlValue(area[[3]]),silent=T)
    } else if (length(area)==2) {
      region<-try(xmlValue(area[[1]]),silent=T)
      team<-"none"
      affiliate<-try(xmlValue(area[[2]]),silent=T)
    } else {
      region<-try(xmlValue(area[[1]]),silent=T)
      team<-"none"
      affiliate<-"none"
    }
    
    s<-try(xmlValue(info[[1]]),silent=T)
    age<-try(xmlValue(info[[2]]),silent=T)
    height<-try(xmlValue(info[[3]]),silent=T)
    weight<-try(xmlValue(info[[4]]),silent=T)
    
    rank<-links[i,2]
    i<-i+1
    
    athT<-try(data.frame(name=sapply(name,xmlValue),
                         rank=rank,
                         region=region,
                         team=team,
                         affiliate=affiliate,
                         s=s,
                         age=age,
                         height=height,
                         weight=weight,
                         fran=wo[1],
                         helen=wo[2],
                         grace=wo[3],
                         filthy50=wo[4],
                         fgb=wo[5],
                         sprint400=wo[6],
                         run5k=wo[7],
                         cj=maxes[1],
                         snatch=maxes[2],
                         dl=maxes[3],
                         bs=maxes[4],
                         mpu=maxes[5]),silent=T)
    
    if(!exists("ath")) {
      ath<-athT
    } else {
      ath<-try(rbind(ath,athT),silent=T)
    }
  }
  return(ath)
}

getAthleteLinks<-function(start,end,s) {
  require(stringr)
  require(RCurl)
  require(XML)
  # Initilazie the vectors
  links<-c()
  ranks<-c()
  
  # Loop through the table pages
  for (i in start:end) {
    # initialize and transform the url to be used by getURL
      url<-"http://games.crossfit.com/scores/leaderboard.php?stage=5&sort=0&page=replace1&division=replace2&region=0&numberperpage=60&competition=0&frontpage=0&expanded=0&year=13&full=1&showtoggles=0&hidedropdowns=1&showathleteac=1&="
    url<-str_replace(url,"replace1",i)
    url<-str_replace(url,"replace2",s)
    
    # Get the URL and parse the HTML
    lead<-getURL(url)
    lead.tree<-try(htmlParse(lead),silent=T)
    
    # Get all of the links from the table and create the links vector
    link<-try(xpathApply(lead.tree,"//td[@class='name']/a",xmlGetAttr,'href'),silent=T)
    link<-try(as.vector(link),silent=T)
    for (i in link) {links<-c(links,i)}
    
    # Get all of the ranks and create the ranks vector
    rank<-try(xpathApply(lead.tree,"//td[@class='number']/text()", xmlValue),silent=T)
    rank<-try(as.vector(rank),silent=T)
    for (i in rank) {ranks<-c(ranks,i)}
  }
  
  # Return a data frame of the links
  return(data.frame(links=links,ranks=ranks))
}

mcAth<-function(i) {
  ath.links<-getAthleteLinks(i,i,1)
  athmtemp<-getAthleteData(ath.links)
  return(athmtemp)
}

cl<-makeCluster(detectCores())
registerDoParallel(cl)
athm<-foreach(i=1:771, .combine="rbind") %dopar% mcAth(i)

write.csv(athm,"~/2013_crossfit_open/scraper/data/athm.csv")