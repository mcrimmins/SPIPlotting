# SPI and SPEI plots using nClimDiv data
# MAC 01/05/22
# adapted from nClimDiv/app.R

# load code libraries
library(reshape2)
library(RCurl)
#library(maps)
library(raster)
#library(ggplot2)
library(cowplot)
library(tidyverse)
#library(zoo)
#library(maptools)
library(SPEI)
library(weathermetrics)
#library(metR)
library(scales)
library(magick)
library(plotly)


# load datasets
# ---- Functions ----
# capitalize county names
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}
# cap first
capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

# add/subtracting months
add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]
# ------

# load supporting data from Rdata file generateSupportingData.R
load("nClimDivApp_Data.RData")

# date list for picker - advances on 10th of month
sysDay<-as.numeric(format(Sys.Date(),"%d"))
if(sysDay<=9){
  date.list<-seq(as.Date("1895/1/1"), add.months(Sys.Date(),-2), "months")
}else{
  date.list<-seq(as.Date("1895/1/1"), add.months(Sys.Date(),-1), "months")
}
latest.date<-max(date.list)
# ----


  # ---- Get nClimDiv data ----
  # get county, div and state data ----
  dataSets<-c("climdiv-pcpncy-v",
              "climdiv-pcpndv-v",
              "climdiv-pcpnst-v",
              "climdiv-tmincy-v",
              "climdiv-tmindv-v",
              "climdiv-tminst-v",
              "climdiv-tmaxcy-v",
              "climdiv-tmaxdv-v",
              "climdiv-tmaxst-v")
  # -----
  # container for all data
  datalist = list()
  
  # get directory listing and find most recent prcp file
  url <- 'ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/'
  # see if FTP is working
  tryCatch(getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE, verbose=TRUE), error=function(e) {
    err <<- conditionMessage(e)
  })
  
  # proceed
  filenames = getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE, verbose=TRUE)
  filelist<-unlist(strsplit(filenames,"\n"))
  
  # loop through dataset    
  for(i in 1:length(dataSets)){ 
    # download files and format into list
    tempName<-filelist[which((grepl(dataSets[i], filelist)) == TRUE)]
    url<-paste0("ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/",tempName)
    tempData<-read.table(url, colClasses = c("character","numeric",
                                             "numeric","numeric","numeric","numeric","numeric",
                                             "numeric","numeric","numeric","numeric","numeric",
                                             "numeric"))
    colnames(tempData)<-c("code",1:12)
    tempData$var<-dataSets[i]
    # add to container
    datalist[[i]] <- tempData
    print(paste0("Downloading ",dataSets[i]))
  }
  # combine into dataframe  
  allData = do.call(rbind, datalist)
  rm(datalist)
  # ----
  # update max.date
  # maxYr<-as.numeric(substr(allData[nrow(allData),1],nchar(allData[nrow(allData),1])-3,nchar(allData[nrow(allData),1])))
  # if(length(which(allData[nrow(allData),]==-99.9))==0){
  #   mm<-12
  #   }else{
  #   mm<-which(allData[nrow(allData),]==-99.9)-2
  # }
  # latest.date<-as.Date(paste0(maxYr,"-",mm,"-01"))
  ##### END get data
  
  # ---- DROP IN PLOTTING CODE
    
  # build table of specified plot regions
  
  # states
  stateList<-as.data.frame(state.list)
    stateList$region<-"st"
    stateList$label<-stateList$state.list
    stateList$region.list<-NA
    stateList$cdiv.list<-NA
    stateList[,c(1,3)] <- lapply(stateList[,c(1,3)], as.character)
  #climate divisions  
  cdivList<-as.data.frame(cdiv.list)  
    cdivList$region<-"dv" 
    # subset to only AZ and NM for cdivs
    cdivList<-cdivList[c(grep("Arizona", cdivList$cdiv.list),grep("New Mexico", cdivList$cdiv.list)),]
    # label col
    label<-tidyr::separate(data = cdivList, col = cdiv.list, into = c("left", "right"), sep = "\\,")
    cdivList$label<-gsub("-", "", label$left)
    cdivList$state.list<-NA
    cdivList$region.list<-NA
    cdivList[,c(1,3)] <- lapply(cdivList[,c(1,3)], as.character)
  # regions  
  specRegList<-as.data.frame(region.list$`Climate Region`)
    specRegList$region<-"rg"
    colnames(specRegList)[1]<-"region.list"
    specRegList$label<-(gsub("-", " ", specRegList$region.list))
    specRegList$label<-gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",    # Uppercase with Base R
                            specRegList$label,
                            perl = TRUE)
    specRegList$state.list<-NA
    specRegList$cdiv.list<-NA
    specRegList[,c(1,3)] <- lapply(specRegList[,c(1,3)], as.character)
    # combine into full list  
    fullList<-rbind.data.frame(cdivList,stateList,specRegList)    
    
    # loop through list...
for(l in 1:length(fullList)){
    
    typePET<-'harg' # thornW or harg
    # selections
    region<-fullList$region[l] # list("Climate Division"='dv', "County"='cy',"State"='st',"Region"='rg')
      state <-fullList$state.list[l]
      cdiv  <- fullList$cdiv.list[l]
      county<- county.list[104]
      specReg<-fullList$region.list[l]
    # rg is not a string on climdiv filenames...create new var with ifelse for rg 
    regChr<-region
    regChr<-ifelse(regChr=="rg", "st", regChr)
    # region subset
    tempData<-allData[which(grepl(regChr, allData$var)==TRUE),]
    # parse code col
    if(region=="cy"){
      # get codes
      geoCode<-strsplit(county,"-")
      stCode<- stateCodes[which(stateCodes$name==((geoCode[[1]][1]))),1]
      cyFIPS<-as.character(county.fips[which(county.fips$polyname==paste0(tolower(geoCode[[1]][1]),",",tolower(geoCode[[1]][2]))),1])
      cyFIPS<-as.numeric(ifelse(nchar(cyFIPS)==4, substr(cyFIPS,2,4), substr(cyFIPS,3,5)))
      # parse into columns
      tempData$state<-(as.numeric(substr(tempData$code, 1,2)))
      tempData$div<-(as.numeric(substr(tempData$code, 3,5)))
      tempData$element<-(as.numeric(substr(tempData$code, 6,7)))
      tempData$year<-(as.numeric(substr(tempData$code, 8,11)))
      tempData<-subset(tempData, state==stCode & div==cyFIPS)
      # centroid
      subArea<-subset(countiesPoly,NAME_2==geoCode[[1]][2] & NAME_1==geoCode[[1]][1])
      centroid<-colMeans(coordinates(subArea))
      # build name string
      titleName<-paste0(geoCode[[1]][2]," County,",geoCode[[1]][1])
    }else if (region=="st"){
      # get codes
      stCode<- stateCodes[which(stateCodes$name==state),1]
      # parse into cols
      tempData$state<-(as.numeric(substr(tempData$code, 1,3)))
      tempData$div<-(as.numeric(substr(tempData$code, 4,4)))
      tempData$element<-(as.numeric(substr(tempData$code, 5,6)))
      tempData$year<-(as.numeric(substr(tempData$code, 7,10)))
      tempData<-subset(tempData, state==stCode & div==0)
      # centroid
      subArea<-subset(statesPoly, NAME_1==(state))
      centroid<-colMeans(coordinates(subArea))
      # build name string
      titleName<-paste0((state))
    }else if (region=="dv") {
      # get codes
      geoCode1<-strsplit(cdiv,"-")
      geoCode2<-strsplit(geoCode1[[1]][2],",")
      stCode<- stateCodes[which(stateCodes$name==((geoCode1[[1]][1]))),1]
      cdiv<- as.numeric(geoCode2[[1]][1])
      # parse into cols
      tempData$state<-(as.numeric(substr(tempData$code, 1,2)))
      tempData$div<-(as.numeric(substr(tempData$code, 3,4)))
      tempData$element<-(as.numeric(substr(tempData$code, 5,6)))
      tempData$year<-(as.numeric(substr(tempData$code, 7,10)))
      tempData<-subset(tempData, state==stCode & div==cdiv)
      # centroid
      subArea<-subset(cdivPoly, STATE==geoCode1[[1]][1] & CD_NEW==cdiv)
      centroid<-colMeans(coordinates(subArea))
      # build name string
      titleName<-paste0(geoCode1[[1]][1]," Climate Division ", cdiv)
    }else{
      ### REGION
      # get codes
      stCode<- regionCodes$code[which(regionCodes$name==specReg)]
      # parse into cols
      tempData$state<-(as.numeric(substr(tempData$code, 1,3)))
      tempData$div<-(as.numeric(substr(tempData$code, 4,4)))
      tempData$element<-(as.numeric(substr(tempData$code, 5,6)))
      tempData$year<-(as.numeric(substr(tempData$code, 7,10)))
      tempData<-subset(tempData, state==stCode & div==0)
      # centroid
      subArea<-subset(combinedRegions, region==(specReg))
      centroid<-colMeans(coordinates(subArea))
      # build name string
      titleName<-CapStr(gsub("-", " ", specReg))
      ### REGION FIX
    } 
    
    # melt data
    tempDataMelt<-melt(tempData, id.vars=c(14,18), measure.vars = 2:13)
    #tempDataMelt$date <- as.yearmon(paste(tempDataMelt$year, as.numeric(tempDataMelt$variable), sep = "-"))
    tempDataMelt$date <- as.Date(paste0(tempDataMelt$year,"-",as.numeric(tempDataMelt$variable),"-01"), format="%Y-%m-%d")
    tempDataMelt<-spread(tempDataMelt, var, value)
    # sort, -999 to NA
    tempDataMelt[tempDataMelt == -9.99] <- NA
    tempDataMelt[tempDataMelt == -99.9] <- NA
    # trim to 2018
    #allDataSubset<-allDataSubset[-(which(allDataSubset$year==2019)),]
    # standard colnames
    colnames(tempDataMelt)[4:6]<-c("precip","tmax","tmin")
    # calc tmean
    tempDataMelt$tmean<-(tempDataMelt$tmax+tempDataMelt$tmin)/2
    # ----
    
    # inset map ---- fix MI boundary ----
    insetmap<-ggplot() +
      geom_polygon(data = states4map, aes(x = long, y = lat, group = group), fill="lightgrey", color="grey",size=0.15)  + # get the state border back on top
      geom_polygon(data = subArea, aes(x = long, y = lat, group = group), fill="orange", color="red", size=0.15)  + # get the state border back on top
      coord_fixed(xlim=c(-125, -68), ylim = c(25,50), ratio = 1)+
      #coord_fixed(xlim=c(out$meta$ll[1]-3.5, out$meta$ll[1]+3.5), ylim=c(out$meta$ll[2]-3.5, out$meta$ll[2]+3.5), ratio = 1) +
      #geom_point(data = point, aes(x = V1, y = V2), size=1, color='red')+
      theme_bw(base_size=5)+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))+
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    
    # calculate indices ----
    ## Loop thru full SPI set
    dfSPI<-tempDataMelt[,1:3]
    for(i in 1:60){
      tempSPI <- spi(tempDataMelt$precip,i, na.rm = TRUE)
      dfSPI[[paste('SPI-',i,sep="")]] <-tempSPI$fitted
    }
    # remove rows with NAs
    dfSPI<-na.omit(dfSPI)
    #indexName="Standardized Precipitation Index"
    #indexNameShort="SPI"
    
    # # SPEI switch?
    if (typePET=="thornW"){
      PET <- thornthwaite(fahrenheit.to.celsius(tempDataMelt$tmean,round=2), as.numeric(centroid[2]), na.rm = TRUE) 
    }else{
      PET <- hargreaves(fahrenheit.to.celsius(tempDataMelt$tmin,round=2),fahrenheit.to.celsius(tempDataMelt$tmax,round=2),Ra=NA, as.numeric(centroid[2]), na.rm = TRUE) 
    }
    dfSPEI<-tempDataMelt[,1:3]
    for(i in 1:60){
      tempSPI <- spei(inches_to_metric(tempDataMelt$precip,unit="mm",round=2)-PET,i, na.rm = TRUE)
      dfSPEI[[paste('SPEI-',i,sep="")]] <-tempSPI$fitted
    }
    # remove rows with NAs
    dfSPEI<-na.omit(dfSPEI)
    #indexName="Standardized Precipitation-Evapotranspiration Index"
    #indexNameShort="SPEI"
    
    # monthly anomalies - https://www.r-bloggers.com/visualize-monthly-precipitation-anomalies/
    tempDataMelt$PET<-PET/25.4
    tempDataMelt$P_PET<-tempDataMelt$precip-tempDataMelt$PET
    moAvg <- tempDataMelt %>%
      group_by(variable) %>%
      summarise(moAvgPrecip = mean(precip, na.rm=TRUE),
                moAvgTemp   = mean(tmean, na.rm=TRUE),
                moAvgPET    = mean(PET, na.rm=TRUE),
                moAvgP_PET  = mean(P_PET, na.rm=TRUE))
    moAvg[,2:5] <-round(moAvg[,2:5],2)
    
    tempDataMelt <- left_join(tempDataMelt, moAvg, by = "variable")
    tempDataMelt$precipAnom <- tempDataMelt$precip-tempDataMelt$moAvgPrecip
    tempDataMelt$tempAnom <- tempDataMelt$tmean-tempDataMelt$moAvgTemp
    tempDataMelt$PETAnom <- tempDataMelt$PET-tempDataMelt$moAvgPET
    tempDataMelt$P_PETAnom <- tempDataMelt$P_PET-tempDataMelt$moAvgP_PET
    # anom sign
    tempDataMelt$pAnomSign<-ifelse(tempDataMelt$precipAnom > 0, "pos", "neg")
    tempDataMelt$petAnomSign<-ifelse(tempDataMelt$P_PETAnom > 0, "pos", "neg")
    tempDataMelt$TAnomSign<-ifelse(tempDataMelt$tempAnom > 0, "pos", "neg")
    # round values
    tempDataMelt[,8:17] <-round(tempDataMelt[,8:17],2)
    
    # plot variables ----
    # date range
    #dateRange<-input$dateRangeMY
    date1<-as.Date("1991-01-01")# by month
    date2<-latest.date # by month
    maxScale<-60# max 60
    
    # SPI contour plot ----
    dfSPI<-melt(dfSPI, id.vars=c(1:3), measure.vars = 4:63)
    dfSPI$value<-as.numeric(dfSPI$value)
    colnames(dfSPI)[2]<-"month"
    # current heat map
    currDfSPI<-dfSPI[which(dfSPI$date==date2),]
    # plot  
    pCurr<- ggplot(currDfSPI, aes((date),as.numeric(variable) , fill = value))+
      geom_tile(width=1)+
      scale_fill_gradientn(colors=c("orange3","orange","yellow","white","green","green2","darkgreen"), name=" ",
                           na.value = "grey50", guide = FALSE, limits=c(-3, 3), oob=squish)+
      theme_bw()+
      scale_y_continuous(limits=c(0,maxScale), expand=c(0,0), breaks=seq(0,60,6))+
      scale_x_date(labels = date_format("%b%Y"), expand=c(0,0))+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_blank())+
      labs(title=" ")+
      theme(plot.margin = unit(c(5, 5, 0, 0), "pt"))
    
    # main plot  
    p1<-  ggplot(dfSPI, aes((date),as.numeric(variable) , fill = value))+
      geom_tile(width=31)+
      #scale_fill_gradient2(low = "brown", high = "green",mid = "white",
      #                    na.value = "grey50", guide = "colourbar", limits=c(-3, 3), oob=squish)+
      scale_fill_gradientn(colors=c("orange3","orange","yellow","white","green","green2","darkgreen"), name=" ",
                           na.value = "grey50", guide = "colourbar", limits=c(-3, 3), oob=squish)+
      scale_x_date(labels = date_format("%Y-%m"), breaks='2 years', expand=c(0,0),
                   limits = c(as.Date(date1),as.Date(date2)))+
      scale_y_continuous(limits=c(0,maxScale), expand=c(0,0), breaks=seq(0,60,6))+
      theme_bw()+
      theme(legend.position="left")+
      theme(plot.title = element_text(face="bold"),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+
      guides(fill = guide_colourbar(barwidth = 1, barheight = 10))+
      ylab("Timescale (mos)")+
      labs(title=paste0(titleName," Standardized Precipitation Index (", format(as.Date(date1), "%b%Y"),
                        " - ",format(as.Date(date2), "%b%Y"),")"))+
      theme(plot.margin = unit(c(5, 0, 0, 0), "pt"))
    
    # precip anoms
    p2<- ggplot(tempDataMelt,aes(date, precipAnom, fill = pAnomSign)) + 
      geom_bar(stat = "identity", show.legend = FALSE) + 
      #scale_y_continuous(breaks = seq(-100, 100, 20)) +
      scale_fill_manual(values = c("orange4", "darkgreen"))+
      scale_x_date(labels = date_format("%Y"), breaks='2 years', expand=c(0,0),
                   limits = c(as.Date(date1),add.months(as.Date(date2),1)))+
      ylab("Precip Anom (in.)")+
      xlab("Month-Year")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))+
      theme(panel.grid.minor = element_blank())
    
    # # trying to get alignments
    #    mainP<-plot_grid(p1, p2, ncol = 1, align = 'v', axis=c('l'),rel_heights = c(3.5,1))
    #    sideP<-plot_grid(pCurr, NULL, ncol = 1, rel_heights = c(3.5,1))
    #    plot_grid(mainP, sideP, nrow = 1, align='h',axis = c('tblr'), rel_widths = c(20,1))
    # # another solution
    #    plot_grid(p1,pCurr,p2,NULL, ncol = 2, nrow = 2, align = 'v',axis = 'b', rel_heights = c(10,10,1,1),
    #              rel_widths = c(20,1,20,1))
    
    # plotting grid using align
    mainCurr <- align_plots(p1, pCurr, align = 'h', axis = 'l')
    mainPrec <- align_plots(p1, p2, align = 'v', axis = 'b')
    
    mainP<-plot_grid(mainCurr[[1]], mainPrec[[2]], ncol = 1, align = 'v', axis=c('l'),rel_heights = c(3.5,1))
    sideP<-plot_grid(pCurr, NULL, ncol = 1, rel_heights = c(3.5,1))
    #plot_grid(mainP, sideP, nrow = 1, align='h',axis = c('tblr'), rel_widths = c(20,1))
    spiPlot<-plot_grid(mainP, sideP, nrow = 1, rel_widths = c(20,1))
    
    # add inset map
    spiPlot<-ggdraw(spiPlot)+draw_plot(insetmap, -0.315, 0.40, scale=0.14)
    
    # add margin
    spiPlot = spiPlot + theme(plot.margin = unit(c(0.25, 0.25, 0.7, 0.25), "in")) 
    # add caption
    captionString <- c( "Data from NOAA-NCEI",
                        "ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/",
                        paste0("Plot created: ", format(Sys.Date(), "%m-%d-%Y")),
                        "The University of Arizona",
                        "https://cals.arizona.edu/climate/")
    spiPlot<-ggdraw(spiPlot) + draw_text(captionString, x =0.125, 
                                         y = c(0.0625,0.0500,0.0375,0.0250,0.0125), hjust = 0,vjust=-0.25, size=8)
    
    # write high res to file ----
    png("spiPlot.png", width = 11, height = 8.5, units = "in", res = 300L)
    #grid.newpage()
    print(spiPlot, newpage = FALSE)
    dev.off()
    
    # add logos
    # Call back the plot
    plot <- image_read("spiPlot.png")
    # And bring in a logo
    #logo_raw <- image_read("./logos/UA_CLIMAS_logos.png") 
    logo_raw <- image_read("UA_CSAP_CLIMAS_logos_horiz.png") 
    logo <- image_resize(logo_raw, geometry_size_percent(width=95,height = 95))
    # Stack them on top of each other
    #final_plot <- image_append((c(plot, logo)), stack = TRUE)
    #final_plot <- image_mosaic((c(plot, logo)))
    final_plot <- image_composite(plot, logo, offset = "+2235+2365")
    # And overwrite the plot without a logo
    image_write(final_plot, "spiPlot.png")  

}
