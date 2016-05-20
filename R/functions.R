#' getID
#'
#' @return Returns a dataframe of the play identification data from NBA.com

getID <- function(){
  ## create a list of all players and teams in the NBA from 2001-2016
idURL <- paste("http://stats.nba.com/stats/commonallplayers?",
               "IsOnlyCurrentSeason=1&LeagueID=00&Season=2015-16",
               sep = "")

idData <- rjson::fromJSON(file = idURL, method="C")
idDf<- data.frame(
  matrix(unlist(idData$resultSets[[1]][[3]]), ncol = 13,byrow = TRUE),
  stringsAsFactors = FALSE)
colnames(idDf)<- idData$resultSets[[1]][[2]]
return(idDf)
}

#' getShotData
#'
#' @param playerName
#'
#' @return Returns a data frame of the shot data for the specified player.
#'
#' @examples getShotData("LeBron James")
getShotData <- function(playerName){
  idDf <- getID()
  ndx <- which(idDf$DISPLAY_FIRST_LAST %in% playerName)
  player <- idDf[ndx,1]

  shotURL <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=&CFPARAMS",
                   "=&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&",
                   "GameID=&GameSegment=&LastNGames=0&",
                   "LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&",
                   "Period=0&Position=&RookieYear=&SeasonSegment=&",
                   "SeasonType=Regular+Season&TeamID=0&VsConference=&",
                   "VsDivision=&PlayerID=",player,
                   "&Season=2015-16",
                   sep = "")
  shotData <- rjson::fromJSON(file = shotURL, method="C")
  shotDataf <- data.frame(
    matrix(unlist(shotData$resultSets[[1]][[3]]),
           ncol=21, byrow = TRUE)
  )
  #set column names
  colnames(shotDataf) <- shotData$resultSets[[1]][[2]]
  #convert coordinates to numerics
  shotDataf$LOC_X <- as.numeric(
    as.character(shotDataf$LOC_X)
  )
  shotDataf$LOC_Y <- as.numeric(
    as.character(shotDataf$LOC_Y)
  )
  shotDataf$SHOT_DISTANCE <- as.numeric(
    as.character(shotDataf$SHOT_DISTANCE)
  )
  shotDataf$MINUTES_REMAINING <- as.numeric(
    as.character(shotDataf$MINUTES_REMAINING)
  )
  shotDataf$SECONDS_REMAINING <- as.numeric(
    as.character(shotDataf$SECONDS_REMAINING)
  )
  shotDataf[which(shotDataf$LOC_X >= -250 & shotDataf$LOC_X <= 250 & shotDataf$LOC_Y >= -50 & shotDataf$LOC_Y <= 420),]
  return(shotDataf)
}

#' basicShotChart
#'
#' @param playerName
#'
#' @return Returns a color coded shot chart where each attempt is represented by a point and the color of the point indicates whether or not the attempt was successful.
#'
#' @examples basicShotChart("Lebron James")
basicShotChart <- function(playerName){
  df <- getShotData(playerName)
  shotChart <- ggplot2::ggplot(df,ggplot2::aes(x=LOC_X, y=LOC_Y)) +
    ggplot2::geom_point(ggplot2::aes(colour = EVENT_TYPE), size = 1)+
    ggplot2::geom_point(ggplot2::aes(x = mean(LOC_X), y = mean(LOC_Y)), size = 2)+
    ggplot2::xlim(-250,250)+
    ggplot2::ylim(-50,420)+
    ggplot2::ggtitle(playerName)
  return(shotChart)
}

shotModelFit <- function(player){
  df <- getShotData(player)
  mod <- stats::glm(SHOT_MADE_FLAG ~ LOC_X + LOC_Y,
             data = df,
             family = binomial)
  prdct <- df[,c("LOC_X","LOC_Y")]
  df$PROB <- stats::predict(mod, newdata = prdct)
  return(df)
}

#' densityShotChart
#'
#' @param player
#'
#' @return Returns a shot chart for the specified player that shows the denisty of shot attempts and probability of making the attempt.
#'
#' @examples densityShotChart("LeBron James")
densityShotChart <- function(player){
  df <- shotModelFit(player)
  shotChart <- ggplot2::ggplot(data = df, ggplot2::aes(x=LOC_X, y=LOC_Y, z=PROB)) +
    ggplot2::xlim(-250,250)+
    ggplot2::ylim(-50,420)+
    ggplot2::geom_point(ggplot2::aes(color = PROB), alpha = 1/2 ) +
    ggplot2::scale_color_gradient(low="yellow",high="red")+
    ggplot2::stat_density2d(geom = "polygon", n=20, ggplot2::aes(fill=..level..))+
    ggplot2::geom_point(ggplot2::aes(color = PROB), alpha = 1/2)+
    ggplot2::ggtitle(player)
  return(shotChart)
}
