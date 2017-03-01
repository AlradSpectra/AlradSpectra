#' A GUI to perform preprocessing, multivariate modeling and prediction using spectroscopic data
#'
#' Alrad Spectra was developed to accomplish tasks such as perform a large range of spectral preprocessing techniques, implement several multivariate calibration methods, which can provide well-fitted and accurate models, statistics assessment, graphical output, validate the models using independent data sets, and predict unknown samples.
#' @author Andre C. Dotto, \email{andrecdot@gmail.com}
#' @author Diego J. Gris, \email{diegojgris@gmail.com}
#' @import gWidgets
#' @export

AlradSpectra <- function() {

  # Create environment for AlradSpectra
  AlradEnv <- new.env()
  
  ###################################################
  ### Structural functions
  ###################################################
  
  # Warning handler
  fwarning     <- function(w)         {dispose(AlradEnv$alert)
                                       gmessage(message = w$message, title = "Warning", icon="warning", parent = window)
                                       stop()
                                       }
  # Error handler
  ferror       <- function(e)         {dispose(AlradEnv$alert)
                                       gmessage(message = e$message, title = "Error", icon="error", parent = window)
                                       stop()
                                       }
  # Makes sure the user really wants to quit Alrad when closing the window
  fconfirmquit <- function(h, ...)    {sure <- gconfirm("Clear Alrad Spectra and quit?", parent=h$obj)
                                       if(as.logical(sure)) {
                                         rm(list = ls(AlradEnv),
                                            envir = AlradEnv) #Remove everything in Alrad Environment
                                         return(FALSE) #Close
                                       } else{
                                           return(TRUE) #Don't close
                                         }
                                       }
  # Clears all data, empties forms and resets Alrad to initial status
  fnew         <- function(...)       {gconfirm("Clear Alrad Spectra and \nstart a new project?",
                                                title="New", icon="warning", parent=window,
                                                handler=function(...) {svalue(file.browse)      <- ""
                                                                       svalue(file.sep)         <- ","
                                                                       svalue(file.dec)         <- "."
                                                                       svalue(spc.start.col)    <- ""
                                                                       svalue(spc.end.col)      <- ""
                                                                       svalue(spc.first)        <- ""
                                                                       svalue(spc.last)         <- ""
                                                                       svalue(soil.var.col)     <- ""
                                                                       svalue(soil.var.nm)      <- ""
                                                                       select.dataset[]         <- ""
                                                                       svalue(pred.file.browse) <- ""
                                                                       svalue(pred.file.sep)    <- ","
                                                                       svalue(pred.file.dec)    <- "."
                                                                       svalue(pred.spc.first)   <- ""
                                                                       svalue(pred.spc.last)    <- ""
                                                                       select.model[]           <- ""
                                                                       svalue(notebook)         <- 1 #Focus on import tab
                                                                       enabled(pp) = FALSE
                                                                       enabled(models) = FALSE
                                                                       enabled(mdl) = FALSE
                                                                       enabled(pred) = FALSE
                                                                       rm(list = ls(AlradEnv),
                                                                          envir = AlradEnv) #Remove everything in Alrad Environment
                                                                       }
                                                )
                                        }
  fopen        <- function(...)        {proj.browse <- gfile("Open File", type="open",
                                                             filter=c("Workspace image (.RData)"="RData"),
                                                             cont = window)
                                        if(!(is.na(proj.browse))) {
                                          rm(list = ls(AlradEnv), envir = AlradEnv) #Remove everything in Alrad Environment
                                          alertop       <- galert("Wait...", title = "Loading Project",
                                                                  delay=10000, parent=notebook)
                                          Sys.sleep(1) #Wait for alert to be shown
                                          load(proj.browse, envir=AlradEnv)
                                          svalue(file.browse)    <- AlradEnv$file.location
                                          svalue(file.sep)       <- AlradEnv$file.separator
                                          svalue(file.dec)       <- AlradEnv$file.decimal
                                          svalue(spc.start.col)  <- AlradEnv$spectra.start.column
                                          svalue(spc.end.col)    <- AlradEnv$spectra.end.column
                                          svalue(spc.first)      <- AlradEnv$spectra.start.number
                                          svalue(spc.last)       <- AlradEnv$spectra.end.number
                                          svalue(soil.var.col)   <- AlradEnv$soil.var.column
                                          svalue(soil.var.nm)    <- AlradEnv$soil.var.name
                                          svalue(notebook)       <- 1 #Focus on import tab
                                          enabled(pp) = TRUE #Enable preprocessing module
                                          enabled(models) = TRUE #Enable modeling module
                                          select.dataset[]       <-  AlradEnv$dataset
                                          svalue(select.dataset) <- "Original"
                                          if(length(AlradEnv$pred.models)!=0) {#If there are models in the loaded data
                                            select.model[]       <- AlradEnv$pred.models
                                            svalue(select.model) <- AlradEnv$pred.models[1]
                                            enabled(pred) = TRUE #Enable prediction module
                                            }
                                          if(exists("spc.pred", envir=AlradEnv)) {#If there is a dataset for prediction
                                            svalue(pred.file.browse) <- AlradEnv$pred.file.location
                                            svalue(pred.file.sep)    <- AlradEnv$pred.file.separator
                                            svalue(pred.file.dec)    <- AlradEnv$pred.file.decimal
                                            svalue(pred.spc.first)   <- AlradEnv$pred.spectra.start.number
                                            svalue(pred.spc.last)    <- AlradEnv$pred.spectra.end.number
                                            enabled(pred.predict) = TRUE #Enable predict group
                                            }
                                          dispose(alertop)
                                          gmessage(message = "Project loaded!", title = "Open Project", parent = window)
                                          }
                                        }
  # Saves current project with all R workspace
  fsave        <- function(...)        {fdialog <- gfile("Save Project", type="save", initialfilename="Project", container=window,
                                                         filter=c("Workspace image (.RData)"="RData"),
                                                         cont = window)
                                        
                                        #If fdialog is not equal to NA, keep running
                                        if(!(is.na(fdialog))) {
                                          AlradEnv$alert <- galert("Wait...", title = "Saving Project", delay=10000, parent=notebook)
                                          Sys.sleep(1) #Wait for alert to be shown
                                          fname <- paste0(fdialog,".RData")
                                          save(list = ls(AlradEnv, all.names = TRUE), file = fname, envir = AlradEnv)
                                          dispose(AlradEnv$alert)
                                          gmessage(message = "Project saved!", title = "Save Project", parent = window)
                                          }
                                        }
  # Handler for quit action. Makes sure the user really wants to quit Alrad.
  fquit        <- function(...)        gconfirm("Clear Alrad Spectra and quit?", icon="warning", parent=window,
                                                handler = function(...) {rm(list = ls(AlradEnv),
                                                                            envir = AlradEnv) #Remove everything in Alrad Environment
                                                                         dispose(window)
                                                                         }
                                                )
  # Creates and shows the window with information about Alrad Spectra
  fabout       <- function(...)       {aboutwin <- gwindow("About Alrad Spectra", width=400, height=300, parent = window)
                                       wingroup <- ggroup(horizontal = FALSE, container = aboutwin)
                                       gimage(system.file("images","AlradLogo.png", package="AlradSpectra"), container = wingroup)
                                       glabel(paste0("A GUI to perform preprocessing, multivariate\n",
                                                     "modeling and prediction using spectral data\n\n",
                                                     "Developed by researchers at\n",
                                                     "Federal University of Santa Maria and\n",
                                                     "Federal University of Santa Catarina, Brazil.\n\n",
                                                     "Authors and contributors:\n",
                                                     "<b>A</b>","ndre Dotto\n",
                                                     "<b>L</b>","uis Ruiz\n",
                                                     "<b>R</b>","icardo Dalmolin\n",
                                                     "<b>A</b>","lexandre ten Caten\n",
                                                     "<b>D</b>","iego Gris\n",
                                                     "\n",
                                                     "For further information:\n",
                                                     "The paper will be available soon.\n",
                                                     "Alrad Spectra source code is available at\n",
                                                     "<i>github.com/AlradSpectra/AlradSpectra</i>",
                                                     sep="", collapse=""),
                                              markup = TRUE, container = wingroup
                                              )
                                       }
  # Opens up a dialog to search for file to be imported
  fbrowse      <- function(h, ...)    {svalue(h) <- gfile("Open File", type="open",
                                                          filter= list("Delimited Files (*.txt, *.csv)" = list(patterns = c("*.txt", "*.csv")),
                                                                       "All files" = list(patterns = c("*"))), cont = window)
                                       }
  # Imports csv file to Alrad environment and sets variables used afterwards
  fimport      <- function(...)       {AlradEnv$alert <- galert("Wait...", title = "Importing File", delay=10000, parent=notebook)
                                       tryCatch({AlradEnv$file.location        <- svalue(file.browse)
                                                 AlradEnv$file.separator       <- svalue(file.sep)
                                                 AlradEnv$file.decimal         <- svalue(file.dec)
                                                 if(AlradEnv$file.separator=="") {
                                                   AlradEnv$alldata <- read.delim(file = AlradEnv$file.location,
                                                                                  header = as.logical(svalue(file.header)),
                                                                                  dec = AlradEnv$file.decimal)
                                                 } else {
                                                   AlradEnv$alldata <- read.table(file = AlradEnv$file.location,
                                                                                  header = as.logical(svalue(file.header)),
                                                                                  sep = AlradEnv$file.separator,
                                                                                  dec = AlradEnv$file.decimal)
                                                 }
                                                 AlradEnv$spectra.start.column <- as.numeric(svalue(spc.start.col))
                                                 AlradEnv$spectra.end.column   <- as.numeric(svalue(spc.end.col))
                                                 AlradEnv$spectra.start.number <- as.numeric(svalue(spc.first))
                                                 AlradEnv$spectra.end.number   <- as.numeric(svalue(spc.last))
                                                 AlradEnv$soil.var.column      <- as.numeric(svalue(soil.var.col))
                                                 AlradEnv$soil.var.name        <- svalue(soil.var.nm)
                                                 fonlyspectra() #Create dataframe with spectra only
                                                 AlradEnv$dataset              <- c("Original")
                                                 select.dataset[]              <-  AlradEnv$dataset
                                                 svalue(select.dataset)        <- "Original"
                                                 AlradEnv$pred.models          <- c()
                                                 },
                                                 warning = function(w) fwarning(w),
                                                 error =  function(e) ferror(e)
                                                 )
                                      enabled(pp) = TRUE
                                      enabled(models) = TRUE
                                      dispose(AlradEnv$alert)
                                      gmessage(message = "Import successful!", title = "File import", parent = window)
                                      }
  # Create dataframe that contains only the spectral data to be used for preprocessing
  fonlyspectra <- function(...)       {spc <- AlradEnv$alldata[,AlradEnv$spectra.start.column:AlradEnv$spectra.end.column]
                                       colnames(spc) <- c(AlradEnv$spectra.start.number:AlradEnv$spectra.end.number)
                                       AlradEnv$Original <- spc
                                       }
  # Opens up a window to display imported data in tabular form
  fview        <- function(h, w, ...) {gtable(h, cont = gwindow("View data", width = w, height = 200, parent = window))}
  # Plots spectral data
  fplot        <- function(h, s, e,
                           ylab="Reflectance", ...) {plotwin <- gwindow("Plot", width = 800, height = 600, parent = window)
                                                     wingroup <- ggroup(horizontal=FALSE, cont=plotwin)
                                                     ggraphics(cont = wingroup, no_popup=TRUE)
                                                     Sys.sleep(1) #Wait for window creation before trying to plot to avoid errors
                                                     gbutton("Save plot", cont=wingroup, handler = function(...) fsaveplot(800, 600))
                                                     graphics::matplot(colnames(h), t(h), xlim = c(s, e),
                                                                       type = "l",
                                                                       xlab = "Wavelength (nm)",
                                                                       ylab = ylab)
                                                     }
  # Export plot as png graphics file
  fsaveplot    <- function(w, h,...)  {fdialog <- gfile("Save File", type="save", initialfilename="Plot", container=window,
                                                        filter=c("Portable Network Graphics (.png)"="png"))
                                       #If fdialog is not equal to NA, keep running
                                       if(!(is.na(fdialog))) {fname <- paste0(fdialog,".png")
                                                              dev.copy(png, fname, width=w, height=h, res=100, antialias = "cleartype")
                                                              dev.off() #Close graphics device
                                                              }
                                       }
  # View Y variable descriptive statistics
  fdescy       <- function(...)       {AlradEnv$alert <- galert("Wait...", title = "Descriptive Statistics",
                                                                delay=10000, parent=notebook)
                                       descstats  <- fitdistrplus::descdist(AlradEnv$alldata[,AlradEnv$soil.var.column], graph=F)
                                       descnames  <- rbind("Obs","Min","Max","Mean","Median","Std Dev","Skewness","Kurtosis")
                                       descvalues <- rbind(nrow(AlradEnv$alldata),
                                                           round(descstats$min,2),
                                                           round(descstats$max,2),
                                                           round(descstats$mean,2),
                                                           round(descstats$median,2),
                                                           round(descstats$sd,2),
                                                           round(descstats$skewness,2),
                                                           round(descstats$kurtosis,2))
                                       desctable  <- data.frame("Parameter"=descnames,"Value"=descvalues)
                                       dispose(AlradEnv$alert)
                                       descwin    <- gwindow("Descriptive statistics", width=300, height=300, parent=window)
                                       desc.lyt   <- glayout(horizontal=FALSE, container=descwin)
                                       desc.lyt[1,1,expand=TRUE]  <- gtable(as.data.frame(desctable), cont = desc.lyt)
                                       desc.lyt[2,1,expand=FALSE] <- gbutton("Save results", cont=desc.lyt,
                                                                            anchor=c(0,-1),
                                                                            handler=function(...) fsaveresults(desctable))
                                       }
  # Plots y histogram
  fhist        <- function(...)       {plotwin  <- gwindow("Histogram", width = 500, height = 500, parent = window)
                                       wingroup <- ggroup(horizontal=FALSE, cont=plotwin)
                                       ggraphics(cont = wingroup, no_popup=TRUE)
                                       Sys.sleep(1) #Wait for window creation before trying to plot to avoid errors
                                       gbutton("Save plot", cont=wingroup, handler = function(...) fsaveplot(500, 450))
                                       histo    <- ggplot2::ggplot(AlradEnv$alldata,
                                                                   ggplot2::aes(x=AlradEnv$alldata[,AlradEnv$soil.var.column])) + 
                                                   ggplot2::geom_histogram(ggplot2::aes(fill = ..count..), binwidth = 0.5) +
                                                   ggplot2::scale_fill_gradient("Frequency", low = "darkolivegreen2",
                                                                                high = "dodgerblue3") +
                                                   ggplot2::labs(x = AlradEnv$soil.var.name, y = "Frequency") +
                                                   ggplot2::guides(fill=FALSE)
                                       print(histo)
                                       }
  # Export preprocessed spectra as csv file
  fsavespectra <- function(h, ...)    {fdialog <- gfile("Save File", type="save", initialfilename="Output", container=window,
                                                        filter=c("Comma Separated Values (.csv)"="csv"))
                                       #If fdialog is not equal to NA, keep running        
                                       if(!(is.na(fdialog))) {fname      <- paste0(fdialog,".csv")
                                                              spectrum   <- seq(AlradEnv$spectra.start.column,
                                                                                AlradEnv$spectra.end.column)
                                                              exportdata <- cbind(AlradEnv$alldata[,-spectrum], h)
                                                              write.csv(exportdata, row.names = FALSE, file = fname)
                                                              }
                                       }
  # Adds preprocessing to combobox in Model tab only if it is not already there
  faddtodtset  <- function(h, ...) {present <- is.element(h, AlradEnv$dataset)
                                       if(present==FALSE)
                                         AlradEnv$dataset <- c(AlradEnv$dataset, h)
                                         select.dataset[] <- AlradEnv$dataset
                                       }
  # Splits dataset in training and validaton sets
  fsplit       <- function(...)       {#set.seed(1)
                                       x           <- eval(parse(text = paste0("AlradEnv$", svalue(select.dataset)))) #Get selected dataset
                                       x           <- cbind(x, AlradEnv$alldata[AlradEnv$soil.var.column]) #Join spectral data and soil property
                                       indices     <- sample(1:nrow(x), size = (svalue(split.val)/100)*nrow(x)) #Random sampling
                                       t           <- x[-indices,] #Training set
                                       v           <- x[ indices,] #Validation set
                                       colnames(t) <- paste("X", colnames(t), sep = "") #Add X before wavelength
                                       colnames(v) <- paste("X", colnames(v), sep = "") #Add X before wavelength
                                       AlradEnv$Train    <- t
                                       AlradEnv$Val      <- v
                                       AlradEnv$last.col <- ncol(AlradEnv$Train) #Get position of last column (soil variable)
                                       #Create formula for models
                                       AlradEnv$form.mdl <- as.formula(paste(colnames(AlradEnv$Train[AlradEnv$last.col]),"~",
                                                                       paste(names(AlradEnv$Train)[c(1:AlradEnv$last.col-1)],
                                                                             collapse="+"), collapse=""))
                                       enabled(mdl) = TRUE #Enable models module
                                       enabled(homo.button) = TRUE #Enable homogeneity test button
                                       enabled(desc.button) = TRUE #Enable descriptive stats button
                                       enabled(boxplot.button) = TRUE #Enable boxplot button
                                       gmessage(paste("Number of training samples:", nrow(AlradEnv$Train),
                                                      "\n\nNumber of validation samples:", nrow(AlradEnv$Val)),
                                                title = "Split", parent = window)
                                       }
  # Homogeneity of variance test
  fhomo        <- function(...)       {AlradEnv$alert <- galert("Wait...", title = "Levene's test", delay=10000, parent=notebook)
                                       categories <- as.factor(c(rep(1,length(AlradEnv$Train[,AlradEnv$last.col])),
                                                                 rep(2,length(AlradEnv$Val[,AlradEnv$last.col]))))
                                       homog.test <- car::leveneTest(AlradEnv$alldata[,AlradEnv$soil.var.column]~categories)
                                       if(homog.test$`Pr(>F)`[1]>0.05) {
                                         lev.res <- c("is", 'are')
                                       } else {
                                         lev.res <- c("is not", 'are not')
                                         }
                                       dispose(AlradEnv$alert)
                                       gmessage(paste("Levene's Test for Homogeneity of Variance",
                                                      "\n\nIf the P-value is greater than 0.05 (significance level), \nthe null hypothesis",
                                                      "is not rejected and it is concluded \nthat there is no significant difference",
                                                      "between \nthe variances of the two groups.",
                                                      "\n\nSignificance level = 0.05",
                                                      "\nDegrees of freedom =",homog.test$Df[2],
                                                      "\nF-value =", round(homog.test$`F value`[1], 3),
                                                      "\nP-value =", round(homog.test$`Pr(>F)`[1], 3),
                                                      "\n\nTest interpretation:",
                                                      "\nThe P-value",lev.res[1],"greater than 0.05.",
                                                      "\nTraining and Validation groups",lev.res[2],"homogeneous."),
                                                title = "Homogeneity of variance test", parent = window)
                                       }
  # View train and validation descriptive statistics
  fdesc        <- function(...)       {AlradEnv$alert <- galert("Wait...", title = "Descriptive Statistics", delay=10000, parent=notebook)
                                       trainds    <- fitdistrplus::descdist(AlradEnv$Train[,AlradEnv$last.col], graph=F)
                                       valds      <- fitdistrplus::descdist(AlradEnv$Val[,AlradEnv$last.col], graph=F)
                                       descnames  <- rbind("Obs","Min","Max","Mean","Median","Std Dev","Skewness","Kurtosis")
                                       descvalues <- rbind(c(nrow(AlradEnv$Train), nrow(AlradEnv$Val)),
                                                           c(round(trainds$min,2), round(valds$min,2)),
                                                           c(round(trainds$max,2), round(valds$max,2)),
                                                           c(round(trainds$mean,2), round(valds$mean,2)),
                                                           c(round(trainds$median,2), round(valds$median,2)),
                                                           c(round(trainds$sd,2), round(valds$sd,2)),
                                                           c(round(trainds$skewness,2), round(valds$skewness,2)),
                                                           c(round(trainds$kurtosis,2), round(valds$kurtosis,2)))
                                       desctable  <- data.frame("Parameter"=descnames,"Training"=descvalues[,1],
                                                                "Validation"=descvalues[,2])
                                       dispose(AlradEnv$alert)
                                       descwin    <- gwindow("Descriptive statistics", width=300, height=300, parent=window)
                                       desc.lyt   <- glayout(horizontal=FALSE, container=descwin)
                                       desc.lyt[1,1,expand=TRUE] <- gtable(as.data.frame(desctable), cont = desc.lyt)
                                       desc.lyt[2,1,expand=FALSE] <- gbutton("Save results", cont=desc.lyt,
                                                                             anchor=c(0,-1),
                                                                             handler=function(...) fsaveresults(desctable))
                                       }
  # Boxplot of Y variable
  fboxplot     <- function(...)       {plotwin    <- gwindow("Plot", width = 400, height = 500, parent = window)
                                       wingroup   <- ggroup(horizontal=FALSE, cont=plotwin)
                                       ggraphics(cont = wingroup, no_popup=TRUE)
                                       Sys.sleep(1) #Wait for window creation before trying to plot to avoid errors
                                       gbutton("Save plot", cont=wingroup, handler = function(...) fsaveplot(400, 450))
                                       categories <- as.factor(c(rep("Training", length(AlradEnv$Train[,AlradEnv$last.col])),
                                                                 rep("Validation", length(AlradEnv$Val[,AlradEnv$last.col]))))
                                       boxpl      <- ggplot2::ggplot(AlradEnv$alldata, ggplot2::aes(x=categories,
                                                                                                    y=AlradEnv$alldata[,AlradEnv$soil.var.column],
                                                                                                    fill=categories)) + 
                                                     ggplot2::geom_boxplot(notch = TRUE) +
                                                     ggplot2::guides(fill=FALSE) +
                                                     ggplot2::labs(x = "", y = AlradEnv$soil.var.name) +
                                                     ggplot2::theme(axis.text = ggplot2::element_text(size=12),
                                                                    axis.title = ggplot2::element_text(size=13))
                                       print(boxpl)
                                       }
  # Disables models module and homo, desc stats and boxplot buttons when dataset or validation size is changed
  fchangesplit <- function(h, ...)    {enabled(mdl) = FALSE
                                       enabled(homo.button) = FALSE
                                       enabled(desc.button) = FALSE
                                       enabled(boxplot.button) = FALSE
                                       }
  # Export model or prediction results
  fsaveresults <- function(h, ...)    {fdialog <- gfile("Save File", type="save", initialfilename="Output", container=window,
                                                        filter=c("Comma Separated Values (.csv)"="csv"))
                                       #If fdialog is not equal to NA, keep running        
                                       if(!(is.na(fdialog))) {fname <- paste0(fdialog,".csv")
                                                              write.csv(h, row.names = FALSE, file = fname)
                                                              }
                                       }
  # Compute model prediction errors
  fstats       <- function(y, yhat)   {n              <- length(y)
                                       r              <- cor(y, yhat)
                                       lmy            <- lm(y~yhat)
                                       a              <- coefficients(lmy)[1]
                                       b              <- coefficients(lmy)[2]
                                       r2             <- summary(lmy)$r.squared[1]
                                       bias           <- mean(yhat)-mean(y)
                                       msd            <- sum((yhat-y)^2)/n
                                       rmse           <- sqrt(msd)
                                       msd.c          <- sum((yhat-bias-y)^2)/n
                                       rmse.c         <- sqrt(msd.c)
                                       sb             <- (mean(yhat)-mean(y))^2
                                       nu             <- ((1-b)^2)*(var(yhat)*((n-1)/n))
                                       lc             <- (1-r^2)*(var(y)*((n-1)/n))
                                       rpd            <- sd(y)/rmse
                                       q1             <- quantile(y)[2]
                                       q3             <- quantile(y)[4]
                                       rpiq           <- (q3-q1)/rmse
                                       error.s        <- round(c(r2, rmse, rpiq ),2)
                                       names(error.s) <- c("R-squared", "RMSE", "RPIQ")
                                       return(error.s)
                                       }
  # Get model accuracy and display in tabular form
  fmdl.stats    <- function(t, v, ...) {t.stats.name <- paste0(deparse(substitute(t)), ".stats") #Create training stats table name
                                        v.stats.name <- paste0(deparse(substitute(v)), ".stats") #Create validation stats table name
                                        assign(t.stats.name, fstats(t[,1], t[,2]), envir = AlradEnv) #Compute training stats
                                        assign(v.stats.name, fstats(v[,1], v[,2]), envir = AlradEnv) #Compute validation stats
                                        results      <- rbind(get(t.stats.name, envir = AlradEnv),
                                                              get(v.stats.name, envir = AlradEnv)) #Merge training and validation stats
                                        Set          <- c("Training", "Validation") #Titles for prediction statistics table
                                        res.table    <- cbind(Set, results) #Create prediction statistics table
                                        statswin     <- gwindow("Prediction statistics", width=300, height=150, parent=window)
                                        stats.lyt    <- glayout(horizontal=FALSE, container=statswin)
                                        stats.lyt[1,1,expand=TRUE] <- gtable(res.table, cont = stats.lyt)
                                        stats.lyt[2,1,expand=FALSE] <- gbutton("Save results", cont=stats.lyt,
                                                                              anchor=c(0,-1),
                                                                              handler=function(...) fsaveresults(res.table))
                                        }
  # Plot measured vs. predicted
  fmdl.plot.res <- function(t, v, ...) {plotwin      <- gwindow("Plot", width = 1000, height = 400, parent = window)
                                        wingroup     <- ggroup(horizontal=FALSE, cont=plotwin)
                                        ggraphics(cont = wingroup, no_popup=TRUE)
                                        t.stats.name <- paste0(deparse(substitute(t)), ".stats") #Create training stats table name
                                        v.stats.name <- paste0(deparse(substitute(v)), ".stats") #Create validation stats table name
                                        assign(t.stats.name, fstats(t[,1], t[,2]), envir = AlradEnv) #Compute training stats
                                        assign(v.stats.name, fstats(v[,1], v[,2]), envir = AlradEnv) #Compute validation stats
                                        train.plot   <- ggplot2::ggplot(t, ggplot2::aes(x=t[,1], y=t[,2])) +
                                                        ggplot2::geom_point(shape=19) +
                                                        ggplot2::ggtitle("Training") +
                                                        ggplot2::labs(x=NULL, y=paste(AlradEnv$soil.var.name, "Predicted")) + 
                                                        ggplot2::theme(plot.title = ggplot2::element_text(size=12, hjust=0.5)) +
                                                        ggplot2::theme(axis.title = ggplot2::element_text(size=12, hjust=0.5)) + 
                                                        ggplot2::xlim(0, max(t)) +
                                                        ggplot2::ylim(0, max(t)) +
                                                        ggplot2::geom_abline(intercept = 0, slope = 1) +
                                                        ggplot2::annotate("text", x=max(v)*0.05, y=seq(max(t)*0.95, max(t)*0.85,-max(t)*0.05), hjust = 0,
                                                                          label = paste(names(get(t.stats.name, envir = AlradEnv)),"=",
                                                                                        get(t.stats.name, envir = AlradEnv)))
                                        val.plot     <- ggplot2::ggplot(v, ggplot2::aes(x=v[,1], y=v[,2])) +
                                                        ggplot2::geom_point(shape=19) +
                                                        ggplot2::ggtitle("Validation") +
                                                        ggplot2::labs(x=NULL, y=NULL) + 
                                                        ggplot2::theme(plot.title = ggplot2::element_text(size=12, hjust=0.5)) +
                                                        ggplot2::theme(axis.title = ggplot2::element_text(size=12, hjust=0.5)) + 
                                                        ggplot2::xlim(0, max(v)) +
                                                        ggplot2::ylim(0, max(v)) +
                                                        ggplot2::geom_abline(intercept = 0, slope = 1) +
                                                        ggplot2::annotate("text", x=max(v)*0.05, y=seq(max(v)*0.95,max(v)*0.85,-max(v)*0.05), hjust = 0,
                                                                          label = paste(names(get(v.stats.name, envir = AlradEnv)),"=",
                                                                                        get(v.stats.name, envir = AlradEnv)))
                                        Sys.sleep(1)
                                        gbutton("Save plot", cont = wingroup, handler = function(...) fsaveplot(800, 400))
                                        gridExtra::grid.arrange(train.plot, val.plot, ncol=2, bottom=paste(AlradEnv$soil.var.name, "Measured"))
                                        }
  # Plot RMSE of PLSR components
  fpls.plot.imp <- function(h, ...)    {plotwin   <- gwindow("Plot", width = 400, height = 400, parent=window)
                                        wingroup  <- ggroup(horizontal=FALSE, cont=plotwin)
                                        ggraphics(cont = wingroup, no_popup=TRUE)
                                        Sys.sleep(1)
                                        gbutton("Save plot", cont = wingroup, handler = function(...) fsaveplot(400, 500))
                                        comp.plot <- ggplot2::ggplot(h) +
                                                     ggplot2::labs(list(x="PLS Components", y="RMSE"))
                                        print(comp.plot)
                                        }
  # Plot variables importance
  fmdl.plot.imp <- function(h, ...)    {plotwin   <- gwindow("Plot", width = 900, height = 300, parent=window)
                                        wingroup  <- ggroup(horizontal=FALSE, cont=plotwin)
                                        ggraphics(cont = wingroup, no_popup=TRUE)
                                        Sys.sleep(1)
                                        gbutton("Save plot", cont = wingroup, handler = function(...) fsaveplot(900, 300))
                                        var.imp   <- caret::varImp(h)$importance
                                        spc.st    <- as.numeric(substring(row.names(var.imp)[1], 2))
                                        spc.lt    <- as.numeric(substring(row.names(var.imp)[length(row.names(var.imp))], 2))
                                        row.names(var.imp) <- c(spc.st:spc.lt)
                                        comp.plot <- ggplot2::ggplot(var.imp, ggplot2::aes(x=c(spc.st:spc.lt), y=var.imp[,1])) +
                                                     ggplot2::scale_x_continuous(breaks = floor(seq(spc.st, spc.lt, (spc.lt-spc.st)/20))) +
                                                     ggplot2::geom_point(pch=20) +
                                                     ggplot2::labs(list(x="Variables", y="Importance"))
                                        print(comp.plot)
                                        }
  # Adds preprocessing to combobox in Model tab only if it is not already there
  faddtomodels  <- function(h, ...) {present <- is.element(h, AlradEnv$pred.models)
                                     if(present==FALSE) {
                                       if(length(AlradEnv$pred.models)==0) { #Special case when the list is empty (first model is being added)
                                         AlradEnv$pred.models <- c(h)
                                         select.model[]       <- AlradEnv$pred.models
                                         svalue(select.model) <-  h
                                       } else {
                                         AlradEnv$pred.models <- c(AlradEnv$pred.models, h)
                                         select.model[]       <- AlradEnv$pred.models
                                         }
                                       }
                                     }
  # Imports csv file to Alrad environment for prediction
  fimport.pred  <- function(...)       {AlradEnv$alert <- galert("Wait...", title = "Importing File", delay=10000, parent=notebook)
                                        tryCatch({AlradEnv$pred.file.location  <- svalue(pred.file.browse)
                                                  AlradEnv$pred.file.separator <- svalue(pred.file.sep)
                                                  AlradEnv$pred.file.decimal   <- svalue(pred.file.dec)
                                                  if(AlradEnv$pred.file.separator=="") {
                                                    spc <- read.delim(file = AlradEnv$pred.file.location,
                                                                      header = as.logical(svalue(pred.file.header)),
                                                                      dec = AlradEnv$pred.file.decimal)
                                                    } else {
                                                      spc <- read.table(file = AlradEnv$pred.file.location,
                                                                        header = as.logical(svalue(pred.file.header)),
                                                                        sep = AlradEnv$pred.file.separator,
                                                                        dec = AlradEnv$pred.file.decimal)
                                                      }
                                                  AlradEnv$pred.spectra.start.number <- as.numeric(svalue(pred.spc.first))
                                                  AlradEnv$pred.spectra.end.number   <- as.numeric(svalue(pred.spc.last))
                                                  colnames(spc)     <- c(AlradEnv$pred.spectra.start.number:AlradEnv$pred.spectra.end.number)
                                                  AlradEnv$spc.pred <- spc
                                                  },
                                                 warning = function(w) fwarning(w),
                                                 error =  function(e) ferror(e)
                                                 )
                                        enabled(pred.predict) = TRUE #Enable predict group
                                        dispose(AlradEnv$alert)
                                        gmessage(message = "Import successful!", title = "File import", parent = window)
                                        }
  # Predict soil property based on a new spectra
  fpredict      <- function(...)       {AlradEnv$alert <- galert("Wait...", title = "Importing File", delay=10000, parent=notebook)
                                        tryCatch({colnames(AlradEnv$spc.pred) <- paste("X", colnames(AlradEnv$spc.pred),
                                                                                       sep = "") #Add X before wavelength
                                                  mdl <- eval(parse(text = paste0("AlradEnv$", svalue(select.model)))) #Get selected model
                                                  if(svalue(select.model)=="PLSR") {#PLSR special case
                                                    pls.pred <-  data.frame(predict(AlradEnv$PLSR, newdata=AlradEnv$spc.pred))
                                                    AlradEnv$prediction <- data.frame(ID=row.names(AlradEnv$spc.pred),
                                                                                      Predicted=pls.pred[,ncol(pls.pred)])
                                                  } else {
                                                    AlradEnv$prediction <- data.frame(ID=row.names(AlradEnv$spc.pred),
                                                                                      Predicted=predict(mdl, newdata=AlradEnv$spc.pred))
                                                    }
                                                  },
                                                 warning = function(w) fwarning(w),
                                                 error =  function(e) ferror(e)
                                                 )
                                        dispose(AlradEnv$alert)
                                        gmessage(message = "Done!", title = "Prediction", parent = window)
                                        }
  
  ###################################################
  ### Preprocessing functions
  ###################################################
  
  # Smoothing
  fnrm         <- function(...) {AlradEnv$alert <- galert("Wait...", title = "Smoothing", delay=10000, parent=notebook)
                                 tryCatch(
                                          {AlradEnv$Smoothing  <- prospectr::movav(AlradEnv$Original, w = as.numeric(svalue(number.smooth)))
                                           faddtodtset("Smoothing")
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(AlradEnv$alert)
                                 gmessage(message = "Done!", title = "Smoothing", icon = "info", parent = window)
                                 }
  # Binning
  fbin         <- function(...) {AlradEnv$alert <- galert("Wait...", title = "Binning", delay=10000, parent=notebook)
                                 tryCatch(
                                          {AlradEnv$Binning <- prospectr::binning(AlradEnv$Original, bin.size = as.numeric(svalue(bin.number)))
                                           faddtodtset("Binning")
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(AlradEnv$alert)
                                 gmessage(message = "Done!", title = "Binning", icon = "info", parent = window)
                                 }
  # Absorbance
  fabs         <- function(...) {AlradEnv$alert <- galert("Wait...", title = "Absorbance", delay=10000, parent=notebook)
                                 tryCatch(
                                          {AlradEnv$Absorbance <- log10(1/AlradEnv$Original)
                                           faddtodtset("Absorbance")
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(AlradEnv$alert)
                                 gmessage(message = "Done!", title = "Absorbance", icon = "info", parent = window)
                                 }
  # Detrend
  fdet         <- function(...) {AlradEnv$alert <- galert("Wait...", title = "Detrend", delay=10000, parent=notebook)
                                 tryCatch(
                                          {AlradEnv$Detrend <- prospectr::detrend(X = AlradEnv$Original,
                                                                                  wav = as.numeric(colnames(AlradEnv$Original)))
                                           faddtodtset("Detrend")
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(AlradEnv$alert)
                                 gmessage(message = "Done!", title = "Detrend", icon = "info", parent = window)
                                 }
  # Continuum Removal
  fcrm         <- function(...) {AlradEnv$alert <- galert("Wait...", title = "Continuum Removal", delay=10000, parent=notebook)
                                 tryCatch(
                                          {ContRem <- prospectr::continuumRemoval(X=AlradEnv$Original,
                                                                                  wav = as.numeric(colnames(AlradEnv$Original)),
                                                                                  type = "R", interpol="linear", method="division")
                                          AlradEnv$ContinuumRemoval <- ContRem[,c(-1,-ncol(ContRem))]
                                           faddtodtset("ContinuumRemoval")
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(AlradEnv$alert)
                                 gmessage(message = "Done!", title = "Continuum Removal", icon = "info", parent = window)
                                 }
  # Savitzky-Golay Derivative
  fsgd         <- function(...) {AlradEnv$alert <- galert("Wait...", title = "Savitzky-Golay Derivative", delay=10000, parent=notebook)
                                 tryCatch(
                                          {AlradEnv$SavitzkyGolayDerivative <- prospectr::savitzkyGolay(AlradEnv$Original,
                                                                                                        p = as.numeric(svalue(sgd.poly)),
                                                                                                        w = as.numeric(svalue(sgd.smooth)),
                                                                                                        m = as.numeric(svalue(sgd.deriv)))
                                           faddtodtset("SavitzkyGolayDerivative")
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(AlradEnv$alert)
                                 gmessage(message = "Done!", title = "Savitzky-Golay Derivative", icon = "info", parent = window)
                                 }
  # Standard Normal Variate
  fsnv         <- function(...) {AlradEnv$alert <- galert("Wait...", title = "SNV", delay=10000, parent=notebook)
                                 tryCatch(
                                          {AlradEnv$SNV <- prospectr::standardNormalVariate(X = AlradEnv$Original)
                                           faddtodtset("SNV")
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(AlradEnv$alert)
                                 gmessage(message = "Done!", title = "SNV", icon = "info", parent = window)
                                 }
  # Multiplicative Scatter Correction
  fmsc         <- function(...) {AlradEnv$alert <- galert("Wait...", title = "MSC", delay=10000, parent=notebook)
                                 tryCatch(
                                          {AlradEnv$MSC <- pls::msc(as.matrix(AlradEnv$Original))
                                           faddtodtset("MSC")
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(AlradEnv$alert)
                                 gmessage(message = "Done!", title = "MSC", icon = "info", parent = window)
                                 }
  # Normalization
  fnor         <- function(...) {AlradEnv$alert <- galert("Wait...", title = "Normalization", delay=10000, parent=notebook)
                                 tryCatch(
                                          {AlradEnv$Normalization <- clusterSim::data.Normalization(AlradEnv$Original,
                                                                                            type = sub(":.*$","", svalue(nor.type)),
                                                                                            normalization = "row")
                                           faddtodtset("Normalization")
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(AlradEnv$alert)
                                 gmessage("Done!", title = "Normalization", icon = "info",  parent = window)
                                 }
  
  ###################################################
  ### Modeling functions
  ###################################################
  
  # MLR
  fmlr        <- function(...)  {AlradEnv$alert <- galert("Wait... \nThis may take a few minutes!", title = "MLR model",
                                                          delay=10000, parent=notebook)
                                 Sys.sleep(1) #Wait for AlradEnv$alert to be shown
                                 tryCatch(
                                          {form.mlr  <- as.formula(paste(colnames(AlradEnv$Train[AlradEnv$last.col]),"~",
                                                                         paste(names(AlradEnv$Train)[c(seq(1,AlradEnv$last.col-1,
                                                                                                           by=svalue(mlr.band.interval)))],
                                                                               collapse="+"),collapse=""))
                                           bootctrl.mlr <- caret::trainControl(method  <- svalue(mlr.resampling),
                                                                               number  <- ifelse(grepl("cv", method), svalue(mlr.kfold),
                                                                                                 (svalue(pls.kfold)+10)),
                                                                               repeats <- ifelse(grepl("cv", method), svalue(mlr.folds),
                                                                                                 number)
                                                                               )
                                           AlradEnv$mlr.test  <- train(form.mlr, data = AlradEnv$Train, method = "glmStepAIC", 
                                                                       trControl = bootctrl.mlr)
                                           AlradEnv$mlr.train <- data.frame(AlradEnv$Train[AlradEnv$last.col],
                                                                            Predicted=predict(AlradEnv$mlr.test)
                                           AlradEnv$mlr.val   <- data.frame(AlradEnv$Val[AlradEnv$last.col],
                                                                            Predicted=predict(AlradEnv$mlr.test, newdata=AlradEnv$Val))
                                           faddtomodels("MLR")
                                           enabled(pred) = TRUE #Enable prediction module
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(AlradEnv$alert)
                                 gmessage("MLR model done", title = "MLR model", parent = window)
                                 }
  # PLSR
  fpls        <- function(...) {AlradEnv$alert <- galert("Wait... \nThis may take a few minutes!", title = "PLSR model",
                                                         delay=10000, parent=notebook)
                                Sys.sleep(1)
                                tryCatch(
                                         {bootctrl.pls <- caret::trainControl(method  <- svalue(pls.resampling),
                                                                              number  <- ifelse(grepl("cv", method), svalue(pls.kfold),
                                                                                                (svalue(pls.kfold)+10)),
                                                                              repeats <- ifelse(grepl("cv", method), svalue(pls.folds),
                                                                                                number)
                                                                              )
                                          Grid               <-  expand.grid(.ncomp = seq(1,svalue(pls.comp), 1))
                                          AlradEnv$pls.test  <- caret::train(AlradEnv$form.mdl, data = AlradEnv$Train, method = 'pls',
                                                                     trControl = bootctrl.pls, tuneGrid = Grid)
                                          AlradEnv$PLSR      <- pls::plsr(AlradEnv$form.mdl, data = AlradEnv$Train,
                                                                          ncomp = AlradEnv$pls.test$bestTune$ncomp)
                                          t.pred             <-  data.frame(AlradEnv$PLSR$fitted.values)
                                          v.pred             <-  data.frame(predict(AlradEnv$PLSR, newdata=AlradEnv$Val))
                                          AlradEnv$pls.train <- data.frame(AlradEnv$Train[AlradEnv$last.col], Predicted=t.pred[,ncol(t.pred)])
                                          AlradEnv$pls.val   <- data.frame(AlradEnv$Val[AlradEnv$last.col], Predicted=v.pred[,ncol(v.pred)])
                                          faddtomodels("PLSR")
                                          enabled(pred) = TRUE #Enable prediction module
                                          },
                                         warning = function(w) fwarning(w),
                                         error =  function(e) ferror(e)
                                         )
                                dispose(AlradEnv$alert)
                                gmessage("PLSR model done", title = "PLSR model", parent = window)
                                }
  # SVM
  fsvm        <- function(...) {AlradEnv$alert <- galert("Wait... \nThis may take a few minutes! ", title = "SVM model", delay=10000, parent=notebook)
                                Sys.sleep(1)
                                tryCatch(
                                         {AlradEnv$bootctrl.svm <- caret::trainControl(method <- svalue(svm.resampling))
                                          if (svalue(svm.kernel, index=TRUE)==1) fsvmlinear()
                                          if (svalue(svm.kernel, index=TRUE)==2) fsvmradial()
                                          AlradEnv$svm.train    <- data.frame(AlradEnv$Train[AlradEnv$last.col], Predicted=AlradEnv$SVM$fitted)
                                          AlradEnv$svm.val      <- data.frame(AlradEnv$Val[AlradEnv$last.col],
                                                                              Predicted=predict(AlradEnv$SVM, newdata=AlradEnv$Val))
                                          faddtomodels("SVM")
                                          enabled(pred) = TRUE #Enable prediction module
                                          },
                                         warning = function(w) fwarning(w),
                                         error =  function(e) ferror(e)
                                         )
                                dispose(AlradEnv$alert)
                                gmessage("SVM model done", title = "SVM model", parent = window)
                                }
  fsvmlinear  <- function(...) {Grid              <- expand.grid(.C = seq(1,16,5))
                                AlradEnv$svm.test <- caret::train(AlradEnv$form.mdl, data = AlradEnv$Train, method = "svmLinear",
                                                                  trControl = AlradEnv$bootctrl.svm, tuneGrid = Grid)
                                AlradEnv$SVM      <- e1071::svm(AlradEnv$form.mdl, data=AlradEnv$Train, kernel="linear", type ="eps",
                                                                cost=AlradEnv$svm.test$bestTune$C)
                                }
  fsvmradial  <- function(...) {Grid              <- expand.grid(.sigma = seq(0.000001,0.1,0.01), .C = seq(1,16,5))
                                AlradEnv$svm.test <- caret::train(AlradEnv$form.mdl, data = AlradEnv$Train, method = "svmRadial",
                                                                  trControl = AlradEnv$bootctrl.svm, tuneGrid = Grid)
                                AlradEnv$SVM      <- e1071::svm(AlradEnv$form.mdl, data=AlradEnv$Train, kernel="radial", type ="eps",
                                                                gamma=AlradEnv$svm.test$bestTune$sigma, cost=AlradEnv$svm.test$bestTune$C)
                                }
  # RF
  frf         <- function(...) {AlradEnv$alert <- galert("Wait... \nThis may take a few minutes! ", title = "RF model", 
                                                         delay=10000, parent=notebook)
                                Sys.sleep(1)
                                tryCatch(
                                         {bootControl <- caret::trainControl(method <- svalue(rf.resampling))
                                          Grid        <- expand.grid(.mtry = seq(svalue(rf.mtry)/5,svalue(rf.mtry),svalue(rf.mtry)/5))
                                          AlradEnv$rf.test     <- caret::train(AlradEnv$form.mdl, data = AlradEnv$Train, 
                                                                               method = 'rf', trControl = bootControl,
                                                                               tuneGrid = Grid, importance = TRUE)
                                          AlradEnv$RF          <- randomForest::randomForest(AlradEnv$form.mdl, 
                                                                                             data=AlradEnv$Train, mtry=AlradEnv$rf.test$bestTune$mtry,
                                                                                             ntree = as.numeric(svalue(rf.ntree)))
                                          AlradEnv$rf.train    <- data.frame(AlradEnv$Train[AlradEnv$last.col], 
                                                                             Predicted=AlradEnv$RF$predicted)
                                          AlradEnv$rf.val      <- data.frame(AlradEnv$Val[AlradEnv$last.col], 
                                                                             Predicted=predict(AlradEnv$RF, newdata=AlradEnv$Val))
                                          faddtomodels("RF")
                                          enabled(pred) = TRUE #Enable prediction module
                                          },
                                         warning = function(w) fwarning(w),
                                         error =  function(e) ferror(e)
                                         )
                                dispose(AlradEnv$alert)
                                gmessage("RF model done", title = "RF model", parent = window)
                                }
  # ANN
  fann         <- function(...) {AlradEnv$alert <- galert("Wait... \nThis may take a few minutes! ", title = "ANN model", 
                                                          delay=10000, parent=notebook)
                                 Sys.sleep(1)
                                 tryCatch(
                                          {bootControl  <- caret::trainControl(method= svalue(ann.resampling), 
                                                                               preProcOptions = list(thresh = 0.95, cutoff = 0.95))
                                           Grid         <- expand.grid(.nhid= seq(1,svalue(ann.hid),ceiling(svalue(ann.hid)/10)),
                                                                       .actfun= c("sin", "radbas", "purelin", "tansig"))
                                           AlradEnv$ann.test     <- caret::train(AlradEnv$form.mdl, data = AlradEnv$Train, method = 'elm', 
                                                                                 trControl = bootControl, tuneGrid = Grid , na.action = na.fail, 
                                                                                 preProcess = c("nzv","center"))
                                           AlradEnv$ANN          <- elmNN::elmtrain(AlradEnv$form.mdl, data=AlradEnv$Train, 
                                                                                    nhid=AlradEnv$ann.test$bestTune$nhid,
                                                                                    actfun= AlradEnv$ann.test$bestTune$actfun)
                                           AlradEnv$ann.train    <- data.frame(AlradEnv$Train[AlradEnv$last.col], 
                                                                               Predicted=AlradEnv$ANN$fitted.values)
                                           AlradEnv$ann.val      <- data.frame(AlradEnv$Val[AlradEnv$last.col], 
                                                                               Predicted=predict(AlradEnv$ANN, newdata=AlradEnv$Val))
                                           faddtomodels("ANN")
                                           enabled(pred) = TRUE #Enable prediction module
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(AlradEnv$alert)
                                 gmessage("ANN model done", title = "ANN model", parent = window)
                                 }
  # GPR
  fgpr        <- function(...) {AlradEnv$alert <- galert("Wait... \nThis may take a few minutes! ", title = "GPR model", 
                                                          delay=10000, parent=notebook)
                                 Sys.sleep(1)
                                 tryCatch(
                                          {bootctrl.gpr <- caret::trainControl(method  <- svalue(gpr.resampling),
                                                                               number  <- ifelse(grepl("cv", method), svalue(gpr.kfold),
                                                                                                (svalue(gpr.kfold)+10))
                                                                               )
                                           if (svalue(gpr.kernel, index=TRUE)==1) fgprlinear()
                                           if (svalue(gpr.kernel, index=TRUE)==2) fgprradial()
                                           AlradEnv$gpr.train    <- data.frame(AlradEnv$Train[AlradEnv$last.col], 
                                                                                Predicted=predict(AlradEnv$gpr, newdata=AlradEnv$Train))
                                           AlradEnv$gpr.val      <- data.frame(AlradEnv$Val[AlradEnv$last.col], 
                                                                                Predicted=predict(AlradEnv$gpr, newdata=AlradEnv$Val))
                                           faddtomodels("GPR")
                                           enabled(pred) = TRUE #Enable prediction module
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(AlradEnv$alert)
                                 gmessage("GPR model done", title = "GPR model", parent = window)
                                 }
  fgprlinear  <- function(...) {AlradEnv$gpr.test <- caret::train(AlradEnv$form.mdl, data = AlradEnv$Train, method = 'gaussprLinear',
                                                            trControl = AlradEnv$bootctrl.gpr, tuneLength = 10)
                                AlradEnv$gpr       <- kernlab::gausspr(AlradEnv$form.mdl, data=AlradEnv$Train, kernel= "vanilladot",
                                                                type = "regression", kpar= "automatic", variance.model = T,
                                                                var=as.numeric(svalue(gpr.var)), cross= svalue(gpr.cross))
                                }
  fgprradial  <- function(...) {Grid      <-  expand.grid(.sigma = seq(.00001,.1,.005))
                                AlradEnv$gpr.test  <- caret::train(AlradEnv$form.mdl, data = AlradEnv$Train, method = 'gaussprRadial', 
                                                                    tuneLength = 10, trControl = AlradEnv$bootctrl.gpr, tuneGrid = Grid)
                                AlradEnv$gpr       <- kernlab::gausspr(AlradEnv$form.mdl, data=AlradEnv$Train, kernel="rbfdot",
                                                                        type ="regression", kpar= "automatic", variance.model = T,
                                                                        var=svalue(gpr.var), cross= svalue(gpr.cross))
                                }

  ###################################################
  ### Vectors
  ###################################################
  
  sgpolynomial         <- c(1:12)
  sgderivarive         <- c(1:4)
  splitnumbers         <- seq(from = 5, to = 50, by = 5)
  normalization.types  <- c("n1: standardization ((x-mean)/sd)",
                            "n5: normalization in range <-1,1> ((x-mean)/max(abs(x-mean)))",
                            "n6: quotient transformation (x/mean)",
                            "n12: normalization ((x-mean)/sqrt(sum((x-mean)^2)))",
                            "n13: normalization with zero being the central point ((x-midrange)/(range/2))")
  train.ctrl.method    <- c("boot", "cv", "LOOCV", "LGOCV", "repeatedcv", "timeslice")
  train.ctrl.method.rf <- c("boot", "cv", "LOOCV", "LGOCV", "repeatedcv", "timeslice", "oob")
  kernel.param.svm     <- c("Support Vector Machines with Linear Kernel",
                            "Support Vector Machines with Radial Kernel")
  actf                 <- c("linear","radial basis","sigmoid","sine","hard-limit","symmetric hard-limit",
                            "satlins","tan-sigmoid","triangular basis", "positive linear")
  kernel.param.gpr    <- c("Linear kernel", "Radial kernel")
  gpr.param.var       <- c(.0001,.001,.01,.1,1,10,100)

  ###################################################
  ### Main window
  ###################################################
  
  ### Create main window
  window        <- gwindow("Alrad Spectra", visible=F, width = 600,height = 600)
  ### Confirm window closing
  addHandlerUnrealize(window, handler = fconfirmquit)
  ### Clear, Quit and About buttons
  action.list   <- list(new =  gaction(label = "New",  icon = "new",  handler = fnew),
                        open =  gaction(label = "Open",  icon = "open",  handler = fopen),
                        save =  gaction(label = "Save",  icon = "save",  handler = fsave),
                        quit = gaction(label = "Quit", icon = "quit",  handler = fquit),
                        about = gaction(label = "About", icon = "about",  handler = fabout))
  toolbar.list  <- c(action.list[c("new", "open", "save")], sep = gseparator(), action.list["quit"],
                     sep = gseparator(), action.list["about"] )
  toolbar       <- gtoolbar(toolbar.list, cont = window)

  ###################################################
  ### Import Data module
  ###################################################
  
  ### Create notebook for modules
  notebook       <- gnotebook(cont = window)
  ### Add Import module to main notebook
  import         <- ggroup(cont = notebook, horizontal = F, label = gettext("      Import Data      "))
  ### Browse file
  frame.imp      <- gframe("File path:", cont = import, horizontal=T)
  file.browse    <- gedit(text = "", cont = frame.imp, width = 100)
  browse.button  <- gbutton("  Browse  ", cont = frame.imp, handler = function(...) fbrowse(file.browse))
  ### Parameters
  frame.file.arg <- gframe("Parameters:", cont = import, horizontal=TRUE)
  lyt.file.arg   <- glayout(cont = frame.file.arg, expand = F)
                    lyt.file.arg[1,1,anchor=c(1,0)]    <- "Separator (leave blank for tab):"
  file.sep       <- lyt.file.arg[1,2,anchor=c(0,0)]    <- gedit(text = ",", cont = lyt.file.arg, width = 6)
                    lyt.file.arg[1,3,anchor=c(1,0)]    <- "Decimal separator:"
  file.dec       <- lyt.file.arg[1,4,anchor=c(0,0)]    <- gedit(text = ".", cont = lyt.file.arg, width = 6)
                    lyt.file.arg[1,9,anchor=c(1,0)]    <- "Header:"
  file.header    <- lyt.file.arg[1,10,anchor=c(0,0)]   <- gcombobox(c("TRUE", "FALSE"), cont = lyt.file.arg)
                    lyt.file.arg[2,1,anchor=c(1,0)]    <- "Spectral data starts at column:"
  spc.start.col  <- lyt.file.arg[2,2,anchor=c(0,0)]    <- gedit(text = "", cont = lyt.file.arg, width = 6)
                    lyt.file.arg[2,3,anchor=c(1,0)]    <- "Spectral data ends at column:"
  spc.end.col    <- lyt.file.arg[2,4,anchor=c(0,0)]    <- gedit(text = "", cont = lyt.file.arg, width = 6)
                    lyt.file.arg[3,1,anchor=c(1,0)]    <- "Spectrum starts at wavelength:"
  spc.first      <- lyt.file.arg[3,2,anchor=c(0,0)]    <- gedit(text = "", cont = lyt.file.arg, width = 6)
                    lyt.file.arg[3,3,anchor=c(1,0)]    <- "Spectrum ends at wavelength:"
  spc.last       <- lyt.file.arg[3,4,anchor=c(0,0)]    <- gedit(text = "", cont = lyt.file.arg, width = 6)
                    lyt.file.arg[4,1,anchor=c(1,0)]    <- "Y variable is at column:"
  soil.var.col   <- lyt.file.arg[4,2,anchor=c(0,0)]    <- gedit(text = "", cont = lyt.file.arg, width = 6)
                    lyt.file.arg[4,3,anchor=c(1,0)]    <- "Y variable name:"
  soil.var.nm    <- lyt.file.arg[4,4:10,anchor=c(0,0)] <- gedit(text = "", cont = lyt.file.arg, width = 6)
  ### Import button
  gbutton("Import data", cont = import, handler = fimport)
  ### View data button
  gbutton("View data", cont = import, handler = function(...) fview(AlradEnv$alldata, 800))
  ### Plot imported data button
  gbutton("View imported spectra", cont = import, handler = function(...) fplot(AlradEnv$Original, AlradEnv$spectra.start.number, 
                                                                                AlradEnv$spectra.end.number))
  ### View descriptive statistics button
  gbutton("View Y descriptive statistics", cont = import, handler = fdescy)
  ### View histogram button
  gbutton("View Y histogram", cont = import, handler = fhist)
  ###################################################
  ### Preprocessing module
  ###################################################
  
  ### Add Preprocessing module to main notebook
  pp  <- gnotebook(cont = notebook, label = gettext(" Spectral Preprocessing"),horizontal = F, width = 30)
  enabled(pp) = FALSE #Disable preprocessing module
  nrm <- ggroup(cont = pp, horizontal = F,label = gettext(" Smoothing "))
  bin <- ggroup(cont = pp, horizontal = F,label = gettext(" Binning "))
  abs <- ggroup(cont = pp, horizontal = F,label = gettext(" Absorbance "))
  det <- ggroup(cont = pp, horizontal = F,label = gettext("  Detrend  "))
  crm <- ggroup(cont = pp, horizontal = F,label = gettext("Continuum Removal"))
  sgd <- ggroup(cont = pp, horizontal = F,label = gettext("   SGD   "))
  snv <- ggroup(cont = pp, horizontal = F,label = gettext("   SNV   "))
  msc <- ggroup(cont = pp, horizontal = F,label = gettext("   MSC   "))
  nor <- ggroup(cont = pp, horizontal = F,label = gettext("Normalization"))
  ### Smoothing
  frame.desc.nrm     <- gframe("Description:", cont = nrm, horizontal = T)
  lyt.desc.nrm       <- glayout(cont = frame.desc.nrm , expand = TRUE)
  lyt.desc.nrm[1,1]  <- "A simple moving average of spectral data using a convolution function. Package: prospectr"
  frame.param.nrm    <- gframe("Parameters:", cont = nrm, horizontal=T)
  lyt.param.nrm      <- glayout(cont = frame.param.nrm, expand = TRUE)
  lyt.param.nrm[1,1] <- "Number of smoothing points"
  number.smooth      <- lyt.param.nrm[2,1] <- gspinbutton(from = 5, to = 101, by = 2, cont = lyt.param.nrm)
  gbutton("Run", cont = nrm, handler = fnrm)
  gbutton("View spectra", cont = nrm, handler = function(...) fplot(AlradEnv$Smoothing, AlradEnv$spectra.start.number, 
                                                                    AlradEnv$spectra.end.number))
  gbutton("Save preprocessed spectra", cont = nrm, handler = function(...) fsavespectra(AlradEnv$Smoothing))
  ### Binning
  frame.desc.bin     <- gframe("Description:", cont = bin, horizontal = T)
  lyt.desc.bin       <- glayout(cont = frame.desc.bin , expand = TRUE)
  lyt.desc.bin[1,1]  <- "Compute average values of a signal in pre-determined bins. Package: prospectr"
  frame.param.bin    <- gframe("Parameters:", cont = bin, horizontal=T)
  lyt.param.bin      <- glayout(cont = frame.param.bin, expand = TRUE)
  lyt.param.bin[1,1] <- "Bin size"
  bin.number         <- lyt.param.bin[2,1:4] <- gspinbutton(from = 2, to = 100, by = 1, cont = lyt.param.bin)
  gbutton("Run", cont = bin, handler = fbin)
  gbutton("View spectra", cont = bin, handler = function(...) fplot(AlradEnv$Binning, AlradEnv$spectra.start.number, 
                                                                    AlradEnv$spectra.end.number))
  gbutton("Save preprocessed spectra", cont = bin, handler = function(...) fsavespectra(AlradEnv$Binning))
  ### Absorbance
  frame.desc.abs     <- gframe("Description:", cont = abs, horizontal=T)
  lyt.desc.abs       <- glayout(cont = frame.desc.abs, expand = TRUE)
  lyt.desc.abs[1,1]  <- "Transforms reflectance to absorbance values (log10(1/R))."
  gbutton("Run", cont = abs, handler = fabs)
  gbutton("View spectra", cont = abs, handler = function(...) fplot(AlradEnv$Absorbance, AlradEnv$spectra.start.number, 
                                                                    AlradEnv$spectra.end.number, ylab="Absorbance"))
  gbutton("Save preprocessed spectra", cont = abs, handler = function(...) fsavespectra(AlradEnv$Absorbance))
  ### Detrend
  frame.desc.det     <- gframe("Description:", cont = det, horizontal=T)
  lyt.desc.det       <- glayout(cont = frame.desc.det, expand = TRUE)
  lyt.desc.det[1,1]  <- "Normalizes each row by applying a Standard Normal Variate transformation followed by fitting a second order \nlinear model and returning the fitted residuals. Package: prospectr"
  gbutton("Run", cont = det, handler = fdet)
  gbutton("View spectra", cont = det, handler = function(...) fplot(AlradEnv$Detrend, AlradEnv$spectra.start.number, 
                                                                    AlradEnv$spectra.end.number))
  gbutton("Save preprocessed spectra", cont = det, handler = function(...) fsavespectra(AlradEnv$Detrend))
  ### Continuum Removal
  frame.desc.crm     <- gframe("Description:", cont = crm, horizontal=T)
  lyt.desc.crm       <- glayout(cont = frame.desc.crm, expand = TRUE)
  lyt.desc.crm[1,1]  <- "The continuum removal technique was introduced by Clark and Roush (1984). The algorithm find points lying on the \nconvex hull of a spectrum, connects the points by linear interpolation and normalizes the spectrum by \ndividing the input data by the interpolated line. Package: prospectr"
  lyt.desc.crm[2,1]  <- "Data type: Reflectance"
  lyt.desc.crm[3,1]  <- "Interpolation method: Linear"
  lyt.desc.crm[4,1]  <- "Normalization method: Division"
  gbutton("Run", cont = crm, handler = fcrm)
  gbutton("View spectra", cont = crm, handler = function(...) fplot(AlradEnv$ContinuumRemoval, AlradEnv$spectra.start.number, 
                                                                    AlradEnv$spectra.end.number))
  gbutton("Save preprocessed spectra", cont = crm, handler = function(...) fsavespectra(AlradEnv$ContinuumRemoval))
  ### SG Derivative
  frame.desc.sgd     <- gframe("Description:",cont = sgd, horizontal = T)
  lyt.desc.sgd       <- glayout(cont = frame.desc.sgd , expand = TRUE)
  lyt.desc.sgd[1,1]  <- "Savitzky-Golay Derivative. The Savitzky-Golay algorithm fits a local polynomial regression on the signal. It requires evenly \nspaced data points. Package: prospectr"
  frame.param.sgd    <- gframe("Parameters:", cont = sgd, horizontal=T)
  lyt.param.sgd      <- glayout(cont = frame.param.sgd, expand = TRUE)
  lyt.param.sgd[1,1] <- "Number of smoothing points"
  sgd.smooth         <- lyt.param.sgd[2,1] <- gspinbutton(from = 5, to = 101, by = 2, cont = lyt.param.sgd)
  lyt.param.sgd[1,2] <- "Polynomial order"
  sgd.poly           <- lyt.param.sgd[2,2] <- gcombobox(sgpolynomial, cont = lyt.param.sgd)
  lyt.param.sgd[1,3] <- "Derivative order"
  sgd.deriv          <- lyt.param.sgd[2,3] <- gcombobox(sgderivarive, cont = lyt.param.sgd)
  gbutton("Run", cont = sgd, handler = fsgd)
  gbutton("View spectra", cont = sgd, handler = function(...) fplot(AlradEnv$SavitzkyGolayDerivative, AlradEnv$spectra.start.number, 
                                                                    AlradEnv$spectra.end.number))
  gbutton("Save preprocessed spectra", cont = sgd, handler = function(...) fsavespectra(AlradEnv$SavitzkyGolayDerivative))
  ### SNV
  frame.desc.snv     <- gframe("Description:", cont = snv, horizontal=T)
  lyt.desc.snv       <- glayout(cont = frame.desc.snv, expand = TRUE)
  lyt.desc.snv[1,1]  <- "Standard Normal Variate normalizes each row by substracting each row by its mean and dividing by \nits standard deviation. Package: prospectr"
  gbutton("Run", cont = snv, handler = fsnv)
  gbutton("View spectra", cont = snv, handler = function(...) fplot(AlradEnv$SNV, AlradEnv$spectra.start.number, 
                                                                    AlradEnv$spectra.end.number))
  gbutton("Save preprocessed spectra", cont = snv, handler = function(...) fsavespectra(AlradEnv$SNV))
  ### MSC
  frame.desc.msc     <- gframe("Description:", cont = msc, horizontal=T)
  lyt.desc.msc       <- glayout(cont = frame.desc.msc, expand = TRUE)
  lyt.desc.msc[1,1]  <- "Performs multiplicative scatter/signal correction on spectral data. Package: pls"
  gbutton("Run", cont = msc, handler = fmsc)
  gbutton("View spectra", cont = msc, handler = function(...) fplot(AlradEnv$MSC, AlradEnv$spectra.start.number, 
                                                                    AlradEnv$spectra.end.number))
  gbutton("Save preprocessed spectra", cont = msc, handler = function(...) fsavespectra(AlradEnv$MSC))
  ### Normalization
  frame.desc.nor     <- gframe("Description:",cont = nor, horizontal = T)
  lyt.desc.nor       <- glayout(cont = frame.desc.nor , expand = TRUE)
  lyt.desc.nor[1,1]  <- "Different types of data normalization. Package: clusterSim"
  frame.param.nor    <- gframe("Parameters:", cont = nor, horizontal=T)
  lyt.param.nor      <- glayout(cont = frame.param.nor, expand = TRUE)
  lyt.param.nor[1,1] <- "Type of Normalization."
  nor.type           <- lyt.param.nor[2,1] <- gradio(normalization.types, checked = T, cont = lyt.param.nor)
  gbutton("Run", cont = nor, handler = fnor)
  gbutton("View spectra", cont = nor, handler = function(...) fplot(AlradEnv$Normalization, AlradEnv$spectra.start.number, 
                                                                    AlradEnv$spectra.end.number))
  gbutton("Save preprocessed spectra", cont = nor, handler = function(...) fsavespectra(AlradEnv$Normalization))

  ###################################################
  ### Modeling module
  ###################################################
  
  ### Add Modeling module to main notebook
  models             <- ggroup(cont = notebook, label = gettext("           Modeling           "), horizontal = F)
  glabel("Select input data for modeling:", cont = models, anchor = c(-1,0))
  select.dataset     <- gcombobox("", cont = models, handler = fchangesplit)
  glabel("Size of validation set (%):", cont = models, anchor = c(-1,0))
  split.val          <- gcombobox(splitnumbers, cont = models, selected = 6, handler = fchangesplit)
  gbutton("Split data", cont = models, handler = fsplit)
  homo.button        <- gbutton("Homogeneity of variance test", cont = models, handler = fhomo)
  desc.button        <- gbutton("View descriptive statistics of groups", cont = models, handler = fdesc)
  boxplot.button     <- gbutton("View box plots", cont = models, handler = fboxplot)
  mdl                <- gnotebook(cont = models)
  enabled(homo.button) = FALSE #Disable homogeneity test button
  enabled(desc.button) = FALSE #Disable descriptive stats button
  enabled(boxplot.button) = FALSE #Disable box plots button
  enabled(models) = FALSE #Disable modeling module
  enabled(mdl)    = FALSE #Disable models notebook
  ### MLR
  mdl.mlr            <- ggroup(cont = mdl, horizontal = F,label = gettext("   MLR   "))
  frame.desc.mlr     <- gframe("Description:",cont = mdl.mlr, horizontal = T)
  lyt.desc.mlr       <- glayout(cont = frame.desc.mlr, expand = TRUE)
  lyt.desc.mlr[1,1]  <- "Multiple Linear Regression. MLR is a statistical method that uses several explanatory variables to predict the outcome of a response variable in a simple linear model. Packages: stats"
  frame.param.mlr    <- gframe("Tuning parameters:", cont = mdl.mlr, horizontal=T)
  lyt.param.mlr      <- glayout(cont = frame.param.mlr , expand = TRUE)
  lyt.param.mlr[1,1] <- "Band interval"
  mlr.band.interval  <- lyt.param.mlr[2,1] <- gspinbutton(from = 1, to = 30, by = 1, value = 25, cont = lyt.param.mlr)
  lyt.param.mlr[1,2] <- "Resampling method"
  mlr.resampling     <- lyt.param.mlr[2,2] <- gcombobox(train.ctrl.method, cont = lyt.param.mlr)
  lyt.param.mlr[1,3] <- "Number of resampling \niterations"
  mlr.folds          <- lyt.param.mlr[2,3] <- gspinbutton(from = 1, to = 500, by = 1, value = 5, cont = lyt.param.mlr)
  lyt.param.mlr[1,4] <- "For cv resampling method only: \nnumber of folds (k-fold)"
  mlr.kfold          <- lyt.param.mlr[2,4] <- gspinbutton(from = 1, to = 500, by = 1,value =  10, cont = lyt.param.mlr)
  gbutton("Run MLR model", cont = mdl.mlr, handler = fmlr)
  gbutton("View variables importance", cont = mdl.mlr, handler = function(...) fmdl.plot.imp(AlradEnv$mlr.test))
  gbutton("MLR prediction statistics", cont = mdl.mlr, handler = function(...) fmdl.stats(AlradEnv$mlr.train, AlradEnv$mlr.val))
  gbutton("View measured vs. predicted",cont = mdl.mlr, handler = function(...) fmdl.plot.res(AlradEnv$mlr.train, AlradEnv$mlr.val))
  ### PLS
  mdl.pls            <- ggroup(cont = mdl, horizontal = F,label = gettext("   PLSR   "))
  frame.desc.pls     <- gframe("Description:",cont = mdl.pls, horizontal = T)
  lyt.desc.pls       <- glayout(cont = frame.desc.pls, expand = TRUE)
  lyt.desc.pls[1,1]  <- "Partial Least Squares Regression. PLSR is considered the most common regression method applied in chemometrics and can deal with complex modeling problems. Packages: pls / caret"
  frame.param.pls    <- gframe("Tuning parameters:", cont = mdl.pls, horizontal=T)
  lyt.param.pls      <- glayout(cont = frame.param.pls , expand = TRUE)
  lyt.param.pls[1,1] <- "Resampling method"
  pls.resampling     <- lyt.param.pls[2,1] <- gcombobox(train.ctrl.method, cont = lyt.param.pls)
  lyt.param.pls[1,2] <- "Number of resampling \niterations"
  pls.folds          <- lyt.param.pls[2,2] <- gspinbutton(from = 1, to = 500, by = 1, value = 5, cont = lyt.param.pls)
  lyt.param.pls[1,3] <- "For cv resampling method only: \nnumber of folds (k-fold)"
  pls.kfold          <- lyt.param.pls[2,3] <- gspinbutton(from = 1, to = 500, by = 1,value =  10, cont = lyt.param.pls)
  lyt.param.pls[1,4] <- "Number of components to \ninclude in the model"
  pls.comp           <- lyt.param.pls[2,4] <- gspinbutton(from = 1, to = 500, by = 1, value =  30, cont = lyt.param.pls)
  gbutton("Run PLSR model", cont = mdl.pls , handler = fpls)
  gbutton("View variables importance", cont = mdl.pls, handler = function(...) fmdl.plot.imp(AlradEnv$pls.test))
  gbutton("View PLS components vs. RMSE", cont = mdl.pls, handler = function(...) fpls.plot.imp(AlradEnv$pls.test))
  gbutton("PLSR prediction statistics", cont = mdl.pls, handler = function(...) fmdl.stats(AlradEnv$pls.train, AlradEnv$pls.val))
  gbutton("View measured vs. predicted",cont = mdl.pls, handler = function(...) fmdl.plot.res(AlradEnv$pls.train, AlradEnv$pls.val))
  ### SVM
  mdl.svm            <- ggroup(cont = mdl, horizontal = F,label = gettext("    SVM    "))
  frame.desc.svm     <- gframe("Description:",cont = mdl.svm, horizontal = T)
  lyt.desc.svm       <- glayout(cont = frame.desc.svm, expand = TRUE)
  lyt.desc.svm[1,1]  <- "Support Vector Machine. SVM models are efficient in modeling linear or nonlinear relationships and handling large databases. Packages: e1071 / caret"
  frame.param.svm    <- gframe("Tuning parameters:", cont = mdl.svm, horizontal=T)
  lyt.param.svm      <- glayout(cont = frame.param.svm , expand = TRUE)
  lyt.param.svm[1,1] <- "Resampling method"
  svm.resampling     <- lyt.param.svm[2,1] <- gcombobox(train.ctrl.method, cont = lyt.param.svm)
  lyt.param.svm[1,2] <- "Kernel parameters"
  svm.kernel         <- lyt.param.svm[2,2] <- gradio(kernel.param.svm, cont = lyt.param.svm)
  gbutton("Run SVM model", cont = mdl.svm, handler = fsvm)
  gbutton("View variables importance", cont = mdl.svm, handler = function(...) fmdl.plot.imp(AlradEnv$svm.test))
  gbutton("SVM prediction statistics", cont = mdl.svm, handler = function(...) fmdl.stats(AlradEnv$svm.train, AlradEnv$svm.val))
  gbutton("View measured vs. predicted",cont = mdl.svm, handler = function(...) fmdl.plot.res(AlradEnv$svm.train, AlradEnv$svm.val))
  ### RF
  mdl.rf             <- ggroup(cont = mdl, horizontal = F,label = gettext("    RF    "))
  frame.desc.rf      <- gframe("Description:",cont = mdl.rf, horizontal = T)
  lyt.desc.rf        <- glayout(cont = frame.desc.rf, expand = TRUE)
  lyt.desc.rf[1,1]   <- "Random Forest. RF models are black boxes approach that are very hard to interpret. Packages: randomForest / caret"
  frame.param.rf     <- gframe("Tuning parameters:", cont = mdl.rf, horizontal=T)
  lyt.param.rf       <- glayout(cont = frame.param.rf , expand = TRUE)
  lyt.param.rf[1,1]  <- "Resampling method"
  rf.resampling      <- lyt.param.rf[2,1]  <- gcombobox(train.ctrl.method.rf, cont = lyt.param.rf)
  lyt.param.rf[1,2]  <- "Randomly selected predictors \n(mtry)"
  rf.mtry            <- lyt.param.rf[2,2]  <- gspinbutton(from = 5, to = 500, by = 5, value = 5, cont = lyt.param.rf)
  lyt.param.rf[1,3]  <- "Number of trees \n(ntree)"
  rf.ntree           <- lyt.param.rf[2,3]  <- gedit(text = "500", cont = lyt.param.rf, width = 4)
  gbutton("Run RF model", cont = mdl.rf, handler = frf)
  gbutton("View variables importance", cont = mdl.rf, handler = function(...) fmdl.plot.imp(AlradEnv$rf.test))
  gbutton("RF prediction statistics", cont = mdl.rf, handler = function(...) fmdl.stats(AlradEnv$rf.train, AlradEnv$rf.val))
  gbutton("View measured vs. predicted",cont = mdl.rf, handler = function(...) fmdl.plot.res(AlradEnv$rf.train, AlradEnv$rf.val))
  ### ANN
  mdl.ann            <- ggroup(cont = mdl, horizontal = F,label = gettext("    ANN    "))
  frame.desc.ann     <- gframe("Description:",cont = mdl.ann, horizontal = T)
  lyt.desc.ann       <- glayout(cont = frame.desc.ann, expand = TRUE)
  lyt.desc.ann[1,1]  <- "Artificial Neural Network. ANN calculates the output from the hidden layer based on the activation function. Packages: elmNN / caret"
  frame.param.ann    <- gframe("Tuning parameters:", cont = mdl.ann, horizontal=T)
  lyt.param.ann      <- glayout(cont = frame.param.ann , expand = TRUE)
  lyt.param.ann[1,1] <- "Resampling method"
  ann.resampling     <- lyt.param.ann[2,1] <- gcombobox(train.ctrl.method, cont = lyt.param.ann)
  lyt.param.ann[1,2] <- "Activation function"
  lyt.param.ann[2,2] <- gcombobox(actf, cont = lyt.param.ann)
  lyt.param.ann[1,3] <- "Hidden units"
  ann.hid            <- lyt.param.ann[2,3] <- gspinbutton(from = 1, to = 50, by = 1, value = 50, cont = lyt.param.ann)
  gbutton("Run ANN model", cont = mdl.ann, handler = fann)
  gbutton("View variables importance", cont = mdl.ann, handler = function(...) fmdl.plot.imp(AlradEnv$ann.test))
  gbutton("ANN prediction statistics", cont = mdl.ann, handler = function(...) fmdl.stats(AlradEnv$ann.train, AlradEnv$ann.val))
  gbutton("View measured vs. predicted", cont = mdl.ann, handler = function(...) fmdl.plot.res(AlradEnv$ann.train, AlradEnv$ann.val))
  ### GPR
  mdl.gpr            <- ggroup(cont = mdl, horizontal = F,label = gettext("    GPR    "))
  frame.desc.gpr     <- gframe("Description:",cont = mdl.gpr, horizontal = T)
  lyt.desc.gpr       <- glayout(cont = frame.desc.gpr, expand = TRUE)
  lyt.desc.gpr[1,1]  <- "Gaussian Process for Regression. Gaussian process applies a kernel function for training and predicting. Packages: kernlab / caret"
  frame.param.gpr    <- gframe("Tuning parameters:", cont = mdl.gpr, horizontal=T)
  lyt.param.gpr      <- glayout(cont = frame.param.gpr , expand = TRUE)
  lyt.param.gpr[1,1] <- "Resampling method"
  gpr.resampling     <- lyt.param.gpr[2,1] <- gcombobox(train.ctrl.method, cont = lyt.param.gpr)
  lyt.param.gpr[1,2] <- "Initial noise variance"
  gpr.var            <- lyt.param.gpr[2,2] <- gcombobox(gpr.param.var, selected = 2, cont = lyt.param.gpr)
  lyt.param.gpr[1,3] <- "For cv resampling method only: \nnumber of folds (k-fold)"
  gpr.cross          <- lyt.param.gpr[2,3] <- gspinbutton(from = 2, to = 100, by = 1, value = 10, cont = lyt.param.gpr)
  lyt.param.gpr[1,4] <- "kernel function \nused in training and predicting"
  gpr.kernel         <- lyt.param.gpr[2,4] <- gradio(kernel.param.gpr, cont = lyt.param.gpr)
  gbutton("Run gpr model", cont = mdl.gpr, handler = fgpr)
  gbutton("View variables importance", cont = mdl.gpr, handler = function(...) fmdl.plot.imp(AlradEnv$gpr.test))
  gbutton("GPR prediction statistics", cont = mdl.gpr, handler = function(...) fmdl.stats(AlradEnv$gpr.train, AlradEnv$gpr.val))
  gbutton("View measured vs. predicted", cont = mdl.gpr, handler = function(...) fmdl.plot.res(AlradEnv$gpr.train, AlradEnv$gpr.val))
  
  ###################################################
  ### Prediction module
  ###################################################
  
  ### Add Prediction module to main notebook
  pred                <- ggroup(cont = notebook, label = gettext("          Prediction          "), horizontal = F)
  ### Create import group
  pred.imp            <- ggroup(cont = pred, horizontal = F)
  ### Browse file
  glabel("Import spectral data for Prediction\n  Conditions:\n-File must contain only spectral data.\n-Spectral data for Prediction and Modeling must be the same length.\n-Spectral data used here must have the same preprocessing used to build the model.",
         cont = pred.imp, anchor = c(-1,0))
  pred.frame.imp      <- gframe("File path:", cont = pred.imp, horizontal=T)
  pred.file.browse    <- gedit(text = "", cont = pred.frame.imp, width = 100)
  pred.browse.button  <- gbutton("  Browse  ", cont = pred.frame.imp, handler = function(...) fbrowse(pred.file.browse))
  ### Parameters
  pred.frame.file.arg <- gframe("Parameters:", cont = pred.imp, horizontal=TRUE)
  pred.lyt.file.arg   <- glayout(cont = pred.frame.file.arg, expand = F)
                         pred.lyt.file.arg[1,1,anchor=c(1,0)] <- "Separator (leave blank for tab):"
  pred.file.sep       <- pred.lyt.file.arg[1,2,anchor=c(0,0)] <- gedit(text = ",", cont = pred.lyt.file.arg, width = 8)
                         pred.lyt.file.arg[1,3,anchor=c(1,0)] <- "Decimal separator:"
  pred.file.dec       <- pred.lyt.file.arg[1,4,anchor=c(0,0)] <- gedit(text = ".", cont = pred.lyt.file.arg, width = 8)
                         pred.lyt.file.arg[1,8,anchor=c(1,0)] <- "Header:"
  pred.file.header    <- pred.lyt.file.arg[1,9,anchor=c(0,0)] <- gcombobox(c("TRUE", "FALSE"), cont = pred.lyt.file.arg)
                         pred.lyt.file.arg[2,1,anchor=c(1,0)] <- "Spectrum starts at wavelength:"
  pred.spc.first      <- pred.lyt.file.arg[2,2,anchor=c(0,0)] <- gedit(text = "", cont = pred.lyt.file.arg, width = 4)
                         pred.lyt.file.arg[2,3,anchor=c(1,0)] <- "Spectrum ends at wavelength:"
  pred.spc.last       <- pred.lyt.file.arg[2,4,anchor=c(0,0)] <- gedit(text = "", cont = pred.lyt.file.arg, width = 4)
  ### Import button
  gbutton("Import data", cont = pred.imp, handler = fimport.pred)
  ### View data button
  gbutton("View data", cont = pred.imp, handler = function(...) fview(AlradEnv$spc.pred, 800))
  ### Plot imported data button
  gbutton("View imported spectra", cont = pred.imp, handler = function(...) fplot(AlradEnv$spc.pred, 
                                                                                  AlradEnv$pred.spectra.start.number, 
                                                                                  AlradEnv$pred.spectra.end.number))
  ### Draw a separator line
  gseparator(cont = pred)
  ### Create predict group
  pred.predict        <- ggroup(cont = pred, horizontal = F)
  ### Select model for prediction
  glabel("Select model for prediction:", cont = pred.predict, anchor = c(-1,0))
  select.model        <- gcombobox("", cont = pred.predict)
  gbutton("Predict", cont = pred.predict, handler = fpredict)
  gbutton("View predictions", cont = pred.predict, handler = function(...) fview(AlradEnv$prediction, 300))
  gbutton("Save predictions", cont = pred.predict, handler = function(...) fsaveresults(AlradEnv$prediction))
  enabled(pred.predict) = FALSE #Disable predict group
  enabled(pred) = FALSE #Disable prediction module

  ### Focus on first tabs
  svalue(notebook) <- 1
  svalue(pp)       <- 1
  svalue(mdl)      <- 1
  ### Window visibility
  visible(window)  <- TRUE
  
} #Closes AlradSpectra() function
