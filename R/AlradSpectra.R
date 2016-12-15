#' A GUI for preprocessing soil spectra and predicting soil properties
#'
#' AlradSpectra provides a friendly GUI to apply preprocessing functions to soil spectra and predict soil properties using models. 
#' @author Andre C. Dotto, \email{andrecdot@gmail.com}
#' @author Diego J. Gris, \email{diegojgris@gmail.com}
#' @import gWidgets
#' @export

AlradSpectra <- function() {

  ###################################################
  ### Auxiliar functions
  ###################################################
  
  # Warning handler
  fwarning     <- function(w)         {dispose(alert)
                                       gmessage(message = w$message, title = "Warning", icon="warning", parent = window)
                                       stop()
                                       }
  # Error handler
  ferror       <- function(e)         {dispose(alert)
                                       gmessage(message = e$message, title = "Error", icon="error", parent = window)
                                       stop()
                                       }
  # Makes sure the user really wants to quit Alrad when closing the window.
  fconfirmquit <- function(h, ...)    {sure <- gconfirm("Are you sure?", parent=h$obj)
                                       if(as.logical(sure))
                                         return(FALSE) #Close
                                       else
                                         return(TRUE) #Don't close
                                       }
  # Clears all data, empties forms and resets Alrad to initial status
  fclear       <- function(...)       {gconfirm("Clear Alrad Spectra?", title="Clear", icon="warning", parent=window,
                                                handler=function(h, ...) {svalue(file.browse)   <- ""
                                                                          svalue(file.sep)      <- ","
                                                                          svalue(spc.start.col) <- ""
                                                                          svalue(spc.end.col)   <- ""
                                                                          svalue(spc.first)     <- ""
                                                                          svalue(spc.last)      <- ""
                                                                          svalue(soil.var.col)  <- ""
                                                                          svalue(notebook)      <- 1 #Focus on import tab
                                                                          enabled(pp) = FALSE
                                                                          enabled(models) = FALSE
                                                                          enabled(mdl) = FALSE
                                                                          rm(envir=.GlobalEnv) #Remove everything in Global Environment
                                                                          }
                                                )
                                       }
  # Handler for quit action. Makes sure the user really wants to quit Alrad.
  fquit        <- function(...)        gconfirm("Are you sure?", icon="warning", parent=window, handler=dispose(window))
  # Creates and shows the window with information about Alrad Spectra
  fabout       <- function(...)       {aboutwin <- gwindow("About Alrad Spectra", width=400, height=300, parent = window)
                                       wingroup <- ggroup(horizontal = FALSE, container = aboutwin)
                                       gimage(system.file("images","AlradLogo2.png", package="AlradSpectra"), container = wingroup)
                                       glabel(paste0("Soil spectra preprocessing and modeling\n\n",
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
                                                     "<i>http://www.linktopaper.com/</i>\n\n",
                                                     sep="", collapse=""),
                                              markup = TRUE, container = wingroup
                                              )
                                       }
  # Opens up a dialog to search for file to be imported
  fbrowse      <- function(h, ...)    {svalue(h) <- gfile("Open File", type="open",
                                                          filter=c("Comma Separated Values (.csv)"="csv"),
                                                          cont = window)}
  # Imports csv file to global environment and sets variables used afterwards
  fimport      <- function(...)       {alert <<- galert("Wait...", title = "Importing File", delay=10000, parent=notebook)
                                       tryCatch({alldata <<- read.table(file = svalue(file.browse),
                                                                        header = as.logical(svalue(file.header)),
                                                                        sep = svalue(file.sep))
                                                 spectra.start.column   <<- as.numeric(svalue(spc.start.col))
                                                 spectra.end.column     <<- as.numeric(svalue(spc.end.col))
                                                 spectra.start.number   <<- as.numeric(svalue(spc.first))
                                                 spectra.end.number     <<- as.numeric(svalue(spc.last))
                                                 soil.var.column        <<- as.numeric(svalue(soil.var.col))
                                                 soil.var.name          <<- colnames(alldata[soil.var.column])
                                                 fonlyspectra() #Create dataframe with spectra only
                                                 dataset                <<- c("Original")
                                                 select.dataset[]       <-  dataset
                                                 svalue(select.dataset) <- "Original"
                                                 },
                                                 warning = function(w) fwarning(w),
                                                 error =  function(e) ferror(e)
                                                 )
                                      enabled(pp) = TRUE
                                      enabled(models) = TRUE
                                      dispose(alert)
                                      gmessage(message = "Import successful!", title = "File import", parent = window)
                                      }
  # Create dataframe that contains only the spectral data to be used for preprocessing
  fonlyspectra <- function(...)       {spc <- alldata[,spectra.start.column:spectra.end.column]
                                       colnames(spc) <- c(spectra.start.number:spectra.end.number)
                                       Original <<- spc
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
                                                     matplot(colnames(h), t(h), xlim = c(s, e),
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
  # Export preprocessed spectra as csv file
  fsavespectra <- function(h, ...)    {fdialog <- gfile("Save File", type="save", initialfilename="Output", container=window,
                                                        filter=c("Comma Separated Values (.csv)"="csv"))
                                       #If fdialog is not equal to NA, keep running        
                                       if(!(is.na(fdialog))) {fname      <- paste0(fdialog,".csv")
                                                              spectrum   <- seq(spectra.start.column, spectra.end.column)
                                                              exportdata <- cbind(alldata[,-spectrum], h)
                                                              write.csv(exportdata, row.names = FALSE, file = fname)
                                                              }
                                       }
  # Adds preprocessing to combobox in Model tab only if it is not already there
  faddtodtset  <- function(h, ...) {present <- is.element(h, dataset)
                                       if(present==FALSE)
                                         dataset <<- c(dataset, h)
                                         select.dataset[] <<- dataset
                                       }
  # Splits dataset in training and validaton sets
  fsplit       <- function(...)       {set.seed(1) #Random Number Generation
                                       x           <- eval(parse(text = svalue(select.dataset))) #Get selected dataset
                                       x           <- cbind(x, alldata[soil.var.column]) #Join spectral data and soil property
                                       indices     <- sample(1:nrow(x), size = (svalue(split.val)/100)*nrow(x)) #Random sampling
                                       t           <- x[-indices,] #Training set
                                       v           <- x[ indices,] #Validation set
                                       colnames(t) <- paste("X", colnames(t), sep = "") #Add X before wavelength
                                       colnames(v) <- paste("X", colnames(v), sep = "") #Add X before wavelength
                                       Train       <<- t #Send to Global Environment
                                       Val         <<- v #Send to Global Environment
                                       last.col    <<- ncol(Train) #Get position of last column (soil variable)
                                       #Create formula for models
                                       form.mdl    <<- as.formula(paste(colnames(Train[last.col]),"~",
                                                                        paste(names(Train)[c(1:last.col-1)], collapse="+"), collapse=""))
                                       enabled(mdl) = TRUE #Enable models module
                                       gmessage(paste("Number of training samples:", nrow(Train),
                                                      "\n\nNumber of validation samples:", nrow(Val)),
                                                title = "Split", parent = window)
                                       }
  # Disables models module when dataset or validation size is changed
  fchangesplit <- function(h, ...)    enabled(mdl) = FALSE
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
                                       error.i        <- round(c(r2, bias, rmse,  rpd, rpiq),2)
                                       names(error.i) <- c("R2", "Bias", "RMSE", "RPD", "RPIQ")
                                       return(error.i)
                                       }
  # Get model accuracy and display in tabular form
  fmdl.stats    <- function(t, v, ...) {t.stats.name <- paste0(deparse(substitute(t)), ".stats") #Create training stats table name
                                        v.stats.name <- paste0(deparse(substitute(v)), ".stats") #Create validation stats table name
                                        assign(t.stats.name, fstats(t[,1], t[,2]), envir = .GlobalEnv) #Compute training stats
                                        assign(v.stats.name, fstats(v[,1], v[,2]), envir = .GlobalEnv) #Compute validation stats
                                        results      <- rbind(get(t.stats.name), get(v.stats.name)) #Merge training and validation stats
                                        Set          <- c("Training", "Validation") #Titles for model results table
                                        res.table    <- cbind(Set, results) #Create model results table
                                        statswin     <- gwindow("Model results", width=320, height=150, parent=window)
                                        stats.lyt    <- glayout(horizontal=FALSE, container=statswin)
                                        stats.lyt[1,1,expand=TRUE] <- gtable(res.table, cont = stats.lyt)
                                        stats.lyt[2,1,expand=TRUE] <- gbutton("Save results", cont=stats.lyt,
                                                                              anchor=c(0,-1),
                                                                              handler=function(...) fsaveresults(res.table))
                                        }
  # Plot model accuracy
  fmdl.plot.res <- function(t, v, ...) {plotwin      <- gwindow("Model accuracy", width = 1000, height = 400, parent = window)
                                        wingroup     <- ggroup(horizontal=FALSE, cont=plotwin)
                                        ggraphics(cont = wingroup, no_popup=TRUE)
                                        t.stats.name <- paste0(deparse(substitute(t)), ".stats") #Create training stats table name
                                        v.stats.name <- paste0(deparse(substitute(v)), ".stats") #Create validation stats table name
                                        assign(t.stats.name, fstats(t[,1], t[,2]), envir = .GlobalEnv) #Compute training stats
                                        assign(v.stats.name, fstats(v[,1], v[,2]), envir = .GlobalEnv) #Compute validation stats
                                        train.plot   <- ggplot2::ggplot(t, ggplot2::aes(x=t[,1], y=t[,2])) +
                                                        ggplot2::geom_point(shape=19) +
                                                        ggplot2::labs(list(title="Training set", x="Measured", y="Predicted")) +
                                                        ggplot2::xlim(0, max(t)) +
                                                        ggplot2::ylim(0, max(t)) +
                                                        ggplot2::geom_abline(intercept = 0, slope = 1) +
                                                        ggplot2::annotate("text", x=max(t)*0.1, y=seq(max(t)*0.9,max(t)*0.7,-max(t)*0.05),
                                                                          label = paste(names(get(t.stats.name)),"=",get(t.stats.name)))
                                        val.plot     <- ggplot2::ggplot(v, ggplot2::aes(x=v[,1], y=v[,2])) +
                                                        ggplot2::geom_point(shape=19) +
                                                        ggplot2::labs(list(title="Validation set", x="Measured", y="Predicted")) +
                                                        ggplot2::xlim(0, max(v)) +
                                                        ggplot2::ylim(0, max(v)) +
                                                        ggplot2::geom_abline(intercept = 0, slope = 1) +
                                                        ggplot2::annotate("text", x=max(v)*0.1,
                                                                          y=seq(max(v)*0.9,max(v)*0.7,-max(v)*0.05),
                                                                          label = paste(names(get(v.stats.name)),"=",get(v.stats.name)))
                                        Sys.sleep(1)
                                        gbutton("Save plot", cont = wingroup, handler = function(...) fsaveplot(800, 400))
                                        Rmisc::multiplot(train.plot, val.plot, cols = 2)
                                        }
  # Plot RMSE of PLSR components
  fpls.plot.imp <- function(h, ...)    {plotwin   <- gwindow("Plot", width = 800, height = 600, parent=window)
                                        wingroup  <- ggroup(horizontal=FALSE, cont=plotwin)
                                        ggraphics(cont = wingroup, no_popup=TRUE)
                                        Sys.sleep(1)
                                        gbutton("Save plot", cont = wingroup, handler = function(...) fsaveplot(800, 600))
                                        comp.plot <- ggplot2::ggplot(h) +
                                                     ggplot2::labs(list(x="Components", y="RMSE"))
                                        Rmisc::multiplot(comp.plot)
                                        }
  # Plot variable importance
  fmdl.plot.imp <- function(h, ...)    {plotwin   <- gwindow("Plot", width = 400, height = 600, parent=window)
                                        wingroup  <- ggroup(horizontal=FALSE, cont=plotwin)
                                        ggraphics(cont = wingroup, no_popup=TRUE)
                                        Sys.sleep(1)
                                        gbutton("Save plot", cont = wingroup, handler = function(...) fsaveplot(800, 600))
                                        comp.plot <- ggplot(varImp(h), top=40)
                                        Rmisc::multiplot(comp.plot)
  }
  # Adds preprocessing to combobox in Model tab only if it is not already there
  faddtomodels  <- function(h, ...) {present <- is.element(h, pred.models)
                                     if(present==FALSE) {
                                       if(length(pred.models)==0) { #Special case when the list is empty (first model is being added)
                                         pred.models          <<- c(h)
                                         select.model[]       <<- pred.models
                                         svalue(select.model) <-  h
                                       } else {
                                           pred.models        <<- c(pred.models, h)
                                           select.model[]     <<- pred.models
                                         }
                                       
                                     }
                                     }
  # Imports csv file to global environment for prediction
  fimport.pred  <- function(...)       {alert <<- galert("Wait...", title = "Importing File", delay=10000, parent=notebook)
                                        tryCatch({spc <- read.table(file = svalue(pred.file.browse),
                                                                    header = as.logical(svalue(pred.file.header)),
                                                                    sep = svalue(pred.file.sep))
                                                  pred.spectra.start.number <<- as.numeric(svalue(pred.spc.first))
                                                  pred.spectra.end.number   <<- as.numeric(svalue(pred.spc.last))
                                                  colnames(spc)             <-  c(pred.spectra.start.number:pred.spectra.end.number)
                                                  spc.pred                  <<- spc
                                                  },
                                                 warning = function(w) fwarning(w),
                                                 error =  function(e) ferror(e)
                                                 )
                                        enabled(pred.predict) = TRUE #Enable predict group
                                        dispose(alert)
                                        gmessage(message = "Import successful!", title = "File import", parent = window)
                                        }
  # Predict soil property based on a new spectra
  fpredict      <- function(...)       {alert <<- galert("Wait...", title = "Importing File", delay=10000, parent=notebook)
                                        tryCatch({colnames(spc.pred) <- paste("X", colnames(spc.pred), sep = "") #Add X before wavelength
                                                  mdl                <- eval(parse(text = svalue(select.model))) #Get selected model
                                                  if(svalue(select.model)=="PLSR") {#PLSR special case
                                                    pls.pred         <-  data.frame(predict(PLSR, newdata=spc.pred))
                                                    prediction       <<- data.frame(ID=row.names(spc.pred),
                                                                                    Predicted=pls.pred[,ncol(pls.pred)])
                                                  } else {
                                                      prediction     <<- data.frame(ID=row.names(spc.pred), Predicted=predict(mdl, newdata=spc.pred))
                                                    }
                                                  },
                                                 warning = function(w) fwarning(w),
                                                 error =  function(e) ferror(e)
                                                 )
                                        dispose(alert)
                                        gmessage(message = "Done!", title = "Prediction", parent = window)
                                        }
  
  ###################################################
  ### Preprocessing functions
  ###################################################
  
  # Smoothing
  fnrm         <- function(...) {alert <<- galert("Wait...", title = "Smoothing", delay=10000, parent=notebook)
                                 tryCatch(
                                          {Smoothing  <<- prospectr::movav(Original, w = as.numeric(svalue(number.smooth)))
                                           faddtodtset("Smoothing")
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(alert)
                                 gmessage(message = "Done!", title = "Smoothing", icon = "info", parent = window)
                                 }
  # Binning
  fbin         <- function(...) {alert <<- galert("Wait...", title = "Binning", delay=10000, parent=notebook)
                                 tryCatch(
                                          {Binning <<- prospectr::binning(Original, bin.size = as.numeric(svalue(bin.number)))
                                           faddtodtset("Binning")
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(alert)
                                 gmessage(message = "Done!", title = "Binning", icon = "info", parent = window)
                                 }
  # Absorbance
  fabs         <- function(...) {alert <<- galert("Wait...", title = "Absorbance", delay=10000, parent=notebook)
                                 tryCatch(
                                          {Absorbance <<- log10(1/Original)
                                           faddtodtset("Absorbance")
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(alert)
                                 gmessage(message = "Done!", title = "Absorbance", icon = "info", parent = window)
                                 }
  # Detrend
  fdet         <- function(...) {alert <<- galert("Wait...", title = "Detrend", delay=10000, parent=notebook)
                                 tryCatch(
                                          {Detrend <<- prospectr::detrend(X = Original, wav = as.numeric(colnames(Original)))
                                           faddtodtset("Detrend")
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(alert)
                                 gmessage(message = "Done!", title = "Detrend", icon = "info", parent = window)
                                 }
  # Continuum Removal
  fcrm         <- function(...) {alert <<- galert("Wait...", title = "Continuum Removal", delay=10000, parent=notebook)
                                 tryCatch(
                                          {ContRem <- prospectr::continuumRemoval(X=Original, wav = as.numeric(colnames(Original)),
                                                                                  type = "R", interpol="linear", method="division")
                                           ContinuumRemoval <<- ContRem[,c(-1,-ncol(ContRem))]
                                           faddtodtset("ContinuumRemoval")
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(alert)
                                 gmessage(message = "Done!", title = "Continuum Removal", icon = "info", parent = window)
                                 }
  # Savitzky-Golay Derivative
  fsgd         <- function(...) {alert <<- galert("Wait...", title = "Savitzky-Golay Derivative", delay=10000, parent=notebook)
                                 tryCatch(
                                          {SavitzkyGolayDerivative <<- prospectr::savitzkyGolay(Original,
                                                                                                p = as.numeric(svalue(sgd.poly)),
                                                                                                w = as.numeric(svalue(sgd.smooth)),
                                                                                                m = as.numeric(svalue(sgd.deriv)))
                                           faddtodtset("SavitzkyGolayDerivative")
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(alert)
                                 gmessage(message = "Done!", title = "Savitzky-Golay Derivative", icon = "info", parent = window)
                                 }
  # Standard Normal Variate
  fsnv         <- function(...) {alert <<- galert("Wait...", title = "SNV", delay=10000, parent=notebook)
                                 tryCatch(
                                          {SNV <<- prospectr::standardNormalVariate(X = Original)
                                           faddtodtset("SNV")
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(alert)
                                 gmessage(message = "Done!", title = "SNV", icon = "info", parent = window)
                                 }
  # Multiplicative Scatter Correction
  fmsc         <- function(...) {alert <<- galert("Wait...", title = "MSC", delay=10000, parent=notebook)
                                 tryCatch(
                                          {MSC <<- pls::msc(as.matrix(Original))
                                           faddtodtset("MSC")
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(alert)
                                 gmessage(message = "Done!", title = "MSC", icon = "info", parent = window)
                                 }
  # Normalization
  fnor         <- function(...) {alert <<- galert("Wait...", title = "Normalization", delay=10000, parent=notebook)
                                 tryCatch(
                                          {Normalization <<- clusterSim::data.Normalization(Original,
                                                                                            type = sub(":.*$","", svalue(nor.type)),
                                                                                            normalization = "row")
                                           faddtodtset("Normalization")
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(alert)
                                 gmessage("Done!", title = "Normalization", icon = "info",  parent = window)
                                 }
  
  ###################################################
  ### Modeling functions
  ###################################################
  
  # MLR
  fmlr        <- function(...)  {alert <<- galert("Wait... \nThis may take a few minutes!", title = "MLR model", delay=10000, parent=notebook)
                                 Sys.sleep(1) #Wait for alert to be shown
                                 tryCatch(
                                          {form.mlr  <- as.formula(paste(colnames(Train[last.col]),"~",
                                                                         paste(names(Train)[c(seq(1,last.col-1, by=svalue(mlr.band.interval)))],
                                                                               collapse="+"),collapse=""))
                                           mlr.model <<- stats::glm(form.mlr, data=Train)
                                           MLR       <<- stats::step(mlr.model, direction="both", trace=0)
                                           mlr.train <<- data.frame(Train[last.col], Predicted=mlr.model$fitted.values)
                                           mlr.val   <<- data.frame(Val[last.col], Predicted=predict(MLR, newdata=Val))
                                           faddtomodels("MLR")
                                           enabled(pred) = TRUE #Enable prediction module
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(alert)
                                 gmessage("MLR model done", title = "MLR model", parent = window)
                                 }
  # PLSR
  fpls        <- function(...) {alert <<- galert("Wait... \nThis may take a few minutes!", title = "PLSR model", delay=10000, parent=notebook)
                                Sys.sleep(1)
                                tryCatch(
                                         {bootctrl.pls <- caret::trainControl(method  <- svalue(pls.resampling),
                                                                              number  <- ifelse(grepl("cv", method), svalue(pls.kfold),
                                                                                                (svalue(pls.kfold)+10)),
                                                                              repeats <- ifelse(grepl("cv", method), svalue(pls.folds),
                                                                                                number)
                                                                              )
                                          Grid      <-  expand.grid(.ncomp = seq(1,svalue(pls.comp), 1))
                                          pls.test  <<- caret::train(form.mdl, data = Train, method = 'pls',
                                                                     trControl = bootctrl.pls, tuneGrid = Grid)
                                          PLSR      <<- pls::plsr(form.mdl, data = Train, ncomp = pls.test$bestTune$ncomp)
                                          t.pred    <-  data.frame(PLSR$fitted.values)
                                          v.pred    <-  data.frame(predict(PLSR, newdata=Val))
                                          pls.train <<- data.frame(Train[last.col], Predicted=t.pred[,ncol(t.pred)])
                                          pls.val   <<- data.frame(Val[last.col], Predicted=v.pred[,ncol(v.pred)])
                                          faddtomodels("PLSR")
                                          enabled(pred) = TRUE #Enable prediction module
                                          },
                                         warning = function(w) fwarning(w),
                                         error =  function(e) ferror(e)
                                         )
                                dispose(alert)
                                gmessage("PLSR model done", title = "PLSR model", parent = window)
                                }
  # SVM
  fsvm        <- function(...) {alert <<- galert("Wait... \nThis may take a few minutes! ", title = "SVM model", delay=10000, parent=notebook)
                                Sys.sleep(1)
                                tryCatch(
                                         {bootctrl.svm <<- caret::trainControl(method <- svalue(svm.resampling))
                                          if (svalue(svm.kernel, index=TRUE)==1) fsvmlinear()
                                          if (svalue(svm.kernel, index=TRUE)==2) fsvmradial()
                                          svm.train    <<- data.frame(Train[last.col], Predicted=SVM$fitted)
                                          svm.val      <<- data.frame(Val[last.col], Predicted=predict(SVM, newdata=Val))
                                          faddtomodels("SVM")
                                          enabled(pred) = TRUE #Enable prediction module
                                          },
                                         warning = function(w) fwarning(w),
                                         error =  function(e) ferror(e)
                                         )
                                dispose(alert)
                                gmessage("SVM model done", title = "SVM model", parent = window)
                                }
  fsvmlinear  <- function(...) {Grid        <-  expand.grid(.C = seq(1,16,5))
                                svm.test    <<- caret::train(form.mdl, data = Train, method = "svmLinear",
                                                             trControl = bootctrl.svm, tuneGrid = Grid)
                                SVM         <<- e1071::svm(form.mdl, data=Train, kernel="linear", type ="eps",
                                                           cost=svm.test$bestTune$C)
                                }
  fsvmradial  <- function(...) {Grid        <-  expand.grid(.sigma = seq(0.000001,0.1,0.01), .C = seq(1,16,5))
                                svm.test    <<- caret::train(form.mdl, data = Train, method = "svmRadial",
                                                             trControl = bootctrl.svm, tuneGrid = Grid)
                                SVM         <<- e1071::svm(form.mdl, data=Train, kernel="radial", type ="eps",
                                                           gamma=svm.test$bestTune$sigma, cost=svm.test$bestTune$C)
                                }
  # RF
  frf         <- function(...) {alert <<- galert("Wait... \nThis may take a few minutes! ", title = "RF model", delay=10000, parent=notebook)
                                Sys.sleep(1)
                                tryCatch(
                                         {bootControl <- caret::trainControl(method <- svalue(rf.resampling))
                                          Grid        <- expand.grid(.mtry = seq(svalue(rf.mtry)/5,svalue(rf.mtry),svalue(rf.mtry)/5))
                                          rf.test     <<- caret::train(form.mdl, data = Train, method = 'rf', trControl = bootControl,
                                                                       tuneGrid = Grid, importance = TRUE)
                                          RF          <<- randomForest::randomForest(form.mdl, data=Train, mtry=rf.test$bestTune$mtry,
                                                                                     ntree = as.numeric(svalue(rf.ntree)))
                                          rf.train    <<- data.frame(Train[last.col], Predicted=RF$predicted)
                                          rf.val      <<- data.frame(Val[last.col], Predicted=predict(RF, newdata=Val))
                                          faddtomodels("RF")
                                          enabled(pred) = TRUE #Enable prediction module
                                          },
                                         warning = function(w) fwarning(w),
                                         error =  function(e) ferror(e)
                                         )
                                dispose(alert)
                                gmessage("RF model done", title = "RF model", parent = window)
                                }
  # ANN
  fann         <- function(...) {alert <<- galert("Wait... \nThis may take a few minutes! ", title = "ANN model", delay=10000, parent=notebook)
                                 Sys.sleep(1)
                                 tryCatch(
                                          {bootControl  <- caret::trainControl(method= svalue(ann.resampling))
                                           Grid         <- expand.grid(.nhid= seq(1,svalue(ann.hid),ceiling(svalue(ann.hid)/10)),
                                                                       .actfun= c("sin", "radbas", "purelin", "tansig"))
                                           ann.test     <<- caret::train(form.mdl, data = Train, method = 'elm', trControl = bootControl,
                                                                         tuneGrid =Grid ,na.action = na.omit)
                                           ANN          <<- elmNN::elmtrain(form.mdl, data=Train, nhid=ann.test$bestTune$nhid,
                                                                            actfun= ann.test$bestTune$actfun)
                                           ann.train    <<- data.frame(Train[last.col], Predicted=ANN$fitted.values)
                                           ann.val      <<- data.frame(Val[last.col], Predicted=predict(ANN, newdata=Val))
                                           faddtomodels("ANN")
                                           enabled(pred) = TRUE #Enable prediction module
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(alert)
                                 gmessage("ANN model done", title = "ANN model", parent = window)
                                 }
  # KBML
  fkbml        <- function(...) {alert <<- galert("Wait... \nThis may take a few minutes! ", title = "KBML model", delay=10000, parent=notebook)
                                 Sys.sleep(1)
                                 tryCatch(
                                          {bootctrl.kbml <<- caret::trainControl(method= svalue(kbml.resampling))
                                           if (svalue(kbml.kernel, index=TRUE)==1) fkbmllinear()
                                           if (svalue(kbml.kernel, index=TRUE)==2) fkbmlradial()
                                           kbml.train    <<- data.frame(Train[last.col], Predicted=predict(KBML, newdata=Train))
                                           kbml.val      <<- data.frame(Val[last.col], Predicted=predict(KBML, newdata=Val))
                                           faddtomodels("KBML")
                                           enabled(pred) = TRUE #Enable prediction module
                                           },
                                          warning = function(w) fwarning(w),
                                          error =  function(e) ferror(e)
                                          )
                                 dispose(alert)
                                 gmessage("KBML model done", title = "KBML model", parent = window)
                                 }
  fkbmllinear  <- function(...) {kbml.test <<- caret::train(form.mdl, data = Train, method = 'gaussprLinear',
                                                            trControl = bootctrl.kbml, tuneLength = 10)
                                KBML       <<- kernlab::gausspr(form.mdl, data=Train, kernel= "vanilladot",
                                                                type = "regression", kpar= "automatic", variance.model = T,
                                                                var=as.numeric(svalue(kbml.var)), cross= svalue(kbml.cross))
                                }
  fkbmlradial  <- function(...) {Grid      <-  expand.grid(.sigma = seq(.00001,.1,.005))
                                kbml.test  <<- caret::train(form.mdl, data = Train, method = 'gaussprRadial', tuneLength = 10,
                                                            trControl = bootctrl.kbml, tuneGrid = Grid)
                                KBML       <<- kernlab::gausspr(form.mdl, data=Train, kernel="rbfdot",
                                                                type ="regression", kpar= "automatic", variance.model = T,
                                                                var=svalue(kbml.var), cross= svalue(kbml.cross))
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
  kernel.param.kbml    <- c("Linear kernel", "Radial kernel")
  kbml.param.var       <- c(.0001,.001,.01,.1,1,10,100)
  pred.models          <- c()

  ###################################################
  ### Main window
  ###################################################
  
  ### Create main window
  window        <- gwindow("Alrad Spectra", visible=F, width = 600,height = 600)
  ### Confirm window closing
  addHandlerUnrealize(window, handler = fconfirmquit)
  ### Clear, Quit and About buttons
  action.list   <- list(clear =  gaction(label = "Clear",  icon = "clear",  handler = fclear),
                        quit = gaction(label = "Quit", icon = "quit",  handler = fquit),
                        about = gaction(label = "About", icon = "about",  handler = fabout))
  toolbar.list  <- c(action.list[c("clear")], sep = gseparator(), action.list["quit"],
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
                    lyt.file.arg[1,1,anchor=c(-1,-1)] <- "Header:"
  file.header    <- lyt.file.arg[2,1,anchor=c(0,0)]   <- gcombobox(c("TRUE", "FALSE"), cont = lyt.file.arg)
                    lyt.file.arg[1,2,anchor=c(-1,-1)] <- "Separator:"
  file.sep       <- lyt.file.arg[2,2,anchor=c(1,1)]   <- gedit(text = ",", cont = lyt.file.arg, width = 1)
                    lyt.file.arg[1,3,anchor=c(1,0)]   <- "Spectral data \nstarts at column:"
  spc.start.col  <- lyt.file.arg[2,3,anchor=c(0,0)]   <- gedit(text = "", cont = lyt.file.arg, width = 2)
                    lyt.file.arg[1,4,anchor=c(1,0)]   <- "Spectral data \nends at column:"
  spc.end.col    <- lyt.file.arg[2,4,anchor=c(0,0)]   <- gedit(text = "", cont = lyt.file.arg, width = 4)
                    lyt.file.arg[1,5,anchor=c(1,0)]   <- "Spectrum starts \nat wavelength:"
  spc.first      <- lyt.file.arg[2,5,anchor=c(0,0)]   <- gedit(text = "", cont = lyt.file.arg, width = 4)
                    lyt.file.arg[1,6,anchor=c(1,0)]   <- "Spectrum ends \nat wavelength:"
  spc.last       <- lyt.file.arg[2,6,anchor=c(0,0)]   <- gedit(text = "", cont = lyt.file.arg, width = 4)
                    lyt.file.arg[1,7,anchor=c(1,0)]   <- "Soil variable \nis at column:"
  soil.var.col   <- lyt.file.arg[2,7,anchor=c(0,0)]   <- gedit(text = "", cont = lyt.file.arg, width = 4)
  ### Import button
  gbutton("Import data", cont = import, handler = fimport)
  ### View data button
  gbutton("View data", cont = import, handler = function(...) fview(alldata, 800))
  ### Plot imported data button
  gbutton("Plot imported spectra", cont = import, handler = function(...) fplot(Original, spectra.start.number, spectra.end.number))

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
  gbutton("Plot spectra", cont = nrm, handler = function(...) fplot(Smoothing, spectra.start.number, spectra.end.number))
  gbutton("Save preprocessed spectra", cont = nrm, handler = function(...) fsavespectra(Smoothing))
  ### Binning
  frame.desc.bin     <- gframe("Description:", cont = bin, horizontal = T)
  lyt.desc.bin       <- glayout(cont = frame.desc.bin , expand = TRUE)
  lyt.desc.bin[1,1]  <- "Compute average values of a signal in pre-determined bins. Package: prospectr"
  frame.param.bin    <- gframe("Parameters:", cont = bin, horizontal=T)
  lyt.param.bin      <- glayout(cont = frame.param.bin, expand = TRUE)
  lyt.param.bin[1,1] <- "Bin size"
  bin.number         <- lyt.param.bin[2,1:4] <- gspinbutton(from = 2, to = 100, by = 1, cont = lyt.param.bin)
  gbutton("Run", cont = bin, handler = fbin)
  gbutton("Plot spectra", cont = bin, handler = function(...) fplot(Binning, spectra.start.number, spectra.end.number))
  gbutton("Save preprocessed spectra", cont = bin, handler = function(...) fsavespectra(Binning))
  ### Absorbance
  frame.desc.abs     <- gframe("Description:", cont = abs, horizontal=T)
  lyt.desc.abs       <- glayout(cont = frame.desc.abs, expand = TRUE)
  lyt.desc.abs[1,1]  <- "Transforms reflectance to absorbance values (log10(1/R))."
  gbutton("Run", cont = abs, handler = fabs)
  gbutton("Plot spectra", cont = abs, handler = function(...) fplot(Absorbance, spectra.start.number, spectra.end.number, ylab="Absorbance"))
  gbutton("Save preprocessed spectra", cont = abs, handler = function(...) fsavespectra(Absorbance))
  ### Detrend
  frame.desc.det     <- gframe("Description:", cont = det, horizontal=T)
  lyt.desc.det       <- glayout(cont = frame.desc.det, expand = TRUE)
  lyt.desc.det[1,1]  <- "Normalizes each row by applying a Standard Normal Variate transformation followed by fitting a second order \nlinear model and returning the fitted residuals. Package: prospectr"
  gbutton("Run", cont = det, handler = fdet)
  gbutton("Plot spectra", cont = det, handler = function(...) fplot(Detrend, spectra.start.number, spectra.end.number))
  gbutton("Save preprocessed spectra", cont = det, handler = function(...) fsavespectra(Detrend))
  ### Continuum Removal
  frame.desc.crm     <- gframe("Description:", cont = crm, horizontal=T)
  lyt.desc.crm       <- glayout(cont = frame.desc.crm, expand = TRUE)
  lyt.desc.crm[1,1]  <- "The continuum removal technique was introduced by Clark and Roush (1984). The algorithm find points lying on the \nconvex hull of a spectrum, connects the points by linear interpolation and normalizes the spectrum by \ndividing the input data by the interpolated line. Package: prospectr"
  lyt.desc.crm[2,1]  <- "Data type: Reflectance"
  lyt.desc.crm[3,1]  <- "Interpolation method: Linear"
  lyt.desc.crm[4,1]  <- "Normalization method: Division"
  gbutton("Run", cont = crm, handler = fcrm)
  gbutton("Plot spectra", cont = crm, handler = function(...) fplot(ContinuumRemoval, spectra.start.number, spectra.end.number))
  gbutton("Save preprocessed spectra", cont = crm, handler = function(...) fsavespectra(ContinuumRemoval))
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
  gbutton("Plot spectra", cont = sgd, handler = function(...) fplot(SavitzkyGolayDerivative, spectra.start.number, spectra.end.number))
  gbutton("Save preprocessed spectra", cont = sgd, handler = function(...) fsavespectra(SavitzkyGolayDerivative))
  ### SNV
  frame.desc.snv     <- gframe("Description:", cont = snv, horizontal=T)
  lyt.desc.snv       <- glayout(cont = frame.desc.snv, expand = TRUE)
  lyt.desc.snv[1,1]  <- "Standard Normal Variate normalizes each row by substracting each row by its mean and dividing by \nits standard deviation. Package: prospectr"
  gbutton("Run", cont = snv, handler = fsnv)
  gbutton("Plot spectra", cont = snv, handler = function(...) fplot(SNV, spectra.start.number, spectra.end.number))
  gbutton("Save preprocessed spectra", cont = snv, handler = function(...) fsavespectra(SNV))
  ### MSC
  frame.desc.msc     <- gframe("Description:", cont = msc, horizontal=T)
  lyt.desc.msc       <- glayout(cont = frame.desc.msc, expand = TRUE)
  lyt.desc.msc[1,1]  <- "Performs multiplicative scatter/signal correction on spectral data. Package: pls"
  gbutton("Run", cont = msc, handler = fmsc)
  gbutton("Plot spectra", cont = msc, handler = function(...) fplot(MSC, spectra.start.number, spectra.end.number))
  gbutton("Save preprocessed spectra", cont = msc, handler = function(...) fsavespectra(MSC))
  ### Normalization
  frame.desc.nor     <- gframe("Description:",cont = nor, horizontal = T)
  lyt.desc.nor       <- glayout(cont = frame.desc.nor , expand = TRUE)
  lyt.desc.nor[1,1]  <- "Different types of data normalization. Package: clusterSim"
  frame.param.nor    <- gframe("Parameters:", cont = nor, horizontal=T)
  lyt.param.nor      <- glayout(cont = frame.param.nor, expand = TRUE)
  lyt.param.nor[1,1] <- "Type of Normalization."
  nor.type           <- lyt.param.nor[2,1] <- gradio(normalization.types, checked = T, cont = lyt.param.nor)
  gbutton("Run", cont = nor, handler = fnor)
  gbutton("Plot spectra", cont = nor, handler = function(...) fplot(Normalization, spectra.start.number, spectra.end.number))
  gbutton("Save preprocessed spectra", cont = nor, handler = function(...) fsavespectra(Normalization))

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
  mdl                <- gnotebook(cont = models)
  enabled(models) = FALSE #Disable modeling module
  enabled(mdl)    = FALSE #Disable models notebook
  ### MLR
  mdl.mlr            <- ggroup(cont = mdl, horizontal = F,label = gettext("   MLR   "))
  frame.desc.mlr     <- gframe("Description:",cont = mdl.mlr, horizontal = T)
  lyt.desc.mlr       <- glayout(cont = frame.desc.mlr, expand = TRUE)
  lyt.desc.mlr[1,1]  <- "Multiple Linear Regression. Packages: stats / MASS / caret"
  frame.param.mlr    <- gframe("Tuning parameters:", cont = mdl.mlr, horizontal=T)
  lyt.param.mlr      <- glayout(cont = frame.param.mlr , expand = TRUE)
  lyt.param.mlr[1,1] <- "Band interval"
  mlr.band.interval  <- lyt.param.mlr[2,1] <- gspinbutton(from = 1, to = 30, by = 1, value = 25, cont = lyt.param.mlr)
  gbutton("Run MLR model", cont = mdl.mlr, handler = fmlr)
  gbutton("MLR model results", cont = mdl.mlr, handler = function(...) fmdl.stats(mlr.train, mlr.val))
  gbutton("Plot model accuracy",cont = mdl.mlr, handler = function(...) fmdl.plot.res(mlr.train, mlr.val))
  ### PLS
  mdl.pls            <- ggroup(cont = mdl, horizontal = F,label = gettext("   PLSR   "))
  frame.desc.pls     <- gframe("Description:",cont = mdl.pls, horizontal = T)
  lyt.desc.pls       <- glayout(cont = frame.desc.pls, expand = TRUE)
  lyt.desc.pls[1,1]  <- "Partial Least Squares Regression. Packages: pls / caret"
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
  gbutton("Plot variable importance", cont = mdl.pls, handler = function(...) fpls.plot.imp(pls.test))
  gbutton("PLSR model results", cont = mdl.pls, handler = function(...) fmdl.stats(pls.train, pls.val))
  gbutton("Plot model accuracy",cont = mdl.pls, handler = function(...) fmdl.plot.res(pls.train, pls.val))
  ### SVM
  mdl.svm            <- ggroup(cont = mdl, horizontal = F,label = gettext("    SVM    "))
  frame.desc.svm     <- gframe("Description:",cont = mdl.svm, horizontal = T)
  lyt.desc.svm       <- glayout(cont = frame.desc.svm, expand = TRUE)
  lyt.desc.svm[1,1]  <- "Support Vector Machine. Packages: e1071 / caret"
  frame.param.svm    <- gframe("Tuning parameters:", cont = mdl.svm, horizontal=T)
  lyt.param.svm      <- glayout(cont = frame.param.svm , expand = TRUE)
  lyt.param.svm[1,1] <- "Resampling method"
  svm.resampling     <- lyt.param.svm[2,1] <- gcombobox(train.ctrl.method, cont = lyt.param.svm)
  lyt.param.svm[1,2] <- "Kernel parameters"
  svm.kernel         <- lyt.param.svm[2,2] <- gradio(kernel.param.svm, cont = lyt.param.svm)
  gbutton("Run SVM model", cont = mdl.svm, handler = fsvm)
  gbutton("Plot variable importance", cont = mdl.svm, handler = function(...) fmdl.plot.imp(svm.test))
  gbutton("SVM model results", cont = mdl.svm, handler = function(...) fmdl.stats(svm.train, svm.val))
  gbutton("Plot model accuracy",cont = mdl.svm, handler = function(...) fmdl.plot.res(svm.train, svm.val))
  ### RF
  mdl.rf             <- ggroup(cont = mdl, horizontal = F,label = gettext("    RF    "))
  frame.desc.rf      <- gframe("Description:",cont = mdl.rf, horizontal = T)
  lyt.desc.rf        <- glayout(cont = frame.desc.rf, expand = TRUE)
  lyt.desc.rf[1,1]   <- "Implements Breiman's Random Forest. Packages: randomForest / caret"
  frame.param.rf     <- gframe("Tuning parameters:", cont = mdl.rf, horizontal=T)
  lyt.param.rf       <- glayout(cont = frame.param.rf , expand = TRUE)
  lyt.param.rf[1,1]  <- "Resampling method"
  rf.resampling      <- lyt.param.rf[2,1]  <- gcombobox(train.ctrl.method.rf, cont = lyt.param.rf)
  lyt.param.rf[1,2]  <- "Randomly selected predictors \n(mtry)"
  rf.mtry            <- lyt.param.rf[2,2]  <- gspinbutton(from = 5, to = 500, by = 5, value = 5, cont = lyt.param.rf)
  lyt.param.rf[1,3]  <- "Number of trees \n(ntree)"
  rf.ntree           <- lyt.param.rf[2,3]  <- gedit(text = "500", cont = lyt.param.rf, width = 4)
  gbutton("Run RF model", cont = mdl.rf, handler = frf)
  gbutton("Plot variable importance", cont = mdl.rf, handler = function(...) fmdl.plot.imp(rf.test))
  gbutton("RF model results", cont = mdl.rf, handler = function(...) fmdl.stats(rf.train, rf.val))
  gbutton("Plot model accuracy",cont = mdl.rf, handler = function(...) fmdl.plot.res(rf.train, rf.val))
  ### ANN
  mdl.ann            <- ggroup(cont = mdl, horizontal = F,label = gettext("    ANN    "))
  frame.desc.ann     <- gframe("Description:",cont = mdl.ann, horizontal = T)
  lyt.desc.ann       <- glayout(cont = frame.desc.ann, expand = TRUE)
  lyt.desc.ann[1,1]  <- "Artificial Neural Network. Packages: elmNN / caret / MASS"
  frame.param.ann    <- gframe("Tuning parameters:", cont = mdl.ann, horizontal=T)
  lyt.param.ann      <- glayout(cont = frame.param.ann , expand = TRUE)
  lyt.param.ann[1,1] <- "Resampling method"
  ann.resampling     <- lyt.param.ann[2,1] <- gcombobox(train.ctrl.method, cont = lyt.param.ann)
  lyt.param.ann[1,2] <- "Activation function"
  lyt.param.ann[2,2] <- gcombobox(actf, cont = lyt.param.ann)
  lyt.param.ann[1,3] <- "Hidden units"
  ann.hid            <- lyt.param.ann[2,3] <- gspinbutton(from = 1, to = 50, by = 1, value = 50, cont = lyt.param.ann)
  gbutton("Run ANN model", cont = mdl.ann, handler = fann)
  gbutton("Plot variable importance", cont = mdl.ann, handler = function(...) fmdl.plot.imp(ann.test))
  gbutton("ANN model results", cont = mdl.ann, handler = function(...) fmdl.stats(ann.train, ann.val))
  gbutton("Plot model accuracy", cont = mdl.ann, handler = function(...) fmdl.plot.res(ann.train, ann.val))
  ### KBML
  mdl.kbml            <- ggroup(cont = mdl, horizontal = F,label = gettext(" KBML "))
  frame.desc.kbml     <- gframe("Description:",cont = mdl.kbml, horizontal = T)
  lyt.desc.kbml       <- glayout(cont = frame.desc.kbml, expand = TRUE)
  lyt.desc.kbml[1,1]  <- "Kernel-Based Machine Learning. Implements Gaussian processes for regression. Packages: kernlab / caret"
  frame.param.kbml    <- gframe("Tuning parameters:", cont = mdl.kbml, horizontal=T)
  lyt.param.kbml      <- glayout(cont = frame.param.kbml , expand = TRUE)
  lyt.param.kbml[1,1] <- "Resampling method"
  kbml.resampling     <- lyt.param.kbml[2,1] <- gcombobox(train.ctrl.method, cont = lyt.param.kbml)
  lyt.param.kbml[1,2] <- "Initial noise variance"
  kbml.var            <- lyt.param.kbml[2,2] <- gcombobox(kbml.param.var, selected = 2, cont = lyt.param.kbml)
  lyt.param.kbml[1,3] <- "For cv resampling method only: \nnumber of folds (k-fold)"
  kbml.cross          <- lyt.param.kbml[2,3] <- gspinbutton(from = 2, to = 100, by = 1, value = 10, cont = lyt.param.kbml)
  lyt.param.kbml[1,4] <- "kernel function \nused in training and predicting"
  kbml.kernel         <- lyt.param.kbml[2,4] <- gradio(kernel.param.kbml, cont = lyt.param.kbml)
  gbutton("Run KBML model", cont = mdl.kbml, handler = fkbml)
  gbutton("Plot variable importance", cont = mdl.kbml, handler = function(...) fmdl.plot.imp(kbml.test))
  gbutton("KBML model results", cont = mdl.kbml, handler = function(...) fmdl.stats(kbml.train, kbml.val))
  gbutton("Plot model accuracy", cont = mdl.kbml, handler = function(...) fmdl.plot.res(kbml.train, kbml.val))
  
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
                         pred.lyt.file.arg[1,1,anchor=c(-1,-1)] <- "Header:"
  pred.file.header    <- pred.lyt.file.arg[2,1,anchor=c(0,0)]   <- gcombobox(c("TRUE", "FALSE"), cont = pred.lyt.file.arg)
                         pred.lyt.file.arg[1,2,anchor=c(-1,-1)] <- "Separator:"
  pred.file.sep       <- pred.lyt.file.arg[2,2,anchor=c(1,1)]   <- gedit(text = ",", cont = pred.lyt.file.arg, width = 1)
                         pred.lyt.file.arg[1,3,anchor=c(1,0)]   <- "Spectrum starts \nat wavelength:"
  pred.spc.first      <- pred.lyt.file.arg[2,3,anchor=c(0,0)]   <- gedit(text = "", cont = pred.lyt.file.arg, width = 4)
                         pred.lyt.file.arg[1,4,anchor=c(1,0)]   <- "Spectrum ends \nat wavelength:"
  pred.spc.last       <- pred.lyt.file.arg[2,4,anchor=c(0,0)]   <- gedit(text = "", cont = pred.lyt.file.arg, width = 4)
  ### Import button
  gbutton("Import data", cont = pred.imp, handler = fimport.pred)
  ### View data button
  gbutton("View data", cont = pred.imp, handler = function(...) fview(spc.pred, 800))
  ### Plot imported data button
  gbutton("Plot imported spectra", cont = pred.imp, handler = function(...) fplot(spc.pred, pred.spectra.start.number, pred.spectra.end.number))
  ### Draw a separator line
  gseparator(cont = pred)
  ### Create predict group
  pred.predict        <- ggroup(cont = pred, horizontal = F)
  ### Select model for prediction
  glabel("Select model for prediction:", cont = pred.predict, anchor = c(-1,0))
  select.model        <- gcombobox("", cont = pred.predict)
  gbutton("Predict", cont = pred.predict, handler = fpredict)
  gbutton("View predictions", cont = pred.predict, handler = function(...) fview(prediction, 300))
  gbutton("Save predictions", cont = pred.predict, handler = function(...) fsaveresults(prediction))
  enabled(pred.predict) = FALSE #Disable predict group
  enabled(pred) = FALSE #Disable prediction module

  ### Focus on first tabs
  svalue(notebook) <- 1
  svalue(pp)       <- 1
  svalue(mdl)      <- 1
  ### Window visibility
  visible(window)  <- TRUE
  
} #Closes AlradSpectra() function
