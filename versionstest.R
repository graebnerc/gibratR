# Dieses Skript überprüft up alle Pakete installiert sind, die für das Gibrat
# Modell benöitgt werden. Falls nicht, werden die Pakete installiert. 
# Am Ende sollten alle Pakete ohne Fehler eingeladen werden. Falls nicht, 
# melden Sie sich bitte mit einem Screenshot der Fehlermeldung bei Claudius Gräbner

# Ich empfehle Ihnen zudem Ihre R Version zu updaten wenn Sie nicht mindestens
# Version 3.6.1 verwenden. Dieses Sktipt überprüft auch die aktuelle Version von
# R und gibt Ihnen einen Hinweis wenn Sie updaten sollten.

versionstest <- c(
  as.double(R.version$major)<3,
  as.double(substr(R.version$minor, 1, 1))<6,
  as.double(substr(R.version$minor, 1, 1))<6
)

if (TRUE %in% versionstest){
  print(paste0(
    "ACHTUNG! Ihre R-Version ist veraltet! ",
    "Sie verwenden Version ", R.version$major, ".", R.version$minor, 
    ", aber Sie sollten mindestens Version 3.6.1 verwenden. ",
    "Installieren Sie zum Update R einfach neu!")
  )
} else{
  print("Super, Ihre Version von R ist ausreichend aktuell!")
}


if (!require(devtools)) install.packages('devtools')
if (!require(shiny)) install.packages('shiny')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(magrittr)) install.packages('magrittr')
if (!require(ggpubr)) install.packages('ggpubr')

ggplot_version <- as.character(packageVersion("ggplot2"))

if ((as.double(substr(ggplot_version, 1, 1)) == 3) &  
    (as.double(substr(ggplot_version, 3, 3)) >= 4) ) {
  print("ggplot2 ist ausreichend aktuell.")
} else {
  cat(paste0(
    "ACHTUNG! Ihre Version von ggplot ist veraltet!\n ",
    "Sie verwenden Version ", ggplot_version, 
    ", aber Sie sollten mindestens Version 3.3.0 verwenden. ",
    "\nInstallieren Sie das Paket neu mit:"))
    cat("\ninstall.packages('ggplot2')")
    cat("\noder")
    cat("\ninstall.packages('tidyverse')")
    cat("\nÜberprüfen Sie danach erneut ob die richtige Version installiert ist!")
}
