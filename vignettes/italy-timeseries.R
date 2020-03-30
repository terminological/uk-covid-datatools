italyTimeseries = read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")

names(italyTimeseries) <- c(
  "date",	"state",	"Hospitalised with symptoms",	"intensive care",	"Total hospitalised",	"Home isolation",	"Total currently positive",	"New currently positive",	"discharged healed",	"deceased",	"Total cases",	"Tested", "note1","note2"
)