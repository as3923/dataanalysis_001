

## Download the loans data

setInternet2(T)
url <- "https://spark-public.s3.amazonaws.com/dataanalysis/loansData.csv"
dest <- "raw/loansData.csv"
download.file(url, dest)
dateDownloaded <- date()


## Interesting graphs
plot(loans$FICO.Range, loans$Interest.Rate)