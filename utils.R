library(data.table)
library(ggplot2)
library(cumstats)
library(scales)


getTaxTbl <- function() {
	aTbl = data.table(rate=c(.1, .12, .22, .24, .32, .35, .37),
			  inc=c(0, 9525, 38700, 82500, 157500, 200000, 500000))
	aTbl[, .SD
	     ][.(inc=0:1000000), on=.(inc), roll=Inf
	     ][, copy(.SD)[aTbl, bracketP := T, on=.(inc)]
	     ][inc == 0, bracketP := NA
	     ][, inc := inc + 1
	     ][, rateMean := cummean(rate)
	     ][, incEarned := inc - inc * rateMean
	     ][, rowid := 1:.N
	     ][, .SD
	     ]
}

getPayTax <- function(pay, bonus, equity, profit, pretax, deduct) {
	rate = bTbl[inc == (pay + bonus + equity + profit) - pretax - deduct][, rateMean]
	rate
	totTax = (pay - pretax + bonus + equity + profit - deduct) * rate
	taxFrom22 = (bonus + equity) * .22
	taxRem = totTax - taxFrom22
	data.table(payTaxRate=taxRem / (pay - pretax), aveTaxRate=rate, totTax=totTax)
}


bTbl = getTaxTbl()


bTbl[, .SD
     ][rowid %in% sample(.N, 10000) | bracketP
     ][, ggplot(.SD, aes(inc, rateMean))
     + geom_line()
     + geom_point(data=.SD[bracketP == T])
     + geom_point(data=.SD[bracketP == T], aes(inc, rate), color='blue')
     + xlab('Income')
     + ylab('Average Tax')
     + scale_x_continuous(label=comma, breaks=pretty_breaks(n=10))
     + scale_y_continuous(label=percent, breaks=pretty_breaks(n=10))
     + theme_bw()
     ]

getPayTax(pay=157000
	  , bonus=35000
	  , equity=157663
	  , profit=1000
	  , pretax=20000
	  , deduct=15550)




