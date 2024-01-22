library(data.table)
library(ggplot2)
library(cumstats)
library(scales)


getTaxTbl <- function() {
	aTbl = data.table(rate=c(.1, .12, .22, .24, .32, .35, .37),
			  inc=c(0, 11600, 47150, 100525, 191950, 243725, 609350)
	)
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
	totPay = pay + bonus + equity + profit
	totInc = totPay - pretax - deduct
	rate = bTbl[inc == totInc][, rateMean]
	totTax = totInc * rate
	taxFrom22 = (bonus + equity) * .22
	taxRem = totTax - taxFrom22
	data.table(payTaxRate=taxRem / (pay - pretax)
		   , aveTaxRate=rate
		   , totTax=totTax
		   , totPay=totPay)
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

source('data.R')

getPayTax(pay=pay
	  , bonus=bonus
	  , equity=equity
	  , profit=profit
	  , pretax=pretax
	  , deduct=deduct)
