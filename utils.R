library(data.table)
library(ggplot2)
library(cumstats)
library(scales)

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
     ][, {bTbl <<- copy(.SD); .SD}
     ]

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

bTbl[, .SD
     ][, dIncEarned := incEarned - shift(incEarned)
     ][rowid %in% sample(.N, 10000) | bracketP
     ][, ggplot(.SD, aes(inc, dIncEarned))
     + geom_point()
     ]

bTbl[, .SD
     ][inc %in% c(
		  350000 
		  , 350000 - 12550
		  , 350000 - 12550 - 19500
		  , 300000 - 12550 - 19500
		  )
     ]

getPayTax <- function(pay, bonus, equity, profit, pretax, deduct) {
	rate = bTbl[inc == (pay + bonus + equity + profit) - pretax - deduct][, rateMean]
	rate
	tax = (pay - pretax + bonus + equity + profit - deduct) * rate
	print(tax)
	taxFrom22 = (bonus + equity) * .22
	taxFrom22
	taxRem = tax - taxFrom22
	taxRem
	taxRem / (pay - pretax)
}

2020
getPayTax(151000, 20075, 44440, 5000, 19000 + 3000, 12000)

2021
getPayTax(155956, 31450, 110214, 0, 19500, 12550)
getPayTax(155956, 31450, 110214, 0, 19500 + 3000, 12550)
getPayTax(155956, 31450, 110214, 5000, 19500 + 3000, 12550)

2022
getPayTax(157000, 35000, 162973, 0, 19500, 12550)
getPayTax(157000, 35000, 162973, 0, 19500 + 3000, 12550)




