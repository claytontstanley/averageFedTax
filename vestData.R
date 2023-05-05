library(data.table)


aTbl = fread('foo.csv')

bTbl = aTbl[, .(Jurisdiction, `Amount Subject to Tax Withholding`, `Percentage of Income Subject to Withholding`)]

bTbl

colnames(aTbl)

aTbl[, .(`Award Date`, `Vesting Date`, `Jurisdiction`, `Total Income of GSUs at Vest`)
     ][grepl('New York', Jurisdiction)
     ][!grepl('2022', `Award Date`)
     ][, rid := 1:.N
     ][order(-rid)
#     ][, as.numeric(sum(as.numeric(gsub('[$,]', '', `Total Income of GSUs at Vest`)) * 100))
     ]

bTbl[grepl('New York', Jurisdiction)][, g := gsub('[$,]*', '', `Amount Subject to Tax Withholding`)][, g := as.numeric(g)][][, sum(g)]

69341 * .0685
