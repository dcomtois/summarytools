library(summarytools)
st_options(style                  = "rmarkdown", # Note that this doesn't apply to dfSummary()
           plain.ascii            = FALSE,       # Absolutely needed
           dfSummary.varnumbers   = FALSE,       # Make dfs tables a bit narrower
           dfSummary.valid.col    = FALSE,       # "   "   "   "   "   "   "   "
           subtitle.strength      = 2)           # uses h4 instead of <strong> for 2nd heading

freq(tobacco$gender)
descr(tobacco$age)
descr(tobacco)
dfSummary(tobacco)

st_options(subtitle.strength = 1)       # uses h4 instead of <strong> for 2nd heading
freq(tobacco$gender)
descr(tobacco$age)
descr(tobacco)
dfSummary(tobacco)
