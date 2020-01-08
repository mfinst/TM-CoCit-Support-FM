phraseTokenizer <-
function(x) RWeka::NGramTokenizer( x, Weka_control(min = 1, max = 3))
