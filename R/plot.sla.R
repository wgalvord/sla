plot.sla <-
function(slaObj, modelType2Plot = 'A', ...) 
{
	## construct new data frame
	input.df <- slaObj$INPUT.df
	summary.input <- summary(slaObj$INPUT.df)
	## 
	group <- input.df[,1] 
	x <- input.df[,2]
	y <- input.df[,3]
	new.df <- data.frame(group, x, y)
	summary.new.df <- summary(new.df)
	n1 <- with(new.df, table(group))[1]
	n2 <- with(new.df, table(group))[2]
	new.group <- c(rep('one', n1), rep('two', n2))
	new.df.2 <- data.frame(new.group, x, y)
	##  
	with(new.df, plot(y ~ x, xlab = names(input.df)[2], ylab = names(input.df)[3], col = ifelse(new.group == 'one', 'blue', 'red'), lwd = 2))
	## get names groups from original data frame for title
	blue.names <- as.character(with(new.df, levels(group)[1]))
	red.names <- as.character(with(new.df, levels(group)[2]))
	## create subtitle string
	sub.title.string <- paste(blue.names, "[blue]", ".......", red.names, "[red]" )
	##
	switch(modelType2Plot, 
	A = {
	title('Model A: 4 Parms Estimated\nInd Intercepts & Ind Slopes', sub = sub.title.string)
	abline(slaObj$Mod.A$coef[1:2], col = 'blue', lwd = 3)
	abline(slaObj$Mod.A$coef[3:4], col = 'red', lwd = 3)
	print(slaObj$Pretty.Table) } ,
	B = {
	title('Model B: 2 Parms Estimated\nCom Intercept & Com Slope', sub = sub.title.string)
	abline(slaObj$Mod.B$coef[1:2], col = 'black', lwd = 3)
	print(slaObj$Pretty.Table) } ,
	C = {
	title('Model C: 3 Parms Estimated\nInd Intercepts & Com Slope', sub = sub.title.string)
	abline(slaObj$Mod.C$coef[c(1,3)], col = 'blue', lwd = 3)
	abline(slaObj$Mod.C$coef[c(2,3)], col = 'red', lwd = 3)
	print(slaObj$Pretty.Table) } ,
	D = {
	title('Model D: 3 Parms Estimated\nCom Intercept & Ind Slopes', sub = sub.title.string)
	abline(slaObj$Mod.D$coef[c(1,2)], col = 'blue', lwd = 3)
	abline(slaObj$Mod.D$coef[c(1,3)], col = 'red', lwd = 3)
	print(slaObj$Pretty.Table) }
	)
	}
