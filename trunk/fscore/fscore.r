# Measures


confusion = function(tp, aux) {
	tmp = ifelse(tp != 0, tp/(tp + aux), 0);
	return(tmp);
}


fm = function(prs, rec) {

	f = (1.25 * prs * rec)/((0.25 * prs) + rec);
	return(f);
}


values = function(train, noise, vote) {

	t = intersect(row.names(train), noise);
	f = setdiff(row.names(train), noise);

	p = row.names(train[train$Class != vote,]);
	n = row.names(train[train$Class == vote,]);

	tp = length(intersect(t, p));
	tn = length(intersect(t, n));

	fp = length(intersect(f, p));
	
	prs = confusion(tp, fp);
	rec = confusion(tp, tn);
	return(fm(prs, rec));
}
