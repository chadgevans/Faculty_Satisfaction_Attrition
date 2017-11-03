Let's look for correlation among the quantitative variables.

```{r faculty features, warning=FALSE}
nums <- sapply(df, is.numeric)
corrplot(cor(df[ , nums], use="pairwise.complete.obs"))

```

### Number of Courses (total) and Number of Institutional Employers

We saw earlier that COURSENUM is correlated with PTTEACH. Let's take a closer look.

```{r, warning=FALSE}
ggplot(aes(x=COURSENUM, y=PTTEACH), data=df) +
  geom_point(color='red')
```

Some of these faculty members teach a striking number of courses.  One faculty member has no additional institutional employers (1 total inst.) and teaches 20 courses for that school.  This is probably some kind of online school or something.  Let's see the distribution of number of courses.

```{r}
table(df$COURSENUM)
```


Most faculty teach between 1 and 5 classes.  Faculty teaching 6 or more classes (in current term) are less common.  There are some faculty with teaching loads that are hard to imagine.

Let's see how much faculty are paid per course.

### Pay per course and total institutional salary

We saw from the correlation plot that PTPAY is correlated with PTSALARY. Among part-time faculty, there is a correlation between their pay and salary.  That makes senes.  Let's take a closer look.

```{r, warning=FALSE}
ggplot(aes(x=PTPAY, y=PTSALARY), data=df) +
geom_point(color='red')
```

Those linearity patterns are tied to the integer values of courses taught.

### Total institutional employers and Pct Salary from teaching at other institutions
This correlation also makes a great deal of sense.  As a faculty member works at additional institutions, his or her pay from other institutions goes up.
```{r, warning=FALSE}
ggplot(aes(x=PTTEACH, y=SALARYSOURCE03), data=df) +
geom_point(color='red')
```

### Satisfaction with compensation and satisfaction with workplace
There is a consistent relationship between satisfaction with compensation and satisfaction with the place one works at.  That isn't surprising.

```{r, warning=FALSE}
ggplot(aes(x=SATIS_COMPENSATION, y=SATIS_WORKPLACE), data=df) +
  geom_point(color='red')
```

### Salary from institution and non-academic income
```{r, warning=FALSE}
ggplot(aes(x=SALARYSOURCE01, y=SALARYSOURCE04), data=df) +
  geom_point(color='red')
```
As a faculty members salary increases from their institution, their non-academic income decreases.  Some of these values are outside the range of possible values.  Should clean up before analysis.

### Pct. Income from other Institutions and Number of Institutions

```{r, warning=FALSE}
ggplot(aes(x=SALARYSOURCE03, y=PTTEACH), data=df) +
  geom_point(color='red')
```
This also makes a great deal of sense.

## Summary of Exploratory Work
* Full time non-tenure track faculty are missing data on the questions pertaining only to part-time faculty.  But generally, missingness is not of too much concern.
* Faculty working at two or more institutions are more likely to teach more courses
* Amount part-time faculty paid "per course" (at current inst) is related to their base institutional salary
* Faculty working at more than one institution get more of their pay from those institutions
* Satisfaction with workplace is related to satisfaction with compensation.
* As base salary decreases, faculty earn more from non-academic sources
* The more faculty work at other institutions, the more of their income come from "other institutions."
* These findings are intuitive and logical.  They affirm the data quality.


## Outcome of interest: Turnover intentions{#turnintent}

The turnover intentions of a faculty member is defined as when any of the following apply:
  * In the past two years, has the faculty member considered:
  * Considered early retirement?
* Considered leaving academe for another job?
* Considered leaving this institution for another?

```{r}
table(df$TURNINTENT)
prop.table(table(df$TURNINTENT, df$FULLSTAT))
```

```{r, results='hide'}
factors <- sapply(df, is.factor)
lapply(names(df[,factors])[-110], function(x) prop.table(table(df[,x],df[,"TURNINTENT"],dnn=c(x,"TURNINTENT")),1)) 
```

```{r, results='hide'}
lapply(names(df[,factors]), function(x) round(prop.table(table(df[,x],df[,"SATIS19"],dnn=c(x,"SATIS19")),1),2))
```

* us citizens think more about turnover
* once mobile, always mobile 81
* inst descriptors.  those saying faculty respect each other are more likely to stay
* administrators care then they are more likely to stay

## Intermediary of interest: Job satisfaction{#satis}
```{r, results='hide'}
df %>% 
  select(starts_with("SATIS"),-SATIS_WORKPLACE,-SATIS_COMPENSATION) %>% 
  names() %>% 
  lapply(function(x){round(prop.table(table(df[,x],df[,"TURNINTENT"],dnn=c(x,"TURNINTENT")),1),2)})
```

