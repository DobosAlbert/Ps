sample=scan("sample2.txt")

outliers_mean = function(smp)
{
  m=mean(smp)
  s=sd(smp)
  outliers=vector()
  j=0
  for(i in 1:length(sample))
    if(sample[i]<m-2*s || sample[i]>m+2*s)
    {
      j = j + 1
      outliers[j] = sample[i]
    }
  outliers
}

outliers_iqr = function(smp)
{
  q1=as.vector(quantile(smp))[2]
  q3=as.vector(quantile(smp))[4]
  iqr=q3-q1
  
  outliers=vector()
  j=0
  for(i in 1:length(sample))
    if(sample[i]<q1-1.5*iqr || sample[i]>q3+1.5*iqr)
    {
      j = j + 1
      outliers[j] = sample[i]
    }
  outliers
}
summary(sample)

outliers_mean(sample)
outliers_iqr(sample)
