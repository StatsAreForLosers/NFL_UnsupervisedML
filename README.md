Top Defenses
================
Alex Chitsazan
09 August, 2018

-   [Top Defenses](#top-defenses)

Top Defenses
------------

Recently took a workshop at the University of Washington on unsupervised machine learning so I decided to apply it to sports. What I did is I used a webscraper that downloaded and scraped the team defense data for the top 5 defenses for every team from 1985 to Presenst. I then scaled and normalized the data and applied a deminsion reduction (multidemensional scaling) algorythm to try and represent the data in a 2D plot. What this multidimensional analysis does is take all of the traits (in this case defensive traits such as yards against, 1st down %, etc, - I'll post the raw table below) and calculates a value of all combined traits in 2 dimensions to plot relative distances, essentially a similarity score between every team in the dataset.

Then I applied a heerierachical clustering algorithm to group defenses into clusters of teirs. In the plot these will be the different colors The colors take these distances and build a tree that puts the most similar teams into groups of related they are. So teams like the 1985 Bears, 1991 Eagles and the 2000 ravens are put in the same group cause they are really really good. It does this for every team and places them in clusters. You can think of these as tiers. This is what the colors are and is called hierarchal clustering.

Some things that I find very interesting:

1.  Wow Ravens 2000 was an all time great. Huge outlier.
2.  Cluster 1 clearly all time great. 85 bears, 2013 seahawks, 2000 Ravens, 2008 Steelers, 1991 Eagles, etc.
3.  Browns\_2011 and patriots\_1988 it looks like may have been outliers (not as good) when compared to top defenses.
4.  Jags and Vikings from 2017 both made top cluster

![](README_files/figure-markdown_github/Defenses-1.png)
