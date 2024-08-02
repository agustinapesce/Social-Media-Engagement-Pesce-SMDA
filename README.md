Replication of the paper: Out-group animosity drives engagement on social media (Rathje et al., 2021) 

1. Motivation

On Facebook, news can be found incidentally while using online sources for other reasons, making the study of its reach and diffusion dynamics relevant. Given the consequences that content publishers' decisions and sharing actions have for post visibility, this research aims to replicate previous work by Rathje et al. (2021) on the relationship between posts “share” actions and the verbal content in them on Facebook. 

Particularly, the population of interest were Facebook pages corresponding to media companies and congress members of the liberal and conservative ideological spectrum. 

Following previous studies, the focus was on: 

- political outgroup-ingroup mentions, 
- positive and negative affect words and 
- moral-emotional remarks 

The association of diffusion with this kind of language is also of interest to assess whether social media platforms may incentivize speeches that are detrimental to social integration, such as outgroup derogation.  

2. Data  

Facebook posts of the media and current congress members were collected through CrowdTangle.

3. Variables

Variable of interest 

Shares (log). The count of shares for the posts was obtained from the CrowdTangle datasets “Shares” columns. This variable was log-transformed to mitigate its skewed behavior. To avoid errors related to the logarithm of zero, one was added to all “Shares” instances before transformation. 

Independent variables 

Democrat and Republican dictionaries. Dictionaries to detect references to the democratic and republican parties and members were created.

Mentions count of positive and negative affect. Computational Affective Science has developed various methods for measuring message connotative meaning. In this research, positive and negative affect will be accounted for separately by adding the amount of positive and negative words instead of calculating a global score of message valence. This is implemented through public dictionaries from previous research. 

Mentions count of moral-emotional language. This variable is also measured through public dictionaries from previous research.

4. Methods 

Linear regression models with ordinary least squares (OLS) as fitting method were built for each of the described datasets (i.e., liberal media, conservative media, liberal congress members and conservative congress members). To calculate the confidence intervals for the model parameters 10,000 samples with replacement where the model was fitted were created. Afterwards, as the significance level was set at 5%, 2.5% and 97.5% percentiles were calculated to get the confidence intervals lower and upper bounds respectively. 

5. Repository

0_variable_op_pesce.ipynb: histograms with data distribution by date and dictionary counts workflow.

1_prepo_analysis_pesce.ipynb: data cleaning, regressions, bootstrapping, tables and graphs.

Social Media Engagement - Pesce SMDA.pdf: extensive report on the design and analyses.

Rathje, S., Van Bavel, J. J., & Van Der Linden, S. (2021). Out-group animosity drives engagement on social media. Proceedings of the National Academy of Sciences, 118(26), e2024292118. https://doi.org/10.1073/pnas.2024292118 
