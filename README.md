# Topic_modelling_AAPL_earnings
## 1. Overview
US listed firms are regulated by the SEC and are expected to submit their SEC filings on a regular basis. These reports provide information on firms’ financial profile and include non-trivial disclosure on firms’ operating style. Hence, SEC filings play an essential role in analyzing a firm’s future performance.
In this project, we want to study the feasibility in analyzing SEC filing using NLP and machine learning techniques. Roughly speaking, there are 4 problems to study:
1. Can NLP or Machine Learning find investment signals from SEC filing
2. Whether past research can still capture trading signals in recent years
3. Improvement on past research
4. Can we generate generic qualitative information on the SEC filing
## 2. Literature Review
In 2015, Cohen, Malloy and Nguyen proposed the idea of “​Lazy Prices​”, which capture trading signals by analyzing the change in linguistic style in the MD&A section of SEC filing. This paper can serve as a starting point for us to answer problems 1 and 2.
In 2017, Robert Musters proposed an interesting way to do ​topic detection using neural networks​. Although the microblogs dataset has a structural difference with the SEC
filing, the text MD&A section is more concise and similar to the microblogs data used in this paper. This paper can serve as a starting point for us to answer problem 4.

## 3. Data Preparation
Kaggle has a convenient ​dataset​ in summarizing links to firms’ SEC filings There is a ​R package​ that also provide the filing data

## 4. Research Process
a. Replicate the result ​in this post​ in Python.
b. Topic modeling using the same dataset
c. Fine tune on the model and make improvement
