# Bitcoin Sentiment Analysis

This project aimed to uncover relationships between the price of bitcoin and the conversation activity around bitcoin across the social media websites, Twitter and Reddit. In the end, we concluded the sentiment of Bitcoin conversation is not a clear indicator of price, however, volume of conversation activity is. 

## Project Structure


Final Report: Contains the final report 
Progress Report: Our progress to date aroun 20 days into the assignment
Project Proposal: The proposal originally submitted for our project
Proposal Presentation: Video Recording of our project proposal
Code: Working copies of code files
Data: Ignored file for storing large datasets
Final Code: Final code files for execution
Final Presentation Slides: Final presentation slides for submission

## Installation

### Raw Data
First, the source data sets must be installed and stored in '../Data/'

Reddit Source: https://www.kaggle.com/datasets/jerryfanelli/reddit-comments-containing-bitcoin-2009-to-2019
Store at: '../Data/bitcoin_reddit_all.csv'

Twitter Source: https://www.kaggle.com/datasets/jaimebadiola/bitcoin-tweets-and-price
Store at: '../Data/twitter_data.csv'

### Data Prep
In order for the workbooks to have the data they need, they must be executed in this order:

1. ../Final Code/Sentiment Analysis.ipynb
2. ../Final Code/data_prep.Rmd
3. ../Final Code/eda.Rmd
4. ../Final Code/regression_analysis.Rmd

The files Generated were then used to complete our final report found at ../Final Report

