# Portfolio Manager App
### Authors
#### Key Vollers - keyvollers@unm.edu
#### Alex Bernal - abernal20@unm.edu
#### Barath Kurapati - barathkurapati@unm.edu
## Basic Trading Strategy
- Take the last year of data.
- Take the 52 week heigh and the 52 week low
- For the buy price ((high - low) * (1/4)) + low 
- For number of shares to buy ((amount of money in portfolio)/5)/(buy price) round to the nearest integer
- For the first sell price ((high - low) * (3/4)) + low
- Number of Shares to Sell  1= (Number of Shares owned)/4
- Second Sale Price = 52 week high
- Number of Shares to Sell 2 = (Number of Shares Owned)/4
