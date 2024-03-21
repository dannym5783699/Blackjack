# Portfolio Manager App
### Authors
#### Key Vollers - keyvollers@unm.edu
#### Alex Bernal - abernal20@unm.edu
#### Barath Kurapati - barathkurapati@unm.edu

## Development Strategy
- Work on Milestones in order
- Individual Issues are all attached to milestones.
- If you add/find new issues make sure you attach them to a milestone.
- Assign yourself to the issue you are working on (it is okay for more then one of us to be working on the same issue)
- Hoomever finishes the issue first gets the Merge. (Someone should review your work before merging into main)
- Use Tags and the Merge Descriptions to indicate where you are at with the issue
- Link usefull information to the README under [Usefull Links](#usefull-links)

## Basic Trading Strategy
- Take the last year of data.
- Take the 52 week heigh and the 52 week low
- For the buy price ((high - low) * (1/4)) + low 
- For number of shares to buy ((amount of money in portfolio)/5)/(buy price) round to the nearest integer
- For the first sell price ((high - low) * (3/4)) + low
- Number of Shares to Sell  1= (Number of Shares owned)/4
- Second Sale Price = 52 week high
- Number of Shares to Sell 2 = (Number of Shares Owned)/4

## Usefull Links
- [Git-Repo](https://lobogit.unm.edu/keyvollers/haskell-final)
- [Current Milestone](https://lobogit.unm.edu/keyvollers/haskell-final/-/milestones/1#tab-issues) 