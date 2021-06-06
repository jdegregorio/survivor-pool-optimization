# NFL Survival Pool Pick Optimization using Genetic Algorithm

## Introduction

### Objective
Optimize the strategy/approach for selecting teams in NFL survival pools.

### General Background
Survivor pools are are sports betting game where each participant in the pool selects just a single team each week that they beleive win. If the selected team loses, the partipapant is eliminated from the pool, if the selected team wins, then the participant advances (i.e. "survives") to the next week.  The participant that survives the longest will win (or split) the prize money.

The catch to this game is that each participant may only select each team *once* the entire season. This presents each participant with an interesting decision.  Shall they select the best teams first, improving the probability of advancing in the first several weeks, or keep some of these teams on reserve for later weeks?

### Additional Complexity
The strategy even becomes more interesting when you consider the following:
  * How might teams power rankings change throughout the season (injuries, etc.)?
  * Will picking the "popular" or "higest-probability" matchup differentiate yourself enough to become the sole remaining survivor?
  * How do multiple entries per participant affect the strategy?
  
  
## Strategy
The high-level strategy involves utilizing a genetic algorithm to evolve a winning strategy.  Different stratigic "genes" will be designed to prioritize different types of information in the pick selection process.  Each strategy will be simulated in a back-test using historical data.

The core data utilized by the strategy will include the following:
  * Projected win probabilities of each game for the rest of the season (using ELO method) including strengh-of-schedule look-ahead.
  * Survivor pick distribution data (poll data available before each week starts)
  * Pool competition data (i.e. how many remaining to beat, what teams do they have left to pick from, etc.)
  
The "genes" that will be used to differentiate strategies include these attributes:
  * Visibility (i.e. how many weeks ahead to consider in planning)
  * Aggression (i.e. how aggressively should the strategy try to differentiate from the crowd)
  * Diversity (i.e. for multi-pick strategies, how similar/different should picks be for a given week)
  * ELO Adjustment Rate (i.e. are team's relative strengths adjusted quickly or slowely based on recent outcomes)
  
  *Each of the above genes to be defined/adjusted on a week-to-week basis (i.e. start the season with high visibility, and end the season with low visibility*
  
## Data Sources
  * Historical NFL Game Results:  https://www.pro-football-reference.com/
  * Historical Surivor Pool Pick Distributions:  https://www.survivorgrid.com/


## Notes

Fitness/Reward:  The estimated amount of money collected at the end of the survivor league. This will be a factor of how many participants are left. By including the possibility of splitting the pot with a tie, it helps steer the optimal policy away from simple survival which might tend to align with 

States:
  * Past Knowledge
    * Previous picks by each player and/or remaining teams available for each player.
    * NFL Matchup outcomes

  * Present Knowledge 
    * Survivor status of other players, remaining players in pool
    * Week of season or weeks remaining
    * Estimated pick distributions
    * Expected-value scores

  * Future Knowledge
    * Schedule of upcoming nfl games
    * Matchup data/stats for each upcoming game (ELO of each team, win probabilities, total matchup ELO)


Actions:
  * Labeled Team
  * Note: Team-related feature data is stored in state-based features
