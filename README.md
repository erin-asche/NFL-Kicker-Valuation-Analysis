NFL Kicker Valuation Analysis

Project Overview:
This project analyzes NFL kicker compensation by integrating player performance, age, and contract data. Using Hierarchical clustering techniques (DIANA and AGNES), kickers were segmented into performance archetypes to evaluate whether player compensation aligned with on-field production. 

Tools & Technology Used:
  - R
  - NFLverse
  - AGNES clustering
  - DIANA clustering
  - dplyr
  - tidyr
  - Tableau


Business Question:
Can NFL kickers be grouped into meaningful performance and compensation
archetypes, and do certain groups deliver greater value relative to
their contracts?

Data Sources:
  - NFL play-by-play data (2021-2023) from NFLverse
  - NFL contract information from OverTheCap data available through NFLverse

Methodology:
1. Collected NFL kicker contract data and player information.
2. Loaded 2021-2023 field goal attempt data.
3. Calculated indoor and outdoor field goal percentages.
4. Measured environmental performance sensitivity.
5. Standardized performance metrics.
6. Applied DIANA and AGNES hierarchical clustering.
7. Integrated age and contract information.
8. Created custom contract value metrics.

Key Metrics:
 - Indoor FG%
 - Outdoor FG%
 - Average kick distance
 - Environmental performance difference
 - Age
 - Average Per Year (APY)
 - Contract value
 - Guaranteed money
 - Performance-per-dollar value score

Skills Demonstrated:
 - Data Cleaning
 - Data Integration
 - Sports Analytics
 - Hierarchical Clustering
 - Feature Engineering
 - Contract Valuation Analysis
 - Statistical Analysis
 - Data Visualization

Key Findings:
Three primary kicker archetypes emerged:

Cluster 1: Veteran High Performance:
 - Average Field Goal %: 88.6%
 - Average Age: 34.1
 - Average Per-Year Salary: $ 3.22 M
 - Highest-performing group with premium compensation levels
  
 Cluster 2: Aging Veteran:
 - Average Field Goal %: 84.3%
 - Average Age: 38.4
 - Average Per-Year Salary: $2.39 M
 - Older, established kickers with moderate compensation and performance
  
Cluster 3: Developmental / Low-cost:
 - Average Field Goal %: 66.1%
 - Average Age: 29.8
 - Average Per-Year Salary: $0.36 M
 - Generated the highest performance-per-dollar value score due to substantially lower compensation

Note:
Value scores were calculated by comparing average field goal percentage to average annual salary (APY), providing a measure of performance relative to compensation.
  
Challenges Encountered:
 - Merging player and contract datasets using unique identifiers
 - Resolving missing birth-date information
 - Standardizing variables before clustering
 - Comparing AGNES and DIANA approaches


Conclusion:
The analysis identified distinct compensation and performance archetypes among NFL kickers, demonstrating how clustering techniques can be used to evaluate contract efficiency and player value in professional sports.
