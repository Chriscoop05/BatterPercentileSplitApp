# BatterPercentileSplitApp
Shows savant percentiles for batters by certain splits such as L/R pitchers, home/away, etc..

1. Run sc2025_getdata.r first to get the necessary data involved in the app + the necessary mutations required to get the columns needed

2. load up the three plot files to ensure the functions are ready to be used

3. Load up your app

## Files

| File | Purpose |
|------|---------|
| `playerCardApp.R`| Builds the UI and Server for our App | 
| `sc2025_getdata.R` | Data loading, mutation, shared objects |
| `plot_contact.R` | Contact percentile plot function |
| `plot_batted_ball.R` | Batted ball percentile plot function |
| `plot_plate.R` | Plate discipline percentile plot function |

