# To Do List

- Add plotting of ratios
- Test on confluence assays
- Develop vignette
- Rigorous testing of helpers. Need test data though


Code used for testing

```
f <- c(growth = "../Incucyte/Vessel 897 T47D 2B Kate sytox/Vessel 897 T47D 2B Kate sytox green Red.txt", death = "../Incucyte/Vessel 897 T47D 2B Kate sytox/Vessel 897 T47D 2B Kate sytox Green radius 10.txt")
map <- "../Incucyte/Vessel 897 T47D 2B Kate sytox/Vessel 897 T47D 2B Kate sytox caspase.PlateMap"
ie <- IncucyteExperiment(f, map)
ie2 <- IncucyteExperiment(f)
```
