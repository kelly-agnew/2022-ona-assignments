# 2022-ona-assignments
Class assignments for McGill MMA ORGB672


Current branch: Exercise 4
Task: Working with USPTO patent examiner data

1. Create variable for application processing time
‘app_proc_time’ that measures the number of days (or
weeks) from application filing date, until the final decision
on it (patented or abandoned)
2. Use linear regression models `lm()` to estimate the
relationship between centrality and `app_proc_time`
– Make sure to control for other characteristics of the examiner
that you think might influence that relationship
3. Does this relationship differ by examiner gender?
– Hint: Include an interaction term `gender x centrality` into
your models
4. Discuss your findings and their implication for the USPTO
