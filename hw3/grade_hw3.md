*Yue Wang*

### Overall Grade: 245/260

### Quality of report: 10/10

-   Is the homework submitted (git tag time) before deadline? Take 10 pts off per day for late submission.  

-   Is the final report in a human readable format html? 

-   Is the report prepared as a dynamic document (Quarto) for better reproducibility?

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how results are produced by just reading the report? Take some points off if the solutions are too succinct to grasp, or there are too many typos/grammar. 

    All yes

### Completeness, correctness and efficiency of solution: 206/210

- Q1 (29/30)
  
    - Q1.1 (14/15)
    
      Looks good, but a bit unreadable. `-1.0`
    
    - Q1.2 (15/15)

- Q2 (10/10)

    - Q2.1 (5/5)
    
    - Q2.2 (5/5) A bar plot of similar suffices.
    
- Q3 (23/25)    
    
    - Q3.1 (5/5)
    
    - Q3.2 (18/20) Student must explain patterns in admission hour, admission minute, and length of hospital display. Just describing the pattern is not enough. There are no wrong or correct explanations; any explanation suffices. 
    
      Need to guess why the unusual patterns occur for admission hour. `-2.0`
      
      Nice catch on the negative length of hospital stay!

- Q4 (14/15)        
    
    - Q4.1 (5/5)
    
    - Q4.2 (9/10) There's not much pattern in gender. But some explanations are expected for anchor age: what are they and why the spike on the right.
    
      Need to guess why there is a peak at age 91 and guess why. Read MIMIC-IV documentation Dr. Zhou picked up in hw3. `-1.0`
    
- Q5 (30/30) Check the final number of rows and the first few rows of the final data frame.

    You have one more row for some reason. The number of rows should be 68,467. `-0.0`

- Q6 (30/30) Check the final number of rows and the first few rows of the final data frame.

- Q7 (30/30) Check the final number of rows and the first few rows of the final data frame.

- Q8 (40/40) This question is open ended. Any graphical summaries are good. Since this question didn't explicitly ask for explanations, it's fine students don't give them. Students who give insights should be encouraged.
	    
### Usage of Git: 5/10

-   Are branches (`main` and `develop`) correctly set up? Is the hw submission put into the `main` branch?

-   Are there enough commits (>=5) in develop branch? Are commit messages clear? The commits should span out not clustered the day before deadline. 
          
-   Is the hw submission tagged? 

-   Are the folders (`hw1`, `hw2`, ...) created correctly? 
  
-   Do not put a lot auxiliary files into version control. 

    Less than 5 commits in the develop branch after hw1 submission. `-5.0`

### Reproducibility: 10/10

-   Are the materials (files and instructions) submitted to the `main` branch sufficient for reproducing all the results? Just click the `Render` button will produce the final `html`? 

-   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

    I was able to render it.

### R code style: 14/20

For bash commands, only enforce the 80-character rule. Take 2 pts off for each violation. 

-   [Rule 2.5](https://style.tidyverse.org/syntax.html#long-lines) The maximum line length is 80 characters. Long URLs and strings are exceptions.  

-   [Rule 2.4.1](https://style.tidyverse.org/syntax.html#indenting) When indenting your code, use two spaces.  

-   [Rule 2.2.4](https://style.tidyverse.org/syntax.html#infix-operators) Place spaces around all infix operators (=, +, -, &lt;-, etc.).  

-   [Rule 2.2.1.](https://style.tidyverse.org/syntax.html#commas) Do not place a space before a comma, but always place one after a comma.  

-   [Rule 2.2.2](https://style.tidyverse.org/syntax.html#parentheses) Do not place spaces around code in parentheses or square brackets. Place a space before left parenthesis, except in a function call.

    Lune 175, 231, 514. `-6.0`
    
    