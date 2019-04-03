# CS3110 Midterm Project Fall 2018
Project submission by Ivana Terziyska (it76), April Ye (yy459), Karson Daecher (kbd45), and Oren Michaely (om72)

## Project Description 
We implemented a flashcard simulation that allows users to study for classes via a CSV file containing terms and definitions. Some key features of our program includes:  
  * Shuffle order of flashcards (front = terms, back = definitions)
  * Star flashcards to be able to specifically either practice or test over problematic cards
  * Unstar flashcards when user has successfully mastered the flashcard material
  * Practice specifically over incorrectly answered flashcards during test
  * Practice Mode
    * Flip flashcard to see either term or definition
  * Test Mode
    * Allows users to choose typo mode or not (allow for typos when answering)
    * Simple algorithm to check for typos via fuzzy matching
    * Show stats of user performance after test
    * Keeps track of and updates high score after every test
    
## Division of Labor
Hours were evenly distributed over the course of 3 weeks, of which pair programming was the primary approach towards implementation. Repo consists of the final week's implementations for flashcards, which was built on top of A7 and A6 (first and second weeks') changes. Each member contributed approx 45 hours.
