# *Exercises for Programmers* in Haskell

## Overview
A personal project to solve the exercises from the book *Exercises for Programmers* in Haskell.

### Solved Exercises
#### Chapter 2: Input, Processing, and Output
| Exercise | dependencies | Memo  | Lv.  |
| -------- | -------------| ----- | -----:|
| Ex01 [x] [Saying Hello](ex01/Main.hs)                      | | |1(1)
| Ex02 [x] [Counting the Number of Characters](ex02/Main.hs) | string-interpolate | |1(1)
| Ex03 [x] [Printing Quotes](ex03/Main.hs)                   | | |1(1)
| Ex04 [ ] [Mad Lib](ex04/Main.hs)                           | | |
| Ex05 [ ] [Simple Math](ex05/Main.hs)                       | | |
| Ex06 [x] [Retirement Calculator](ex06/Main.hs)             | time ||2(1)
#### Chapter 3: Calculations
| Exercise | dependencies | Memo  | Lv.  |
| -------- | -------------| ----- | -----:|
| Ex07 [x] [Area of a Rectangular Room](ex07/Main.hs)    | transformers ||1(1)
| Ex08 [ ] [Pizza Party](ex08/Main.hs)                   | |
| Ex09 [ ] [Paint Calculator](ex09/Main.hs)              | |
| Ex10 [ ] [Self-Checkout](ex10/Main.hs)                 | |
| Ex11 [ ] [Currency Conversion](ex11/Main.hs)           | |
| Ex12 [ ] [Computing Simple Interest](ex12/Main.hs)     | |
| Ex13 [ ] [Determining Compound Interest](ex13/Main.hs) | |
#### Chapter 4: Making Decisions
| Exercise | dependencies | Memo  | Lv.  |
| -------- | -------------| ----- | -----:|
| Ex14 [x] [Tax Calculator](ex14/Main.hs)                 |transformers, regex-tdfa, text |money, rounding |2(1)|
| Ex15 [ ] [Password Validation](ex15/Main.hs)            | | | |
| Ex16 [ ] [Legal Driving Age](ex16/Main.hs)              | | | |
| Ex17 [ ] [Blood Alcohol Calculator](ex17/Main.hs)       | | | |
| Ex18 [ ] [Temperature Converter](ex18/Main.hs)          | | | |
| Ex19 [ ] [BMI Calculator](ex19/Main.hs)                 | | | |
| Ex20 [ ] [Multistate Sales Tax Calculator](ex20/Main.hs)| | | |
| Ex21 [ ] [Numbers to Names](ex21/Main.hs)               | | | |
| Ex22 [ ] [Comparing Numbers](ex22/Main.hs)              | | | |
| Ex23 [ ] [Troubleshooting Car Issues](ex23/Main.hs)     | | | |
#### Chapter 5: Functions
| Exercise | dependencies | Memo  | Lv.  |
| -------- | -------------| ----- | -----:|
| Ex24 [x] [Anagram Checker](ex24/Main.hs)                 | quickcheck | PBT |2(1)|
| Ex25 [ ] [Password Strength Indicator](ex25/Main.hs)     | | | |
| Ex26 [ ] [Months to Pay Off a Credit Card](ex26/Main.hs) | | | |
| Ex27 [ ] [Validating Inputs](ex27/Main.hs)               | | | |
#### Chapter 6: Repetition
| Exercise | dependencies | Memo  | Lv.  |
| -------- | -------------| ----- | -----:|
| Ex28 [x] [Adding Numbers](ex28/Main.hs)        | | |2(1)|
| Ex29 [ ] [Handling Bad Input](ex29/Main.hs)    | | | |
| Ex30 [ ] [Multiplication Table](ex30/Main.hs)  | | | |
| Ex31 [ ] [Karvonen Heart Rate](ex31/Main.hs)   | | | |
| Ex32 [ ] [Guess the Number Game](ex32/Main.hs) | | | |
#### Chapter 7: Data Structures
| Exercise | dependencies | Memo  | Lv.  |
| -------- | -------------| ----- | -----:|
| Ex33 [x] [Magic 8 Ball](ex33/Main.hs)          | exceptions, haskeline, random | |2(1)|
| Ex34 [ ] [Employee List Removal](ex34/Main.hs) | | | |
| Ex35 [ ] [Picking a Winner](ex35/Main.hs)      | | | |
| Ex36 [ ] [Computing Statistics](ex36/Main.hs)  | | | |
| Ex37 [ ] [Password Generator](ex37/Main.hs)    | | | |
| Ex38 [ ] [Filtering Values](ex39/Main.hs)      | | | |
| Ex39 [ ] [Sorting Records](ex39/Main.hs)       | | | |
| Ex40 [ ] [Filtering Records](ex40/Main.hs)     | | | |
#### Chapter 8: Working with Files
| Exercise | dependencies | Memo  | Lv.  |
| -------- | -------------| ----- | -----:|
| Ex41 [x] [Name Sorter](ex41/Main.hs)           | | |2(2)|
| Ex42 [ ] [Parsing a Data File](ex42/Main.hs)   | | | |
| Ex43 [ ] [Website Generator](ex43/Main.hs)     | | | |
| Ex44 [ ] [Product Search](ex44/Main.hs)        | | | |
| Ex45 [ ] [Word Finder](ex45/Main.hs)           | | | |
| Ex46 [ ] [Word Frequency Finder](ex46/Main.hs) | | | |
#### Chapter 9: Working with External Services
| Exercise | dependencies | Memo  | Lv.  |
| -------- | -------------| ----- | -----:|
| Ex47 [ ] [Who’s in Space?](ex47/Main.hs)                | |     
| Ex48 [ ] [Grabbing the Weather](ex48/Main.hs)           | |          
| Ex49 [x] [Flickr Photo Search](ex49/Main.hs)            | aeson, gi-gtk, http-client, bytestring, text | GUI/GTK3, HTTP client, multi-thread |5(4)
| Ex50 [ ] [Movie Recommendations](ex50/Main.hs)          | |           
| Ex51 [ ] [Pushing Notes to Firebase](ex51/Main.hs)      | | 
| Ex52 [ ] [Creating Your Own Time Service](ex52/Main.hs) | |  |
#### Chapter 10: Full Programs
| Exercise | dependencies | Memo  | Lv.  |
| -------- | -------------| ----- | -----:|
| Ex53 [ ] [Todo List](ex53/Main.hs)          | | |
| Ex54 [ ] [URL Shortener](ex54/Main.hs)      | | |
| Ex55 [ ] [Text Sharing](ex55/Main.hs)       | | |
| Ex56 [x] [Tracking Inventory](ex56/Main.hs) |scotty, lucid, text, aeson, aeson-casing, bytestring, directory | Web App | 4(3)
| Ex57 [ ] [Trivia App](ex57/Main.hs)         | | |

- ※ [x] Completed, [ ] Pending
- ※ Lv. (level): p(q), where:
  - p: Programming skill level used to solve the exercise (1 = beginner, 5 = advanced).
  - q: Complexity of the exercise requirements (1 = simple, 5 = complex).

  Note: Even if the requirement level (q) is low, the programming level (p) might be higher if advanced techniques are intentionally used for learning purposes.

## Technologies Used
- ghc 9.8.4

### Dependency
- string-interpolate *
- transformers *
- scotty
- lucid
- text *
- aeson
- aeson-casing
- bytestring
- directory

## How to Run
Run the following directly under the project.
```
$ stack run ex[nn]
```

## How to Run Test
```
$ stack test :ex[nn]-test
```
----
Individual details are described in the README.md file of each respective folder.

### Example
```
$ stack run ex03
...
What is the quote? These aren't the droids you're looking for.
Who said it? Obi-Wan Kenobi
Obi-Wan Kenobi says, "These aren't the droids you're looking for."

$ stack test :ex03-test
exercises-for-programmer-in-haskell> test (suite: ex03-test)
                                                 
+++ OK, passed 100 tests.

exercises-for-programmer-in-haskell> Test suite ex03-test passed
Completed 2 action(s).
```

## Notes
- I relied on Vibe Coding just a little bit.

## References
- [Exercises for Programmers](https://www.oreilly.com/library/view/exercises-for-programmers/9781680501513/)
