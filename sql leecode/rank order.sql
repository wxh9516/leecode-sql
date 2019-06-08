#Write a SQL query to rank scores. If there is a tie between two scores, both should have the same ranking. Note that after a tie, the next ranking number should be the next consecutive integer value. In other words, there should be no "holes" between ranks.
+----+-------+
| Id | Score |
+----+-------+
| 1  | 3.50  |
| 2  | 3.65  |
| 3  | 4.00  |
| 4  | 3.85  |
| 5  | 4.00  |
| 6  | 3.65  |
+----+-------+
For example, given the above Scores table, your query should generate the following report (order by highest score):
+-------+------+
| Score | Rank |
+-------+------+
| 4.00  | 1    |
| 4.00  | 1    |
| 3.85  | 2    |
| 3.65  | 3    |
| 3.65  | 3    |
| 3.50  | 4    |
+-------+------+

###
use test_one;
Create table Scores(
    Id INT,
    Score Float
    );
Insert Into Scores (Id, Score) Values
(1, 3.5),
(2, 3.65),
(3, 4.0),
(4, 3.85),
(5, 4.0),
(6, 3.65);

show tables;

select Score,dense_rank() over (order by Score desc) as 'Rank'
from Scores;

#2
select Score,rank() over (order by Score desc) as 'Rank'
from Scores;

#difference between dense_rank() and rank():
#dense_rank:1 1 2 3 3 4
#rank(): 1 1 3 4 4 6