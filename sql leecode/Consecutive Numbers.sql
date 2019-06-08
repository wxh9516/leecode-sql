Write a SQL query to find all numbers that appear at least three times consecutively.
+----+-----+
| Id | Num |
+----+-----+
| 1  |  1  |
| 2  |  1  |
| 3  |  1  |
| 4  |  2  |
| 5  |  1  |
| 6  |  2  |
| 7  |  2  |
+----+-----+
For example, given the above Logs table, 1 is the only number that appears consecutively for at least three times.
+-----------------+
| ConsecutiveNums |
+-----------------+
| 1               |
+-----------------+

###
use test_one;
Create table Logs(
    Id INT,
    Num INT
);
Insert into Logs (Id, Num) Values
(1,1),
(2,1),
(3,1),
(4,2),
(5,1),
(6,2),
(7,2);

###
select Num,count(Num) as ConsecutiveNums
from Logs group by Num;

with cte as (select Num,count(Num) as ConsecutiveNums
from Logs group by Num)
select Num,ConsecutiveNums from cte 
where ConsecutiveNums>3;    #or >=3 ?