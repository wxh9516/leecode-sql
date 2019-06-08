Write a SQL query to get the second highest salary from the Employee table.
+----+--------+
| Id | Salary |
+----+--------+
| 1  | 100    |
| 2  | 200    |
| 3  | 300    |
+----+--------+
#For example, given the above Employee table, the query should return 200 as the 
#second highest salary. If there is no second highest salary, then the query should 
#return null.
+---------------------+
| SecondHighestSalary |
+---------------------+
| 200                 |
+---------------------+

###
use test_one;
drop table Employee;
Create table Employee(
    Id INT,
    Salary INT
);
Insert into Employee (Id, Salary) Values
(1, 100),
(2, 200),
(3, 300),
(4,200);

#
select max(Salary) as SecondaryHighest from Employee E
where Salary<(select max(Salary) from Employee);