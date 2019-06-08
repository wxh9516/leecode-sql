Write a SQL query to get the nth highest salary from the Employee table.
+----+--------+
| Id | Salary |
+----+--------+
| 1  | 100    |
| 2  | 200    |
| 3  | 300    |
+----+--------+
#For example, given the above Employee table, the nth highest salary where n = 2 is 200. If there is no nth highest salary, then the query should return null.
+------------------------+
| getNthHighestSalary(2) |
+------------------------+
| 200                    |
+------------------------+

### guide
SELECT DISTINCT column1, column2, ...
FROM table_name; 
#to list different value in a column

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

#!!!
with CTE AS
(
    SELECT ID, Salary,
           ROW_NUMBER() OVER (ORDER BY Salary DESC) as RN
    FROM Employee
)
SELECT ID, Salary
FROM CTE
WHERE RN = 3;

#2
with cte as (
	select Id,Salary, row_number() over (order by Salary desc) as rn
    from Employee
)
select Id,Salary from cte where rn=2;

###...
Select Id, Salary, Dense_Rank() Over (Order by Salary Desc) From Employee;
Select Id, Salary, Rank() Over (Order by Salary Desc) From Employee;