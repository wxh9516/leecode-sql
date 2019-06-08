The Employee table holds all employees. Every employee has an Id, and there is also a column for the department Id.
+----+-------+--------+--------------+
| Id | Name  | Salary | DepartmentId |
+----+-------+--------+--------------+
| 1  | Joe   | 85000  | 1            |
| 2  | Henry | 80000  | 2            |
| 3  | Sam   | 60000  | 2            |
| 4  | Max   | 90000  | 1            |
| 5  | Janet | 69000  | 1            |
| 6  | Randy | 85000  | 1            |
| 7  | Will  | 70000  | 1            |
+----+-------+--------+--------------+
The Department table holds all departments of the company.
+----+----------+
| Id | Name     |
+----+----------+
| 1  | IT       |
| 2  | Sales    |
+----+----------+
Write a SQL query to find employees who earn the top three salaries in each of the department. For the above tables, your SQL query should return the following rows (order of rows does not matter).
+------------+----------+--------+
| Department | Employee | Salary |
+------------+----------+--------+
| IT         | Max      | 90000  |
| IT         | Randy    | 85000  |
| IT         | Joe      | 85000  |
| IT         | Will     | 70000  |
| Sales      | Henry    | 80000  |
| Sales      | Sam      | 60000  |
+------------+----------+--------+

###
use test_one;
drop table Employee;
Create table Employee(
    Id INT,
    Name varchar(20),
    Salary INT,
    DepartmentId INT
);
Insert Into Employee (Id, Name, Salary, DepartmentId) Values
(1, 'Joe', 70000, 1),
(2, 'Henry', 80000, 2),
(3, 'Sam', 60000, 2),
(4, 'Max', 90000, 1),
(5, 'Janet', 69000, 1),
(6, 'Randy', 85000, 1),
(7, 'Xiang', 70000, 1);

drop table Department;
Create table Department(
    Id INT,
    Name varchar(20)
    );
Insert Into Department (Id, Name) Values
(1, 'IT'),
(2, 'Sales');

###
select E.Name,Salary,D.Name as Department from Employee E
left join Department D on E.DepartmentId=D.Id;

with cte as (
select E.Name,Salary,D.Name as Department from Employee E
left join Department D on E.DepartmentId=D.Id
)
select Department,Name as Employee,Salary,
dense_rank() over (partition by Department order by Salary desc) as rk
from cte;

with mte as (
with cte as (
select E.Name,Salary,D.Name as Department from Employee E
left join Department D on E.DepartmentId=D.Id
)
select Department,Name as Employee,Salary,
dense_rank() over (partition by Department order by Salary desc) as rk
from cte
)
select Department,Employee,Salary from mte 
where rk<=3;

#partition by !!!