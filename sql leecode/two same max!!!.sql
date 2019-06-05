The Employee table holds all employees. Every employee has an Id, a salary, and there is also a column for the department Id.
+----+-------+--------+--------------+
| Id | Name  | Salary | DepartmentId |
+----+-------+--------+--------------+
| 1  | Joe   | 70000  | 1            |
| 2  | Jim   | 90000  | 1            |
| 3  | Henry | 80000  | 2            |
| 4  | Sam   | 60000  | 2            |
| 5  | Max   | 90000  | 1            |
+----+-------+--------+--------------+
The Department table holds all departments of the company.
+----+----------+
| Id | Name     |
+----+----------+
| 1  | IT       |
| 2  | Sales    |
+----+----------+
Write a SQL query to find employees who have the highest salary in each of the departments. For the above tables, your SQL query should return the following rows (order of rows does not matter).
+------------+----------+--------+
| Department | Employee | Salary |
+------------+----------+--------+
| IT         | Max      | 90000  |
| IT         | Jim      | 90000  |
| Sales      | Henry    | 80000  |
+------------+----------+--------+


###
show databases;
use test_one;
show tables in test_one;

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
(5,'Jim',90000,1);

Create table Department(
    Id INT,
    Name varchar(20)
    );

Insert Into Department (Id, Name) Values
(1, 'IT'),
(2, 'Sales');

select * from Employee;

###
select D.Name as Department,E.Name as Employee,Salary from Employee E
join Department D on E.DepartmentId=D.Id
where (E.DepartmentId,Salary) in 
(
select E.DepartmentId, MAX(Salary) from Employee group by DepartmentId   #???
);
#if use 'group by E.DepartmentId',will be wrong,why?   -group by +colname?

select D.Name as Department,E.Name as Employee,Salary from Employee E
join Department D on E.DepartmentId=D.Id;

#or
SELECT
    Department.name AS 'Department',
    Employee.name AS 'Employee',
    Salary
FROM
    Employee
        JOIN
    Department ON Employee.DepartmentId = Department.Id
WHERE
    (Employee.DepartmentId , Salary) IN
    (   SELECT
            DepartmentId, MAX(Salary)
        FROM
            Employee
        GROUP BY DepartmentId
    );

#or
Select D.Name AS Department, T.Name AS Employee, T.Salary AS Salary
From Department D, 
    (
    Select Name, Salary, E.DepartmentId from Employee E JOIN (
    Select DepartmentId, Max(Salary) AS MaxSalary From Employee
    Group by DepartmentId) tmp
    ON E.DepartmentId = tmp.DepartmentId
    Where E.Salary = tmp.MaxSalary
    ) AS T
Where D.Id = T.DepartmentId;
