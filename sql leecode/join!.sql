Suppose that a website contains two tables, the Customers table and the Orders table. Write a SQL query to find all customers who never order anything.
Table: Customers.
+----+-------+
| Id | Name  |
+----+-------+
| 1  | Joe   |
| 2  | Henry |
| 3  | Sam   |
| 4  | Max   |
+----+-------+
Table: Orders.
+----+------------+
| Id | CustomerId |
+----+------------+
| 1  | 3          |
| 2  | 1          |
+----+------------+
Using the above tables as example, return the following:
+-----------+
| Customers |
+-----------+
| Henry     |
| Max       |
+-----------+

###
show databases;
create database test_one;
use test_one;
show tables;

Create table Customers (
    Id INT,
    Name VARCHAR(50)
);
Create table Orders (
    Id INT,
    CustomerId INT
);
Insert into Customers (Id, Name) Values (1, 'Joe'), (2, 'Henry'), (3, 'Sam'), (4, 'Max');
Insert into Orders (Id, CustomerId) Values (1, 3), (2, 1);

###
Select C.Name As Customers
From Customers C left Join Orders O
On C.Id = O.CustomerId
Where O.CustomerId is null;

#or
Select Name As Customers from Customers
Where Name Not In (
    Select Name from Customers C JOIN Orders O 
    ON C.Id = O.CustomerId
);
#JOIN table on ...

