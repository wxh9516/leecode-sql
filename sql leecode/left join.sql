#Table: Person
#+-------------+---------+
#| Column Name | Type    |
#+-------------+---------+
#| PersonId    | int     |
#| FirstName   | varchar |
#| LastName    | varchar |
#+-------------+---------+
#PersonId is the primary key column for this table.

#Table: Address
#+-------------+---------+
#| Column Name | Type    |
#+-------------+---------+
#| AddressId   | int     |
#| PersonId    | int     |
#| City        | varchar |
#| State       | varchar |
#+-------------+---------+
#AddressId is the primary key column for this table.

#Write a SQL query for a report that provides the following information 
#for each person in the Person table, regardless if there is an address 
#for each of those people:
#FirstName, LastName, City, State

select Person.FirstName, Person.LastName, Adress.City, Adress.State 
from Person left join Adress 
on Person.PersonId=Adress.PersonId;

#or
select P.FirstName,P.LastName,A.City,A.State
from Person as P left join Address as A
on P.PersonId = A.PersonId










