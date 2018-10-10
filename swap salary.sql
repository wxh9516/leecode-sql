#Given a table salary, such as the one below, that has m=male and f=female values. Swap all f and m values (i.e., change all f values to m and vice versa) with a single update query and no intermediate temp table.

#For example:
#| id | name | sex | salary |
#|----|------|-----|--------|
#| 1  | A    | m   | 2500   |
#| 2  | B    | f   | 1500   |
#| 3  | C    | m   | 5500   |
#| 4  | D    | f   | 500    |
#After running your query, the above salary table should have the following rows:
#| id | name | sex | salary |
#|----|------|-----|--------|
#| 1  | A    | f   | 2500   |
#| 2  | B    | m   | 1500   |
#| 3  | C    | f   | 5500   |
#| 4  | D    | m   | 500    |

#1.
#UPDATE table_name SET col_name = new_value WHERE col_name = value_x;

#2.
create database test_one;
use test_one;

CREATE TABLE recipes (
  recipe_id INT NOT NULL,
  recipe_name VARCHAR(30) NOT NULL,
  PRIMARY KEY (recipe_id),
  UNIQUE (recipe_name)
);

INSERT INTO recipes 
    (recipe_id, recipe_name) 
VALUES 
    (1,"Tacos"),
    (2,"Tomato Soup"),
    (3,"Grilled Cheese");

select * from recipes;

show tables;

#select * from salary;
#select id, count(*) from salary GROUP BY id;
#select distinct * from salary;

#drop table salary;

CREATE TABLE salary (
  id INT NOT NULL,
  name VARCHAR(30) NOT NULL,
  sex varchar(30),
  salary int,
  PRIMARY KEY (id),
  UNIQUE (name)
);

select * from salary;
INSERT INTO salary 
    (id,name,sex,salary) 
VALUES 
    (1,'A','f',2500),
    (2,'B','m',1500),
    (3,'C','m',300),
    (4,'D','m',500);

UPDATE salary
SET sex = CASE WHEN sex = 'm' THEN 'f' 
                  WHEN sex = 'f' THEN 'm' END
WHERE id IN (1,2,3,4);   #???
#where id>0;

#2.1
UPDATE salary SET sex = IF(sex = 'm', 'f', 'm');   #???error

#IF(condition, value_if_true, value_if_false)

#3.
#CASE expression
#    WHEN condition1 THEN result1
#    WHEN condition2 THEN result2
#   ...
#    WHEN conditionN THEN resultN
#    ELSE result
#END

#4.
#SELECT column_name(s)
#FROM table_name
#WHERE column_name IN (value1,value2,...)
