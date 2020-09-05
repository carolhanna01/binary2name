select customer,
       count(*), avg(age), max(age), min(age)
  from gnats
  where
      gnatsstate < 3
  and gnatsclass < 4
  and severity = 1
  group by customer
  order by customer;

select customer, gnatsid, age
  from gnats
  where
      gnatsstate < 3
  and gnatsclass < 4
  and severity = 1
  order by customer, age;

select customer,
       count(*), avg(age), max(age), min(age)
  from gnats
  where
      gnatsstate < 3
  and gnatsclass < 4
  and severity = 2
  group by customer
  order by customer;

