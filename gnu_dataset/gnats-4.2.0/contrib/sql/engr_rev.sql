select respengr,
       count(*), avg(age), max(age), min(age)
  from gnats
  where
      gnatsstate < 3
  and gnatsclass < 4
  and severity = 1
  group by respengr
  order by respengr;

select respengr, gnatsid, age
  from gnats
  where
      gnatsstate < 3
  and gnatsclass < 4
  and severity = 1
  order by respengr, age;

select respengr,
       count(*), avg(age), max(age), min(age)
  from gnats
  where
      gnatsstate < 3
  and gnatsclass < 4
  and severity = 2
  group by respengr
  order by respengr;

