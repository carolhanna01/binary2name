select category,
       count(*) count, avg(age) avg_age, max(age) max_age ,min(age) min_age
  from gnats
  where
      gnatsstate < 3
  and gnatsclass < 4
  and severity = 1
  group by category
  order by category;

select category, respengr, gnatsid, age, customer
  from gnats
  where
      gnatsstate < 3
  and gnatsclass < 4
  and severity = 1
  order by category, respengr, age;
