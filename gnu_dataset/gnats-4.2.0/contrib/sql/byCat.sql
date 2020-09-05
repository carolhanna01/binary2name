select category, count(*) from gnats
  where
  gnatsclass < 4 and
  arrival > "92-05-01 00:00"
  and gnatsstate < 5
  group by category
  order by category
