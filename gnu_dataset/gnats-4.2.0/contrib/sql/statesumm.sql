
select statenum, statestr, count(*)
  from gnats, statecode
  where gnatsstate=statenum and
        gnatsclass < 5
  group by statestr, statenum
  order by statenum;

select " ", "Total", count(*)
  from gnats
  where
       gnatsclass <5;
