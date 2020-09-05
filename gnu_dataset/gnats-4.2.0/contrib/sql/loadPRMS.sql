
delete from gnats;

load from "/s1/users/smlieu/chron/bugs/reports/gnats.curr"
   insert into gnats (
     gnatsID,
     category,
     synopsis,
     confidential,
     severity,
     priority,
     respEngr,
     gnatsState,
     gnatsClass,
     customer,
     arrival,
     custContact,
     release
   );
