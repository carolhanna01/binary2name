DATABASE engdb

#-----------------------------------------------------------------------------
#  globals definition
#-----------------------------------------------------------------------------

GLOBALS
  DEFINE	hist		RECORD
		  asof		DATE,
		  state		ARRAY [5] of INTEGER,
		  total		INTEGER
		END RECORD,
		i		INTEGER
END GLOBALS

#-----------------------------------------------------------------------------
#  main
#-----------------------------------------------------------------------------

MAIN

let hist.asof=today
let hist.total=0

for i = 1 to 5
  select count(*)
    into hist.state[i]
    from gnats
    where
          gnatsclass < 5 and
	  gnatsstate = i
  let hist.total=hist.total+hist.state[i]
end for

insert into history
  values (hist.asof, hist.state[1], hist.state[2], hist.state[3], hist.state[4], hist.state[5], hist.total)

END MAIN

