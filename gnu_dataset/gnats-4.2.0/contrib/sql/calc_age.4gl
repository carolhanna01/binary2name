
DATABASE engdb

#-----------------------------------------------------------------------------
#  globals definition
#-----------------------------------------------------------------------------

GLOBALS
  DEFINE
		arr_date	DATETIME year to minute,
		t_date		DATETIME year to hour,
		p_age		interval day(5) to hour,
		p_tag		CHAR(8)
END GLOBALS

#-----------------------------------------------------------------------------
#  main
#-----------------------------------------------------------------------------

MAIN

declare p_curs CURSOR for
  select arrival 
  from gnats
  where gnatsstate <5
for update

let t_date = extend (today, year to hour) + interval (8) hour to hour

{let p_tag = "TAGGED-4"}
{display t_date, p_tag}

foreach p_curs into arr_date
  let p_age = t_date - arr_date
  
  {display arr_date}
  {display p_age}

  update gnats 
    {set (age, tag) = (p_age, p_tag)}
    set (age) = (p_age)
    where current of p_curs
  whenever error stop
end foreach

END MAIN

