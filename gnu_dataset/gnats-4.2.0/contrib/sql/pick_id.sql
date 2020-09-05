select gnatsid, severity, age, customer, synopsis from gnats
  where gnatsid = "p0000355" or
        gnatsid = "p0000383" or
        gnatsid = "p0000389" or
        gnatsid = "p0000553" or
        gnatsid = "p0001048"
  order by gnatsid
