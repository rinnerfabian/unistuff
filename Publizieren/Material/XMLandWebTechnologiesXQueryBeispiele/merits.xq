for $s in doc("students.xml")//student
order by 
  count($s/results/result[fn:contains(@grade,"A")]) descending,
  fn:count($s/major) descending,
  xs:integer($s/age/text()) ascending
return ($s/name/text(), "; ")
