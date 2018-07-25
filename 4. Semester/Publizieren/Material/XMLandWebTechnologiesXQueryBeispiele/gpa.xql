declare function local:gpa($s) {
  fn:avg(for $g in $s/results/result/@grade return local:grade($g))  
};

declare function local:grade($g) {
  if ($g="A") then 4.0 else if ($g="A-") then 3.7
  else if ($g="B+") then 3.3 else if ($g="B")  then 3.0
  else if ($g="B-") then 2.7 else if ($g="C+") then 2.3
  else if ($g="C")  then 2.0 else if ($g="C-") then 1.7
  else if ($g="D+") then 1.3 else if ($g="D")  then 1.0
  else if ($g="D-") then 0.7 else 0
};

<gpas>
  { for $s in fn:doc("students.xml")//student 
    return <gpa id="{$s/@id}" gpa="{local:gpa($s)}"/>
  }
</gpas>
