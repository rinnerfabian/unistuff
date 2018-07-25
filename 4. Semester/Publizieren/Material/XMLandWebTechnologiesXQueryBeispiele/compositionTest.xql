(: Demo für Komponierbarkeit :)

for $x in ( <doubles> {
for $s in fn:doc("students.xml")//student
let $m := $s/major
where <xyz/>
order by $s/@id
return <double>
         { $s/name/text() }
       </double>
} </doubles> )
return $x/*[1]