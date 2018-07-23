<doubles> {
for $s in fn:doc("students.xml")//student
let $m := $s/major
where count($m) ge 2
order by $s/@id ascending
return <double>
         <student>{$s/name/text()}</student>
         <majors>{for $singleM in $m return <maj>{$singleM/text()}</maj>}</majors>
       </double>
}
</doubles>