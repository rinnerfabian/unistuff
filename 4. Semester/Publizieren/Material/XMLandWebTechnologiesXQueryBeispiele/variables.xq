declare variable $a := 1;
(let $a:= 2
return ((let $a:= $a* $a
         return $a), $a)
,
$a)