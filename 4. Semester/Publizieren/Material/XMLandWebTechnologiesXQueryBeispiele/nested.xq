for $a in 1 to 3
for $b in 1 to 2
where $a * $b > 1
return for $c in 1
       let $d :=($a, $b, $c)
       return <X>{ $d }</X>