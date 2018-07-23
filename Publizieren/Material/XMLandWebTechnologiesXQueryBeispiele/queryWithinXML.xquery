<result>{
for $n in 1 to 150
return <number id = "{$n}" roman = "{format-integer($n, "I")}">{format-integer($n, "w")}</number>
}</result>