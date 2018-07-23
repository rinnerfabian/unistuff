for $i in 1 to 5
return element { "node" || $i} {attribute {"id" } { $i }, text { "Text " || format-integer($i, "I") }}