xquery version "3.0";

for $s in doc("studentsCopy.xml")//student
return (delete node $s/@id, insert node <id>{string($s/@id)}</id> as first into $s)