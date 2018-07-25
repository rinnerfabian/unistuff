declare namespace rcp = "http://www.brics.dk/ixwt/recipes";
for $r in fn:doc("recipes.xml")//rcp:recipe
for $i in $r//rcp:ingredient/@name
for $s in fn:doc("fridge.xml")//stuff[text()=$i]
return <result>
  <recipe>{$r/rcp:title/text()}</recipe>
  <ingr>{$i}</ingr>
  </result>