declare namespace rcp = "http://www.brics.dk/ixwt/recipes";
for $r in fn:doc("recipes.xml")//rcp:recipe
where every $i in $r//rcp:ingredient/@name
      satisfies (some $s in fn:doc("fridge.xml")//stuff
            satisfies $s/text()=$i)
return ($r/rcp:title/text(), "; ")

