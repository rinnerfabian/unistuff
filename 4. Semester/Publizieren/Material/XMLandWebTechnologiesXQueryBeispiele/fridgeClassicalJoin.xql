declare namespace rcp = "http://www.brics.dk/ixwt/recipes";
for $i in fn:doc("recipes.xml")//rcp:recipe//rcp:ingredient/@name
for $s in fn:doc("fridge.xml")//stuff
where $s/text()=$i
return <result><recipe>
{$i/ancestor::rcp:recipe/rcp:title/text()}
</recipe><ingr>{$i}</ingr></result>
