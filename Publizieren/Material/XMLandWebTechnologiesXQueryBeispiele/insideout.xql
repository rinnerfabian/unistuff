declare namespace rcp = "http://www.brics.dk/ixwt/recipes";
<ingredients> 
  { for $i in 
      distinct-values(fn:doc("recipes.xml")//rcp:ingredient/@name)
    return <ingredient name="{$i}">
             { for $r in fn:doc("recipes.xml")//rcp:recipe
               where $r//rcp:ingredient[@name=$i]
               return <title>{$r/rcp:title/text()}</title>
             }
           </ingredient>
  }
</ingredients>
