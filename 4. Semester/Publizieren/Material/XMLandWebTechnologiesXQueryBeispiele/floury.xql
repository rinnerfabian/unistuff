declare namespace rcp = "http://www.brics.dk/ixwt/recipes";
<floury>
  { for $r in 
      fn:doc("recipes.xml")//
      rcp:recipe[.//rcp:ingredient[@name="flour"]]
    return <dish>{$r/rcp:title/text()}</dish>
  }
</floury>
