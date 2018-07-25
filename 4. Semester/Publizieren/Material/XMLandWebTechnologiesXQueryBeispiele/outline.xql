declare namespace rcp = "http://www.brics.dk/ixwt/recipes";

declare function local:ingredients($i,$p) {
  fn:string-join(
    for $j in $i/rcp:ingredient
    return fn:string-join(
      ($p,$j/@name,"&#x0A;",
      local:ingredients($j,fn:concat($p,"  "))),
      ""),
    "")
};

declare function local:recipes($r) {
  fn:concat($r/rcp:title/text(),"&#x0A;",
            local:ingredients($r,"  "))
};

fn:string-join(
  for $r in fn:doc("recipes.xml")
            //rcp:recipe[5]
  return local:recipes($r),
  ""
)
