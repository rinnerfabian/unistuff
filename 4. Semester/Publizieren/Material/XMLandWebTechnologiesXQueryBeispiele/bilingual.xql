declare default element namespace "http://businesscard.org";
declare variable $lang:="Danish";
element { if ($lang="Danish") then "kort" else "card" } {
  element { if ($lang="Danish") then "navn" else "name" } 
    { text { "John Doe" } },
  element { if ($lang="Danish") then "titel" else "title" } 
    { text { "CEO, Widget Inc." } },
  element { "email" } 
    { text { "john.doe@widget.inc" } },
  element { if ($lang="Danish") then "telefon" else "phone" } 
    { text { "(202) 456-1414" } },
  element  logo  {
    attribute { "uri" } { "widget.gif" }
  }
}
