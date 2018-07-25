declare namespace b = "http://businesscard.org";

declare function local:t-card($x) {
  <html>
    <head>
      <title> { $x/b:name/text() } </title>
    </head>
    <body bgcolor="#ffffff">
      <table border="3" bgcolor="#00ffff">
        <tr>
          <td>
            { local:t-name($x/b:name) },
            { local:t-title($x/b:title) },
            <tt>{ local:t-email($x/b:email) }</tt><br/>
            { if ($x/b:phone)
              then (text{ "Phone:" }, 
                    local:t-phone($x/b:phone), 
                    element br {()})
              else ()
            }
          </td>
          <td>
            { if ($x/b:logo)
              then element img { attribute src { $x/b:logo/@uri } }
              else ()
            }
          </td>
        </tr>
      </table>
    </body>
  </html>
};

declare function local:t-name($x)  { $x/text() };
declare function local:t-title($x) { $x/text() };
declare function local:t-email($x) { $x/text() };
declare function local:t-phone($x) { $x/text() };

local:t-card(fn:doc("card.xml")/b:card)
