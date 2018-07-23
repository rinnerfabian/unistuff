declare function local:gen($n) {
  if ($n eq 0) then <bar/>
  else <foo>{ local:gen($n -1), local:gen($n -1) }</foo>
};
