declare variable $inclLogo := xs:boolean("true");
<card>
<name>Jane Doe</name>
<title>CEO, Widget Inc.</title>
<email>jane.doe@widget.com</email>
{if ($inclLogo) then <logo/> else ""}
</card>