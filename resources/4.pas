program Test;
var a, b, c, d: Integer;

function calculator(a, b, c, d: integer): integer;
begin
	calculator :=  a + b  * c - d;
end;

begin
readln(a,b,c,d);
writeln('a + b * c - d = ', calculator(a,b,c,d));
end.