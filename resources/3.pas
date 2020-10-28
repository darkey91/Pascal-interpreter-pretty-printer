program HelloWorld;
var name : string;
    tmp : integer;
    r : real;

    procedure kek;
    var r : real;
    begin
        r := 42.0;
    end;

begin
    writeln('Enter your name');
    readln(name);
    writeln('Hello, ', name);

    tmp := 42;
    if (tmp <> 42 or not (tmp = 42)) then
        writeln('Waaat');

    r := 1.0;
    kek;
    if (r <> 1.0) then
        writeln('Waaaaaat2');
end.